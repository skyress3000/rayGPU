----------------------------------------------------------------------------
--
--  Rasterizer module
--
--  Revision History:
--     21 May 21  Ray Wendt         Initial revision.
--      7 Jun 21  Ray Wendt         Added pipeline.
--      8 Jun 21  Ray Wendt         Fixed syntax errors.
--      9 Jun 21  Ray Wendt         Connected output to FIFO and added done
--                                  bit pipeline.
--      9 Jun 21  Ray Wendt         Fixed discrepancy between signed pixel coords
--                                  and unsigned display coords.
--      9 Jun 21  Ray Wendt         Added depthbuffer clear state.
--      9 Jun 21  Ray Wendt         Connected triangle setup multiplier + 
--                                  reciprocal inputs.
--     10 Jun 21  Ray Wendt         Added reset to idle state.
--     10 Jun 21  Ray Wendt         Added pausing when FIFO full.
--     10 Jun 21  Ray Wendt         Fixed vertex data load address incrementing.
--     10 Jun 21  Ray Wendt         Fixed indexing during attr scaling.
--     12 Jun 21  Ray Wendt         Fixed typo 2->i in attr1 barycentric coord
--                                  multiplier.
--     13 Jun 21  Ray Wendt         Redesigned pipeline to only advance every
--                                  other clock instead of extra stages for
--                                  multiply, since that gets messed up when
--                                  FIFO full causes pipeline to pause and take
--                                  >1 clock to advance but the multiplier
--                                  remains always enabled
--     16 Jun 21  Ray Wendt         Added header comment
--
----------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.gpu_float.all;

-- rasterizer
--
--  Description:
--      This module implements the rasterizer for the GPU. The rasterize, once
--      triggered, loads shaded vertices from vertex data RAM in groups of 3.
--      The vertices are converted to NDC, and the NDC coordinates are cast to
--      integer pixel coordinates for the triangle, which are used to loop over
--      a rectangle containing the triangle and generate a fragment at each pixel.
--      For each fragment, barycentric coordinates are calculated and used to 
--      determine if the point is inside the triangle, then used to interpolate
--      depth and vertex attributes. The depth is tested against the previous
--      depth for that pixel in the depth buffer, and if the new fragment is less
--      deep, the new depth is saved and the fragment is emitted into the FIFO to
--      be shaded. See section 5 of the user manual for more details on the math.
--      The module also will loop through the depth buffer and reset every depth
--      value to infinity when in idle state and triggered to do so.
--
entity rasterizer is
    port (
        -- system clock
        clk             : in        std_logic;
        -- active low reset
        rst             : in        std_logic;

        -- set high to begin rasterizing vertices in memory
        start_rasterize : in        std_logic;
        -- set high to begin depth buffer reset
        start_depth_rst : in        std_logic;

        -- vertex data size register value
        v_data_size     : in        std_logic_vector(15 downto 0);
        -- vertex data base address register value
        v_data_base     : in        std_logic_vector(15 downto 0);
        -- vertex count register value
        v_count         : in        std_logic_vector(15 downto 0);

        -- data read from vertex data RAM
        v_data_in       : in        std_logic_vector(15 downto 0);

        -- address for vertex data RAM while being accessed by rasterizer
        v_data_addr     : buffer    std_logic_vector(10 downto 0);

        -- FIFO full signal to pause output into FIFO
        FIFO_full       : in        std_logic;

        -- FIFO tail element values representing rasterized fragments

        -- register values to push into tail element
        FIFO_reg_0      : out       quad;
        FIFO_reg_1      : out       quad;
        FIFO_reg_2      : out       quad;
        FIFO_reg_3      : out       quad;
        -- done bit to push into tail element -- high if this is the last fragment
        -- for this rasterize
        FIFO_done       : out       std_logic;
        -- pixel coordinates to push into tail element
        FIFO_px_x       : out       unsigned(7 downto 0);
        FIFO_px_y       : out       unsigned(6 downto 0);

        -- high to push rasterize output into FIFO
        FIFO_push       : out       std_logic;

        -- set high once rasterize is done, cleared when rasterize begins
        rasterize_done  : out       std_logic
    );
end entity;

architecture behavioral of rasterizer is
    -- component declarations for floating point functions
    component float_mult is
        port (
            clk             : in    std_logic;
            multiplicand_a  : in    float;
            multiplicand_b  : in    float;
            product         : out   float
        );
    end component;

    component float_adder is
        port (
            addend_a    : in    float;
            addend_b    : in    float;
            sum         : out   float
        );
    end component;

    component float_recip is
        port (
            f       : in  float;
            recip   : out float
        );
    end component;

    component edge_function is
        port (
            clk     : in    std_logic;
            v1_0    : in    float;
            v1_1    : in    float;
            v2_0    : in    float;
            v2_1    : in    float;
            v3_0    : in    float;
            v3_1    : in    float;
            ef      : out   float
        );
    end component;

    component float_to_pixel is
        port (
            float_in    : in    float;
            pixel       : out   signed(7 downto 0)
        );
    end component;

    component pixel_to_float is
        port (
            pixel_in    : in    signed(7 downto 0);
            float_out   : out   float
        );
    end component;

    -- component declaration for depthbuffer RAM
    component depthbuffer_RAM
        port (
            clka    : in std_logic;
            wea     : in std_logic_vector(0 downto 0);
            addra   : in std_logic_vector(14 downto 0);
            dina    : in std_logic_vector(15 downto 0);
            clkb    : in std_logic;
            addrb   : in std_logic_vector(14 downto 0);
            doutb   : out std_logic_vector(15 downto 0)
        );
    end component;

    ---- depthbuffer interface signals ----

    signal depthbuffer_wr:          std_logic_vector(0 downto 0);
    signal depthbuffer_addra:       std_logic_vector(14 downto 0);
    signal depthbuffer_addrb:       std_logic_vector(14 downto 0);
    signal depthbuffer_din:         std_logic_vector(15 downto 0);

    -- depthbuffer clear index reg
    signal depthbuffer_clr_idx:     unsigned(14 downto 0);
    -- total number of pixels = 160*128-1, max value for depthbuffer_clr_idx
    constant TOTAL_PIXEL_CNT:       unsigned(14 downto 0) := "100111111111111";

    ---- vertex loading FSM ----

    -- states for the FSM that processes vertices for each triangle
    type triangle_setup_state_t is (
        IDLE,           -- idle state
        LOAD_VERTICES,  -- load vertex data from RAM for 3 vertices
        RECIP_W,        -- w <- 1/w for each vertex
        V1_TO_NDC,      -- v1(x,y,z) <- (x/w,y/w,z/w) to convert to NDC
        V2_TO_NDC,      -- same for v2
        V3_TO_NDC,      -- same for v3
        GET_PX_COORDS,  -- cast x and y values to ints in pixel coord space (1.0 -> 128)
        GET_REC_BOUNDS, -- find minimum and maximum pixel x and y for this triangle
        V1_ATTR_SCALE,  -- loop through each attribute of v1 and scale by 1/w
        V2_ATTR_SCALE,  -- same for v2
        V3_ATTR_SCALE,  -- same for v3
        AREA_COMP,      -- compute triangle area using edge_function(v1, v2, v3)
        RECIP_AREA,     -- take reciprocal of triangle area
        GEN_FRAGS,      -- loop through pixels within the bounds and generate fragments
        CLEAR_DEPTHBUFF -- clear the depth buffer (set every pixel to -inf so the new)
    );     
    signal triangle_setup_state:    triangle_setup_state_t;

    -- goes high after one clock in LOAD_VERTICES state to account for clock delay
    -- on vertex data memory read
    signal vertex_reg_load:         std_logic;

    -- current vertex being loaded, 0=v1, 1=v2, 2=v3
    signal curr_local_vertex:       unsigned(1 downto 0);
    -- index of the current vertex in the list of all vertices to be rasterized
    signal curr_vertex_index:       unsigned(15 downto 0);
    -- current vertex register being loaded (0 = coords, 1-3 = attrs)
    signal vertex_reg:              unsigned(1 downto 0);
    -- current vertex register component being loaded
    signal vertex_reg_comp:         unsigned(1 downto 0);

    -- single bit counter for 2-clock states that use a multiplier
    signal mult_clock:              std_logic;

    -- current pixel coordinates of fragment being generated into the pipeline
    signal gen_frag_x:              signed(7 downto 0);
    signal gen_frag_y:              signed(7 downto 0);

    ---- vertex loading regs ----

    -- vertex coordinates and attributes, corresponding to outputs from its vertex
    -- shader for each of the 3 vertices
    signal v1_coords:               quad;
    signal v1_attr1:                quad;
    signal v1_attr2:                quad;
    signal v1_attr3:                quad;
    signal v2_coords:               quad;
    signal v2_attr1:                quad;
    signal v2_attr2:                quad;
    signal v2_attr3:                quad;
    signal v3_coords:               quad;
    signal v3_attr1:                quad;
    signal v3_attr2:                quad;
    signal v3_attr3:                quad;

    -- signed pixel coordinates for each vertex, i.e. -128 to 127 where (0,0) is
    -- the center of the display
    signal v1_px_x:                 signed(7 downto 0);
    signal v1_px_y:                 signed(7 downto 0);
    signal v2_px_x:                 signed(7 downto 0);
    signal v2_px_y:                 signed(7 downto 0);
    signal v3_px_x:                 signed(7 downto 0);
    signal v3_px_y:                 signed(7 downto 0);

    -- max and min of v1 and v2 pixel coord x values and y values, combinationally
    -- (used for finding 3-value max and min)
    signal max_x_v1v2:              signed(7 downto 0);
    signal min_x_v1v2:              signed(7 downto 0);
    signal max_y_v1v2:              signed(7 downto 0);
    signal min_y_v1v2:              signed(7 downto 0);

    -- max and min of pixel coord x values and y values
    signal max_x:                   signed(7 downto 0);
    signal min_x:                   signed(7 downto 0);
    signal max_y:                   signed(7 downto 0);
    signal min_y:                   signed(7 downto 0);

    -- reciprocal of triangle area (will be just the triangle area for one
    -- clock before the recip is computed)
    signal area_recip:              float;

    ---- signals for floating point ops used by triangle setup ----

    -- 4 input multiplicand a values
    signal multiplicand_a_1:        float;
    signal multiplicand_a_2:        float;
    signal multiplicand_a_3:        float;
    signal multiplicand_a_4:        float;
    -- only one multiplicand b is used
    signal multiplicand_b:          float;
    -- 4 product output values
    signal product_1:               float;
    signal product_2:               float;
    signal product_3:               float;
    signal product_4:               float;

    -- 3 reciprocal inputs
    signal recip_in_1:              float;
    signal recip_in_2:              float;
    signal recip_in_3:              float;
    -- 3 reciprocal outputs
    signal recip_1:                 float;
    signal recip_2:                 float;
    signal recip_3:                 float;

    -- cast to pixel outputs
    signal v1_x_int:                signed(7 downto 0);
    signal v1_y_int:                signed(7 downto 0);
    signal v2_x_int:                signed(7 downto 0);
    signal v2_y_int:                signed(7 downto 0);
    signal v3_x_int:                signed(7 downto 0);
    signal v3_y_int:                signed(7 downto 0);

    -- output of edge function calculator for area
    signal area_ef:                 float;

    ---- pipeline signals ----

    -- stage 1: pixels -> float
    -- stage 2: barycentric EF
    -- stage 3: inside triangle test + barycentric normalize
    -- stage 4: interpolate z, 1/w clock 
    -- stage 5: attr interpolate clock 
    -- stage 6: attr scale by 1/w clock + fetch depth buffer value
    -- stage 7: depth test + emit fragment into FIFO

    -- values stored during each stage
    --    fragment valid bit
    --    | pixel x,y coordinates
    --    | | NDC x,y coords
    --    | | | barycentric coords
    --    | | | | depth value
    --    | | | | | interpolated 1/w value
    --    | | | | | | interpolated attr 1
    --    | | | | | | | interpolated attr 2
    --    | | | | | | | | interpolated attr 3
    --    | | | | | | | | |
    -- 1 :x x x            
    -- 2 :x x x x          
    -- 3 :x x x x             
    -- 4 :x x x x x x
    -- 5 :x x x   x x x x x
    -- 6 :x x x   x   x x x
    -- 7 :x x x   x   x x x

    -- number of stages in the pipeline
    constant PIPELINE_STAGES        : integer := 7;

    -- fragment valid bit along entire pipeline, represents that we still have a
    -- valid fragment to be processed (can be removed for not being in triangle, or
    -- not passing depth test)
    signal frag_valid:              std_logic_vector(PIPELINE_STAGES-1 downto 0);

    -- done bit along entire pipeline, indicates fragment emission is finished
    signal pipeline_done:           std_logic_vector(PIPELINE_STAGES-1 downto 0);

    -- pixel coordinate array along entire pipeline
    type pipeline_px_arr_t is array (PIPELINE_STAGES-1 downto 0) of signed(7 downto 0);
    signal pipeline_px_x:           pipeline_px_arr_t;
    signal pipeline_px_y:           pipeline_px_arr_t;
    -- coordinates as floats in NDC, array along entire pipeline
    type pipeline_ndc_arr_t is array (PIPELINE_STAGES-1 downto 0) of float;
    signal pipeline_x_f:            pipeline_ndc_arr_t;
    signal pipeline_y_f:            pipeline_ndc_arr_t;

    -- barycentric coordinate array from pipeline stage 2 to stage 4
    type pipeline_bary_arr_t is array (3 downto 1) of float;
    signal pipeline_b0:             pipeline_bary_arr_t;
    signal pipeline_b1:             pipeline_bary_arr_t;
    signal pipeline_b2:             pipeline_bary_arr_t;

    -- interpolated fragment depth (z) value array from pipeline stage 4 to end
    type pipeline_depth_arr_t is array (PIPELINE_STAGES-1 downto 3) of float;
    signal pipeline_depth:          pipeline_depth_arr_t;

    -- interpolated 1/w value array from pipeline stage 4 to stage 5
    type pipeline_w_recip_arr_t is array (4 downto 3) of float;
    signal pipeline_w_recip:        pipeline_w_recip_arr_t;

    -- interpolated attr1 array from pipeline stage 5 to end
    type pipeline_attr1_arr_t is array (PIPELINE_STAGES-1 downto 4) of quad;
    signal pipeline_attr1:          pipeline_attr1_arr_t;

    -- interpolated attr2 array from pipeline stage 5 to end
    type pipeline_attr2_arr_t is array (PIPELINE_STAGES-1 downto 4) of quad;
    signal pipeline_attr2:          pipeline_attr2_arr_t;

    -- interpolated attr3 array from pipeline stage 5 to end
    type pipeline_attr3_arr_t is array (PIPELINE_STAGES-1 downto 4) of quad;
    signal pipeline_attr3:          pipeline_attr3_arr_t;

    -- pipeline state to advance every other clock; advances when high
    signal pipeline_state:          std_logic;

    -- pixel coordinates from stage 10 converted to unsigned display coordinates
    signal depth_check_x:           unsigned(7 downto 0);
    signal depth_check_y:           unsigned(7 downto 0);

    -- pixel coordinates from stage 12 converted to unsigned display coordinates
    signal out_px_x:                unsigned(7 downto 0);
    signal out_px_y:                unsigned(7 downto 0);

    -- gen_frag_x and gen_frag_y converted back to floats as inputs to stage 1
    signal frag_x_f:                float;
    signal frag_y_f:                float;

    -- outputs of barycentric coordinate computation
    signal bary_0:                  float;
    signal bary_1:                  float;
    signal bary_2:                  float;

    -- outputs of normalize barycentric coords by 1/area
    signal norm_b0:                 float;
    signal norm_b1:                 float;
    signal norm_b2:                 float;

    -- products of barycenctric coords with corresponding z values
    signal b0_z_prod:               float;
    signal b1_z_prod:               float;
    signal b2_z_prod:               float;

    -- sum of b0*z1 and b1*z2, used for the full interpolated z sum
    signal z_intermed_sum:          float;
    -- sum b0*z1 + b1*z2 + b2*z3, which is the interpolated fragment z value
    signal z_interpolated:          float;

    -- products of barycenctric coords with corresponding 1/w values
    signal b0_w_recip_prod:         float;
    signal b1_w_recip_prod:         float;
    signal b2_w_recip_prod:         float;

    -- sum of b0*1/w1 and b1*1/w2, used for the full interpolated 1/w sum
    signal w_recip_intermed_sum:    float;
    -- sum b0*1/w1 + b1*1/w2 + b2*1/w3, which is the interpolated fragment 1/w value
    signal w_recip_interpolated:    float;

    -- products of barycenctric coords with corresponding attr1 values, for each attr1 element
    signal b0_attr1_prod:           quad;
    signal b1_attr1_prod:           quad;
    signal b2_attr1_prod:           quad;

    -- sum of b0*v1[attr1[i]] and b1*v2[attr1[i]], used for the full interpolated attr1[i] sum
    signal attr1_intermed_sum:      quad;
    -- sum b0*v1[attr1[i]] + b1*v2[attr1[i]] + b2*v2[attr1[i]], 
    -- which is the interpolated fragment attr1[i] value
    signal attr1_interpolated:      quad;

    -- products of barycenctric coords with corresponding attr2 values, for each attr2 element
    signal b0_attr2_prod:           quad;
    signal b1_attr2_prod:           quad;
    signal b2_attr2_prod:           quad;

    -- sum of b0*v1[attr2[i]] and b1*v2[attr2[i]], used for the full interpolated attr2[i] sum
    signal attr2_intermed_sum:      quad;
    -- sum b0*v1[attr2[i]] + b1*v2[attr2[i]] + b2*v2[attr2[i]], 
    -- which is the interpolated fragment attr2[i] value
    signal attr2_interpolated:      quad;

    -- products of barycenctric coords with corresponding attr3 values, for each attr3 element
    signal b0_attr3_prod:           quad;
    signal b1_attr3_prod:           quad;
    signal b2_attr3_prod:           quad;

    -- sum of b0*v1[attr3[i]] and b1*v2[attr3[i]], used for the full interpolated attr3[i] sum
    signal attr3_intermed_sum:      quad;
    -- sum b0*v1[attr3[i]] + b1*v2[attr3[i]] + b2*v2[attr3[i]], 
    -- which is the interpolated fragment attr3[i] value
    signal attr3_interpolated:      quad;

    -- each attribute scaled by interpolated 1/w
    signal attr1_scaled:            quad;
    signal attr2_scaled:            quad;
    signal attr3_scaled:            quad;

    -- old depth fetched from depth buffer
    signal old_depth:               float;
    -- negated value of old depth
    signal old_depth_neg:           float;

    -- current depth - old depth, used for depth test
    signal depth_diff:              float;
begin
    ---- logic used by vertex processing state machine ----

    -- 4 multipliers used by triangle setup
    tri_mult_1: float_mult port map(
        clk             => clk,
        multiplicand_a  => multiplicand_a_1,
        multiplicand_b  => multiplicand_b,
        product         => product_1);
    tri_mult_2: float_mult port map(
        clk             => clk,
        multiplicand_a  => multiplicand_a_2,
        multiplicand_b  => multiplicand_b,
        product         => product_2);
    tri_mult_3: float_mult port map(
        clk             => clk,
        multiplicand_a  => multiplicand_a_3,
        multiplicand_b  => multiplicand_b,
        product         => product_3);
    tri_mult_4: float_mult port map(
        clk             => clk,
        multiplicand_a  => multiplicand_a_4,
        multiplicand_b  => multiplicand_b,
        product         => product_4);

    -- 3 recip calculators used by triangle setup
    tri_recip_1: float_recip port map(
        f               => recip_in_1,
        recip           => recip_1);
    tri_recip_2: float_recip port map(
        f               => recip_in_2,
        recip           => recip_2);
    tri_recip_3: float_recip port map(
        f               => recip_in_3,
        recip           => recip_3);

    -- cast v1(x) to pixel coordinate
    v1_x_cast: float_to_pixel port map(
        float_in        => v1_coords(0),
        pixel           => v1_x_int);
    -- cast v1(y) to pixel coordinate
    v1_y_cast: float_to_pixel port map(
        float_in        => v1_coords(1),
        pixel           => v1_y_int);
    -- cast v2(x) to pixel coordinate
    v2_x_cast: float_to_pixel port map(
        float_in        => v2_coords(0),
        pixel           => v2_x_int);
    -- cast v2(y) to pixel coordinate
    v2_y_cast: float_to_pixel port map(
        float_in        => v2_coords(1),
        pixel           => v2_y_int);
    -- cast v3(x) to pixel coordinate
    v3_x_cast: float_to_pixel port map(
        float_in        => v3_coords(0),
        pixel           => v3_x_int);
    -- cast v3(y) to pixel coordinate
    v3_y_cast: float_to_pixel port map(
        float_in        => v3_coords(1),
        pixel           => v3_y_int);

    -- edge function for triangle area
    tri_area_ef: edge_function port map(
        clk             => clk,
        v1_0            => v1_coords(0),
        v1_1            => v1_coords(1),
        v2_0            => v2_coords(0),
        v2_1            => v2_coords(1),
        v3_0            => v3_coords(0),
        v3_1            => v3_coords(1),
        ef              => area_ef);

                            -- multiplier 1 gets component 0
                            -- of selected attribute while scaling attributes
    multiplicand_a_1    <=  v1_attr1(0)     when (triangle_setup_state = V1_ATTR_SCALE) and (vertex_reg = "00") else
                            v1_attr2(0)     when (triangle_setup_state = V1_ATTR_SCALE) and (vertex_reg = "01") else
                            v1_attr3(0)     when (triangle_setup_state = V1_ATTR_SCALE) and (vertex_reg = "10") else
                            v2_attr1(0)     when (triangle_setup_state = V2_ATTR_SCALE) and (vertex_reg = "00") else
                            v2_attr2(0)     when (triangle_setup_state = V2_ATTR_SCALE) and (vertex_reg = "01") else
                            v2_attr3(0)     when (triangle_setup_state = V2_ATTR_SCALE) and (vertex_reg = "10") else
                            v3_attr1(0)     when (triangle_setup_state = V3_ATTR_SCALE) and (vertex_reg = "00") else
                            v3_attr2(0)     when (triangle_setup_state = V3_ATTR_SCALE) and (vertex_reg = "01") else
                            v3_attr3(0)     when (triangle_setup_state = V3_ATTR_SCALE) and (vertex_reg = "10") else
                            -- or x while scaling vertex coordinates
                            v1_coords(0)    when triangle_setup_state = V1_TO_NDC else
                            v2_coords(0)    when triangle_setup_state = V2_TO_NDC else
                            v3_coords(0);   --when triangle_setup_state = V3_TO_NDC

                            -- multiplier 2 gets component 1
                            -- of selected attribute while scaling attributes
    multiplicand_a_2    <=  v1_attr1(1)     when (triangle_setup_state = V1_ATTR_SCALE) and (vertex_reg = "00") else
                            v1_attr2(1)     when (triangle_setup_state = V1_ATTR_SCALE) and (vertex_reg = "01") else
                            v1_attr3(1)     when (triangle_setup_state = V1_ATTR_SCALE) and (vertex_reg = "10") else
                            v2_attr1(1)     when (triangle_setup_state = V2_ATTR_SCALE) and (vertex_reg = "00") else
                            v2_attr2(1)     when (triangle_setup_state = V2_ATTR_SCALE) and (vertex_reg = "01") else
                            v2_attr3(1)     when (triangle_setup_state = V2_ATTR_SCALE) and (vertex_reg = "10") else
                            v3_attr1(1)     when (triangle_setup_state = V3_ATTR_SCALE) and (vertex_reg = "00") else
                            v3_attr2(1)     when (triangle_setup_state = V3_ATTR_SCALE) and (vertex_reg = "01") else
                            v3_attr3(1)     when (triangle_setup_state = V3_ATTR_SCALE) and (vertex_reg = "10") else
                            -- or x while scaling vertex coordinates
                            v1_coords(1)    when triangle_setup_state = V1_TO_NDC else
                            v2_coords(1)    when triangle_setup_state = V2_TO_NDC else
                            v3_coords(1);   --when triangle_setup_state = V3_TO_NDC

                            -- multiplier 3 gets component 2
                            -- of selected attribute while scaling attributes
    multiplicand_a_3    <=  v1_attr1(2)     when (triangle_setup_state = V1_ATTR_SCALE) and (vertex_reg = "00") else
                            v1_attr2(2)     when (triangle_setup_state = V1_ATTR_SCALE) and (vertex_reg = "01") else
                            v1_attr3(2)     when (triangle_setup_state = V1_ATTR_SCALE) and (vertex_reg = "10") else
                            v2_attr1(2)     when (triangle_setup_state = V2_ATTR_SCALE) and (vertex_reg = "00") else
                            v2_attr2(2)     when (triangle_setup_state = V2_ATTR_SCALE) and (vertex_reg = "01") else
                            v2_attr3(2)     when (triangle_setup_state = V2_ATTR_SCALE) and (vertex_reg = "10") else
                            v3_attr1(2)     when (triangle_setup_state = V3_ATTR_SCALE) and (vertex_reg = "00") else
                            v3_attr2(2)     when (triangle_setup_state = V3_ATTR_SCALE) and (vertex_reg = "01") else
                            v3_attr3(2)     when (triangle_setup_state = V3_ATTR_SCALE) and (vertex_reg = "10") else
                            -- or x while scaling vertex coordinates
                            v1_coords(2)    when triangle_setup_state = V1_TO_NDC else
                            v2_coords(2)    when triangle_setup_state = V2_TO_NDC else
                            v3_coords(2);   --when triangle_setup_state = V3_TO_NDC

                            -- multiplier 4 gets component 3
                            -- of selected attribute while scaling attributes
    multiplicand_a_4    <=  v1_attr1(3)     when (triangle_setup_state = V1_ATTR_SCALE) and (vertex_reg = "00") else
                            v1_attr2(3)     when (triangle_setup_state = V1_ATTR_SCALE) and (vertex_reg = "01") else
                            v1_attr3(3)     when (triangle_setup_state = V1_ATTR_SCALE) and (vertex_reg = "10") else
                            v2_attr1(3)     when (triangle_setup_state = V2_ATTR_SCALE) and (vertex_reg = "00") else
                            v2_attr2(3)     when (triangle_setup_state = V2_ATTR_SCALE) and (vertex_reg = "01") else
                            v2_attr3(3)     when (triangle_setup_state = V2_ATTR_SCALE) and (vertex_reg = "10") else
                            v3_attr1(3)     when (triangle_setup_state = V3_ATTR_SCALE) and (vertex_reg = "00") else
                            v3_attr2(3)     when (triangle_setup_state = V3_ATTR_SCALE) and (vertex_reg = "01") else
                            v3_attr3(3);    --when (triangle_setup_state = V3_ATTR_SCALE) and (vertex_reg = "10")
                            -- (not used while scaling vertex coordinates)

    -- always multiply by 1/w of the current vertex
    multiplicand_b      <=  v1_coords(3)    when (triangle_setup_state = V1_TO_NDC) or (triangle_setup_state = V1_ATTR_SCALE) else
                            v2_coords(3)    when (triangle_setup_state = V2_TO_NDC) or (triangle_setup_state = V2_ATTR_SCALE) else
                            v3_coords(3);   --when (triangle_setup_state = V3_TO_NDC) or (triangle_setup_state = V3_ATTR_SCALE)
    
    -- 1st recip calculator computes area reciprocal or 1/w1 when in appropriate state
    recip_in_1          <=  area_recip      when triangle_setup_state = RECIP_AREA else
                            v1_coords(3);
    -- other recip calculators always take 1/w
    recip_in_2          <=  v2_coords(3);
    recip_in_3          <=  v3_coords(3);

    -- find 2-value maxs and mins of pixel coords combinationally
    max_x_v1v2 <= v1_px_x when v1_px_x > v2_px_x else
                  v2_px_x;
    min_x_v1v2 <= v1_px_x when v1_px_x < v2_px_x else
                  v2_px_x;
    max_y_v1v2 <= v1_px_y when v1_px_y > v2_px_y else
                  v2_px_y;
    min_y_v1v2 <= v1_px_y when v1_px_y < v2_px_y else
                  v2_px_y;

    ---- logic used by pipeline ----

    -- cast generated fragment x to NDC float
    frag_x_cast: pixel_to_float port map(
        pixel_in        => gen_frag_x,
        float_out       => frag_x_f);
    -- cast generated fragment y to NDC float
    frag_y_cast: pixel_to_float port map(
        pixel_in        => gen_frag_y,
        float_out       => frag_y_f);

    -- edge functions to compute barycentric coords using fragment location
    -- and triangle vertices
    bary_0_ef: edge_function port map(
        clk             => clk,
        v1_0            => v2_coords(0),
        v1_1            => v2_coords(1),
        v2_0            => v3_coords(0),
        v2_1            => v3_coords(1),
        v3_0            => pipeline_x_f(0),
        v3_1            => pipeline_y_f(0),
        ef              => bary_0);
    bary_1_ef: edge_function port map(
        clk             => clk,
        v1_0            => v3_coords(0),
        v1_1            => v3_coords(1),
        v2_0            => v1_coords(0),
        v2_1            => v1_coords(1),
        v3_0            => pipeline_x_f(0),
        v3_1            => pipeline_y_f(0),
        ef              => bary_1);
    bary_2_ef: edge_function port map(
        clk             => clk,
        v1_0            => v1_coords(0),
        v1_1            => v1_coords(1),
        v2_0            => v2_coords(0),
        v2_1            => v2_coords(1),
        v3_0            => pipeline_x_f(0),
        v3_1            => pipeline_y_f(0),
        ef              => bary_2);

    -- multiply barycentric coords from stage 2 by triangle area reciprocal to
    -- normalize them (should result in values 0 to 1)
    b0_norm_mult: float_mult port map(
        clk             => clk,
        multiplicand_a  => pipeline_b0(1),
        multiplicand_b  => area_recip,
        product         => norm_b0);
    b1_norm_mult: float_mult port map(
        clk             => clk,
        multiplicand_a  => pipeline_b1(1),
        multiplicand_b  => area_recip,
        product         => norm_b1);
    b2_norm_mult: float_mult port map(
        clk             => clk,
        multiplicand_a  => pipeline_b2(1),
        multiplicand_b  => area_recip,
        product         => norm_b2);

    -- multiply each z value by the corresponding barycentric coord
    b0_z_mult: float_mult port map(
        clk             => clk,
        multiplicand_a  => pipeline_b0(2),
        multiplicand_b  => v1_coords(2),
        product         => b0_z_prod);
    b1_z_mult: float_mult port map(
        clk             => clk,
        multiplicand_a  => pipeline_b1(2),
        multiplicand_b  => v2_coords(2),
        product         => b1_z_prod);
    b2_z_mult: float_mult port map(
        clk             => clk,
        multiplicand_a  => pipeline_b2(2),
        multiplicand_b  => v3_coords(2),
        product         => b2_z_prod);

    -- add the scaled z values to get interpolated z
    z_interp_1: float_adder port map (
        addend_a        => b0_z_prod,
        addend_b        => b1_z_prod,
        sum             => z_intermed_sum);
    z_interp_2: float_adder port map (
        addend_a        => z_intermed_sum,
        addend_b        => b2_z_prod,
        sum             => z_interpolated);

    -- multiply each 1/w value by the corresponding barycentric coord
    b0_w_recip_mult: float_mult port map(
        clk             => clk,
        multiplicand_a  => pipeline_b0(2),
        multiplicand_b  => v1_coords(3),
        product         => b0_w_recip_prod);
    b1_w_recip_mult: float_mult port map(
        clk             => clk,
        multiplicand_a  => pipeline_b1(2),
        multiplicand_b  => v2_coords(3),
        product         => b1_w_recip_prod);
    b2_w_recip_mult: float_mult port map(
        clk             => clk,
        multiplicand_a  => pipeline_b2(2),
        multiplicand_b  => v3_coords(3),
        product         => b2_w_recip_prod);

    -- add the scaled z values to get interpolated 1/w
    w_recip_interp_1: float_adder port map (
        addend_a        => b0_w_recip_prod,
        addend_b        => b1_w_recip_prod,
        sum             => w_recip_intermed_sum);
    w_recip_interp_2: float_adder port map (
        addend_a        => w_recip_intermed_sum,
        addend_b        => b2_w_recip_prod,
        sum             => w_recip_interpolated);

    -- generate interpolation for each element of each attribute
    gen_attr_interpolation: for i in 0 to 3 generate
        -- multiply each attr1[i] value by the corresponding barycentric coord
        b0_attr1_i_mult: float_mult port map(
            clk             => clk,
            multiplicand_a  => pipeline_b0(3),
            multiplicand_b  => v1_attr1(i),
            product         => b0_attr1_prod(i));
        b1_attr1_i_mult: float_mult port map(
            clk             => clk,
            multiplicand_a  => pipeline_b1(3),
            multiplicand_b  => v2_attr1(i),
            product         => b1_attr1_prod(i));
        b2_attr1_i_mult: float_mult port map(
            clk             => clk,
            multiplicand_a  => pipeline_b2(3),
            multiplicand_b  => v3_attr1(i),
            product         => b2_attr1_prod(i));

        -- add the scaled attr1[0] values to get interpolated attr1[0]
        attr1_i_interp_1: float_adder port map (
            addend_a        => b0_attr1_prod(i),
            addend_b        => b1_attr1_prod(i),
            sum             => attr1_intermed_sum(i));
        attr1_i_interp_2: float_adder port map (
            addend_a        => attr1_intermed_sum(i),
            addend_b        => b2_attr1_prod(i),
            sum             => attr1_interpolated(i));

        -- multiply each attr2[i] value by the corresponding barycentric coord
        b0_attr2_i_mult: float_mult port map(
            clk             => clk,
            multiplicand_a  => pipeline_b0(3),
            multiplicand_b  => v1_attr2(i),
            product         => b0_attr2_prod(i));
        b1_attr2_i_mult: float_mult port map(
            clk             => clk,
            multiplicand_a  => pipeline_b1(3),
            multiplicand_b  => v2_attr2(i),
            product         => b1_attr2_prod(i));
        b2_attr2_i_mult: float_mult port map(
            clk             => clk,
            multiplicand_a  => pipeline_b2(3),
            multiplicand_b  => v3_attr2(i),
            product         => b2_attr2_prod(i));

        -- add the scaled attr2[0] values to get interpolated attr2[0]
        attr2_i_interp_1: float_adder port map (
            addend_a        => b0_attr2_prod(i),
            addend_b        => b1_attr2_prod(i),
            sum             => attr2_intermed_sum(i));
        attr2_i_interp_2: float_adder port map (
            addend_a        => attr2_intermed_sum(i),
            addend_b        => b2_attr2_prod(i),
            sum             => attr2_interpolated(i));

        -- multiply each attr3[i] value by the corresponding barycentric coord
        b0_attr3_i_mult: float_mult port map(
            clk             => clk,
            multiplicand_a  => pipeline_b0(3),
            multiplicand_b  => v1_attr3(i),
            product         => b0_attr3_prod(i));
        b1_attr3_i_mult: float_mult port map(
            clk             => clk,
            multiplicand_a  => pipeline_b1(3),
            multiplicand_b  => v2_attr3(i),
            product         => b1_attr3_prod(i));
        b2_attr3_i_mult: float_mult port map(
            clk             => clk,
            multiplicand_a  => pipeline_b2(3),
            multiplicand_b  => v3_attr3(i),
            product         => b2_attr3_prod(i));

        -- add the scaled attr3[i] values to get interpolated attr3[i]
        attr3_i_interp_1: float_adder port map (
            addend_a        => b0_attr3_prod(i),
            addend_b        => b1_attr3_prod(i),
            sum             => attr3_intermed_sum(i));
        attr3_i_interp_2: float_adder port map (
            addend_a        => attr3_intermed_sum(i),
            addend_b        => b2_attr3_prod(i),
            sum             => attr3_interpolated(i));

        -- multiply attribute values by interpolated 1/w
        attr1_i_scale_mult: float_mult port map(
            clk             => clk,
            multiplicand_a  => pipeline_attr1(4)(i),
            multiplicand_b  => pipeline_w_recip(4),
            product         => attr1_scaled(i));
        attr2_i_scale_mult: float_mult port map(
            clk             => clk,
            multiplicand_a  => pipeline_attr2(4)(i),
            multiplicand_b  => pipeline_w_recip(4),
            product         => attr2_scaled(i));
        attr3_i_scale_mult: float_mult port map(
            clk             => clk,
            multiplicand_a  => pipeline_attr3(4)(i),
            multiplicand_b  => pipeline_w_recip(4),
            product         => attr3_scaled(i));
    end generate gen_attr_interpolation;

    -- get display coords from pixel coords

    -- go from (-80,79) to (0,159)
    depth_check_x <= unsigned(pipeline_px_x(5) + 80);
    out_px_x <= unsigned(pipeline_px_x(6) + 80);
    -- go from (-64,63) to (0,127)
    depth_check_y <= unsigned(pipeline_px_y(5) + 64);
    out_px_y <= unsigned(pipeline_px_y(6) + 64);

    -- write final stage fragment z to depth buffer if valid, or write inf to every pixel while resetting
    -- depth buffer
    depthbuffer_wr <= "1" when triangle_setup_state = CLEAR_DEPTHBUFF else
        (0 => frag_valid(6));
    depthbuffer_addra <= std_logic_vector(depthbuffer_clr_idx) when triangle_setup_state = CLEAR_DEPTHBUFF else
        std_logic_vector(out_px_x) & std_logic_vector(out_px_y(6 downto 0));
    depthbuffer_din <= "0111110000000000" when triangle_setup_state = CLEAR_DEPTHBUFF else
        pipeline_depth(6);

    -- fetch depth of old fragment at stage 6 location
    depthbuffer_addrb <= std_logic_vector(depth_check_x) 
        & std_logic_vector(depth_check_y(6 downto 0));

    -- instantiate depthbuffer RAM
    depthbuffer: depthbuffer_RAM port map (
        clka    => clk,
        wea     => depthbuffer_wr, 
        addra   => depthbuffer_addra,
        dina    => depthbuffer_din,
        clkb    => clk,
        addrb   => depthbuffer_addrb,
        doutb   => old_depth
    );

    -- flip sign of old depth
    old_depth_neg <= (not old_depth(15)) & old_depth(14 downto 0);

    -- subtract it from new depth for comparison
    depth_adder: float_adder port map (
        addend_a        => pipeline_depth(6),
        addend_b        => old_depth_neg,
        sum             => depth_diff);

    -- done once back to idle state
    rasterize_done <= '1' when triangle_setup_state = IDLE else '0';

    -- vertex FSM process
    process(clk)
    begin
        if rising_edge(clk) then
            if rst = '0' then
                -- reset to idle
                triangle_setup_state <= IDLE;
            else
                case triangle_setup_state is
                    when IDLE =>
                        if start_rasterize  = '1' then
                            -- start loading vertex data and start relevant index regs
                            -- at 0
                            triangle_setup_state <= LOAD_VERTICES;
                            curr_local_vertex <= "00";
                            curr_vertex_index <= (others => '0');
                            vertex_reg <= "00";
                            vertex_reg_comp <= "00";
                            mult_clock <= '0';
                            vertex_reg_load <= '0';
                            -- flip high bit of base address to get the half of memory
                            -- used for saving shaded vertex data
                            v_data_addr <= (not v_data_base(10)) & v_data_base(9 downto 0);
                        elsif start_depth_rst = '1' then
                            -- start resetting depth buffer
                            triangle_setup_state <= CLEAR_DEPTHBUFF;
                            depthbuffer_clr_idx <= (others => '0');
                        end if;
                    when CLEAR_DEPTHBUFF =>
                        -- increment index to reset
                        depthbuffer_clr_idx <= depthbuffer_clr_idx + 1;
                        -- go back to idle once we're done resetting
                        if depthbuffer_clr_idx = TOTAL_PIXEL_CNT then
                            triangle_setup_state <= IDLE;
                        end if;
                    when LOAD_VERTICES =>
                        -- increment load address unless about to end
                        if not ((vertex_reg_comp = "11") and (vertex_reg = unsigned(v_data_size(1 downto 0))) and
                            (curr_local_vertex = "10")) then
                            v_data_addr <= std_logic_vector(unsigned(v_data_addr) + 1);
                        end if;
                        -- reg load enable is delayed by a clock to account for
                        -- vertex data memory read delay
                        vertex_reg_load <= '1';

                        if vertex_reg_load = '1' then
                            vertex_reg_comp <= vertex_reg_comp + 1;

                            if vertex_reg_comp = "11" then
                                -- if done with last component go to next register
                                vertex_reg <= vertex_reg + 1;

                                if vertex_reg = unsigned(v_data_size(1 downto 0)) then
                                    -- if done with last register then go to next vertex
                                    vertex_reg <= "00";
                                    curr_local_vertex <= curr_local_vertex + 1;
                                    curr_vertex_index <= curr_vertex_index + 1;

                                    if curr_local_vertex = "10" then
                                        -- if done loading 3rd vertex for this triangle,
                                        -- move on to next state
                                        triangle_setup_state <= RECIP_W;
                                    end if;
                                end if;
                            end if;

                            -- write to the selected component of selected register
                            if curr_local_vertex = "00" then
                                if vertex_reg = "00" then
                                    v1_coords(to_integer(vertex_reg_comp)) <= v_data_in;
                                elsif vertex_reg = "01" then
                                    v1_attr1(to_integer(vertex_reg_comp)) <= v_data_in;
                                elsif vertex_reg = "10" then
                                    v1_attr2(to_integer(vertex_reg_comp)) <= v_data_in;
                                else
                                    v1_attr3(to_integer(vertex_reg_comp)) <= v_data_in;
                                end if;
                            elsif curr_local_vertex = "01" then
                                if vertex_reg = "00" then
                                    v2_coords(to_integer(vertex_reg_comp)) <= v_data_in;
                                elsif vertex_reg = "01" then
                                    v2_attr1(to_integer(vertex_reg_comp)) <= v_data_in;
                                elsif vertex_reg = "10" then
                                    v2_attr2(to_integer(vertex_reg_comp)) <= v_data_in;
                                else
                                    v2_attr3(to_integer(vertex_reg_comp)) <= v_data_in;
                                end if;
                            else
                                if vertex_reg = "00" then
                                    v3_coords(to_integer(vertex_reg_comp)) <= v_data_in;
                                elsif vertex_reg = "01" then
                                    v3_attr1(to_integer(vertex_reg_comp)) <= v_data_in;
                                elsif vertex_reg = "10" then
                                    v3_attr2(to_integer(vertex_reg_comp)) <= v_data_in;
                                else
                                    v3_attr3(to_integer(vertex_reg_comp)) <= v_data_in;
                                end if;
                            end if;
                        end if;
                    when RECIP_W =>
                        -- w values get their reciprocals from general reciprocal calculators
                        v1_coords(3) <= recip_1;
                        v2_coords(3) <= recip_2;
                        v3_coords(3) <= recip_3;

                        -- immediately go to next state
                        triangle_setup_state <= V1_TO_NDC;
                    when V1_TO_NDC =>
                        -- multiply takes 2 clocks
                        mult_clock <= not mult_clock;

                        if mult_clock = '1' then
                            -- operation finished, (x,y,z) get multiplied by 1/w from multipliers
                            v1_coords(0) <= product_1;
                            v1_coords(1) <= product_2;
                            v1_coords(2) <= product_3;

                            triangle_setup_state <= V2_TO_NDC;
                        end if;
                    when V2_TO_NDC =>
                        -- multiply takes 2 clocks
                        mult_clock <= not mult_clock;

                        if mult_clock = '1' then
                            -- operation finished, (x,y,z) get multiplied by 1/w from multipliers
                            v2_coords(0) <= product_1;
                            v2_coords(1) <= product_2;
                            v2_coords(2) <= product_3;

                            triangle_setup_state <= V3_TO_NDC;
                        end if;
                    when V3_TO_NDC =>
                        -- multiply takes 2 clocks
                        mult_clock <= not mult_clock;

                        if mult_clock = '1' then
                            -- operation finished, (x,y,z) get multiplied by 1/w from multipliers
                            v3_coords(0) <= product_1;
                            v3_coords(1) <= product_2;
                            v3_coords(2) <= product_3;

                            triangle_setup_state <= GET_PX_COORDS;
                        end if;
                    when GET_PX_COORDS =>
                        -- get x and y values cast to ints
                        v1_px_x <= v1_x_int;
                        v1_px_y <= v1_y_int;
                        v2_px_x <= v2_x_int;
                        v2_px_y <= v2_y_int;
                        v3_px_x <= v3_x_int;
                        v3_px_y <= v3_y_int;

                        -- immediately go to next state
                        triangle_setup_state <= GET_REC_BOUNDS;
                    when GET_REC_BOUNDS =>
                        -- find 3-value maxs and mins
                        if max_x_v1v2 > v3_px_x then
                            max_x <= max_x_v1v2;
                        else
                            max_x <= v3_px_x;
                        end if;

                        if min_x_v1v2 < v3_px_x then
                            min_x <= min_x_v1v2;
                        else
                            min_x <= v3_px_x;
                        end if;

                        if max_y_v1v2 > v3_px_y then
                            max_y <= max_y_v1v2;
                        else
                            max_y <= v3_px_y;
                        end if;

                        if min_y_v1v2 < v3_px_y then
                            min_y <= min_y_v1v2;
                        else
                            min_y <= v3_px_y;
                        end if;

                        -- immediately go to next state
                        triangle_setup_state <= V1_ATTR_SCALE;
                        -- attr scale uses reg counter to count attr
                        vertex_reg <= "00";
                    when V1_ATTR_SCALE =>
                        -- multiply takes 2 clocks
                        mult_clock <= not mult_clock;

                        if mult_clock = '1' then
                            -- operation finished, attr gets multiplied by 1/w from multipliers
                            if vertex_reg = "00" then
                                v1_attr1(0) <= product_1;
                                v1_attr1(1) <= product_2;
                                v1_attr1(2) <= product_3;
                                v1_attr1(3) <= product_4;

                                -- increment which attr we're scaling
                                vertex_reg <= vertex_reg + 1;
                            elsif vertex_reg = "01" then
                                v1_attr2(0) <= product_1;
                                v1_attr2(1) <= product_2;
                                v1_attr2(2) <= product_3;
                                v1_attr2(3) <= product_4;

                                -- increment which attr we're scaling
                                vertex_reg <= vertex_reg + 1;
                            else
                                v1_attr3(0) <= product_1;
                                v1_attr3(1) <= product_2;
                                v1_attr3(2) <= product_3;
                                v1_attr3(3) <= product_4;

                                -- on last attr so next state
                                triangle_setup_state <= V2_ATTR_SCALE;
                                -- reset attr to scale
                                vertex_reg <= "00";
                            end if;
                        end if;
                    when V2_ATTR_SCALE =>
                        -- multiply takes 2 clocks
                        mult_clock <= not mult_clock;

                        if mult_clock = '1' then
                            -- operation finished, attr gets multiplied by 1/w from multipliers
                            if vertex_reg = "00" then
                                v2_attr1(0) <= product_1;
                                v2_attr1(1) <= product_2;
                                v2_attr1(2) <= product_3;
                                v2_attr1(3) <= product_4;

                                -- increment which attr we're scaling
                                vertex_reg <= vertex_reg + 1;
                            elsif vertex_reg = "01" then
                                v2_attr2(0) <= product_1;
                                v2_attr2(1) <= product_2;
                                v2_attr2(2) <= product_3;
                                v2_attr2(3) <= product_4;

                                -- increment which attr we're scaling
                                vertex_reg <= vertex_reg + 1;
                            else
                                v2_attr3(0) <= product_1;
                                v2_attr3(1) <= product_2;
                                v2_attr3(2) <= product_3;
                                v2_attr3(3) <= product_4;

                                -- on last attr so next state
                                triangle_setup_state <= V3_ATTR_SCALE;
                                -- reset attr to scale
                                vertex_reg <= "00";
                            end if;
                        end if;
                    when V3_ATTR_SCALE =>
                        -- multiply takes 2 clocks
                        mult_clock <= not mult_clock;

                        if mult_clock = '1' then
                            -- operation finished, attr gets multiplied by 1/w from multipliers
                            if vertex_reg = "00" then
                                v3_attr1(0) <= product_1;
                                v3_attr1(1) <= product_2;
                                v3_attr1(2) <= product_3;
                                v3_attr1(3) <= product_4;
                            elsif vertex_reg = "01" then
                                v3_attr2(0) <= product_1;
                                v3_attr2(1) <= product_2;
                                v3_attr2(2) <= product_3;
                                v3_attr2(3) <= product_4;
                            else
                                v3_attr3(0) <= product_1;
                                v3_attr3(1) <= product_2;
                                v3_attr3(2) <= product_3;
                                v3_attr3(3) <= product_4;

                                -- on last attr so next state
                                triangle_setup_state <= AREA_COMP;
                            end if;

                            -- increment which attr we're scaling
                            vertex_reg <= vertex_reg + 1;
                        end if;
                    when AREA_COMP =>
                        -- edge function takes 2 clocks (uses multiply)
                        mult_clock <= not mult_clock;

                        if mult_clock = '1' then
                            -- operation finished, area gets edge function of vertices
                            area_recip <= area_ef;

                            triangle_setup_state <= RECIP_AREA;
                        end if;
                    when RECIP_AREA =>
                        -- area gets its reciprocal from recip calculator
                        area_recip <= recip_1;

                        -- immediately go to next state
                        triangle_setup_state <= GEN_FRAGS;
                        -- init fragment pixel counters with minimum values
                        gen_frag_x <= min_x;
                        gen_frag_y <= min_y;
                    when GEN_FRAGS =>
                        -- only update if FIFO is not full
                        if FIFO_full = '0' then
                            -- only update when pipeline advances
                            if pipeline_state = '1' then
                                -- always increment fragment x
                                gen_frag_x <= gen_frag_x + 1;

                                if gen_frag_x = max_x then
                                    -- if done with a row, reset x and inc to next row
                                    gen_frag_x <= min_x;
                                    gen_frag_y <= gen_frag_y + 1;

                                    if gen_frag_y = max_y then
                                        -- if done with the entire rectangle
                                        if curr_vertex_index > unsigned(v_count) then
                                            -- if done with every vertex, done rasterizing and go back to idle
                                            triangle_setup_state <= IDLE;
                                        else
                                            -- otherwise start loading another 3 vertices for the next triangle
                                            triangle_setup_state <= LOAD_VERTICES;
                                            curr_local_vertex <= "00";
                                            vertex_reg <= "00";
                                            vertex_reg_comp <= "00";
                                            vertex_reg_load <= '0';
                                        end if;
                                    end if;
                                end if;
                            end if;
                        end if;
                end case;
            end if;
        end if;
    end process;

    -- hook up pipeline outputs to fragment FIFO

    -- reg 0 gets fragment coordinates (excluding w)
    FIFO_reg_0 <= (pipeline_x_f(6), pipeline_y_f(6), pipeline_depth(6), (others => '0'));
    -- other regs get interpolated attributes
    FIFO_reg_1 <= pipeline_attr1(6);
    FIFO_reg_2 <= pipeline_attr2(6);
    FIFO_reg_3 <= pipeline_attr3(6);

    FIFO_px_x <= out_px_x;
    FIFO_px_y <= out_px_y(6 downto 0);

    FIFO_done <= pipeline_done(6);

    -- push into FIFO if fragment is valid and pipeline is advancing
    FIFO_push   <=  frag_valid(6) when (FIFO_full = '0') and (pipeline_state = '1') else
                    '0';

    -- fragment pipeline process
    process(clk)
    begin
        if rising_edge(clk) then
            -- only update if FIFO not full
            if FIFO_full = '0' then
                -- each stage takes 2 clocks
                -- doesn't matter that GEN_FRAGS can start on either state because it'll wait
                pipeline_state <= not pipeline_state;

                if pipeline_state = '1' then
                    ---- stage 1: cast fragment pixel coordinates back to NDC floats

                    -- incoming fragments only valid when in fragment generation state
                    frag_valid(0)   <=  '1' when triangle_setup_state = GEN_FRAGS else
                                        '0';

                    -- done when condition to go back to idle is true
                    pipeline_done(0) <= '1' when (gen_frag_x = max_x) 
                                            and (gen_frag_y = max_y) 
                                            and (curr_vertex_index > unsigned(v_count)) else
                                        '0';

                    -- get the pixel coordinates from fragment generation
                    pipeline_px_x(0)    <= gen_frag_x;
                    pipeline_px_y(0)    <= gen_frag_y;

                    -- get the pixel coordinates converted back to float
                    pipeline_x_f(0) <= frag_x_f;
                    pipeline_y_f(0) <= frag_y_f;

                    ---- stage 2: barycentric edge function computation

                    -- shift elements that don't change
                    frag_valid(1) <= frag_valid(0);
                    pipeline_done(1) <= pipeline_done(0);
                    pipeline_px_x(1) <= pipeline_px_x(0);
                    pipeline_px_y(1) <= pipeline_px_y(0);
                    pipeline_x_f(1) <= pipeline_x_f(0);
                    pipeline_y_f(1) <= pipeline_y_f(0);

                    -- get calculated barycentric coords into pipeline
                    pipeline_b0(1) <= bary_0;
                    pipeline_b1(1) <= bary_1;
                    pipeline_b2(1) <= bary_2;

                    ---- stage 3: test point interior to triangle and normalize barycentric coords

                    -- point is inside triangle if all barycentric coords are positive, or all are negative
                    -- (all pos vs all neg indicates which side of the triangle is facing camera)
                    if ((sign(pipeline_b0(1)) = '1') and (sign(pipeline_b1(1)) = '1') and (sign(pipeline_b2(1)) = '1')) or
                        ((sign(pipeline_b0(1)) = '0') and (sign(pipeline_b1(1)) = '0') and (sign(pipeline_b2(1)) = '0')) then
                        frag_valid(2) <= frag_valid(1);
                    else
                        frag_valid(2) <= '0';
                    end if;

                    -- shift elements that don't change
                    pipeline_done(2) <= pipeline_done(1);
                    pipeline_px_x(2) <= pipeline_px_x(1);
                    pipeline_px_y(2) <= pipeline_px_y(1);
                    pipeline_x_f(2) <= pipeline_x_f(1);
                    pipeline_y_f(2) <= pipeline_y_f(1);

                    -- barycentric coords get normalized by triangle area recip
                    pipeline_b0(2) <= norm_b0;
                    pipeline_b1(2) <= norm_b1;
                    pipeline_b2(2) <= norm_b2;

                    ---- stage 4: interpolate z and 1/w values to get fragment z and 1/w

                    -- shift elements that don't change
                    frag_valid(3) <= frag_valid(2);
                    pipeline_done(3) <= pipeline_done(2);
                    pipeline_px_x(3) <= pipeline_px_x(2);
                    pipeline_px_y(3) <= pipeline_px_y(2);
                    pipeline_x_f(3) <= pipeline_x_f(2);
                    pipeline_y_f(3) <= pipeline_y_f(2);
                    pipeline_b0(3) <= pipeline_b0(2);
                    pipeline_b1(3) <= pipeline_b1(2);
                    pipeline_b2(3) <= pipeline_b2(2);

                    -- get interpolated z and 1/w
                    -- (calculated by b0*v1[z]+b1*v2[z]+b2*v3[z] and similar for 1/w instead of z)
                    pipeline_depth(3) <= z_interpolated;
                    pipeline_w_recip(3) <= w_recip_interpolated;

                    ---- stage 5: interpolate (all elements of) attributes

                    -- shift elements that don't change
                    frag_valid(4) <= frag_valid(3);
                    pipeline_done(4) <= pipeline_done(3);
                    pipeline_px_x(4) <= pipeline_px_x(3);
                    pipeline_px_y(4) <= pipeline_px_y(3);
                    pipeline_x_f(4) <= pipeline_x_f(3);
                    pipeline_y_f(4) <= pipeline_y_f(3);
                    pipeline_depth(4) <= pipeline_depth(3);
                    pipeline_w_recip(4) <= pipeline_w_recip(3);

                    -- get each interpolated attribute
                    pipeline_attr1(4) <= attr1_interpolated;
                    pipeline_attr2(4) <= attr2_interpolated;
                    pipeline_attr3(4) <= attr3_interpolated;

                    ---- stage 6: attribute scale by 1/w

                    -- shift elements that don't change
                    frag_valid(5) <= frag_valid(4);
                    pipeline_done(5) <= pipeline_done(4);
                    pipeline_px_x(5) <= pipeline_px_x(4);
                    pipeline_px_y(5) <= pipeline_px_y(4);
                    pipeline_x_f(5) <= pipeline_x_f(4);
                    pipeline_y_f(5) <= pipeline_y_f(4);
                    pipeline_depth(5) <= pipeline_depth(4);

                    -- get each attribute scaled by 1/w
                    pipeline_attr1(5) <= attr1_scaled;
                    pipeline_attr2(5) <= attr2_scaled;
                    pipeline_attr3(5) <= attr3_scaled;

                    ---- stage 7: depth test

                    -- shift elements that don't change
                    pipeline_done(6) <= pipeline_done(5);
                    pipeline_px_x(6) <= pipeline_px_x(5);
                    pipeline_px_y(6) <= pipeline_px_y(5);
                    pipeline_x_f(6) <= pipeline_x_f(5);
                    pipeline_y_f(6) <= pipeline_y_f(5);
                    pipeline_depth(6) <= pipeline_depth(5);
                    pipeline_attr1(6) <= pipeline_attr1(5);
                    pipeline_attr2(6) <= pipeline_attr2(5);
                    pipeline_attr3(6) <= pipeline_attr3(5);

                    -- only emit this fragment if it's less deep than the previous fragment
                    -- at this location
                    -- and if pixel coordinates are in range
                    if (sign(depth_diff) = '1') 
                        and (pipeline_px_x(5) < 80) and (pipeline_px_x(5) >= -80)
                        and (pipeline_px_y(5) < 128) and (pipeline_px_y(5) >= -128) then
                        frag_valid(6) <= frag_valid(5);
                    else
                        frag_valid(6) <= '0';
                    end if;
                end if;
            end if;
        end if;
    end process;
end architecture;