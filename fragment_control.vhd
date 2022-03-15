----------------------------------------------------------------------------
--
--  Fragment processing module
--
--  Revision History:
--     17 May 21  Ray Wendt         Initial revision.
--     20 May 21  Ray Wendt         Finished changing logic from vertex loading
--                                  + saving to fragments.
--      8 Jun 21  Ray Wendt         Fixed syntax errors.
--      9 Jun 21  Ray Wendt         Added eqn for done signal.
--     10 Jun 21  Ray Wendt         Added reset to idle state.
--     10 Jun 21  Ray Wendt         Fixed multi-clock instruction PC handling.
--     10 Jun 21  Ray Wendt         Added missing eqn for FIFO_comp_sel.
--     10 Jun 21  Ray Wendt         Switched output_reg_array to reg 1 because I
--                                  previously specified it that way for some
--                                  reason.
--     10 Jun 21  Ray Wendt         Fixed input to color casting.
--     10 Jun 21  Ray Wendt         Updated handling of FIFO_done.
--     12 Jun 21  Ray Wendt         Added processor reset signal.
--     16 Jun 21  Ray Wendt         Added header comment.
--
----------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.gpu_float.all;

--  fragment_control
--
--  Description:
--      This module controls the fragment shader processors. When the FIFO becomes not
--      empty, it loads fragments from the FIFO until 16 fragments are loaded, then
--      runs the fragment shader program and saves the output color from OUT1 to the
--      framebuffer. This process repeats until the FIFO indicates fragment generation
--      is done and it is empty, at which point the remaining fragments are processed
--      and the module returns to idle. While idle, the module also allows writing to
--      fragment program memory and fragment shader constants through the system data
--      and address bus input.
--
entity fragment_control is
    port (
        -- system clock
        clk         : in        std_logic;
        -- active low reset
        rst         : in        std_logic;

        -- 16 bit system data input
        data_in     : in        std_logic_vector(15 downto 0);
        -- 13 bit system write address
        --  01xaaaaaaaaii   : addr in fragment program RAM, 16-bit word i
        --  10xxxxxxxiijj   : element j in fragment constant i
        addr        : in        std_logic_vector(12 downto 0);
        -- write enable for writing to fragment control regs or program
        write_en    : in        std_logic;

        -- FIFO head element values
        FIFO_reg_0  : in        quad;
        FIFO_reg_1  : in        quad;
        FIFO_reg_2  : in        quad;
        FIFO_reg_3  : in        quad;
        FIFO_px_x   : in        unsigned(7 downto 0);
        FIFO_px_y   : in        unsigned(6 downto 0);
        FIFO_done   : in        std_logic;

        -- high when the FIFO is empty
        FIFO_empty  : in        std_logic;

        -- FIFO pop element signal
        FIFO_pop    : out       std_logic;

        -- framebuffer address out
        fbuff_addr  : out       std_logic_vector(14 downto 0);
        -- framebuffer write data
        fbuff_data  : out       std_logic_vector(17 downto 0);
        -- high when writing to framebuffer
        fbuff_wr    : out       std_logic;

        -- set high once shading is done, cleared when shading begins
        shade_done  : out       std_logic
    );
end entity;

architecture behavioral of fragment_control is
    -- component declaration for shader program memory
    component shader_prog_ram
        port (
            clka    : in    std_logic;
            ena     : in    std_logic;
            wea     : in    std_logic_vector(0 downto 0);
            addra   : in    std_logic_vector(7 downto 0);
            dina    : in    std_logic_vector(47 downto 0);
            douta   : out   std_logic_vector(47 downto 0)
        );
    end component;

    -- component declaration for shader processor
    component shader_proc is
        port (
            clk             : in        std_logic;
            rst             : in        std_logic;
            enable          : in        std_logic;
            store_in_reg    : in        std_logic;
            in_reg_idx      : in        unsigned(1 downto 0);
            in_reg_comp     : in        unsigned(1 downto 0);
            in_reg_wr_val   : in        float;
            op_a_sel        : in        unsigned(3 downto 0);
            op_a_swizzle    : in        std_logic_vector(7 downto 0);
            op_a_negate     : in        std_logic;
            op_b_sel        : in        unsigned(3 downto 0);
            op_b_swizzle    : in        std_logic_vector(7 downto 0);
            op_b_negate     : in        std_logic;
            dest_sel        : in        unsigned(2 downto 0);
            dest_mask       : in        std_logic_vector(3 downto 0);
            opcode          : in        std_logic_vector(2 downto 0);
            op_state        : in        std_logic;
            constant_reg_0  : in        quad;
            constant_reg_1  : in        quad;
            constant_reg_2  : in        quad;
            constant_reg_3  : in        quad;
            output_reg_0    : buffer    quad;
            output_reg_1    : buffer    quad;
            output_reg_2    : buffer    quad;
            output_reg_3    : buffer    quad
        );
    end component;

    -- component declaration for float -> color cast module
    component float_to_color is
        port (
            float_in    : in    float;
            color       : out   unsigned(5 downto 0)
        );
    end component;

    -- opcode constants
    constant OP_MOV:            std_logic_vector(2 downto 0) := "000";
    constant OP_MUL:            std_logic_vector(2 downto 0) := "001";
    constant OP_ADD:            std_logic_vector(2 downto 0) := "010";
    constant OP_DP3:            std_logic_vector(2 downto 0) := "011";
    constant OP_DP4:            std_logic_vector(2 downto 0) := "100";
    constant OP_END:            std_logic_vector(2 downto 0) := "101";

    -- address patterns for the system write input (see table in inputs for descriptions)
    constant FPROG_RAM_ADDR:        std_logic_vector(12 downto 0) := "01-----------";
    constant FCONST_ADDR:           std_logic_vector(12 downto 0) := "10-----------";

    -- fragment shading FSM state
    --  IDLE: no shading in progress
    --  LOAD: currently loading fragment inputs
    --  RUN:  running shader program
    --  SAVE: saving shader output color to framebuffer
    type fragment_shade_state_t is (IDLE, LOAD, RUN, SAVE);
    signal fragment_shade_state:    fragment_shade_state_t;
    -- index of the fragment shader currently being accessed for load or save
    signal curr_frag_shader:        unsigned(3 downto 0);
    -- highest index of the fragment shaders currently in use
    signal highest_frag_shader:     unsigned(3 downto 0);
    -- index of the input or output reg currently being loaded or saved from
    -- current fragment shader
    signal curr_io_reg:             unsigned(1 downto 0);
    -- index of the component of the current reg being loaded or saved
    signal curr_reg_comp:           unsigned(1 downto 0);

    -- reset signal for fragment processors to reset them before running program
    signal fragment_rst:            std_logic;

    -- component selected from FIFO as input to shader processor
    signal FIFO_comp_sel:           float;

    -- high when writing to any fragment processor input reg
    signal fragment_input_write:    std_logic;
    -- individual fragment processor input reg write enables
    signal fragment_input_write_i:  std_logic_vector(15 downto 0);  

    -- instruction register for shader program
    signal IR:                      std_logic_vector(47 downto 0);
    -- clock counter for the current instruction
    signal op_state:                std_logic;
    -- program counter for shader program
    signal fprog_counter:           unsigned(7 downto 0);
    -- instruction enable signal for fragment processors
    signal instr_enable:            std_logic;

    -- program memory address input
    signal fprog_addr:              std_logic_vector(7 downto 0);
    -- program memory data input
    signal fprog_data_in:           std_logic_vector(47 downto 0);
    -- since the system write input is only 16 bits, the high 32 bits of fragment
    -- program data write are stored in a buffer and a write to the low 16 bits 
    -- writes the buffer plus the current value to RAM
    signal fprog_data_buff:         std_logic_vector(31 downto 0);
    -- program memory write enable
    signal fprog_write_en:          std_logic_vector(0 downto 0);
    -- data read from program memory
    signal fprog_data_out:          std_logic_vector(47 downto 0);

    -- arrays of fragment pixel x and y coordinates, each associated with the fragment
    -- loaded into the fragment shader of the same index
    type frag_px_x_arr_t is array (15 downto 0) of unsigned(7 downto 0);
    type frag_px_y_arr_t is array (15 downto 0) of unsigned(6 downto 0);
    signal frag_px_x_arr:           frag_px_x_arr_t;
    signal frag_px_y_arr:           frag_px_y_arr_t;

    -- fragment processor constants
    signal fprog_constant_0:        quad;
    signal fprog_constant_1:        quad;
    signal fprog_constant_2:        quad;
    signal fprog_constant_3:        quad;

    -- array of output register 1s from the 16 processors
    type output_reg_array_t is array (0 to 15) of quad;
    signal output_reg_array:        output_reg_array_t;
    -- currently selected element of output_reg_array
    signal curr_output:             quad;

    -- 6-bit integer color outputs from the selected processor to be written to the framebuffer
    signal out_r:                   unsigned(5 downto 0);
    signal out_g:                   unsigned(5 downto 0);
    signal out_b:                   unsigned(5 downto 0);
begin
    -- write to fragment shader inputs while loading and not waiting for more FIFO data
    fragment_input_write <= '1' when (fragment_shade_state = LOAD) and (FIFO_empty = '0') else
                            '0';

    -- select register component value from FIFO head
    FIFO_comp_sel   <=  FIFO_reg_0(to_integer(curr_reg_comp)) when curr_io_reg = "00" else
                        FIFO_reg_1(to_integer(curr_reg_comp)) when curr_io_reg = "01" else
                        FIFO_reg_2(to_integer(curr_reg_comp)) when curr_io_reg = "10" else
                        FIFO_reg_3(to_integer(curr_reg_comp));-- when curr_io_reg = "01"

    vertex_shaders: for i in 0 to 15 generate
        -- enable input reg write when any input reg write is enabled and the 
        -- current shader index matches
        fragment_input_write_i(i) <= '1' when (fragment_input_write = '1') 
                                    and (to_integer(curr_frag_shader) = i) else
                                   '0';

        -- connect each fragment shader
        fshader_i: shader_proc port map (
            clk             => clk,
            rst             => rst,
            enable          => instr_enable,                -- shared instruction enable
            store_in_reg    => fragment_input_write_i(i),   -- indexed input enable
            in_reg_idx      => curr_io_reg,                 -- reg select from FSM
            in_reg_comp     => curr_reg_comp,
            in_reg_wr_val   => FIFO_comp_sel,               -- data comes from FIFO
            op_a_sel        => unsigned(IR(19 downto 16)),  -- control lines directly from IR
            op_a_swizzle    => IR(15 downto 8),
            op_a_negate     => IR(7),
            op_b_sel        => unsigned(IR(32 downto 29)),
            op_b_swizzle    => IR(28 downto 21),
            op_b_negate     => IR(20),
            dest_sel        => unsigned(IR(6 downto 4)),
            dest_mask       => IR(3 downto 0),
            opcode          => IR(35 downto 33),
            op_state        => op_state,
            constant_reg_0  => fprog_constant_0,            -- shared constant regs
            constant_reg_1  => fprog_constant_1,
            constant_reg_2  => fprog_constant_2,
            constant_reg_3  => fprog_constant_3,
            output_reg_0    => open,
            output_reg_1    => output_reg_array(i),         -- frag shaders only use output 1
            output_reg_2    => open,
            output_reg_3    => open);
    end generate vertex_shaders;

    -- connect shader program memory
    fprog_ram: shader_prog_ram port map (
        clka    => clk,
        ena     => '1', -- always enabled
        wea     => fprog_write_en,
        addra   => fprog_addr,
        dina    => fprog_data_in,
        douta   => fprog_data_out);

    -- reset when done saving outputs so it's ready for next batch of inputs
    fragment_rst    <=  '0' when (fragment_shade_state = SAVE)
                            and (curr_frag_shader = highest_frag_shader) else
                        rst;

    -- select output for color from current shader
    curr_output <= output_reg_array(to_integer(curr_frag_shader));

    -- cast shader outputs to 6 bit RGB color values
    cast_r: float_to_color port map (
        float_in    => curr_output(0),
        color       => out_r);

    cast_g: float_to_color port map (
        float_in    => curr_output(1),
        color       => out_g);

    cast_b: float_to_color port map (
        float_in    => curr_output(2),
        color       => out_b);

    -- write to vertex program memory on a write to low 16 bits of vertex prog mem
    -- can only change program while not actively shading
    fprog_write_en <= "1" when std_match(addr, FPROG_RAM_ADDR) 
                        and (addr(1 downto 0) = "00")
                        and (fragment_shade_state = IDLE)
                        and (write_en = '1') else
                      "0";

    -- vertex program addr is PC while program is running, otherwise from system addr
    fprog_addr <= std_logic_vector(fprog_counter) when fragment_shade_state = RUN else
                  addr(9 downto 2);

    -- vertex program input is combination of buffer for two high words and data in
    -- for low word
    fprog_data_in <= fprog_data_buff & data_in;

    -- pop FIFO element when we're on the last component and last register of the
    -- current element (i.e. about to increment to next element)
    FIFO_pop <= '1' when (curr_reg_comp = "11") and (curr_io_reg = "11") 
                    and (fragment_shade_state = LOAD) else
                '0';

    -- framebuffer data is concatenation of integer color outputs
    fbuff_data  <= std_logic_vector(out_b) 
        & std_logic_vector(out_g) 
        & std_logic_vector(out_r);
    -- write to framebuffer while saving outputs
    fbuff_wr    <= '1' when fragment_shade_state = SAVE else
                   '0';
    -- pixel address is concatenation of x and y coordinates
    fbuff_addr  <= std_logic_vector(frag_px_x_arr(to_integer(curr_frag_shader))) 
                    & std_logic_vector(frag_px_y_arr(to_integer(curr_frag_shader)));

    -- done while in idle state
    shade_done <= '1' when fragment_shade_state = IDLE else '0';

    -- process to update FSM state and associated count regsisters
    process(clk)
    begin
        if rising_edge(clk) then
            if rst = '0' then
                fragment_shade_state <= IDLE;
            else
                case fragment_shade_state is
                    when IDLE =>
                        -- start shading once data appears in the FIFO
                        if FIFO_empty = '0' then
                            -- start in load state
                            fragment_shade_state <= LOAD;
                            -- reset index regs
                            curr_frag_shader <= (others => '0');
                            curr_io_reg <= (others => '0');
                            curr_reg_comp <= (others => '0');
                        end if;
                    when LOAD =>
                        -- wait until the FIFO isn't empty
                        if FIFO_empty = '0' then
                            -- increment register component
                            curr_reg_comp <= curr_reg_comp + 1;
                            if curr_reg_comp = "11" then
                                -- if done with component 3, then go to next register
                                curr_io_reg <= curr_io_reg + 1;
                                if curr_io_reg = "11" then
                                    -- if done with last register, then 
                                    -- go to next fragment/shader processor pair
                                    curr_frag_shader <= curr_frag_shader + 1;

                                    if curr_frag_shader = "1111" then
                                        -- using all frag shaders
                                        highest_frag_shader <= "1111";

                                        -- if we've reached the last processor then stop 
                                        -- writing inputs and start running the program
                                        fragment_shade_state <= RUN;
                                        -- start vertex program at beginning
                                        fprog_counter <= (others => '0');
                                        -- start IR with all 0s
                                        IR <= (others => '0');
                                        instr_enable <= '0';
                                    end if;
                                end if;
                            end if;
                        elsif FIFO_done = '1' then
                            -- incremented past the last shader, so highest is index
                            -- decremented
                            highest_frag_shader <= curr_frag_shader - 1;

                            -- the last fragment has been loaded, so stop writing inputs and
                            -- start running the program
                            fragment_shade_state <= RUN;
                            -- start vertex program at beginning
                            fprog_counter <= (others => '0');
                            -- start IR with all 0s
                            IR <= (others => '0');
                            instr_enable <= '0';
                        end if;
                    when RUN =>
                        -- enable running instructions after PC hits 1, this accounts
                        -- for 2 clock delay from memory read + IR clock to actually get
                        -- the first instruction into the IR
                        if fprog_counter(0) = '1' then
                            instr_enable <= '1';
                        end if;

                        -- increment PC as long as we don't have a multi-clock opcode on
                        -- the prog data bus (have to check prog data instead of IR because
                        -- of one-clock memory read delay)
                        -- or increment if we do but we're on the first clock of a previous
                        -- multi-clock instruction
                        if not ((fprog_data_out(35 downto 33) = OP_DP4) 
                            or (fprog_data_out(35 downto 33) = OP_DP3) 
                            or (fprog_data_out(35 downto 33) = OP_MUL)) or
                            (((IR(35 downto 33) = OP_DP4) 
                            or (IR(35 downto 33) = OP_DP3) 
                            or (IR(35 downto 33) = OP_MUL)) and (op_state = '0')) then
                            fprog_counter <= fprog_counter + 1;
                        end if;

                        -- IR gets new instruction unless in the first state of a 2-cycle
                        -- instruction
                        if not ((IR(35 downto 33) = OP_DP4) 
                            or (IR(35 downto 33) = OP_DP3) 
                            or (IR(35 downto 33) = OP_MUL)) or (op_state = '1') then
                            IR <= fprog_data_out;
                        end if;

                        -- if we have a multi-clock instruction, invert op state to get
                        -- to the next state, otherwise reset
                        if (IR(35 downto 33) = OP_DP4) 
                            or (IR(35 downto 33) = OP_DP3) 
                            or (IR(35 downto 33) = OP_MUL) then
                            op_state <= not op_state;
                        else
                            op_state <= '0';
                        end if;

                        -- if program is finished then save shader outputs
                        if (IR(35 downto 33) = OP_END) and (instr_enable = '1') then
                            fragment_shade_state <= SAVE;
                            -- program no longer running
                            instr_enable <= '0';
                            -- reset index regs
                            curr_frag_shader <= (others => '0');
                        end if;
                    when SAVE =>
                        -- increment fragment shader (only takes one clock for saving 
                        -- output from each)
                        curr_frag_shader <= curr_frag_shader + 1;

                        if curr_frag_shader = highest_frag_shader then
                            if FIFO_done = '1' then
                                -- finished saving outputs from last fragment, so done
                                -- and go back to IDLE
                                fragment_shade_state <= IDLE;
                            else
                                -- otherwise go back
                                -- to LOAD to process next group of 16 fragments
                                fragment_shade_state <= LOAD;
                                -- reset index regs
                                curr_frag_shader <= (others => '0');
                                curr_io_reg <= (others => '0');
                                curr_reg_comp <= (others => '0');
                            end if;
                        end if;
                end case;
            end if;
        end if;
    end process;

    -- process to update pixel coord arrays
    process(clk)
    begin
        if rising_edge(clk) then
            if (fragment_shade_state = LOAD) and (FIFO_empty = '0') then
                -- load pixel coords into current index when loading and FIFO has
                -- data
                frag_px_x_arr(to_integer(curr_frag_shader)) <= FIFO_px_x;
                frag_px_y_arr(to_integer(curr_frag_shader)) <= FIFO_px_y;
            end if;
        end if;
    end process;

    -- process to update control registers
    process(clk)
    begin
        if rising_edge(clk) then
            if rst = '0' then
                -- reset all regs to 0
                fprog_constant_0 <= QUAD_ZERO;
                fprog_constant_1 <= QUAD_ZERO;
                fprog_constant_2 <= QUAD_ZERO;
                fprog_constant_3 <= QUAD_ZERO;
                fprog_data_buff <= (others => '0');
            elsif write_en = '1' then
                -- only change registers when system write enabled

                if std_match(addr, FPROG_RAM_ADDR) then
                    if addr(1 downto 0) = "01" then
                        -- write to middle 16 bits of vertex prog data
                        fprog_data_buff(15 downto 0) <= data_in;
                    end if;
                    if addr(1 downto 0) = "10" then
                        -- write to high 16 bits of vertex prog data
                        fprog_data_buff(31 downto 16) <= data_in;
                    end if;
                end if;

                if std_match(addr, FCONST_ADDR) then
                    -- write to selected component of selected vertex program constant
                    case addr(3 downto 2) is
                        when "00" =>
                            fprog_constant_0(to_integer(unsigned(addr(1 downto 0)))) <= data_in;
                        when "01" =>
                            fprog_constant_1(to_integer(unsigned(addr(1 downto 0)))) <= data_in;
                        when "10" =>
                            fprog_constant_2(to_integer(unsigned(addr(1 downto 0)))) <= data_in;
                        when "11" =>
                            fprog_constant_3(to_integer(unsigned(addr(1 downto 0)))) <= data_in;
                        when others =>
                            -- can't get here
                    end case;
                end if;
            end if;
        end if;
    end process;
end architecture;