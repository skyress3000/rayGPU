----------------------------------------------------------------------------
--
--  Vertex processing module
--
--  Revision History:
--     15 May 21  Ray Wendt         Initial revision.
--     16 May 21  Ray Wendt         Partial implementation.
--     17 May 21  Ray Wendt         Finished implementation.
--     17 May 21  Ray Wendt         Fixed syntax issues.
--      9 Jun 21  Ray Wendt         Added eqn for done signal.
--     10 Jun 21  Ray Wendt         Added reset to idle state.
--     10 Jun 21  Ray Wendt         Fixed multi-clock instruction PC handling.
--     10 Jun 21  Ray Wendt         Fixed system write to vertex data RAM.
--     10 Jun 21  Ray Wendt         Fixed save loop indexing.
--     10 Jun 21  Ray Wendt         Fixed vertex data load address incrementing.
--     12 Jun 21  Ray Wendt         Added processor reset signal.
--     12 Jun 21  Ray Wendt         Added vertex_input_write reset in needed places
--                                  (after system reset and return to idle).
--     16 Jun 21  Ray Wendt         Added header comment.
--
----------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.gpu_float.all;

--  vertex_control
--
--  Description:
--      This module controls the vertex shader processors. When vertex shade is triggered,
--      it loads vertices from vertex data RAM until 4 vertices are loaded, then
--      runs the vertex shader program and saves the outputs to vertex data RAM. Vertices
--      are loaded starting at the vertex data base address, and saved starting at that
--      address with the high bit flipped. This process repeats until the number of vertices
--      specified by vertex count register are processed. While idle, the module also allows 
--      writing to vertex program memory, vertex shader constants, vertex data RAM, and vertex
--      data size, base address, and count registers through the system data and address bus 
--      input.
--
entity vertex_control is
    port (
        -- system clock
        clk         : in        std_logic;
        -- active low reset
        rst         : in        std_logic;

        -- 16 bit system data input
        data_in     : in        std_logic_vector(15 downto 0);
        -- 13 bit system write address
        --  00aaaaaaaaaaa   : addr in vertex data RAM
        --  01xaaaaaaaaii   : addr in vertex program RAM, 16-bit word i
        --  10xxxxxxxiijj   : element j in vertex constant i
        --  11xxxxxxxxx00   : vertex data size reg
        --  11xxxxxxxxx01   : vertex data base addr reg
        --  11xxxxxxxxx10   : vertex count reg
        addr        : in        std_logic_vector(12 downto 0);
        -- write enable for writing to vertex control regs or program
        write_en    : in        std_logic;

        -- set high to begin shading vertices in memory
        start_shade : in        std_logic;

        -- data read from vertex data RAM
        v_data_in   : in        std_logic_vector(15 downto 0);

        -- address for vertex data RAM while being accessed by vertex control
        v_data_addr : out       std_logic_vector(10 downto 0);
        -- high when writing to vertex data RAM from vertex control
        v_data_wr   : out       std_logic;
        -- data output to write to vertex data RAM
        v_data_out  : out       std_logic_vector(15 downto 0);

        -- vertex data size register value
        v_data_size : buffer    std_logic_vector(15 downto 0);
        -- vertex data base address register value
        v_data_base : buffer    std_logic_vector(15 downto 0);
        -- vertex count register value
        v_count     : buffer    std_logic_vector(15 downto 0);

        -- set high once shading is done, cleared when shading begins
        shade_done  : out       std_logic
    );
end entity;

architecture behavioral of vertex_control is
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

    -- opcode constants
    constant OP_MOV:            std_logic_vector(2 downto 0) := "000";
    constant OP_MUL:            std_logic_vector(2 downto 0) := "001";
    constant OP_ADD:            std_logic_vector(2 downto 0) := "010";
    constant OP_DP3:            std_logic_vector(2 downto 0) := "011";
    constant OP_DP4:            std_logic_vector(2 downto 0) := "100";
    constant OP_END:            std_logic_vector(2 downto 0) := "101";

    -- address patterns for the system write input (see table in inputs for descriptions)
    constant VDATA_RAM_ADDR:        std_logic_vector(12 downto 0) := "00-----------";
    constant VPROG_RAM_ADDR:        std_logic_vector(12 downto 0) := "01-----------";
    constant VCONST_ADDR:           std_logic_vector(12 downto 0) := "10-----------";
    constant VDATA_SIZE_ADDR:       std_logic_vector(12 downto 0) := "11---------00";
    constant VDATA_BASE_ADDR:       std_logic_vector(12 downto 0) := "11---------01";
    constant VCOUNT_ADDR:           std_logic_vector(12 downto 0) := "11---------10";

    -- vertex shading FSM state
    --  IDLE: no shading in progress
    --  LOAD: currently loading shader inputs
    --  RUN:  running shader program
    --  SAVE: saving shader outputs
    type vertex_shade_state_t is (IDLE, LOAD, RUN, SAVE);
    signal vertex_shade_state:      vertex_shade_state_t;
    -- index of the vertex shader currently being accessed for load or save
    signal curr_vertex_shader:      unsigned(1 downto 0);
    -- index of the input or output reg currently being loaded or saved from
    -- current vertex shader
    signal curr_io_reg:             unsigned(1 downto 0);
    -- index of the component of the current reg being loaded or saved
    signal curr_reg_comp:           unsigned(1 downto 0);
    -- current address in vertex data being accessed for load
    signal curr_vdata_load_addr:    unsigned(10 downto 0);
    -- current address in vertex data being accessed for save
    signal curr_vdata_save_addr:    unsigned(10 downto 0);
    -- index of the last vertex loaded
    signal curr_vertex_load:        unsigned(15 downto 0);
    -- index of the last vertex saved
    signal curr_vertex_save:        unsigned(15 downto 0);

    -- reset signal for vertex processors to reset them before running program
    signal vertex_rst:              std_logic;

    -- high when writing to any vertex processor input reg
    signal vertex_input_write:      std_logic;
    -- individual vertex processor input reg write enables
    signal vertex_input_write_i:    std_logic_vector(3 downto 0);  

    -- instruction register for shader program
    signal IR:                      std_logic_vector(47 downto 0);
    -- clock counter for the current instruction
    signal op_state:                std_logic;
    -- program counter for shader program
    signal vprog_counter:           unsigned(7 downto 0);
    -- instruction enable signal for vertex processors
    signal instr_enable:            std_logic;

    -- program memory address input
    signal vprog_addr:              std_logic_vector(7 downto 0);
    -- program memory data input
    signal vprog_data_in:           std_logic_vector(47 downto 0);
    -- since the system write input is only 16 bits, the high 32 bits of vertex
    -- program data write are stored in a buffer and a write to the low 16 bits 
    -- writes the buffer plus the current value to RAM
    signal vprog_data_buff:         std_logic_vector(31 downto 0);
    -- program memory write enable
    signal vprog_write_en:          std_logic_vector(0 downto 0);
    -- data read from program memory
    signal vprog_data_out:          std_logic_vector(47 downto 0);

    -- vertex processor constants
    signal vprog_constant_0:        quad;
    signal vprog_constant_1:        quad;
    signal vprog_constant_2:        quad;
    signal vprog_constant_3:        quad;

    -- arrays of output registers from the 4 processors
    type output_reg_array_t is array (0 to 3) of quad;
    signal output_reg_0_array:      output_reg_array_t;
    signal output_reg_1_array:      output_reg_array_t;
    signal output_reg_2_array:      output_reg_array_t;
    signal output_reg_3_array:      output_reg_array_t;
begin
    vertex_shaders: for i in 0 to 3 generate
        -- enable input reg write when any input reg write is enabled and the 
        -- current shader index matches
        vertex_input_write_i(i) <= '1' when (vertex_input_write = '1') 
                                    and (to_integer(curr_vertex_shader) = i) else
                                   '0';

        -- connect each vertex shader
        vshader_i: shader_proc port map (
            clk             => clk,
            rst             => vertex_rst,
            enable          => instr_enable,                -- shared instruction enable
            store_in_reg    => vertex_input_write_i(i),     -- indexed input enable
            in_reg_idx      => curr_io_reg,                 -- reg select from FSM
            in_reg_comp     => curr_reg_comp,
            in_reg_wr_val   => v_data_in,                   -- data comes from vertex data RAM
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
            constant_reg_0  => vprog_constant_0,            -- shared constant regs
            constant_reg_1  => vprog_constant_1,
            constant_reg_2  => vprog_constant_2,
            constant_reg_3  => vprog_constant_3,
            output_reg_0    => output_reg_0_array(i),       -- output into arrays of output regs
            output_reg_1    => output_reg_1_array(i),
            output_reg_2    => output_reg_2_array(i),
            output_reg_3    => output_reg_3_array(i));
    end generate vertex_shaders;

    -- connect shader program memory
    vprog_ram: shader_prog_ram port map (
        clka    => clk,
        ena     => '1', -- always enabled
        wea     => vprog_write_en,
        addra   => vprog_addr,
        dina    => vprog_data_in,
        douta   => vprog_data_out);

    -- reset when done saving outputs so it's ready for next batch of inputs
    vertex_rst  <=  '0' when (vertex_shade_state = SAVE)
                        and (curr_reg_comp = "11")
                        and (curr_io_reg = unsigned(v_data_size(1 downto 0)))
                        and ((curr_vertex_save = unsigned(v_count)) or (curr_vertex_shader = "11")) else
                    rst;

    -- write to vertex program memory on a write to low 16 bits of vertex prog mem
    -- can only change program while not actively shading
    vprog_write_en <= "1" when std_match(addr, VPROG_RAM_ADDR) 
                        and (addr(1 downto 0) = "00")
                        and (vertex_shade_state = IDLE)
                        and (write_en = '1') else
                      "0";

    -- vertex program addr is PC while program is running, otherwise from system addr
    vprog_addr <= std_logic_vector(vprog_counter) when vertex_shade_state = RUN else
                  addr(9 downto 2);

    -- vertex program input is combination of buffer for two high words and data in
    -- for low word
    vprog_data_in <= vprog_data_buff & data_in;

    -- select either load or save addr depending on state, or address from system
    -- write when idle
    v_data_addr <= std_logic_vector(curr_vdata_load_addr) when vertex_shade_state = LOAD else
                   std_logic_vector(curr_vdata_save_addr) when vertex_shade_state = SAVE else
                   addr(10 downto 0);
    -- write to vertex data only when in save output state, or idle and system
    -- write to vertex data
    v_data_wr   <= '1' when (vertex_shade_state = SAVE) 
                        or ((vertex_shade_state = IDLE) 
                            and std_match(addr, VDATA_RAM_ADDR)
                            and (write_en = '1')) else
                   '0';

    -- write to vertex data from data in on system write
    -- otherwise select vertex data mem write data as selected component of selected output reg 
    -- from selected vertex shader
    v_data_out  <= data_in when ((vertex_shade_state = IDLE) 
                        and std_match(addr, VDATA_RAM_ADDR)
                        and (write_en = '1')) else
                   output_reg_0_array(to_integer(curr_vertex_shader))(to_integer(curr_reg_comp)) 
                        when curr_io_reg = "00" else
                   output_reg_1_array(to_integer(curr_vertex_shader))(to_integer(curr_reg_comp)) 
                        when curr_io_reg = "01" else
                   output_reg_2_array(to_integer(curr_vertex_shader))(to_integer(curr_reg_comp)) 
                        when curr_io_reg = "10" else
                   output_reg_3_array(to_integer(curr_vertex_shader))(to_integer(curr_reg_comp));

    -- done while in idle state
    shade_done <= '1' when vertex_shade_state = IDLE else '0';

    -- process to update FSM state and associated count regsisters
    process(clk)
    begin
        if rising_edge(clk) then
            if rst = '0' then
                -- reset to idle
                vertex_shade_state <= IDLE;
                vertex_input_write <= '0';
                --curr_vertex_shader <= (others => '0');
                --curr_vertex_load <= (others => '0');
                --curr_vertex_save <= (others => '0');
                --curr_io_reg <= (others => '0');
                --curr_reg_comp <= (others => '0');
                --curr_vdata_load_addr <= (others => '0');
                --curr_vdata_save_addr <= (others => '0');
                instr_enable <= '0';
            else
                case vertex_shade_state is
                    when IDLE =>
                        -- shading start from idle
                        if start_shade = '1' then
                            -- start in load state
                            vertex_shade_state <= LOAD;
                            -- reset index regs
                            curr_vertex_shader <= (others => '0');
                            curr_vertex_load <= (others => '0');
                            curr_vertex_save <= (others => '0');
                            curr_io_reg <= (others => '0');
                            curr_reg_comp <= (others => '0');
                            -- start at the base for vertex data in memory
                            curr_vdata_load_addr <= unsigned(v_data_base(10 downto 0));
                            -- flip high bit of base address to get the half of memory
                            -- used for saving shaded vertex data
                            curr_vdata_save_addr <= unsigned((not v_data_base(10)) & v_data_base(9 downto 0));
                        end if;
                    when LOAD =>
                        -- increment read address unless about to end
                        if not ((curr_reg_comp = "11") and (curr_io_reg = unsigned(v_data_size(1 downto 0))) and
                            ((curr_vertex_shader = "11") or (curr_vertex_load = unsigned(v_count)))) then
                            curr_vdata_load_addr <= curr_vdata_load_addr + 1;
                        end if;
                        -- input write enable is delayed by a clock to account for
                        -- vertex data memory read delay
                        vertex_input_write <= '1';

                        -- use that delay for incrementing which reg we're writing
                        if vertex_input_write = '1' then
                            -- increment register component
                            curr_reg_comp <= curr_reg_comp + 1;
                            if curr_reg_comp = "11" then
                                -- if done with component 3, then go to next register
                                curr_io_reg <= curr_io_reg + 1;
                                if curr_io_reg = unsigned(v_data_size(1 downto 0)) then
                                    -- if done with last register in data size, then 
                                    -- go to next vertex/shader processor pair
                                    curr_vertex_shader <= curr_vertex_shader + 1;
                                    curr_vertex_load <= curr_vertex_load + 1;
                                    curr_io_reg <= "00";

                                    if (curr_vertex_shader = "11") 
                                        or (curr_vertex_load = unsigned(v_count)) then
                                        -- if we've reached the last processor, or the 
                                        -- last vertex, then stop writing inputs and
                                        -- start running the program
                                        vertex_shade_state <= RUN;
                                        -- start vertex program at beginning
                                        vprog_counter <= (others => '0');
                                        -- start IR with all 0s
                                        IR <= (others => '0');
                                        instr_enable <= '0';
                                        op_state <= '0';
                                    end if;
                                end if;
                            end if;
                        end if;
                    when RUN =>
                        -- enable running instructions after PC hits 1, this accounts
                        -- for 2 clock delay from memory read + IR clock to actually get
                        -- the first instruction into the IR
                        if vprog_counter(0) = '1' then
                            instr_enable <= '1';
                        end if;

                        -- increment PC as long as we don't have a multi-clock opcode on
                        -- the prog data bus (have to check prog data instead of IR because
                        -- of one-clock memory read delay)
                        -- or increment if we do but we're on the first clock of a previous
                        -- multi-clock instruction
                        if not ((vprog_data_out(35 downto 33) = OP_DP4) 
                            or (vprog_data_out(35 downto 33) = OP_DP3) 
                            or (vprog_data_out(35 downto 33) = OP_MUL)) or
                            (((IR(35 downto 33) = OP_DP4) 
                            or (IR(35 downto 33) = OP_DP3) 
                            or (IR(35 downto 33) = OP_MUL)) and (op_state = '0')) then
                            vprog_counter <= vprog_counter + 1;
                        end if;

                        -- IR gets new instruction unless in the first state of a 2-cycle
                        -- instruction
                        if not ((IR(35 downto 33) = OP_DP4) 
                            or (IR(35 downto 33) = OP_DP3) 
                            or (IR(35 downto 33) = OP_MUL)) or (op_state = '1') then
                            IR <= vprog_data_out;
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
                            vertex_shade_state <= SAVE;
                            -- program no longer running
                            instr_enable <= '0';
                            -- reset index regs
                            curr_vertex_shader <= (others => '0');
                            curr_io_reg <= (others => '0');
                            curr_reg_comp <= (others => '0');
                        end if;
                    when SAVE =>
                        -- increment save address
                        curr_vdata_save_addr <= curr_vdata_save_addr + 1;
                        -- increment register component
                        curr_reg_comp <= curr_reg_comp + 1;
                        if curr_reg_comp = "11" then
                            -- if done with component 3, then go to next register
                            curr_io_reg <= curr_io_reg + 1;
                            if curr_io_reg = unsigned(v_data_size(1 downto 0)) then
                                -- if done with last register in data size, then 
                                -- go to next vertex/shader processor pair
                                curr_vertex_shader <= curr_vertex_shader + 1;
                                curr_vertex_save <= curr_vertex_save + 1;
                                curr_io_reg <= (others => '0');

                                if curr_vertex_save = unsigned(v_count) then
                                    -- finished saving outputs from last vertex, so done
                                    -- and go back to IDLE
                                    vertex_shade_state <= IDLE;
                                    vertex_input_write <= '0';
                                elsif curr_vertex_shader = "11" then
                                    -- if we've reached the last processor, then go back
                                    -- to LOAD to process next group of 4 vertices
                                    vertex_shade_state <= LOAD;
                                    -- reset index regs
                                    curr_vertex_shader <= (others => '0');
                                    curr_io_reg <= (others => '0');
                                    curr_reg_comp <= (others => '0');
                                    vertex_input_write <= '0';
                                end if;
                            end if;
                        end if;
                end case;
            end if;
        end if;
    end process;

    -- process to update control registers
    process(clk)
    begin
        if rising_edge(clk) then
            if rst = '0' then
                -- reset all regs to 0
                vprog_constant_0 <= QUAD_ZERO;
                vprog_constant_1 <= QUAD_ZERO;
                vprog_constant_2 <= QUAD_ZERO;
                vprog_constant_3 <= QUAD_ZERO;
                vprog_data_buff <= (others => '0');
                v_data_size <= (others => '0');
                v_data_base <= (others => '0');
                v_count <= (others => '0');
            elsif write_en = '1' then
                -- only change registers when system write enabled

                if std_match(addr, VPROG_RAM_ADDR) then
                    if addr(1 downto 0) = "01" then
                        -- write to middle 16 bits of vertex prog data
                        vprog_data_buff(15 downto 0) <= data_in;
                    end if;
                    if addr(1 downto 0) = "10" then
                        -- write to high 16 bits of vertex prog data
                        vprog_data_buff(31 downto 16) <= data_in;
                    end if;
                end if;

                if std_match(addr, VCONST_ADDR) then
                    -- write to selected component of selected vertex program constant
                    case addr(3 downto 2) is
                        when "00" =>
                            vprog_constant_0(to_integer(unsigned(addr(1 downto 0)))) <= data_in;
                        when "01" =>
                            vprog_constant_1(to_integer(unsigned(addr(1 downto 0)))) <= data_in;
                        when "10" =>
                            vprog_constant_2(to_integer(unsigned(addr(1 downto 0)))) <= data_in;
                        when "11" =>
                            vprog_constant_3(to_integer(unsigned(addr(1 downto 0)))) <= data_in;
                        when others =>
                            -- can't get here
                    end case;
                end if;

                if std_match(addr, VDATA_SIZE_ADDR) then
                    -- write to data size register
                    v_data_size <= data_in;
                end if;

                if std_match(addr, VDATA_BASE_ADDR) then
                    -- write to data base addr register
                    v_data_base <= data_in;
                end if;

                if std_match(addr, VCOUNT_ADDR) then
                    -- write to vertex count register
                    v_count <= data_in;
                end if;
            end if;
        end if;
    end process;
end architecture;