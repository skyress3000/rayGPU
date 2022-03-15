----------------------------------------------------------------------------
--
--  GPU Shader Processor
--
--  Revision History:
--     25 Apr 21  Ray Wendt         Initial revision.
--     28 Apr 21  Ray Wendt         Added multiply operation.
--     11 May 21  Ray Wendt         Fixed syntax issues.
--     17 May 21  Ray Wendt         Changed op state to an input since it's also
--                                  needed by the IR control logic.
--      9 Jun 21  Ray Wendt         Switched dot product to use only 3 adders in a
--                                  configuration more optimized for timing.
--     10 Jun 21  Ray Wendt         Added missing result entry for MOV.
--     16 Jun 21  Ray Wendt         Added header comment.
--
----------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.gpu_float.all;

-- shader_proc
--
--  Description: 
--      This module implements a single shader processor used for vertex and
--      fragment shading. It has an input bus to write to input registers, as
--      well as inputs for control lines taken directly from encoded instructions
--      to be executed while instruction execution is enabled. The processor 
--      contains 12 quad registers; 4 inputs, 4 scratch, and 4 outputs, as well as
--      inputs for 4 constant registers. See the individual input descriptions and
--      user manual for details on the instruction set.
--
entity shader_proc is
    port (
        -- system clock
        clk             : in        std_logic;
        -- active low reset
        rst             : in        std_logic;

        -- processor enable, will only store instruction result if enable high
        enable          : in        std_logic;

        -- high to write in_reg_wr_val to the select input register component
        store_in_reg    : in        std_logic;
        -- index of input register to write to
        in_reg_idx      : in        unsigned(1 downto 0);
        -- index of component of input register to write to
        in_reg_comp     : in        unsigned(1 downto 0);
        -- value to write into input register
        in_reg_wr_val   : in        float;

        -- operand A control

        -- operand A select
        -- 0-3:   input reg 0-3
        -- 4-7:   constant reg 0-3
        -- 8-11:  scratch reg 0-3
        op_a_sel        : in        unsigned(3 downto 0);
        -- operand A swizzle
        -- ith element of operand A is selected by the ith pair of swizzle bits
        -- as an index from the source register, i.e.
        -- OpA[i] = SrcA[swizzle[i*2+1 downto i*2]]
        -- for i in 0 to 3
        op_a_swizzle    : in        std_logic_vector(7 downto 0);
        -- operand A negate
        -- negates operand A if high
        op_a_negate     : in        std_logic;

        -- operand B control

        -- (same as operand A lines)
        op_b_sel        : in        unsigned(3 downto 0);
        op_b_swizzle    : in        std_logic_vector(7 downto 0);
        op_b_negate     : in        std_logic;

        -- destination control

        -- destination select
        -- 0-3: scratch reg 0-3
        -- 4-7: output reg 0-3
        dest_sel        : in        unsigned(2 downto 0);
        -- destination mask, for each bit, if high that component of the destination
        -- register will update, otherwise it will hold its value
        dest_mask       : in        std_logic_vector(3 downto 0);

        -- instruction opcode
        -- 000: MOV
        -- 001: MUL
        -- 010: ADD
        -- 011: DP3
        -- 100: DP4
        -- 101: END
        opcode          : in        std_logic_vector(2 downto 0);

        -- counts instruction clock, high indicates 2nd clock when result should 
        -- be stored for 2 clock instructions
        op_state        : in        std_logic;

        -- global constant register values
        constant_reg_0  : in        quad;
        constant_reg_1  : in        quad;
        constant_reg_2  : in        quad;
        constant_reg_3  : in        quad;

        -- output register values
        output_reg_0    : buffer    quad;
        output_reg_1    : buffer    quad;
        output_reg_2    : buffer    quad;
        output_reg_3    : buffer    quad
    );
end entity;

architecture behavioral of shader_proc is
    -- component declarations for floating point arithmetic
    component float_adder is
        port (
            addend_a    : in    float;
            addend_b    : in    float;
            sum         : out   float
        );
    end component;

    component float_mult is
        port (
            clk             : in    std_logic;
            multiplicand_a  : in    float;
            multiplicand_b  : in    float;
            product         : out   float
        );
    end component;

    -- opcode constants
    constant OP_MOV:            std_logic_vector(2 downto 0) := "000";
    constant OP_MUL:            std_logic_vector(2 downto 0) := "001";
    constant OP_ADD:            std_logic_vector(2 downto 0) := "010";
    constant OP_DP3:            std_logic_vector(2 downto 0) := "011";
    constant OP_DP4:            std_logic_vector(2 downto 0) := "100";
    constant OP_END:            std_logic_vector(2 downto 0) := "101";

    -- control line to write result to register, high if writing
    signal reg_update:          std_logic;

    -- input registers
    signal in_reg_0:            quad;
    signal in_reg_1:            quad;
    signal in_reg_2:            quad;
    signal in_reg_3:            quad;
    -- internal scratch registers
    signal scratch_reg_0:       quad;
    signal scratch_reg_1:       quad;
    signal scratch_reg_2:       quad;
    signal scratch_reg_3:       quad;

    -- values of selected registers for operands a and b
    signal operand_a_reg:       quad;
    signal operand_b_reg:       quad;
    -- selected operand a and b with swizzle applied
    signal operand_a_sw_val:    quad;
    signal operand_b_sw_val:    quad;
    -- selected operand a and b with swizzle and negate applied
    signal operand_a:           quad;
    signal operand_b:           quad;

    -- result of componentwise adder
    signal add_result:          quad;
    -- result of componentwise multiply
    signal mult_result:         quad;
    -- sum of multiply components 0 and 1
    signal dot_product_01:      float;
    -- multiply component 3 as input to dot product, zerod for DP3
    signal dot_in_3:            float;
    -- sum of multiply components 2 and 3
    signal dot_product_23:      float;
    signal dot_product_sum:     float;

    -- result of operation, selected from the above 3 or just operand A depending 
    -- on opcode
    signal op_result:           quad;
begin
    -- select operand a and b register values
    operand_a_reg <= in_reg_0       when op_a_sel = 0 else
                     in_reg_1       when op_a_sel = 1 else
                     in_reg_2       when op_a_sel = 2 else
                     in_reg_3       when op_a_sel = 3 else
                     constant_reg_0 when op_a_sel = 4 else
                     constant_reg_1 when op_a_sel = 5 else
                     constant_reg_2 when op_a_sel = 6 else
                     constant_reg_3 when op_a_sel = 7 else
                     scratch_reg_0  when op_a_sel = 8 else
                     scratch_reg_1  when op_a_sel = 9 else
                     scratch_reg_2  when op_a_sel = 10 else
                     scratch_reg_3  when op_a_sel = 11 else
                     QUAD_ZERO; -- should never get here
    
    operand_b_reg <= in_reg_0       when op_b_sel = 0 else
                     in_reg_1       when op_b_sel = 1 else
                     in_reg_2       when op_b_sel = 2 else
                     in_reg_3       when op_b_sel = 3 else
                     constant_reg_0 when op_b_sel = 4 else
                     constant_reg_1 when op_b_sel = 5 else
                     constant_reg_2 when op_b_sel = 6 else
                     constant_reg_3 when op_b_sel = 7 else
                     scratch_reg_0  when op_b_sel = 8 else
                     scratch_reg_1  when op_b_sel = 9 else
                     scratch_reg_2  when op_b_sel = 10 else
                     scratch_reg_3  when op_b_sel = 11 else
                     QUAD_ZERO; -- should never get here

    -- swizzle and negate each component of each operand
    op_component_select: for i in 0 to 3 generate
        -- apply swizzle to operands by selecting component of operand register with
        -- index from swizzle select input
        operand_a_sw_val(i) <= operand_a_reg(0) when op_a_swizzle(i*2+1 downto i*2) = "00" else
                               operand_a_reg(1) when op_a_swizzle(i*2+1 downto i*2) = "01" else
                               operand_a_reg(2) when op_a_swizzle(i*2+1 downto i*2) = "10" else
                               operand_a_reg(3);

        operand_b_sw_val(i) <= operand_b_reg(0) when op_b_swizzle(i*2+1 downto i*2) = "00" else
                               operand_b_reg(1) when op_b_swizzle(i*2+1 downto i*2) = "01" else
                               operand_b_reg(2) when op_b_swizzle(i*2+1 downto i*2) = "10" else
                               operand_b_reg(3);

        -- flip sign of each component if negating
        operand_a(i) <= (sign(operand_a_sw_val(i)) xor op_a_negate) & operand_a_sw_val(i)(14 downto 0);
        operand_b(i) <= (sign(operand_b_sw_val(i)) xor op_b_negate) & operand_b_sw_val(i)(14 downto 0);
    end generate op_component_select;

    -- generate adder for each component
    componentwise_add: for i in 0 to 3 generate
        adder_i: float_adder port map (
            addend_a    => operand_a(i),
            addend_b    => operand_b(i),
            sum         => add_result(i));
    end generate componentwise_add;

    -- generate multiplier for each component
    componentwise_mult: for i in 0 to 3 generate
        multiplier_i: float_mult port map (
            clk             => clk,
            multiplicand_a  => operand_a(i),
            multiplicand_b  => operand_b(i),
            product         => mult_result(i));
    end generate componentwise_mult;

    -- add multiply result 0 and 1
    dot_01: float_adder port map (
        addend_a    => mult_result(0),
        addend_b    => mult_result(1),
        sum         => dot_product_01);

    -- select multiply result 3 or 0 depending on DP instruction
    dot_in_3 <= (others => '0') when opcode = OP_DP3 else
                mult_result(3);

    -- add multiply result 2 and 3
    dot_23: float_adder port map (
        addend_a    => mult_result(2),
        addend_b    => dot_in_3,
        sum         => dot_product_23);

    -- add intermediate sums to get dot product
    dot_sum: float_adder port map (
        addend_a    => dot_product_01,
        addend_b    => dot_product_23,
        sum         => dot_product_sum);

    -- select result based on opcode
    op_result <= add_result     when opcode = OP_ADD else
                 mult_result    when opcode = OP_MUL else
                 (dot_product_sum, dot_product_sum, dot_product_sum, dot_product_sum) 
                                when (opcode = OP_DP3) or (opcode = OP_DP4) else
                 operand_a      when opcode = OP_MOV else
                 QUAD_ZERO;

    -- update destination register when enabled
    -- END instruction does not write to dest
    -- instructions involving multiply do not write to dest on 1st clock
    reg_update <= '1' when (opcode /= OP_END)
                  and not (((opcode = OP_DP4) or (opcode = OP_DP3) or (opcode = OP_MUL)) and (op_state = '0')) 
                  and (enable = '1')
                  else '0';

    -- process to update registers
    process(clk)
    begin
        if rising_edge(clk) then
            -- reset all regs to 0
            if rst = '0' then
                in_reg_0 <= QUAD_ZERO;
                in_reg_1 <= QUAD_ZERO;
                in_reg_2 <= QUAD_ZERO;
                in_reg_3 <= QUAD_ZERO;
                scratch_reg_0 <= QUAD_ZERO;
                scratch_reg_1 <= QUAD_ZERO;
                scratch_reg_2 <= QUAD_ZERO;
                scratch_reg_3 <= QUAD_ZERO;
                output_reg_0 <= QUAD_ZERO;
                output_reg_1 <= QUAD_ZERO;
                output_reg_2 <= QUAD_ZERO;
                output_reg_3 <= QUAD_ZERO;
            else
                if reg_update = '1' then
                    -- update the selected destination register
                    case dest_sel is
                        -- mux each component based on mask
                        -- having the for loop repeated is ugly but it should synthesize
                        -- better by not repeating the entire destination case statement 
                        -- 4 times
                        when "000" =>
                            for i in 0 to 3 loop
                                scratch_reg_0(i) <= (op_result(i) and dest_mask(i)) or
                                                    (scratch_reg_0(i) and not dest_mask(i));
                            end loop;
                        when "001" =>
                            for i in 0 to 3 loop
                                scratch_reg_1(i) <= (op_result(i) and dest_mask(i)) or
                                                    (scratch_reg_1(i) and not dest_mask(i));
                            end loop;
                        when "010" =>
                            for i in 0 to 3 loop
                                scratch_reg_2(i) <= (op_result(i) and dest_mask(i)) or
                                                    (scratch_reg_2(i) and not dest_mask(i));
                            end loop;
                        when "011" =>
                            for i in 0 to 3 loop
                                scratch_reg_3(i) <= (op_result(i) and dest_mask(i)) or
                                                    (scratch_reg_3(i) and not dest_mask(i));
                            end loop;
                        when "100" =>
                            for i in 0 to 3 loop
                                output_reg_0(i) <= (op_result(i) and dest_mask(i)) or
                                                   (output_reg_0(i) and not dest_mask(i));
                            end loop;
                        when "101" =>
                            for i in 0 to 3 loop
                                output_reg_1(i) <= (op_result(i) and dest_mask(i)) or
                                                   (output_reg_1(i) and not dest_mask(i));
                            end loop;
                        when "110" =>
                            for i in 0 to 3 loop
                                output_reg_2(i) <= (op_result(i) and dest_mask(i)) or
                                                   (output_reg_2(i) and not dest_mask(i));
                            end loop;
                        when "111" =>
                            for i in 0 to 3 loop
                                output_reg_3(i) <= (op_result(i) and dest_mask(i)) or
                                                   (output_reg_3(i) and not dest_mask(i));
                            end loop;
                        when others =>
                            -- do nothing
                    end case;
                end if;

                if store_in_reg = '1' then
                    -- write to selected component of selected input register
                    case in_reg_idx is
                        when "00" =>
                            in_reg_0(to_integer(in_reg_comp)) <= in_reg_wr_val;
                        when "01" =>
                            in_reg_1(to_integer(in_reg_comp)) <= in_reg_wr_val;
                        when "10" =>
                            in_reg_2(to_integer(in_reg_comp)) <= in_reg_wr_val;
                        when "11" =>
                            in_reg_3(to_integer(in_reg_comp)) <= in_reg_wr_val;
                        when others =>
                            -- do nothing
                    end case;
                end if;
            end if;
        end if;
    end process;
end architecture;