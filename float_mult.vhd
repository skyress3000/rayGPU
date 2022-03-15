----------------------------------------------------------------------------
--
--  Floating point multiplier
--
--  Multiplies 2 half-precision (16 bit) floating point values.
--
--  Revision History:
--     28 Apr 21  Ray Wendt         Initial revision.
--      2 May 21  Ray Wendt         Added over/underflow clamping.
--     11 May 21  Ray Wendt         Fixed syntax issues.
--     22 May 21  Ray Wendt         Fixed clamping issues.
--     22 May 21  Ray Wendt         Added extra bit to DSP slice because high
--                                  bit 1 makes it do weird things.
--     22 May 21  Ray Wendt         Fixed another clamping issue.
--     22 May 21  Ray Wendt         Added clock delay to sign+exp.
--     15 Jun 21  Ray Wendt         Added header comment.
--
----------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library unimacro;
use unimacro.vcomponents.all;

use work.gpu_float.all;

--  Description: 
--      This module implements a 16-bit floating point multiplier. The multiplier invokes
--      the Xilinx DSP primitive multiplier to multiply the mantissas, which introduces
--      one clock of delay between its inputs and outputs. No rounding is performed, i.e.
--      the low bits of the product of mantissas are truncated. Underflow results in an 
--      output of 0 and overflow results in -inf or +inf as appropriate. Multiplication by
--      zero always gives 0 (with the appropriate sign), and multiplication of a nonzero
--      value with infinity always gives infinity.
entity float_mult is
    port (
        -- system clock
        clk             : in    std_logic;
        -- the two values to be multiplied
        multiplicand_a  : in    float;
        multiplicand_b  : in    float;
        -- their product, delayed by 1 clock cycle
        product         : out   float
    );
end entity;

architecture behavioral of float_mult is
    -- product of signs
    signal sign_product         : std_logic;
    -- product of mantissas as 11-bit unsigned values
    signal raw_mantissa_product : std_logic_vector(23 downto 0);
    -- trimmed mantissa product with leading one cut off
    signal mantissa_product     : unsigned(9 downto 0);
    -- sum of the two exponents, with an extra bit to detect overflow
    signal exp_sum              : unsigned(5 downto 0);
    -- exponent of the product, i.e. exp_sum-15 to normalize it, with an extra
    -- 1 added if the high bit of the product was set
    -- also has extra bit to detect underflow
    signal exp_product          : unsigned(5 downto 0);
    -- registers high if either operand was infinity or zero as appropriate
    signal inf_in               : std_logic;
    signal zero_in              : std_logic;
    -- high if exp_product summed to exactly 0 or infinity
    signal prod_inf             : std_logic;
    signal prod_zero            : std_logic;

    signal test_ma              : std_logic_vector(10 downto 0);
    signal test_mb              : std_logic_vector(10 downto 0);
begin
    -- instantiate the DSP slice multiplier to multiply the mantissas
    MULT_MACRO_inst : MULT_MACRO
    generic map (
        DEVICE  => "7SERIES",
        LATENCY => 1,   -- use minimum latency
        WIDTH_A => 12,  -- inputs are 11 bit + 1 because DSP multiplier doesn't
                        -- like high bit set to 1
        WIDTH_B => 12
    )
    port map (
        P       => raw_mantissa_product,
        A       => '0' & std_logic_vector(mantissa(multiplicand_a)),
        B       => '0' & std_logic_vector(mantissa(multiplicand_b)),
        CE      => '1', -- always enabled
        CLK     => clk,
        RST     => '0'  -- never need to reset
    );

    test_ma <= std_logic_vector(mantissa(multiplicand_a));
    test_mb <= std_logic_vector(mantissa(multiplicand_b));

    -- get the 10 bits after the leading 1 in the product, which is either in the
    -- leftmost bit if we got a carry, or the second to leftmost otherwise (since 
    -- each mantissa is >= 1.0000000000)
    mantissa_product <= unsigned(raw_mantissa_product(20 downto 11)) when raw_mantissa_product(21) = '1' else
                        unsigned(raw_mantissa_product(19 downto 10));

    -- clock exponent and sign calculation to match delay in multiplier
    process(clk)
    begin
        if rising_edge(clk) then
            -- multiply the signs
            sign_product <= sign(multiplicand_a) xor sign(multiplicand_b);
            -- sum the exponents
            exp_sum <= ('0' & exponent(multiplicand_a)) + ('0' & exponent(multiplicand_b));

            zero_in <= '1' when (exponent(multiplicand_a) = "00000") or (exponent(multiplicand_b) = "00000") else
                       '0';
            inf_in  <= '1' when (exponent(multiplicand_a) = "11111") or (exponent(multiplicand_b) = "11111") else
                       '0';
        end if;
    end process;

    -- normalize exponent and increment if product added a digit on the left
    exp_product <= "100000"     when zero_in = '1' else -- product with 0 is 0
                   "111111"     when inf_in = '1' else  -- product with inf is inf
                   exp_sum - 14 when raw_mantissa_product(21) = '1' else
                   exp_sum - 15;

    -- test if the sum was inf or zero
    prod_inf    <= '1' when exp_product(4 downto 0) = "11111" else
                   '0';
    prod_zero   <= '1' when exp_product(4 downto 0) = "00000" else
                   '0';

    -- combine sign, exp, and mantissa to get product
    -- clamp exponent to 0 if we have underflow
    -- clamp exponent to all 1s if we have overflow
    -- set mantissa to 0 if result is 0 or infinity
    product <= sign_product 
        & std_logic_vector((exp_product(4 downto 0) and not exp_product(5)) or (exp_product(5) and exp_sum(5)))
        & std_logic_vector(mantissa_product and (not exp_product(5)) and (not prod_inf) and (not prod_zero));
end architecture;