----------------------------------------------------------------------------
--
--  Floating point adder
--
--  Adds 2 half-precision (16 bit) floating point values.
--
--  Revision History:
--     23 Apr 21  Ray Wendt         Initial revision.
--     25 Apr 21  Ray Wendt         Finished logic for mantissa sum shifting.
--     11 May 21  Ray Wendt         Fixed syntax issues.
--      4 Jun 21  Ray Wendt         Added infinity handling.
--      4 Jun 21  Ray Wendt         Fixed mantissa for 0 operands.
--      6 Jun 21  Ray Wendt         Fixed handling of overflow and underflow with
--                                  non-zero/inf operands.
--      6 Jun 21  Ray Wendt         Fixed first barrel shifter indexing.
--     12 Jun 21  Ray Wendt         Added entity description.
--
----------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.gpu_float.all;

--  Description: 
--
--    This module implements a 16-bit floating point adder. No rounding
--    is performed, i.e. the two mantissas are aligned to the same power of two and
--    added with any non-overlapping bits in the operands being truncated. Underflow
--    results in an output of 0 and overflow results in -inf or +inf as appropriate.
--    If the first operand is infinity, the sum will always be infinity with the same
--    sign. If the second operand is infinity and the first is not, the sum will be
--    infinity with the sign matching the second operand.
--
entity float_adder is
    port (
        -- the two values to be added
        addend_a    : in    float;
        addend_b    : in    float;
        -- their sum
        sum         : out   float
    );
end entity;

architecture behavioral of float_adder is
    -- whether the exponent of a is smaller than b
    signal a_smaller                : std_logic;
    -- addend with smaller exponent
    signal smaller                  : float;
    -- addend with larger exponent
    signal larger                   : float;
    -- difference between larger and smaller exponent, tells us how many bits
    -- to shift the smaller mantissa by
    signal exponent_diff            : unsigned(4 downto 0);
    -- mantissa of smaller value
    signal small_mantissa           : unsigned(10 downto 0);
    -- mantissa of larger value
    signal large_mantissa           : unsigned(10 downto 0);
    -- mantissa of smaller value shifted to match the larger exponent
    signal small_mantissa_shifted   : unsigned(10 downto 0);
    -- mantissas represented in Q2.10 plus sign bit, 2s complement format to be added
    -- extra bit before . to avoid overflow
    signal mantissa_signed_1        : unsigned(12 downto 0);
    signal mantissa_signed_2        : unsigned(12 downto 0);
    -- sum of signed mantissas
    signal mantissa_sum             : unsigned(12 downto 0);
    -- abs value of signed mantissa
    signal mantissa_sum_abs         : unsigned(11 downto 0);
    -- absolute value of mantissa sum, shifted so the leading 1 is on the left
    signal mantissa_sum_norm        : unsigned(10 downto 0);
    -- negative change in exponent caused by normalization of mantissa
    signal exponent_shift           : unsigned(5 downto 0);
    -- larger exponent minus exponent_shift, with extra bit to detect under or
    -- overflow
    signal exponent_shifted         : unsigned(6 downto 0);
    -- exponent of the result, which gets exponent_shifted, except clamped to all
    -- 1s (infinity) for overflow or all 0s for underflow
    signal final_exponent           : unsigned(4 downto 0);
    -- the sign of the result -- usually the sign of the mantissa sum, except
    -- for cases involving infinity
    signal final_sign               : std_logic;
    -- mantissa of the result, clamped to 0 for 0 and infinity
    signal final_mantissa           : unsigned(9 downto 0);
begin
    -- test if a exponent smaller
    a_smaller <= '1' when exponent(addend_a) < exponent(addend_b) else
                 '0';

    -- select the smaller and larger addend
    smaller <= addend_a when a_smaller = '1' else
               addend_b;
    larger  <= addend_b when a_smaller = '1' else
               addend_a;

    -- find the difference between the exponents
    exponent_diff <= exponent(larger) - exponent(smaller);

    -- mantissa should be all 0s when input is 0
    small_mantissa <= (others => '0') when exponent(smaller) = 0 else 
                      mantissa(smaller);
    large_mantissa <= (others => '0') when exponent(larger) = 0 else 
                      mantissa(larger);

    -- barrel shifter to make mantissas match
    -- if the difference in exponent is greater than 10 then the smaller mantissa 
    -- just gets shifted all the way down to 0
    small_mantissa_shifted <= small_mantissa                            when exponent_diff = 0 else
                              "0" & small_mantissa(10 downto 1)         when exponent_diff = 1 else
                              "00" & small_mantissa(10 downto 2)        when exponent_diff = 2 else
                              "000" & small_mantissa(10 downto 3)       when exponent_diff = 3 else
                              "0000" & small_mantissa(10 downto 4)      when exponent_diff = 4 else
                              "00000" & small_mantissa(10 downto 5)     when exponent_diff = 5 else
                              "000000" & small_mantissa(10 downto 6)    when exponent_diff = 6 else
                              "0000000" & small_mantissa(10 downto 7)   when exponent_diff = 7 else
                              "00000000" & small_mantissa(10 downto 8)  when exponent_diff = 8 else
                              "000000000" & small_mantissa(10 downto 9) when exponent_diff = 9 else
                              "0000000000" & small_mantissa(10)         when exponent_diff = 10 else
                              (others => '0');

    -- negate mantissas if signed, and convert to signed Q2.10
    mantissa_signed_1 <= "00" & small_mantissa_shifted when sign(smaller) = '0' else
                         (not ("00" & small_mantissa_shifted)) + 1;
    mantissa_signed_2 <= "00" & large_mantissa when sign(larger) = '0' else
                         (not ("00" & large_mantissa)) + 1;

    -- sum signed mantissas and take absolute value
    mantissa_sum <= mantissa_signed_1 + mantissa_signed_2;

    mantissa_sum_abs <= mantissa_sum(11 downto 0) when mantissa_sum(12) = '0' else
                        (not mantissa_sum(11 downto 0)) + 1;

    -- barrel shifter to get mantissa back to 1. etc
    mantissa_sum_norm <= mantissa_sum_abs(11 downto 1)                  when mantissa_sum_abs(11) = '1' else 
                         mantissa_sum_abs(10 downto 0)                  when mantissa_sum_abs(10) = '1' else 
                         mantissa_sum_abs(9 downto 0) & "0"             when mantissa_sum_abs(9) = '1' else 
                         mantissa_sum_abs(8 downto 0) & "00"            when mantissa_sum_abs(8) = '1' else 
                         mantissa_sum_abs(7 downto 0) & "000"           when mantissa_sum_abs(7) = '1' else 
                         mantissa_sum_abs(6 downto 0) & "0000"          when mantissa_sum_abs(6) = '1' else 
                         mantissa_sum_abs(5 downto 0) & "00000"         when mantissa_sum_abs(5) = '1' else 
                         mantissa_sum_abs(4 downto 0) & "000000"        when mantissa_sum_abs(4) = '1' else 
                         mantissa_sum_abs(3 downto 0) & "0000000"       when mantissa_sum_abs(3) = '1' else 
                         mantissa_sum_abs(2 downto 0) & "00000000"      when mantissa_sum_abs(2) = '1' else 
                         mantissa_sum_abs(1 downto 0) & "000000000"     when mantissa_sum_abs(1) = '1' else 
                         mantissa_sum_abs(0)          & "0000000000";

    -- counts leading 0s of the mantissa sum to know how much we shifted by
    exponent_shift <= "111111"              when mantissa_sum_abs(11) = '1' else -- exponent_shift = -1
                      to_unsigned(0, 6)     when mantissa_sum_abs(10) = '1' else 
                      to_unsigned(1, 6)     when mantissa_sum_abs(9) = '1' else 
                      to_unsigned(2, 6)     when mantissa_sum_abs(8) = '1' else 
                      to_unsigned(3, 6)     when mantissa_sum_abs(7) = '1' else 
                      to_unsigned(4, 6)     when mantissa_sum_abs(6) = '1' else 
                      to_unsigned(5, 6)     when mantissa_sum_abs(5) = '1' else 
                      to_unsigned(6, 6)     when mantissa_sum_abs(4) = '1' else 
                      to_unsigned(7, 6)     when mantissa_sum_abs(3) = '1' else 
                      to_unsigned(8, 6)     when mantissa_sum_abs(2) = '1' else 
                      to_unsigned(9, 6)     when mantissa_sum_abs(1) = '1' else 
                      to_unsigned(10, 6)    when mantissa_sum_abs(0) = '1' else 
                      to_unsigned(11, 6);

    -- subtract the shift from the xponent
    exponent_shifted <= ("00" & exponent(larger)) - exponent_shift;

    -- select final exponent
    final_exponent  <=  (others => '1') when (exponent(addend_a) = "11111") -- exponent of result is infinity if
                            or (exponent(addend_b) = "11111") else          -- either input is infinity
                        (others => '0') when (exponent_shift = 11)          -- exponent of result is 0 if sum is 0
                            or (exponent_shifted(5) = '1') else             -- or we underflowed
                        exponent_shifted(4 downto 0);

    -- sign of result in infinity cases comes from first infinity of the two addends
    -- otherwise get sign from the signed mantissa sum
    final_sign  <=  sign(addend_a) when exponent(addend_a) = "11111" else
                    sign(addend_b) when exponent(addend_b) = "11111" else
                    mantissa_sum(12);

    -- clamp the mantissa to 0 if result is inf or 0, otherwise get normalized mantissa
    final_mantissa  <=  (others => '0') when (final_exponent = "11111") or (final_exponent = "00000") else
                        mantissa_sum_norm(9 downto 0);

    -- combine result sign, exponent, and mantissa to get floating point sum
    sum <= final_sign
        & std_logic_vector(final_exponent)
        & std_logic_vector(final_mantissa);
end architecture;