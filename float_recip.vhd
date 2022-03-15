----------------------------------------------------------------------------
--
--  Float reciprocal calculator module
--
--  Description
--
--  Revision History:
--     20 May 21  Ray Wendt         Initial revision.
--      8 Jun 21  Ray Wendt         Fixed syntax errors.
--     15 Jun 21  Ray Wendt         Added header comment.
--
----------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.gpu_float.all;

--  Description: 
--
--      This module computes the reciprocal of a 16-bit floating point value. The
--      reciprocal of the mantissa is calculated using a lookup table on the first
--      6 bits, so it is not performed to full precision, with a maximum error of
--      1.5% (rounded up). Underflow is clamped to 0 and overflow is clamped to
--      infinity. Also, the reciprocal of 0 is given as infinity and vice versa.
--
entity float_recip is
    port (
        -- input float to compute the reciprocal of
        f       : in  float;
        -- output float = 1/f
        recip   : out float
    );
end entity;

architecture behavioral of float_recip is
    -- table to approximate the inverse of the mantissa
    --
    -- table entries are the 10-bit mantissa (not including leading 1) of 1/f,
    -- where the first 6 bits of the mantissa of f (again not including leading 1)
    -- are the index in the table
    --
    -- entry values are the mantissas taken from the inv_lookup table computed in 
    -- GPU_model.py
    type inv_mantissa_table_t is array (0 to 63) of unsigned(9 downto 0);
    constant inv_mantissa_table : inv_mantissa_table_t := 
        ("0000000000", "1111100000", "1111000010", "1110100100", 
        "1110001000", "1101101100", "1101010000", "1100110110", 
        "1100011100", "1100000100", "1011101011", "1011010100", 
        "1010111101", "1010100110", "1010010000", "1001111011", 
        "1001100110", "1001010010", "1000111110", "1000101011",
        "1000011000", "1000000110", "0111110100", "0111100011", 
        "0111010001", "0111000001", "0110110000", "0110100000", 
        "0110010001", "0110000001", "0101110010", "0101100100", 
        "0101010101", "0101000111", "0100111001", "0100101100", 
        "0100011111", "0100010010", "0100000101", "0011111001",
        "0011101100", "0011100000", "0011010101", "0011001001", 
        "0010111110", "0010110010", "0010101000", "0010011101", 
        "0010010010", "0010001000", "0001111110", "0001110100", 
        "0001101010", "0001100000", "0001010111", "0001001101", 
        "0001000100", "0000111011", "0000110010", "0000101010",
        "0000100001", "0000011001", "0000010000", "0000001000");

    -- exponent and mantissa of the input float
    signal exponent_in: unsigned(4 downto 0);
    signal mantissa_in: unsigned(10 downto 0);

    -- exponent of the inverse
    signal new_exponent: unsigned(4 downto 0);
    -- mantissa of the inverse
    signal new_mantissa: unsigned(9 downto 0);
begin
    exponent_in <= exponent(f);
    mantissa_in <= mantissa(f);

                    -- 0 input gives infinity output
    new_exponent <= "11111"             when exponent_in = "00000" else
                    -- infinity input gives 0 output
                    -- 2^15 input also gives 0 output because it would map to subnormal
                    "00000"             when exponent_in(4 downto 1) = "1111" else
                    -- if mantissa is 1.0, exponent is just -exponent(f)
                    30 - exponent_in    when mantissa_in = "10000000000" else
                    -- otherwise need to subtract an extra one to account for a
                    -- mantissa > 1.0 getting inverted into a value < 1.0
                    -- this subtracting an extra 1 means that > 2^14 also maps to
                    -- subnormal
                    29 - exponent_in;

                    -- if output is 0 or inf, mantissa should be 0
    new_mantissa <= (others => '0') when (new_exponent = "11111") or (new_exponent = "00000") else
                    -- otherwise approximate the inverse of the mantissa using the
                    -- lookup table with the upper 6 bits of the input mantissa, not
                    -- including the leading 1
                    inv_mantissa_table(to_integer(mantissa_in(9 downto 4)));

    -- construct the reciprocal from computed elements
    recip <= sign(f) & std_logic_vector(new_exponent) & std_logic_vector(new_mantissa);
end architecture;