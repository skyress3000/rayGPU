----------------------------------------------------------------------------
--
--  Float to integer color module
--
--  Converts a float to a 6-bit integer color, mapping 1.0 -> 64
--
--  Revision History:
--     20 May 21  Ray Wendt         Initial revision.
--      8 Jun 21  Ray Wendt         Fixed syntax errors.
--     16 Jun 21  Ray Wendt         Added header comment.
--
----------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.gpu_float.all;

--  float_to_color
--
--  Description:
--      This module casts a floating point value to a 6-bit integer color value.
--      An input of 1/64 corresponds to one bit of output, and 0 or any negative
--      value gives 0, so an input of 63/64 or greater gives an output of 63.
--
entity float_to_color is
    port (
        -- float to convert to color
        float_in    : in    float;
        -- output color where 1/64 in float_in corresponds to one LSB, so any 
        -- float_in >= 63/64 results in color = 63, any float_in < 1/64 results in
        -- color = 0, and scales linearly between those
        color       : out   unsigned(5 downto 0)
    );
end entity;

architecture behavioral of float_to_color is
    -- mantissa of input
    signal in_mantissa: unsigned(10 downto 0);
begin
    in_mantissa <= mantissa(float_in);

            -- clamp any negative value or value < 1/64 to 0
    color <= "000000"                           when (sign(float_in) = '1') or (exponent(float_in) < 9) else
            -- clamp any value >= 64 to 63 (max color)
             "111111"                           when exponent(float_in) >= 15 else
             -- else shift the mantissa appropriately to cast to 6-bit int
             in_mantissa(10 downto 5)           when exponent(float_in) = 14 else
             "0" & in_mantissa(10 downto 6)     when exponent(float_in) = 13 else
             "00" & in_mantissa(10 downto 7)    when exponent(float_in) = 12 else
             "000" & in_mantissa(10 downto 8)   when exponent(float_in) = 11 else
             "0000" & in_mantissa(10 downto 9)  when exponent(float_in) = 10 else
             "00000" & in_mantissa(10);         --when exponent(float_in) = 9
end architecture;