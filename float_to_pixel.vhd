----------------------------------------------------------------------------
--
--  Float to integer pixel coordinate module
--
--  Converts a float to an 8-bit signed pixel coord, mapping (-1.0, 127/128) ->
--  (-128,127)
--
--  Revision History:
--     21 May 21  Ray Wendt         Initial revision.
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
--      This module casts a floating point value to an 8-bit integer pixel coordinate.
--      An input of 1/128 corresponds to one bit of output, and the limits are given by
--      input of -1.0 or less give -128 output, and input or 127/128 or greater give
--      127 output.
--
entity float_to_pixel is
    port (
        -- float to convert to pixel coord
        float_in    : in    float;
        -- output pixel coord with (-1.0, 127/128) in -> (-128,127) out
        -- values outside input domain clamp to max or min output
        pixel       : out   signed(7 downto 0)
    );
end entity;

architecture behavioral of float_to_pixel is
    -- mantissa of input float
    signal mantissa_in: unsigned(10 downto 0);
    -- absolute value of output value
    signal abs_pixel: signed(7 downto 0);
begin
    mantissa_in <= mantissa(float_in);

                 -- clamp any value < 1/128 to 0
    abs_pixel <= "00000000"                                     when exponent(float_in) < 8 else
                 -- special case -1.0 -> -128
                 "10000000"                                     when float_in = "1011110000000000" else
                 -- clamp any other value with abs >= 1.0 to 127
                 "01111111"                                     when exponent(float_in) >= 15 else
                 -- else shift the mantissa appropriately to cast to positive 8-bit signed int
                 "0" & signed(mantissa_in(10 downto 4))         when exponent(float_in) = 14 else
                 "00" & signed(mantissa_in(10 downto 5))        when exponent(float_in) = 13 else
                 "000" & signed(mantissa_in(10 downto 6))       when exponent(float_in) = 12 else
                 "0000" & signed(mantissa_in(10 downto 7))      when exponent(float_in) = 11 else
                 "00000" & signed(mantissa_in(10 downto 8))     when exponent(float_in) = 10 else
                 "000000" & signed(mantissa_in(10 downto 9))    when exponent(float_in) = 9 else
                 "0000000" & mantissa_in(10);                   --when exponent(float_in) = 8

    -- negate if input is negative
    pixel <= -abs_pixel when sign(float_in) = '1' else
             abs_pixel;
end architecture;