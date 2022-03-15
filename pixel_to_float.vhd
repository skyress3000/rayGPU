----------------------------------------------------------------------------
--
--  Integer to float pixel coordinate module
--
--  Converts an 8-bit signed pixel coord to a float, mapping (-128,127) ->
--  (-1.0, 127/128)
--
--  Revision History:
--      6 Jun 21  Ray Wendt         Initial revision.
--      8 Jun 21  Ray Wendt         Fixed syntax errors.
--     16 Jun 21  Ray Wendt         Added header comment.
--
----------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.gpu_float.all;

-- pixel_to_float
--
--  Description:
--      This module casts a signed 8-bit pixel coordinate to floating point
--      NDC coordinate, with -128 mapping to -1.0 and 127 mapping to 127/128.
--
entity pixel_to_float is
    port (
        -- pixel coord to convert to float
        pixel_in    : in    signed(7 downto 0);
        -- output float coord with (-128,127) in -> (-1.0, 127/128) out
        float_out   : out   float
    );
end entity;

architecture behavioral of pixel_to_float is
    -- absolute value of input value
    signal abs_pixel:           signed(7 downto 0);
    -- upper 6 bits of the result mantissa (without a leading 1)
    signal mantissa_rounded:    unsigned(5 downto 0);
    -- exponent of the result
    signal result_exponent:     unsigned(4 downto 0);
begin
    -- take abs of input
    abs_pixel   <=  -pixel_in when pixel_in(7) = '1' else
                    pixel_in;

    -- barrel shifter to get the bits after the most significant 1
    mantissa_rounded    <=  unsigned(abs_pixel(5 downto 0))             when abs_pixel(6) = '1' else
                            unsigned(abs_pixel(4 downto 0)) & "0"       when abs_pixel(5) = '1' else
                            unsigned(abs_pixel(3 downto 0)) & "00"      when abs_pixel(4) = '1' else
                            unsigned(abs_pixel(2 downto 0)) & "000"     when abs_pixel(3) = '1' else
                            unsigned(abs_pixel(1 downto 0)) & "0000"    when abs_pixel(2) = '1' else
                            abs_pixel(0)                    & "00000"   when abs_pixel(1) = '1' else
                            (others => '0');

    -- use the location of the most significant 1 in the input to get its exponent
    result_exponent <=  to_unsigned(15, 5) when abs_pixel(7) = '1' else -- only true when input is -128
                        to_unsigned(14, 5) when abs_pixel(6) = '1' else
                        to_unsigned(13, 5) when abs_pixel(5) = '1' else
                        to_unsigned(12, 5) when abs_pixel(4) = '1' else
                        to_unsigned(11, 5) when abs_pixel(3) = '1' else
                        to_unsigned(10, 5) when abs_pixel(2) = '1' else
                        to_unsigned(9, 5)  when abs_pixel(1) = '1' else
                        to_unsigned(8, 5)  when abs_pixel(0) = '1' else
                        (others => '0');

    -- combine the input sign, computed exponent, and mantissa into the output as a float
    float_out   <=  pixel_in(7) & 
                    std_logic_vector(result_exponent) & 
                    std_logic_vector(mantissa_rounded) & "0000";
end architecture;