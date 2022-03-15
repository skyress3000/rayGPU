----------------------------------------------------------------------------
--
--  Floating point package
--
--  Includes a type for half precision (16-bit) floating point values and
--  functions for extracting the sign, mantissa, and exponents from a float.
--  Floats are specified as follows:
--      seeeeemmmmmmmmmm
--  where the value is equal to (-1)^s * 2^(eeeee - 15) * 1.mmmmmmmmmm
--
--  Special values:
--  Any value with exponent 0 is treated as 0, and should have m = 0
--  Any value with exponent 31 is treated as infinity, and should have m = 0
--
--  Revision History:
--     23 Apr 21  Ray Wendt         Initial revision.
--     25 Apr 21  Ray Wendt         Added quad type and 0 constants.
--     11 May 21  Ray Wendt         Fixed syntax issues.
--
----------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package gpu_float is
    -- type for a 16-bit floating point value, as specified above
    subtype float is std_logic_vector(15 downto 0);
    -- 4-dimensional vector of floats, used for various values in the GPU
    -- often represents a position with 0=x, 1=y, 2=z, 3=w
    -- also often represents a color with 0=r, 1=g, 2=g (and 3=a but alpha isn't
    -- actually supported)
    type quad is array (3 downto 0) of float;

    -- zero constants for both above types (technically this is positive 0, negative
    -- 0 is also a valid floating point value by setting the sign bit to 1)
    constant float_zero: float := x"0000";
    constant quad_zero:  quad  := (float_zero, float_zero, float_zero, float_zero);

    -- utility function to extract the sign of a floating point value (1 if
    -- negative, 0 otherwise)
    function sign(f: float)     return std_logic;
    -- utility function to extract the exponent of a floating point value,
    -- where the float is scaled by 2^(e-15), except when the value is 0 in
    -- which case it indicates the float equals 0
    function exponent(f: float) return unsigned;
    -- utility function to extract the mantissa of a floating point value,
    -- represented as a Q1.10 fixed point value
    function mantissa(f: float) return unsigned;
end gpu_float; 

package body gpu_float is
    -- function implementations are trivial -- they just slice the given float
    -- see above for function definitions

    function sign(f: float) return std_logic is
    begin
        return f(15);
    end sign;

    function exponent(f: float) return unsigned is
    begin
        return unsigned(f(14 downto 10));
    end exponent;

    function mantissa(f: float) return unsigned is
    begin
        -- leading 1 in mantissa is implied, so need to tack it on here
        return unsigned('1' & f(9 downto 0));
    end mantissa;
end gpu_float;