----------------------------------------------------------------------------
--
--  Floating point multiplier testbench
--
--  Revision History:
--     22 May 21  Ray Wendt         Initial revision.
--     15 Jun 21  Ray Wendt         Added header comment.
--
----------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;
use ieee.std_logic_textio.all;

use work.gpu_float.all;

-- float_mult_tb
--
--  Description:
--      This module implements a testbench for the floating point multiplier module.
--      The testbench loads and checks test vectors from a file mult_vectors.txt;
--      see gen_float_tests.py for the test coverage of the vectors.
--
entity float_mult_tb is
end entity;

architecture tb of float_mult_tb is
    -- component for UUT
    component float_mult is
        port (
            clk             : in    std_logic;
            multiplicand_a  : in    float;
            multiplicand_b  : in    float;
            product         : out   float
        );
    end component;

    -- signals for UUT
    signal clk              : std_logic;
    signal multiplicand_a   : float;
    signal multiplicand_b   : float;
    signal product          : float;

    -- file for reading in vectors
    file vec_file           : text;

    -- converts float value to string to display
    function float_to_string(f: float) return string is
        variable real_val: real := 0.0;
    begin
        if exponent(f) = "11111" then
            if mantissa(f) /= "10000000000" then
                -- should be infinity but mantissa isn't 0
                return "INVALID";
            elsif sign(f) = '0' then
                -- positive infinity
                return "inf";
            else
                -- negative infinity
                return "-inf";
            end if;
        elsif exponent(f) = "00000" then
            if mantissa(f) /= "10000000000" then
                -- should be 0 but mantissa isn't 0
                return "INVALID";
            elsif sign(f) = '0' then
                -- positive 0
                return "0.0";
            else
                -- negative 0
                return "-0.0";
            end if;
        else
            -- combine exponent and mantissa
            real_val := 2.0**(to_integer(exponent(f)) - 15) * (real(to_integer(mantissa(f))) * 2.0**(-10));
            -- negate if appropriate and return string representation
            if sign(f) = '0' then
                return real'image(real_val);
            else
                return real'image(-real_val);
            end if;
        end if;
    end float_to_string;

    -- clock period for testing
    constant clk_period : time := 10 ns;
    -- signal to stop generating clock signal
    signal end_sim : boolean := false;
begin
    -- connect UUT
    UUT: float_mult port map(
        clk             => clk,
        multiplicand_a  => multiplicand_a,
        multiplicand_b  => multiplicand_b,
        product         => product);

    -- generate clock
    clk_process: process
    begin
        if end_sim = false then
            clk <= '0';
            wait for clk_period/2;
            clk <= '1';
            wait for clk_period/2;
        else
            wait;
        end if;
    end process;

    -- stimulus process
    stim_proc: process
        -- line input from vector file
        variable in_line : line;
        -- variable to read spaces from vector
        variable space: character;

        -- input floats read from vector file
        variable vec_input_a        : float;
        variable vec_input_b        : float;
        -- output float read from vector
        variable vec_output         : float;
    begin
        file_open(vec_file, "mult_vectors.txt", read_mode);

        -- loop through lines in test vector file
        while not endfile(vec_file) loop
            -- read the inputs from this line in the file
            readline(vec_file, in_line);
            read(in_line, vec_input_a);
            read(in_line, space);
            read(in_line, vec_input_b);
            read(in_line, space);
            read(in_line, vec_output);

            -- set UUT inputs
            multiplicand_a <= vec_input_a;
            multiplicand_b <= vec_input_b;

            -- clock it
            wait for clk_period;

            -- check output sign+exponent matches and mantissa is within one LSB
            assert (product(15 downto 10) = vec_output(15 downto 10)) and
                (abs (to_integer(unsigned(product(9 downto 0))) - to_integer(unsigned(vec_output(9 downto 0)))) <= 1)
            report "Product of " & float_to_string(multiplicand_a) & " and " & float_to_string(multiplicand_b) 
                & " value " & float_to_string(product) & " doesn't match expected " & float_to_string(vec_output);
        end loop;

        -- done
        file_close(vec_file);
        end_sim <= true;

        wait;
    end process;
end architecture;