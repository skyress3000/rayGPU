----------------------------------------------------------------------------
--
--  Edge function module
--
--  Revision History:
--     21 May 21  Ray Wendt         Initial revision.
--     15 Jun 21  Ray Wendt         Added header comment.
--
----------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.gpu_float.all;

--  edge_function
--
--  Description:
--      This module calculates the edge function of three 2-component vectors,
--      defined as: ef(v1,v2,v3)=(v3_0-v1_0)*(v2_1-v1_1) - (v3_1-v1_1)*(v2_0-v1_0)
--      with one clock of delay since it uses multipliers. Edge cases are the same
--      as standard for adders and multipliers.
--
entity edge_function is
    port (
        -- system clock
        clk     : in    std_logic;

        -- first point coordinates
        v1_0    : in    float;
        v1_1    : in    float;
        -- second point coordinates
        v2_0    : in    float;
        v2_1    : in    float;
        -- third point coordinates
        v3_0    : in    float;
        v3_1    : in    float;

        -- edge function value given by
        -- (v3_0-v1_0)*(v2_1-v1_1) - (v3_1-v1_1)*(v2_0-v1_0)
        ef      : out   float
    );
end entity;

architecture behavioral of edge_function is
    -- component declarations for floating point math
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

    -- negated components of v1
    signal neg_v1_0:        float;
    signal neg_v1_1:        float;

    -- value of v2_0-v1_0
    signal diff_v2_0:       float;
    -- value of v2_1-v1_1
    signal diff_v2_1:       float;
    -- value of v3_0-v1_0
    signal diff_v3_0:       float;
    -- value of v3_1-v1_1
    signal diff_v3_1:       float;

    -- value of (v3_0-v1_0)*(v2_1-v1_1)
    signal product_1:       float;
    -- value of (v3_1-v1_1)*(v2_0-v1_0)
    signal product_2:       float;

    -- negated product_2
    signal neg_product_2:   float;
begin
    -- negate by flipping sign bit
    neg_v1_0 <= (not v1_0(15)) & v1_0(14 downto 0);
    neg_v1_1 <= (not v1_1(15)) & v1_1(14 downto 0);
    neg_product_2 <= (not product_2(15)) & product_2(14 downto 0);

    -- adders to compute the differences
    add_1: float_adder port map (
        addend_a    => v2_0,
        addend_b    => neg_v1_0,
        sum         => diff_v2_0);
    add_2: float_adder port map (
        addend_a    => v2_1,
        addend_b    => neg_v1_1,
        sum         => diff_v2_1);
    add_3: float_adder port map (
        addend_a    => v3_0,
        addend_b    => neg_v1_0,
        sum         => diff_v3_0);
    add_4: float_adder port map (
        addend_a    => v3_1,
        addend_b    => neg_v1_1,
        sum         => diff_v3_1);

    -- multiply the differences together
    mult_1: float_mult port map(
        clk             => clk,
        multiplicand_a  => diff_v3_0,
        multiplicand_b  => diff_v2_1,
        product         => product_1);
    mult_2: float_mult port map(
        clk             => clk,
        multiplicand_a  => diff_v3_1,
        multiplicand_b  => diff_v2_0,
        product         => product_2);

    -- get difference of the products as the final result
    add_final: float_adder port map (
        addend_a    => product_1,
        addend_b    => neg_product_2,
        sum         => ef);
end architecture;