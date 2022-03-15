----------------------------------------------------------------------------
--
--  GPU top-level testbench
--
--  Revision History:
--     10 Jun 21  Ray Wendt         Initial revision.
--     10 Jun 21  Ray Wendt         Fixed vertex constant input vectors.
--     10 Jun 21  Ray Wendt         Fixed other input vector floats.
--     10 Jun 21  Ray Wendt         Switched PLL rst to active high.
--     13 Jun 21  Ray Wendt         Switched to test theta=0.2.
--     16 Jun 21  Ray Wendt         Added header comment.
--
----------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.gpu_float.all;

-- rayGPU_tb
--
--  Description:
--      This module implements a testbench for the full GPU. The testbench runs
--      the triangle demo using a set of vectors for writes to the system data
--      bus generated directly from the gpu_control.ino triangle demo, so the
--      results should be exactly the same. The output is not checked since doing
--      so would be too complex, but waveform values can be compared with values
--      printed from GPU_model.py.
--
entity rayGPU_tb is
end entity;

architecture tb of rayGPU_tb is
    -- UUT
    component rayGPU is
        port (
            clk         : in    std_logic;
            rst         : in    std_logic;
            PLL_rst     : in    std_logic;
            addr        : in    std_logic_vector(15 downto 0);
            data_in     : in    std_logic_vector(15 downto 0);
            wr_clk      : in    std_logic;
            MOSI        : out   std_logic;
            DCX         : out   std_logic;
            SCK         : out   std_logic;
            CSX         : out   std_logic
        );
    end component;

    -- UUT IOs
    signal clk:     std_logic;
    signal rst:     std_logic;
    signal PLL_rst: std_logic;
    signal addr:    std_logic_vector(15 downto 0);
    signal data_in: std_logic_vector(15 downto 0);
    signal wr_clk:  std_logic;
    signal MOSI:    std_logic;
    signal DCX:     std_logic;
    signal SCK:     std_logic;
    signal CSX:     std_logic;

    constant N_VECS: integer := 100;

    type addr_vecs_t is array(0 to N_VECS-1) of std_logic_vector(15 downto 0);
    -- address vectors for triangle demo
    -- these vectors come directly from outputs of Arduino demo program
    constant addr_vecs: addr_vecs_t := (
        x"3802", x"3801", x"3800", x"2000", x"2001", x"2002", x"2003", x"2004", 
        x"2005", x"2006", x"2007", x"2008", x"2009", x"200a", x"200b", x"200c", 
        x"200d", x"200e", x"200f", x"2010", x"2011", x"2012", x"2013", x"2014", 
        x"2015", x"2016", x"2017", x"2018", x"2019", x"201a", x"201b", x"201c", 
        x"201d", x"201e", x"201f", x"2020", x"2021", x"2022", x"2023", x"2024", 
        x"2025", x"2026", x"2027", x"2028", x"2029", x"202a", x"202b", x"202c", 
        x"202d", x"202e", x"202f", x"2802", x"2801", x"2800", x"2806", x"2805", 
        x"2804", x"280a", x"2809", x"2808", x"280e", x"280d", x"280c", x"2812", 
        x"2811", x"2810", x"2816", x"2815", x"2814", x"281a", x"2819", x"2818", 
        x"281e", x"281d", x"281c", x"2822", x"2821", x"2820", x"4802", x"4801", 
        x"4800", x"4806", x"4805", x"4804", x"3000", x"3001", x"3002", x"3003",
        x"3004", x"3005", x"3006", x"3007", x"3008", x"3009", x"300a", x"300b",
        x"300c", x"300d", x"300e", x"300f"
    );
    type data_vecs_t is array(0 to N_VECS-1) of std_logic_vector(15 downto 0);
    -- data vectors for triangle demo
    constant data_vecs: data_vecs_t := (
        x"0005", x"0000", x"0001", x"ae66", x"aa66", x"2e66", x"3c00", x"3c00",
        x"0000", x"0000", x"3c00", x"0000", x"0000", x"b266", x"3c00", x"3c00",
        x"3800", x"0000", x"3c00", x"30cc", x"aa66", x"2e66", x"3c00", x"3c00",
        x"3c00", x"0000", x"3c00", x"ae66", x"b0cc", x"0000", x"3c00", x"0000",
        x"3c00", x"0000", x"3c00", x"0000", x"30cc", x"0000", x"3c00", x"0000",
        x"0000", x"3c00", x"3c00", x"2e66", x"b0cc", x"0000", x"3c00", x"3800",
        x"0000", x"3c00", x"3c00", x"0002", x"9c80", x"000f", x"0002", x"bc80",
        x"551f", x"0005", x"3c88", x"e40f", x"0002", x"dc80", x"aa1f", x"0005",
        x"3c88", x"e40f", x"0002", x"fc80", x"ff1f", x"0005", x"3c88", x"e44f",
        x"0000", x"0001", x"e45f", x"000a", x"0000", x"0000", x"0000", x"0001",
        x"e45f", x"000a", x"0000", x"0000", x"3bd7", x"0000", x"b8c4", x"b25b",
        x"0000", x"3c00", x"0000", x"0000", x"b25b", x"0000", x"c1e1", x"bbd7",
        x"0000", x"1d1e", x"3400", x"3a00"
    );

    -- clock periods
    constant main_clk_period: time := 50 ns; -- 20 MHz
    constant wr_clk_period: time := 2 us; -- 0.5 MHz
begin
    -- instantiate UUT
    UUT: rayGPU port map (
        clk         => clk,
        rst         => rst,
        PLL_rst     => PLL_rst,
        addr        => addr,
        data_in     => data_in,
        wr_clk      => wr_clk,
        MOSI        => MOSI,
        DCX         => DCX,
        SCK         => SCK,
        CSX         => CSX
    );

    -- generate main clock
    clk_proc: process
    begin
        clk <= '0';
        wait for main_clk_period/2;
        clk <= '1';
        wait for main_clk_period/2;
    end process;

    stim_proc: process
    begin
        rst <= '1';

        -- reset PLL
        PLL_rst <= '1';
        wait for 1 us;
        PLL_rst <= '0';

        -- wait for PLL to start
        wait for 15 us;

        -- reset system
        rst <= '0';
        wait for 1 us;
        rst <= '1';
        wait for 1 us;

        -- write inputs to initialize
        for i in 0 to N_VECS-1 loop
            addr <= addr_vecs(i);
            data_in <= data_vecs(i);

            wr_clk <= '0';
            wait for wr_clk_period/2;
            wr_clk <= '1';
            wait for wr_clk_period/2;
        end loop;

        -- write to control reg to start render
        addr <= x"8000";
        data_in <= x"0001";
        wr_clk <= '0';
        wait for wr_clk_period/2;
        wr_clk <= '1';
        wait for wr_clk_period/2;

        -- wait for render to finish
        wait for 6 ms;
        -- swap framebuffers to output
        addr <= x"8000";
        data_in <= x"0002";
        wr_clk <= '0';
        wait for wr_clk_period/2;
        wr_clk <= '1';
        wait for wr_clk_period/2;

        -- wait forever
        wait;
    end process;
end architecture;