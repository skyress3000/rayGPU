----------------------------------------------------------------------------
--
--  Top-level GPU
--
--  Revision History:
--      8 Jun 21  Ray Wendt         Initial revision.
--      9 Jun 21  Ray Wendt         Updated FIFO done connections.
--      9 Jun 21  Ray Wendt         Added PLL clock generator since the design
--                                  doesn't run at 100 MHz.
--     10 Jun 21  Ray Wendt         Added separate PLL reset.
--     10 Jun 21  Ray Wendt         Connected FIFO full to rasterizer.
--     10 Jun 21  Ray Wendt         Added SIZE_BITS to FIFO.
--     16 Jun 21  Ray Wendt         Added header comment.
--
----------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.gpu_float.all;

-- rayGPU
--
--  Description:
--      This module implements the top level of the GPU with system IO pins. The GPU
--      takes vertex data written by the user to RAM, processes it using programmable
--      shader processors, then rasterizes a triangle for every 3 vertices, and processes
--      each pixel in the triangle using more programmable shader processors. The system
--      outputs a double-buffered frame over SPI to a 160x128 TFT display. See the user
--      manual for more operation details.
--
entity rayGPU is
    port (
        -- system clock, should be 100 MHz
        clk         : in    std_logic;
        -- active low system reset
        rst         : in    std_logic;
        -- active high PLL reset (separate because if PLL is off, main reset does
        -- nothing to regs)
        PLL_rst     : in    std_logic;

        -- system write address
        --  00100aaaaaaaaaaa    : addr in vertex data RAM
        --  00101xaaaaaaaaii    : addr in vertex program RAM, 16-bit word i
        --  00110xxxxxxxiijj    : element j in vertex constant i
        --  00111xxxxxxxxx00    : vertex data size reg
        --  00111xxxxxxxxx01    : vertex data base addr reg
        --  00111xxxxxxxxx10    : vertex count reg
        --  01001xaaaaaaaaii    : addr in fragment program RAM, 16-bit word i
        --  01010xxxxxxxiijj    : element j in fragment constant i
        --  100xxxxxxxxxxxxx    : control register
        addr        : in    std_logic_vector(15 downto 0);
        -- system data input, written to register or RAM referred to by addr
        data_in     : in    std_logic_vector(15 downto 0);
        -- system data write clock, data_in is written on rising edge
        wr_clk      : in    std_logic;

        -- temp LED test outputs
        test_out_1  : out   std_logic;
        test_out_2  : out   std_logic;
        test_out_3  : out   std_logic;
        test_out_4  : out   std_logic;

        -- TFT data in pin
        MOSI        : out   std_logic;
        -- TFT data/command select pin
        DCX         : out   std_logic;
        -- TFT clock pin (idles low)
        SCK         : out   std_logic;
        -- TFT select pin (active low)
        CSX         : out   std_logic
    );
end entity;

architecture behavioral of rayGPU is
    -- PLL 20MHz clock generator
    component clk_gen is
        port
        (
            clk_out1          : out    std_logic;
            reset             : in     std_logic;
            locked            : out    std_logic;
            clk_in1           : in     std_logic
        );
    end component;
    -- main clock as output from PLL
    signal main_clk: std_logic;

    -- component declarations for all modules
    component vertex_control is
        port (
            clk         : in        std_logic;
            rst         : in        std_logic;
            data_in     : in        std_logic_vector(15 downto 0);
            addr        : in        std_logic_vector(12 downto 0);
            write_en    : in        std_logic;
            start_shade : in        std_logic;
            v_data_in   : in        std_logic_vector(15 downto 0);
            v_data_addr : out       std_logic_vector(10 downto 0);
            v_data_wr   : out       std_logic;
            v_data_out  : out       std_logic_vector(15 downto 0);
            v_data_size : buffer    std_logic_vector(15 downto 0);
            v_data_base : buffer    std_logic_vector(15 downto 0);
            v_count     : buffer    std_logic_vector(15 downto 0);
            shade_done  : out       std_logic
        );
    end component;

    component fragment_control is
        port (
            clk         : in        std_logic;
            rst         : in        std_logic;
            data_in     : in        std_logic_vector(15 downto 0);
            addr        : in        std_logic_vector(12 downto 0);
            write_en    : in        std_logic;
            FIFO_reg_0  : in        quad;
            FIFO_reg_1  : in        quad;
            FIFO_reg_2  : in        quad;
            FIFO_reg_3  : in        quad;
            FIFO_px_x   : in        unsigned(7 downto 0);
            FIFO_px_y   : in        unsigned(6 downto 0);
            FIFO_done   : in        std_logic;
            FIFO_empty  : in        std_logic;
            FIFO_pop    : out       std_logic;
            fbuff_addr  : out       std_logic_vector(14 downto 0);
            fbuff_data  : out       std_logic_vector(17 downto 0);
            fbuff_wr    : out       std_logic;
            shade_done  : out       std_logic
        );
    end component;

    component rasterizer is
        port (
            clk             : in        std_logic;
            rst             : in        std_logic;
            start_rasterize : in        std_logic;
            start_depth_rst : in        std_logic;
            v_data_size     : in        std_logic_vector(15 downto 0);
            v_data_base     : in        std_logic_vector(15 downto 0);
            v_count         : in        std_logic_vector(15 downto 0);
            v_data_in       : in        std_logic_vector(15 downto 0);
            v_data_addr     : buffer    std_logic_vector(10 downto 0);
            FIFO_full       : in        std_logic;
            FIFO_reg_0      : out       quad;
            FIFO_reg_1      : out       quad;
            FIFO_reg_2      : out       quad;
            FIFO_reg_3      : out       quad;
            FIFO_done       : out       std_logic;
            FIFO_px_x       : out       unsigned(7 downto 0);
            FIFO_px_y       : out       unsigned(6 downto 0);
            FIFO_push       : out       std_logic;
            rasterize_done  : out       std_logic
        );
    end component;

    component fragment_FIFO is
        generic (
            FIFO_SIZE   : integer;
            SIZE_BITS   : integer
        );
        port (
            clk         : in        std_logic;
            rst         : in        std_logic;
            tail_reg_0  : in        quad;
            tail_reg_1  : in        quad;
            tail_reg_2  : in        quad;
            tail_reg_3  : in        quad;
            done_set    : in        std_logic;
            tail_px_x   : in        unsigned(7 downto 0);
            tail_px_y   : in        unsigned(6 downto 0);
            push        : in        std_logic;
            pop         : in        std_logic;
            head_reg_0  : out       quad;
            head_reg_1  : out       quad;
            head_reg_2  : out       quad;
            head_reg_3  : out       quad;
            done        : out       std_logic;
            head_px_x   : out       unsigned(7 downto 0);
            head_px_y   : out       unsigned(6 downto 0);
            full        : buffer    std_logic;
            empty       : out       std_logic
        );
    end component;

    component display_control is
        port (
            clk             : in    std_logic;
            rst             : in    std_logic;
            fbuff_addr      : in    std_logic_vector(14 downto 0);
            fbuff_data      : in    std_logic_vector(17 downto 0);
            fbuff_wr        : in    std_logic;
            start_fbuff_rst : in    std_logic;
            fbuff_swap      : in    std_logic;
            frame_tx_done   : out   std_logic;
            MOSI            : out   std_logic;
            DCX             : out   std_logic;
            SCK             : out   std_logic;
            CSX             : out   std_logic
        );
    end component;

    -- vertex data RAM component (lives in top level because both vertex shaders
    -- and rasterizer access it)
    component vertex_data_ram
        port (
            clka    : in std_logic;
            wea     : in std_logic_vector(0 downto 0);
            addra   : in std_logic_vector(10 downto 0);
            dina    : in std_logic_vector(15 downto 0);
            douta   : out std_logic_vector(15 downto 0)
        );
    end component;

    -- vertex data RAM IO signals
    signal v_data_wr:           std_logic_vector(0 downto 0);
    signal v_data_addr:         std_logic_vector(10 downto 0);
    signal v_data_in:           std_logic_vector(15 downto 0);
    signal v_data_out:          std_logic_vector(15 downto 0);

    -- addresses for vertex data RAM from vertex shade control and rasterizer
    signal shade_v_addr:        std_logic_vector(10 downto 0);
    signal rasterize_v_addr:    std_logic_vector(10 downto 0);

    ---- overall system control state machine ----

    type system_state_t is (
        IDLE,                       -- idle state
        RESET_BUFFERS_BEGIN,        -- begin resetting depth + frame buffers
        RESET_BUFFERS,              -- resetting buffers in progress
        VERTEX_SHADE_BEGIN,         -- begin vertex shading
        VERTEX_SHADE,               -- vertex shading in progress
        RASTERIZE_FRAG_SHADE_BEGIN, -- begin rasterize + frag shade
        RASTERIZE_FRAG_SHADE        -- rasterizing + fragment shading in progress (concurrently)
    );
    signal system_state:        system_state_t;

    ---- system register/RAM write bus ----

    -- 2 DFFs to synchronize wr_clk to the system clock domain
    signal wr_clk_sync:         std_logic_vector(1 downto 0);
    -- edge detector on synchronized wr_clk
    signal wr_clk_delay:        std_logic;
    signal wr_clk_edge:         std_logic;

    ---- fragment + vertex shade control signals ----

    -- write signals for registers in vertex and fragment control modules
    signal vertex_reg_wr:       std_logic;
    signal fragment_reg_wr:     std_logic;

    -- vertex control module register values
    signal v_data_size:         std_logic_vector(15 downto 0);
    signal v_data_base:         std_logic_vector(15 downto 0);
    signal v_count:             std_logic_vector(15 downto 0);

    -- start signals for vertex control module
    signal start_vertex_shade:  std_logic;

    -- done signals for vertex and fragment control modules
    signal vertex_shade_done:   std_logic;
    signal frag_shade_done:     std_logic;

    ---- rasterize control signals ----

    -- start signal for rasterizer
    signal start_rasterize:     std_logic;
    -- done output
    signal rasterize_done:      std_logic;

    ---- FIFO IO signals ----

    -- tail inputs
    signal FIFO_tail_reg_0:     quad;
    signal FIFO_tail_reg_1:     quad;
    signal FIFO_tail_reg_2:     quad;
    signal FIFO_tail_reg_3:     quad;
    signal FIFO_tail_px_x:      unsigned(7 downto 0);
    signal FIFO_tail_px_y:      unsigned(6 downto 0);
    -- head outputs
    signal FIFO_head_reg_0:     quad;
    signal FIFO_head_reg_1:     quad;
    signal FIFO_head_reg_2:     quad;
    signal FIFO_head_reg_3:     quad;
    signal FIFO_head_px_x:      unsigned(7 downto 0);
    signal FIFO_head_px_y:      unsigned(6 downto 0);
    -- control
    signal FIFO_done_set:       std_logic;
    signal FIFO_done:           std_logic;
    signal FIFO_pop:            std_logic;
    signal FIFO_push:           std_logic;
    signal FIFO_full:           std_logic;
    signal FIFO_empty:          std_logic;

    ---- framebuffer interface signals ----

    signal fbuff_addr:          std_logic_vector(14 downto 0);
    signal fbuff_data:          std_logic_vector(17 downto 0);
    signal fbuff_wr:            std_logic;
    signal fbuff_swap:          std_logic;


    -- clear depthbuffer + framebuffer signal
    signal start_buff_rst:      std_logic;
begin
    -- set up clock generator
    clk_gen_inst: clk_gen port map ( 
        -- outputs to main clock
        clk_out1    => main_clk,
        -- PLL reset                
        reset       => PLL_rst,
        -- currently don't care when it locks, just assume there won't be writes
        -- immediately on powerup
        locked      => open,
        -- input from clock pin
        clk_in1     => clk
    );

    -- connect the components

    vertex_control_unit: vertex_control port map (
        clk         => main_clk,
        rst         => rst,
        data_in     => data_in,
        addr        => addr(12 downto 0),
        write_en    => vertex_reg_wr,
        start_shade => start_vertex_shade,
        v_data_in   => v_data_out,
        v_data_addr => shade_v_addr,
        v_data_wr   => v_data_wr(0),
        v_data_out  => v_data_in,
        v_data_size => v_data_size,
        v_data_base => v_data_base,
        v_count     => v_count,
        shade_done  => vertex_shade_done
    );

    fragment_control_unit: fragment_control port map (
        clk         => main_clk,
        rst         => rst,
        data_in     => data_in,
        addr        => addr(12 downto 0),
        write_en    => fragment_reg_wr,
        -- input from FIFO head
        FIFO_reg_0  => FIFO_head_reg_0,
        FIFO_reg_1  => FIFO_head_reg_1,
        FIFO_reg_2  => FIFO_head_reg_2,
        FIFO_reg_3  => FIFO_head_reg_3,
        FIFO_px_x   => FIFO_head_px_x,
        FIFO_px_y   => FIFO_head_px_y,
        FIFO_done   => FIFO_done,
        FIFO_empty  => FIFO_empty,
        FIFO_pop    => FIFO_pop,
        fbuff_addr  => fbuff_addr,
        fbuff_data  => fbuff_data,
        fbuff_wr    => fbuff_wr,
        shade_done  => frag_shade_done
    );

    rasterizer_unit: rasterizer port map (
        clk             => main_clk,
        rst             => rst,
        start_rasterize => start_rasterize,
        start_depth_rst => start_buff_rst,
        v_data_size     => v_data_size,
        v_data_base     => v_data_base,
        v_count         => v_count,
        v_data_in       => v_data_out,
        v_data_addr     => rasterize_v_addr,
        FIFO_full       => FIFO_full,
        -- output to FIFO tail
        FIFO_reg_0      => FIFO_tail_reg_0,
        FIFO_reg_1      => FIFO_tail_reg_1,
        FIFO_reg_2      => FIFO_tail_reg_2,
        FIFO_reg_3      => FIFO_tail_reg_3,
        FIFO_done       => FIFO_done_set,
        FIFO_px_x       => FIFO_tail_px_x,
        FIFO_px_y       => FIFO_tail_px_y,
        FIFO_push       => FIFO_push,
        rasterize_done  => rasterize_done
    );

    fragment_FIFO_unit: fragment_FIFO generic map (
        -- FIFO holds one full set of fragments
        FIFO_SIZE   => 16,
        SIZE_BITS   => 4
    ) port map (
        clk         => main_clk,
        rst         => rst,
        tail_reg_0  => FIFO_tail_reg_0,
        tail_reg_1  => FIFO_tail_reg_1,
        tail_reg_2  => FIFO_tail_reg_2,
        tail_reg_3  => FIFO_tail_reg_3,
        done_set    => FIFO_done_set,
        tail_px_x   => FIFO_tail_px_x,
        tail_px_y   => FIFO_tail_px_y,
        push        => FIFO_push,
        pop         => FIFO_pop,
        head_reg_0  => FIFO_head_reg_0,
        head_reg_1  => FIFO_head_reg_1,
        head_reg_2  => FIFO_head_reg_2,
        head_reg_3  => FIFO_head_reg_3,
        done        => FIFO_done,
        head_px_x   => FIFO_head_px_x,
        head_px_y   => FIFO_head_px_y,
        full        => FIFO_full,
        empty       => FIFO_empty
    );

    display_control_unit: display_control port map (
        clk             => main_clk,
        rst             => rst,
        fbuff_addr      => fbuff_addr,
        fbuff_data      => fbuff_data,
        fbuff_wr        => fbuff_wr,
        start_fbuff_rst => start_buff_rst,
        fbuff_swap      => fbuff_swap,
        -- don't use this because we can let the user switch buffers faster than
        -- tx happens -- it will just result in tearing
        frame_tx_done   => open,
        -- top-level output ports
        MOSI            => MOSI,
        DCX             => DCX,
        SCK             => SCK,
        CSX             => CSX
    );

    vdata_ram: vertex_data_ram port map (
        clka    => main_clk,
        wea     => v_data_wr,
        addra   => v_data_addr,
        dina    => v_data_in,
        douta   => v_data_out
    );

    -- select address for vertex data RAM based on state
    -- needs to always be shader address except while rasterizing, because shader control
    -- handles write to vertex data RAM while idle
    v_data_addr <=  rasterize_v_addr when system_state = RASTERIZE_FRAG_SHADE else
                    shade_v_addr;

    -- wr_clk edge detection process
    process(main_clk)
    begin
        if rising_edge(main_clk) then
            -- shift wr_clk through the two synchronizing DFFs
            wr_clk_sync <= wr_clk_sync(0) & wr_clk;
            -- delay the syncronized signal by one clock for edge detection
            wr_clk_delay <= wr_clk_sync(1);
        end if;
    end process;


    -- TEMP DEBUG LED CONTROL
    process(main_clk)
    begin
        if rising_edge(main_clk) then
            if rst = '0' then 
                test_out_1 <= '0';
                test_out_2 <= '0';
            else
                if not (fbuff_data = "000000000000000000") then
                    test_out_1 <= '1';
                end if;

                if system_state = RASTERIZE_FRAG_SHADE then
                    test_out_2 <= '1';
                end if;
            end if;
        end if;
    end process;

    test_out_3 <= '1' when addr = x"DEAD" else '0';
    test_out_4 <= '1' when data_in = x"BEEF" else '0';





    -- detect rising edge on sync'd wr_clk
    wr_clk_edge <= wr_clk_sync(1) and not wr_clk_delay;

    -- vertex register addrs have bit 13 set
    vertex_reg_wr <= wr_clk_edge and addr(13);
    -- fragment register addrs have bit 14 set
    fragment_reg_wr <= wr_clk_edge and addr(14);

    -- swap framebuffers on a write to bit 1 of control reg
    fbuff_swap <= addr(15) and data_in(1) and wr_clk_edge;

    -- system control state machine
    process(main_clk)
    begin
        if rising_edge(main_clk) then
            if rst = '0' then
                system_state <= IDLE;
            else
                case system_state is
                    when IDLE =>
                        -- start render on a write to bit 0 of control reg
                        if (addr(15) = '1') and (data_in(0) = '1') and (wr_clk_edge = '1') then
                            system_state <= RESET_BUFFERS_BEGIN;
                        end if;
                    when RESET_BUFFERS_BEGIN =>
                        -- only stay in begin state for one clock
                        system_state <= RESET_BUFFERS;
                    when RESET_BUFFERS =>
                        -- rasterize done only high when not resetting buffer, so we can use it here
                        if rasterize_done = '1' then
                            system_state <= VERTEX_SHADE_BEGIN;
                        end if;
                    when VERTEX_SHADE_BEGIN =>
                        -- only stay in begin state for one clock
                        system_state <= VERTEX_SHADE;
                    when VERTEX_SHADE =>
                        -- once vertex shading is done, start rasterizing and shading fragments
                        if vertex_shade_done = '1' then
                            system_state <= RASTERIZE_FRAG_SHADE_BEGIN;
                        end if;
                    when RASTERIZE_FRAG_SHADE_BEGIN =>
                        -- only stay in begin state for one clock
                        system_state <= RASTERIZE_FRAG_SHADE;
                    when RASTERIZE_FRAG_SHADE =>
                        -- once rasterize and fragment shading is done, render is done so go back to idle
                        if (frag_shade_done = '1') and (rasterize_done = '1') then
                            system_state <= IDLE;
                        end if;
                end case;
            end if;
        end if;
    end process;

    -- vertex shade begin state starts vertex shading
    start_vertex_shade <= '1' when system_state = VERTEX_SHADE_BEGIN else '0';
    -- rasterize/frag shade begin state starts rasterize
    start_rasterize <= '1' when system_state = RASTERIZE_FRAG_SHADE_BEGIN else '0';
    -- buffer reset begin starts depthbuffer reset
    start_buff_rst <= '1' when system_state = RESET_BUFFERS_BEGIN else '0';
end architecture;