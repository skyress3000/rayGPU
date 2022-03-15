----------------------------------------------------------------------------
--
--  TFT Display SPI module
--
--  Revision History:
--     20 May 21  Ray Wendt         Initial revision.
--     30 May 21  Ray Wendt         Removed CS control from this module and
--                                  added reset for SCK reg.
--     16 Jun 21  Ray Wendt         Added header comment.
--
----------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.gpu_float.all;

-- TFT_SPI
--
--  Description:
--      This module implements a master SPI controller designed specifically for
--      sending commands and data to a display with ST7735R driver chip running in
--      4-wire serial mode. The SPI sends data MSB first with SCK idling low and
--      data out shifting on every falling SCK edge. In addition to SPI pins, the
--      module also contains the DCX pin used to indicate command or data, which
--      is set to the value of cmd_mode_in at the beginning of a transaction.
--
entity TFT_SPI is
    generic (
        -- number of system clocks per 1/2 SCK period
        CLK_DIV: integer
    );
    port (
        -- system clock
        clk         : in        std_logic;
        -- active low reset
        rst         : in        std_logic;

        -- data or command to send to the display over SPI
        data_in     : in        std_logic_vector(7 downto 0);
        -- data or command mode for this transaction
        --  low: command mode
        --  high: data mode
        cmd_mode_in : in        std_logic;
        -- high to being write of data_in with the indicated mode over SPI
        wr          : in        std_logic;

        -- high when the module is done with a transaction, i.e. in idle state
        done        : out       std_logic;

        -- TFT data in pin
        MOSI        : out       std_logic;
        -- TFT data/command select pin
        DCX         : buffer    std_logic;
        -- TFT clock pin (idles low)
        SCK         : buffer    std_logic
    );
end entity;

architecture behavioral of TFT_SPI is
    -- shift register for data output
    signal data_shift:      std_logic_vector(7 downto 0);
    -- counter for dividing system clock to SCK
    signal clk_div_cntr:    integer range 0 to CLK_DIV-1;
    -- counts 1/2 SCK periods
    signal bit_cntr:        unsigned(3 downto 0);

    -- 2 states, either idle or actively transmitting
    type SPI_state_t is (IDLE, TX);
    signal SPI_state:       SPI_state_t;
begin
    -- done if not transmitting
    done <= '1' when SPI_state = IDLE else
            '0';

    -- transmit MSB of shift reg
    MOSI <= data_shift(7);

    process(clk)
    begin
        if rising_edge(clk) then
            if rst = '0' then
                -- reset to idle
                SPI_state <= IDLE;
                bit_cntr <= (others => '0');
                SCK <= '0';
            else
                -- write during idle state begins tx
                if (SPI_state = IDLE) and (wr = '1') then
                    SPI_state <= TX;
                    -- store data and DCX value for this transaction
                    data_shift <= data_in;
                    DCX <= cmd_mode_in;
                end if;

                if SPI_state = TX then
                    -- increment the clock div counter
                    clk_div_cntr <= clk_div_cntr + 1;

                    -- about to roll over clock div counter
                    if clk_div_cntr = CLK_DIV-1 then
                        -- increment number of 1/2 SCK periods and invert SCK to 
                        -- create SPI clock
                        bit_cntr <= bit_cntr + 1;
                        SCK <= not SCK;

                        if SCK = '1' then
                            -- shift the data on falling SCK edges
                            data_shift <= data_shift(6 downto 0) & '0';
                        end if;

                        if bit_cntr = "1111" then
                            -- go back to idle when 16th 1/2 SCK period finishes
                            SPI_state <= IDLE;
                        end if;
                    end if;
                end if;
            end if;
        end if;
    end process;
end architecture;