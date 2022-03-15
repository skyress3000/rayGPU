----------------------------------------------------------------------------
--
--  Framebuffer ROM + TFT display tester module
--
--  Revision History:
--     30 May 21  Ray Wendt         Initial revision.
--     31 May 21  Ray Wendt         Syntax fixes and switched to ROM for testing.
--     31 May 21  Ray Wendt         Added edge detector to fbuff_swap and switched
--                                  to active low for physical testing
--     15 Jun 21  Ray Wendt         Added header comment.
--
----------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.gpu_float.all;

--  display_control_tester
--
--  Description:
--      This module implements the GPU framebuffers and display control logic,
--      but with ROM replacing the normal framebuffers for testing purposes. When
--      reset, the module sends a sequence of SPI commands to the display to 
--      initialize it and turn power on. When the framebuffer swap input goes high, 
--      the active and inactive framebuffers are switched, and the module sends 
--      all the pixel data from the newly inactive buffer to be output on the 
--      display.
--
entity display_control is
    port (
        -- system clock
        clk             : in    std_logic;
        -- active low reset
        rst             : in    std_logic;

        -- framebuffer write address (bus has no effect)
        fbuff_addr      : in    std_logic_vector(14 downto 0);
        -- framebuffer write data
        fbuff_data      : in    std_logic_vector(17 downto 0);
        -- high when writing to framebuffer
        fbuff_wr        : in    std_logic;

        -- set high to swap active framebuffer and write the previous active
        -- framebuffer to the display
        fbuff_swap      : in    std_logic;

        -- high while not transmitting a frame to the display (fbuff_swap will
        -- not have an effect until the tx is done)
        frame_tx_done   : out   std_logic;

        -- TFT data in pin
        MOSI            : out   std_logic;
        -- TFT data/command select pin
        DCX             : out   std_logic;
        -- TFT clock pin (idles low)
        SCK             : out   std_logic;
        -- TFT select pin (active low)
        CSX             : out   std_logic
    );
end entity;

architecture behavioral of display_control is
    -- component declaration for the SPI controller
    component TFT_SPI is
        generic (
            CLK_DIV: integer
        );
        port (
            clk         : in        std_logic;
            rst         : in        std_logic;
            data_in     : in        std_logic_vector(7 downto 0);
            cmd_mode_in : in        std_logic;
            wr          : in        std_logic;
            done        : out       std_logic;
            MOSI        : out       std_logic;
            DCX         : buffer    std_logic;
            SCK         : buffer    std_logic
        );
    end component;

    -- component declaration for framebuffer RAM
    component framebuffer_ROM
        port (
            clka    : in std_logic;
            --wea     : in std_logic_vector(0 downto 0);
            addra   : in std_logic_vector(14 downto 0);
            --dina    : in std_logic_vector(17 downto 0);
            douta   : out std_logic_vector(17 downto 0)
        );
    end component;

    -- constants for ST7735R display driver command values
    constant CMD_SWRESET    : std_logic_vector(7 downto 0) := x"01";
    constant CMD_SLPOUT     : std_logic_vector(7 downto 0) := x"11";
    constant CMD_NORON      : std_logic_vector(7 downto 0) := x"13";
    constant CMD_INVOFF     : std_logic_vector(7 downto 0) := x"20";
    constant CMD_DISPON     : std_logic_vector(7 downto 0) := x"29";
    constant CMD_CASET      : std_logic_vector(7 downto 0) := x"2A";
    constant CMD_RASET      : std_logic_vector(7 downto 0) := x"2B";
    constant CMD_RAMWR      : std_logic_vector(7 downto 0) := x"2C";
    constant CMD_MADCTL     : std_logic_vector(7 downto 0) := x"36";
    constant CMD_COLMOD     : std_logic_vector(7 downto 0) := x"3A";
    constant CMD_FRMCTR1    : std_logic_vector(7 downto 0) := x"B1";
    constant CMD_FRMCTR2    : std_logic_vector(7 downto 0) := x"B2";
    constant CMD_FRMCTR3    : std_logic_vector(7 downto 0) := x"B3";
    constant CMD_INVCTR     : std_logic_vector(7 downto 0) := x"B4";
    constant CMD_PWCTR1     : std_logic_vector(7 downto 0) := x"C0";
    constant CMD_PWCTR2     : std_logic_vector(7 downto 0) := x"C1";
    constant CMD_PWCTR3     : std_logic_vector(7 downto 0) := x"C2";
    constant CMD_PWCTR4     : std_logic_vector(7 downto 0) := x"C3";
    constant CMD_PWCTR5     : std_logic_vector(7 downto 0) := x"C4";
    constant CMD_VMCTR1     : std_logic_vector(7 downto 0) := x"C5";
    constant CMD_GMCTRP1    : std_logic_vector(7 downto 0) := x"E0";
    constant CMD_GMCTRN1    : std_logic_vector(7 downto 0) := x"E1";

    -- record represents one byte to send during TFT init sequence
    type SPI_init_byte_t is record
        is_data : std_logic;                    -- high if this is a data byte, low for command byte
        data    : std_logic_vector(7 downto 0); -- data to send
    end record SPI_init_byte_t;

    -- array of all bytes to send during TFT init sequence (excluding the few that need delays
    -- which have unique states)
    -- the commands & values for the init sequence are taken from adafruit's library
    -- code since there's configuration specific to their breakout board which is not
    -- found in the ST7735R driver chip datasheet. The relevant library code for display
    -- initalization can be found here: 
    -- https://github.com/adafruit/Adafruit-ST7735-Library/blob/master/Adafruit_ST7735.cpp
    -- and the comments describing each command from the library have been included as well
    constant N_SPI_INIT_BYTES   : integer := 83; -- total number of bytes sent during init

    type SPI_init_bytes_t is array (0 to N_SPI_INIT_BYTES-1) of SPI_init_byte_t;
    constant SPI_init_bytes     : SPI_init_bytes_t := (
        -- Framerate ctrl normal mode
        -- Comment from adafruit: Rate = fosc/(1x2+40) * (LINE+2C+2D)
        (is_data => '0',    data => CMD_FRMCTR1),
        (is_data => '1',    data => x"01"),
        (is_data => '1',    data => x"2C"),
        (is_data => '1',    data => x"2D"),
        -- Framerate ctrl idle mode
        -- Comment from adafruit: Rate = fosc/(1x2+40) * (LINE+2C+2D)
        (is_data => '0',    data => CMD_FRMCTR2),
        (is_data => '1',    data => x"01"),
        (is_data => '1',    data => x"2C"),
        (is_data => '1',    data => x"2D"),
        -- Framerate ctrl partial mode
        -- Comment from adafruit: Dot inversion mode, line inversion mode
        (is_data => '0',    data => CMD_FRMCTR3),
        (is_data => '1',    data => x"01"),
        (is_data => '1',    data => x"2C"),
        (is_data => '1',    data => x"2D"),
        (is_data => '1',    data => x"01"),
        (is_data => '1',    data => x"2C"),
        (is_data => '1',    data => x"2D"),
        -- Display inversion ctrl
        -- Comment from adafruit: No inversion
        (is_data => '0',    data => CMD_INVCTR),
        (is_data => '1',    data => x"07"),
        -- Power ctrl
        -- Comment from adafruit: -4.6V, AUTO mode
        (is_data => '0',    data => CMD_PWCTR1),
        (is_data => '1',    data => x"A2"),
        (is_data => '1',    data => x"02"),
        (is_data => '1',    data => x"84"),
        -- Power ctrl
        -- Comment from adafruit: VGH25=2.4C VGSEL=-10 VGH=3 * AVDD
        (is_data => '0',    data => CMD_PWCTR2),
        (is_data => '1',    data => x"C5"),
        -- Power ctrl
        -- Comment from adafruit: Opamp current small, boost frequency
        (is_data => '0',    data => CMD_PWCTR3),
        (is_data => '1',    data => x"0A"),
        (is_data => '1',    data => x"00"),
        -- Power ctrl
        -- Comment from adafruit: BCLK/2, opamp current small & medium low
        (is_data => '0',    data => CMD_PWCTR4),
        (is_data => '1',    data => x"8A"),
        (is_data => '1',    data => x"2A"),
        -- Power ctrl
        (is_data => '0',    data => CMD_PWCTR5),
        (is_data => '1',    data => x"8A"),
        (is_data => '1',    data => x"EE"),
        -- Power ctrl
        (is_data => '0',    data => CMD_VMCTR1),
        (is_data => '1',    data => x"0E"),
        -- Display inversion off
        (is_data => '0',    data => CMD_INVOFF),
        -- Memory access direction control
        -- Comment from adafruit: row/col addr, bottom-top refresh
        (is_data => '0',    data => CMD_MADCTL),
        (is_data => '1',    data => x"C8"),
        -- Set color mode
        -- 18-bit color
        (is_data => '0',    data => CMD_COLMOD),
        (is_data => '1',    data => x"06"),
        -- Column address set
        -- Start=0, End=127
        (is_data => '0',    data => CMD_CASET),
        (is_data => '1',    data => x"00"),
        (is_data => '1',    data => x"00"),
        (is_data => '1',    data => x"00"),
        (is_data => '1',    data => x"7F"),
        -- Row address set
        -- Start=0, End=160
        (is_data => '0',    data => CMD_RASET),
        (is_data => '1',    data => x"00"),
        (is_data => '1',    data => x"00"),
        (is_data => '1',    data => x"00"),
        (is_data => '1',    data => x"9F"),
        -- Positive polarity gamma correction
        -- Values from adafruit
        (is_data => '0',    data => CMD_GMCTRP1),
        (is_data => '1',    data => x"02"),
        (is_data => '1',    data => x"1C"),
        (is_data => '1',    data => x"07"),
        (is_data => '1',    data => x"12"),
        (is_data => '1',    data => x"37"),
        (is_data => '1',    data => x"32"),
        (is_data => '1',    data => x"29"),
        (is_data => '1',    data => x"2D"),
        (is_data => '1',    data => x"29"),
        (is_data => '1',    data => x"25"),
        (is_data => '1',    data => x"2B"),
        (is_data => '1',    data => x"39"),
        (is_data => '1',    data => x"00"),
        (is_data => '1',    data => x"01"),
        (is_data => '1',    data => x"03"),
        (is_data => '1',    data => x"10"),
        -- Negative polarity gamma correction
        -- Values from adafruit
        (is_data => '0',    data => CMD_GMCTRN1),
        (is_data => '1',    data => x"03"),
        (is_data => '1',    data => x"1D"),
        (is_data => '1',    data => x"07"),
        (is_data => '1',    data => x"06"),
        (is_data => '1',    data => x"2E"),
        (is_data => '1',    data => x"2C"),
        (is_data => '1',    data => x"29"),
        (is_data => '1',    data => x"2D"),
        (is_data => '1',    data => x"2E"),
        (is_data => '1',    data => x"2E"),
        (is_data => '1',    data => x"37"),
        (is_data => '1',    data => x"3F"),
        (is_data => '1',    data => x"00"),
        (is_data => '1',    data => x"00"),
        (is_data => '1',    data => x"02"),
        (is_data => '1',    data => x"10")
    );

    -- number of system clocks at 100 MHz to delay for init commands that require a delay

    -- 50 ms delay
    constant SWRESET_DELAY_COUNTS   : unsigned(26 downto 0) := to_unsigned(5000000, 27);
    -- 500 ms delay
    constant SLPOUT_DELAY_COUNTS    : unsigned(26 downto 0) := to_unsigned(50000000, 27);
    -- 10 ms delay
    constant NORON_DELAY_COUNTS     : unsigned(26 downto 0) := to_unsigned(1000000, 27);
    -- 100 ms delay
    constant DISPON_DELAY_COUNTS    : unsigned(26 downto 0) := to_unsigned(10000000, 27);

    -- state machine for managing transmission to display
    type TFT_tx_state_t is (
        SWRESET_SEND,       -- send software reset command (beginning of init)
        SWRESET_SPI_WAIT,   -- wait for SPI send to complete
        SWRESET_DELAY,      -- delay 50 ms after reset
        SLPOUT_SEND,        -- send out of sleep mode command
        SLPOUT_SPI_WAIT,    -- wait for SPI send to complete
        SLPOUT_DELAY,       -- delay 500 ms after out of sleep
        INIT_BYTE_SEND,     -- send byte of init sequence (repeats for every byte)
        INIT_BYTE_SPI_WAIT, -- wait for SPI send to complete
        NORON_SEND,         -- send normal display on command
        NORON_SPI_WAIT,     -- wait for SPI send to complete
        NORON_DELAY,        -- delay 10 ms after display on
        DISPON_SEND,        -- send main screen on command
        DISPON_SPI_WAIT,    -- wait for SPI send to complete
        DISPON_DELAY,       -- delay 100 ms after screen on
        IDLE,               -- idle after display turned on
        RAMWR_SEND,         -- send RAM write command to begin frame transmission
        RAMWR_SPI_WAIT,     -- wait for SPI send to complete
        PX_R_SEND,          -- send red pixel value
        PX_R_SPI_WAIT,      -- wait for SPI send to complete
        PX_G_SEND,          -- send green pixel value
        PX_G_SPI_WAIT,      -- wait for SPI send to complete
        PX_B_SEND,          -- send blue pixel value
        PX_B_SPI_WAIT       -- wait for SPI send to complete
    );
    signal TFT_tx_state : TFT_tx_state_t;

    -- counts the current byte in the init sequence
    signal init_byte_ctr: unsigned(6 downto 0);
    -- counts system clock cycles for the init delay states
    signal delay_ctr: unsigned(26 downto 0);
    -- current column (x) while transmitting a frame
    signal px_col_ctr: unsigned(7 downto 0);
    -- current row (y) while transmitting a frame
    signal px_row_ctr: unsigned(6 downto 0);

    -- input signals to SPI controller
    signal SPI_data     : std_logic_vector(7 downto 0);
    signal SPI_cmd_mode : std_logic;
    signal SPI_wr       : std_logic;
    -- done output from SPI controller
    signal SPI_done     : std_logic;

    -- IO signals for both framebuffers
    signal fbuff_1_wr   : std_logic_vector(0 downto 0);
    signal fbuff_1_addr : std_logic_vector(14 downto 0);
    signal fbuff_1_din  : std_logic_vector(17 downto 0);
    signal fbuff_1_dout : std_logic_vector(17 downto 0);
    signal fbuff_2_wr   : std_logic_vector(0 downto 0);
    signal fbuff_2_addr : std_logic_vector(14 downto 0);
    signal fbuff_2_din  : std_logic_vector(17 downto 0);
    signal fbuff_2_dout : std_logic_vector(17 downto 0);

    -- high when framebuffer 2 is active, low when framebuffer 1 is active
    -- where "active" = writing from the module inputs is enabled
    signal fbuff_active : std_logic;
begin
    -- connect SPI controller
    spi_controller: TFT_SPI
        generic map (
            CLK_DIV => 7 -- divide clock to 14 MHz (max supported by display)
        )
        port map (
            clk         => clk,
            rst         => rst,
            data_in     => SPI_data,
            cmd_mode_in => SPI_cmd_mode,
            wr          => SPI_wr,
            done        => SPI_done,
            MOSI        => MOSI,
            DCX         => DCX,
            SCK         => SCK
        );

                -- SPI data gets command for specific command send states
    SPI_data <= CMD_SWRESET                                     when TFT_tx_state = SWRESET_SEND else
                CMD_SLPOUT                                      when TFT_tx_state = SLPOUT_SEND else
                CMD_NORON                                       when TFT_tx_state = NORON_SEND else
                CMD_DISPON                                      when TFT_tx_state = DISPON_SEND else
                CMD_RAMWR                                       when TFT_tx_state = RAMWR_SEND else
                -- get data to send for current init byte
                SPI_init_bytes(to_integer(init_byte_ctr)).data  when TFT_tx_state = INIT_BYTE_SEND else
                -- get 6-bit color from inactive framebuffer and tack on 0s in low bits if sending pixel color
                fbuff_1_dout(5 downto 0) & "00"                 when (TFT_tx_state = PX_R_SEND) and (fbuff_active = '1') else
                fbuff_2_dout(5 downto 0) & "00"                 when (TFT_tx_state = PX_R_SEND) and (fbuff_active = '0') else
                fbuff_1_dout(11 downto 6) & "00"                when (TFT_tx_state = PX_G_SEND) and (fbuff_active = '1') else
                fbuff_2_dout(11 downto 6) & "00"                when (TFT_tx_state = PX_G_SEND) and (fbuff_active = '0') else
                fbuff_1_dout(17 downto 12) & "00"               when (TFT_tx_state = PX_B_SEND) and (fbuff_active = '1') else
                fbuff_2_dout(17 downto 12) & "00"               when (TFT_tx_state = PX_B_SEND) and (fbuff_active = '0') else
                (others => '0');

                    -- command mode is 0 for all specific command send states
    SPI_cmd_mode <= '0'                                                 when (TFT_tx_state = SWRESET_SEND)
                                                                        or (TFT_tx_state = SLPOUT_SEND)
                                                                        or (TFT_tx_state = NORON_SEND)
                                                                        or (TFT_tx_state = DISPON_SEND)
                                                                        or (TFT_tx_state = RAMWR_SEND) else
                    -- for init byte send, get mode for this byte
                    SPI_init_bytes(to_integer(init_byte_ctr)).is_data   when TFT_tx_state = INIT_BYTE_SEND else
                    -- all other states are data
                    '1';

    -- write to SPI on any data send state
    SPI_wr <= '1'   when (TFT_tx_state = SWRESET_SEND)
                    or (TFT_tx_state = SLPOUT_SEND)
                    or (TFT_tx_state = NORON_SEND)
                    or (TFT_tx_state = DISPON_SEND)
                    or (TFT_tx_state = RAMWR_SEND)
                    or (TFT_tx_state = INIT_BYTE_SEND)
                    or (TFT_tx_state = PX_R_SEND)
                    or (TFT_tx_state = PX_G_SEND)
                    or (TFT_tx_state = PX_B_SEND) else
              '0';

    -- SPI select enabled on any SPI send or wait state
    CSX <= '0'  when (TFT_tx_state = SWRESET_SEND)
                or (TFT_tx_state = SWRESET_SPI_WAIT)
                or (TFT_tx_state = SLPOUT_SEND)
                or (TFT_tx_state = SLPOUT_SPI_WAIT)
                or (TFT_tx_state = NORON_SEND)
                or (TFT_tx_state = NORON_SPI_WAIT)
                or (TFT_tx_state = DISPON_SEND)
                or (TFT_tx_state = DISPON_SPI_WAIT)
                or (TFT_tx_state = RAMWR_SEND)
                or (TFT_tx_state = RAMWR_SPI_WAIT)
                or (TFT_tx_state = INIT_BYTE_SEND)
                or (TFT_tx_state = INIT_BYTE_SPI_WAIT)
                or (TFT_tx_state = PX_R_SEND)
                or (TFT_tx_state = PX_R_SPI_WAIT)
                or (TFT_tx_state = PX_G_SEND)
                or (TFT_tx_state = PX_G_SPI_WAIT)
                or (TFT_tx_state = PX_B_SEND)
                or (TFT_tx_state = PX_B_SPI_WAIT) else
           '1';

    -- connect framebuffers
    fbuff_1: framebuffer_ROM port map (
        clka    => clk,
        --wea     => fbuff_1_wr,
        addra   => fbuff_1_addr,
        --dina    => fbuff_1_din,
        douta   => fbuff_1_dout
    );
    fbuff_2: framebuffer_ROM port map (
        clka    => clk,
        --wea     => fbuff_2_wr,
        addra   => fbuff_2_addr,
        --dina    => fbuff_2_din,
        douta   => fbuff_2_dout
    );

    -- write when this framebuffer is active and we have a write
    fbuff_1_wr <= "1" when (fbuff_active = '0') and fbuff_wr = '1' else
                  "0";
    fbuff_2_wr <= "1" when (fbuff_active = '1') and fbuff_wr = '1' else
                  "0";

    -- if active, address comes from write input, otherwise address the current
    -- pixel to output to the display
    fbuff_1_addr <= fbuff_addr when fbuff_active = '0' else
                    std_logic_vector(px_col_ctr & px_row_ctr);
    fbuff_2_addr <= fbuff_addr when fbuff_active = '1' else
                    std_logic_vector(px_col_ctr & px_row_ctr);

    -- data in is always from write input
    fbuff_1_din <= fbuff_data;
    fbuff_2_din <= fbuff_data;

    -- state machine control
    process(clk)
        -- temporary for edge detection with physical button
        variable prev_fbuff_swap: std_logic;
    begin
        if rising_edge(clk) then
            if rst = '0' then
                -- reset to first state of init sequence
                TFT_tx_state <= SWRESET_SEND;
                -- reset to framebuffer 1 active
                fbuff_active <= '0';
            else
                case TFT_tx_state is
                    when SWRESET_SEND =>
                        -- immediately go to next state
                        TFT_tx_state <= SWRESET_SPI_WAIT;
                    when SWRESET_SPI_WAIT =>
                        if SPI_done = '1' then
                            -- go to delay state once SPI is done and init delay
                            -- counter
                            TFT_tx_state <= SWRESET_DELAY;
                            delay_ctr <= SWRESET_DELAY_COUNTS;
                        end if;
                    when SWRESET_DELAY =>
                        -- decrement counter
                        delay_ctr <= delay_ctr - 1;
                        -- done once we hit 0
                        if delay_ctr = 0 then
                            TFT_tx_state <= SLPOUT_SEND;
                        end if;
                    when SLPOUT_SEND =>
                        -- immediately go to next state
                        TFT_tx_state <= SLPOUT_SPI_WAIT;
                    when SLPOUT_SPI_WAIT =>
                        if SPI_done = '1' then
                            -- go to delay state once SPI is done and init delay
                            -- counter
                            TFT_tx_state <= SLPOUT_DELAY;
                            delay_ctr <= SLPOUT_DELAY_COUNTS;
                        end if;
                    when SLPOUT_DELAY =>
                        -- decrement counter
                        delay_ctr <= delay_ctr - 1;
                        -- done once we hit 0
                        if delay_ctr = 0 then
                            -- go to init byte send and set to first init byte in sequence
                            TFT_tx_state <= INIT_BYTE_SEND;
                            init_byte_ctr <= (others => '0');
                        end if;
                    when INIT_BYTE_SEND =>
                        -- immediately go to next state
                        TFT_tx_state <= INIT_BYTE_SPI_WAIT;
                    when INIT_BYTE_SPI_WAIT =>
                        if SPI_done = '1' then
                            -- increment current byte once SPI done
                            init_byte_ctr <= init_byte_ctr + 1;
                            if init_byte_ctr = N_SPI_INIT_BYTES-1 then
                                -- if we're on the last init byte, done with init sequence
                                TFT_tx_state <= NORON_SEND;
                            else
                                -- otherwise send the next byte
                                TFT_tx_state <= INIT_BYTE_SEND;
                            end if;
                        end if;
                    when NORON_SEND =>
                        -- immediately go to next state
                        TFT_tx_state <= NORON_SPI_WAIT;
                    when NORON_SPI_WAIT =>
                        if SPI_done = '1' then
                            -- go to delay state once SPI is done and init delay
                            -- counter
                            TFT_tx_state <= NORON_DELAY;
                            delay_ctr <= NORON_DELAY_COUNTS;
                        end if;
                    when NORON_DELAY =>
                        -- decrement counter
                        delay_ctr <= delay_ctr - 1;
                        -- done once we hit 0
                        if delay_ctr = 0 then
                            TFT_tx_state <= DISPON_SEND;
                        end if;
                    when DISPON_SEND =>
                        -- immediately go to next state
                        TFT_tx_state <= DISPON_SPI_WAIT;
                    when DISPON_SPI_WAIT =>
                        if SPI_done = '1' then
                            -- go to delay state once SPI is done and init delay
                            -- counter
                            TFT_tx_state <= DISPON_DELAY;
                            delay_ctr <= DISPON_DELAY_COUNTS;
                        end if;
                    when DISPON_DELAY =>
                        -- decrement counter
                        delay_ctr <= delay_ctr - 1;
                        -- done once we hit 0, this is the end of all init ops, so go to
                        -- idle
                        if delay_ctr = 0 then
                            TFT_tx_state <= IDLE;
                        end if;
                    when IDLE =>
                        if fbuff_swap = '0' and prev_fbuff_swap = '1' then
                            -- begin output triggered by framebuffer swap
                            TFT_tx_state <= RAMWR_SEND;
                            -- perform the swap
                            fbuff_active <= not fbuff_active;
                        end if;
                    when RAMWR_SEND =>
                        -- immediately go to next state
                        TFT_tx_state <= RAMWR_SPI_WAIT;
                        -- init pixel counters
                        px_row_ctr <= (others => '0');
                        px_col_ctr <= (others => '0');
                    when RAMWR_SPI_WAIT =>
                        if SPI_done = '1' then
                            -- after RAM write command, begin sending pixel data
                            TFT_tx_state <= PX_R_SEND;
                        end if;
                    when PX_R_SEND =>
                        -- immediately go to next state
                        TFT_tx_state <= PX_R_SPI_WAIT;
                    when PX_R_SPI_WAIT =>
                        if SPI_done = '1' then
                            -- wait for SPI
                            TFT_tx_state <= PX_G_SEND;
                        end if;
                    when PX_G_SEND =>
                        -- immediately go to next state
                        TFT_tx_state <= PX_G_SPI_WAIT;
                    when PX_G_SPI_WAIT =>
                        if SPI_done = '1' then
                            -- wait for SPI
                            TFT_tx_state <= PX_B_SEND;
                        end if;
                    when PX_B_SEND =>
                        -- immediately go to next state
                        TFT_tx_state <= PX_B_SPI_WAIT;

                        -- increment pixel counters here to give time for RAM read
                        -- before next pixel begins

                        -- always increment y counter
                        px_row_ctr <= px_row_ctr + 1;
                        -- increment x counter when y wraps
                        if px_row_ctr = 127 then
                            px_col_ctr <= px_col_ctr + 1;
                        end if;
                    when PX_B_SPI_WAIT =>
                        if SPI_done = '1' then
                            if (px_row_ctr = 0) and (px_col_ctr = 160) then
                                -- done with all pixel data if we just incremented
                                -- past the last pixel at (159,127)
                                TFT_tx_state <= IDLE;
                            else
                                -- otherwise send the next pixel
                                TFT_tx_state <= PX_R_SEND;
                            end if; 
                        end if;
                end case;
            end if;

            prev_fbuff_swap := fbuff_swap;
        end if;
    end process;
end architecture;