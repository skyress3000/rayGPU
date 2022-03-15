----------------------------------------------------------------------------
--
--  Raw fragment FIFO
--
--  Revision History:
--     17 May 21  Ray Wendt         Initial revision.
--     20 May 21  Ray Wendt         Added done bit to signal rasterize finish to
--                                  fragment shade FSM.
--     20 May 21  Ray Wendt         Added pixel coords and switched to records for
--                                  array elements.
--      9 Jun 21  Ray Wendt         Changed done bit to a flag since it won't always
--                                  come with a particular fragment (if the last 
--                                  fragment fails the inside triangle or depth test).
--     10 Jun 21  Ray Wendt         Added SIZE_BITS and switched pointers to unsigned
--                                  because the simulation doesn't play nice with
--                                  ranged integers.
--     10 Jun 21  Ray Wendt         Changed sticking condition for done flag.
--
----------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.gpu_float.all;

--  fragment_FIFO
--
--  Description:
--      This module implements a variable length FIFO queue for fragments emitted
--      by the fragment pipeline. The tail element inputs are pushed when the push
--      signal is high, except for if the queue is full, when pushing has no effect.
--      The FIFO module also contains a done flag which can be set by the rasterizer
--      to indicate that fragment generation is finished, and is reset once new data
--      is pushed after the flag is set.
--
entity fragment_FIFO is
    generic (
        -- number of elements in the FIFO, should be power of 2
        FIFO_SIZE   : integer;
        -- number of bits needed to represent FIFO_SIZE
        SIZE_BITS   : integer
    );
    port (
        -- system clock
        clk         : in        std_logic;
        -- active low reset
        rst         : in        std_logic;

        -- register values to push into tail element
        tail_reg_0  : in        quad;
        tail_reg_1  : in        quad;
        tail_reg_2  : in        quad;
        tail_reg_3  : in        quad;
        -- high to set the done bit once fragments are done being pushed into the
        -- FIFO
        done_set    : in        std_logic;
        -- pixel coordinates to push into tail element
        tail_px_x   : in        unsigned(7 downto 0);
        tail_px_y   : in        unsigned(6 downto 0);

        -- high to push the tail reg inputs into the tail element
        push        : in        std_logic;
        -- high to pop the head element
        pop         : in        std_logic;

        -- register values of the current head element
        head_reg_0  : out       quad;
        head_reg_1  : out       quad;
        head_reg_2  : out       quad;
        head_reg_3  : out       quad;
        -- done flag output, set by done_set and sticks high until new data is pushed
        done        : out       std_logic;
        -- pixel coordinates of current head element
        head_px_x   : out       unsigned(7 downto 0);
        head_px_y   : out       unsigned(6 downto 0);

        -- high when the FIFO is full -- if so, pushes will have no effect
        full        : buffer    std_logic;
        -- high when the FIFO is empty
        empty       : out       std_logic
    );
end entity;

architecture behavioral of fragment_FIFO is
    -- single element in the FIFO
    type FIFO_elem_t is record
        -- four input register values for this fragment
        reg_0   : quad;
        reg_1   : quad;
        reg_2   : quad;
        reg_3   : quad;
        -- pixel coords of this fragment
        px_x    : unsigned(7 downto 0);
        px_y    : unsigned(6 downto 0);
    end record FIFO_elem_t;
    -- FIFO represented by array of FIFO elements
    type FIFO_array_t is array (FIFO_SIZE-1 downto 0) of FIFO_elem_t;
    signal FIFO_array: FIFO_array_t;
    -- index of head element
    signal head_ptr: unsigned(SIZE_BITS-1 downto 0);
    -- index of next empty element (tail element + 1)
    signal tail_ptr: unsigned(SIZE_BITS-1 downto 0);
    -- element referenced by head_ptr
    signal head_elem: FIFO_elem_t;
begin
    -- get head element values from array
    head_elem  <= FIFO_array(to_integer(head_ptr));
    head_reg_0 <= head_elem.reg_0;
    head_reg_1 <= head_elem.reg_1;
    head_reg_2 <= head_elem.reg_2;
    head_reg_3 <= head_elem.reg_3;
    head_px_x  <= head_elem.px_x;
    head_px_y  <= head_elem.px_y;

    -- empty when head and tail refer to same element, and not full
    empty <= '1' when (head_ptr = tail_ptr) and (full = '0') else
             '0';

    process(clk)
    begin
        if rising_edge(clk) then
            if rst = '0' then
                -- reset to empty and at index 0
                head_ptr <= (others => '0');
                tail_ptr <= (others => '0');
                full <= '0';
                done <= '0';
            else
                -- done flag set by done_set input, cleared by new data
                if done_set = '1' then
                    done <= '1';
                elsif push = '1' then
                    done <= '0';
                end if;

                -- pop only possible if not empty
                if (pop = '1') and (empty = '0') then
                    -- pop by incrementing head pointer to next element
                    head_ptr <= head_ptr + 1;
                    -- if we were full, aren't anymore
                    full <= '0';
                end if;

                -- push only possible if not full
                if (push = '1') and (full = '0') then
                    -- increment tail pointer to next element
                    tail_ptr <= tail_ptr + 1;
                    -- new tail element gets input values
                    FIFO_array(to_integer(tail_ptr)) <= (reg_0 => tail_reg_0,
                                                        reg_1 => tail_reg_1,
                                                        reg_2 => tail_reg_2,
                                                        reg_3 => tail_reg_3,
                                                        px_x => tail_px_x,
                                                        px_y => tail_px_y);
                    -- if the tail pointer runs into the head, is full now
                    if tail_ptr + 1 = head_ptr then
                        full <= '1';
                    end if;
                end if;
            end if;
        end if;
    end process;
end architecture;