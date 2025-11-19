library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity RLE_decoder is
  port (
    clk            : in  std_logic;
    reset          : in  std_logic;
    data_in        : in  std_logic_vector(15 downto 0);
    start          : in  std_logic;
    reduced_length : in  unsigned(7 downto 0);
    data_out       : out std_logic_vector(7 downto 0);
    done           : out std_logic
  );
end entity RLE_decoder;

architecture rtl of RLE_decoder is

  type mem_t is array (0 to 63) of std_logic_vector(7 downto 0);
  signal mem : mem_t := (others => (others => '0'));

  type rle_t is array (0 to 255) of std_logic_vector(15 downto 0);
  signal rle_buffer : rle_t := (others => (others => '0'));

  type zigzag_t is array (0 to 63) of integer range 0 to 63;
  constant zigzag_order : zigzag_t := (
    0, 1, 8, 16, 9, 2,
    3, 10, 17, 24, 32, 25, 18, 11, 4,
    5, 12, 19, 26, 33, 40,
    48, 41, 34, 27, 20, 13, 6,
    7, 14, 21, 28, 35, 42, 49, 56,
    57, 50, 43, 36, 29, 22, 15,
    23, 30, 37, 44, 51, 58,
    59, 52, 45, 38, 31,
    39, 46, 53, 60,
    61, 54, 47,
    55, 62,
    63
  );

  ----------------------------------------------------------------
  -- FSM states
  ----------------------------------------------------------------
  type state_t is (S_IDLE, S_FILL_RLE, S_EXPAND, S_OUTPUT);
  signal state : state_t := S_IDLE;

  signal write_ptr     : integer range 0 to 255 := 0;
  signal zigzag_ptr    : integer range 0 to 64  := 0;
  signal read_ptr      : integer range 0 to 255 := 0;
  signal out_ptr       : integer range 0 to 63  := 0;
  signal symbol_reg    : std_logic_vector(7 downto 0) := (others => '0');
  
  signal repeat_left   : integer range 0 to 255 := 0;

begin
  process(clk, reset)
    variable len_int : integer := 0;
    variable cnt_tmp : integer;
  begin
    if rising_edge(clk) then
      if reset = '1' then
        len_int := 0;
        mem <= (others => (others => '0'));
        rle_buffer <= (others => (others => '0'));
        state <= S_IDLE;
        write_ptr <= 0;
        zigzag_ptr <= 0;
        read_ptr <= 0;
        out_ptr <= 0;
        symbol_reg <= (others => '0');
        
        repeat_left <= 0;

      else
        case state is
          when S_IDLE =>
            if start = '1' then
              len_int := to_integer(reduced_length);
              rle_buffer(0) <= data_in;
              write_ptr <= 1;
              zigzag_ptr <= 0;
              read_ptr <= 0;
              out_ptr <= 0;
              state <= S_FILL_RLE;
            end if;

          when S_FILL_RLE =>
            rle_buffer(write_ptr) <= data_in;
            if write_ptr = len_int then
              read_ptr <= 0;
              zigzag_ptr <= 0;
              repeat_left <= 0;
              symbol_reg <= (others => '0');
              state <= S_EXPAND;
            else
              write_ptr <= write_ptr + 1;
            end if;

          when S_EXPAND =>
            if zigzag_ptr >= 64 then
              out_ptr <= 0;
              state <= S_OUTPUT;
            else
              if repeat_left = 0 then
                if read_ptr < len_int then
                  cnt_tmp := to_integer(unsigned(rle_buffer(read_ptr)(15 downto 8)));
                  symbol_reg <= rle_buffer(read_ptr)(7 downto 0);
                  read_ptr <= read_ptr + 1;
                  repeat_left <= cnt_tmp;
                
                end if;
              else
                mem(zigzag_order(zigzag_ptr)) <= symbol_reg;
                repeat_left <= repeat_left - 1;
                zigzag_ptr <= zigzag_ptr + 1;
              end if;
            end if;

          when S_OUTPUT =>
            if out_ptr = 63 then
              state <= S_IDLE;
              out_ptr <= 0;
              write_ptr <= 0;
            else
              out_ptr <= out_ptr + 1;
            end if;

          when others =>
            state <= S_IDLE;
        end case;
      end if;
    end if;
  end process;

  done <= '1' when state = S_OUTPUT else '0';
  data_out <= mem(out_ptr) when state = S_OUTPUT else (others => '0');

end architecture;
