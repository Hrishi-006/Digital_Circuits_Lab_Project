library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity RLE_encoder is
    port (
        clk            : in std_logic;
        reset          : in std_logic;
        start          : in std_logic;
        data_in        : in std_logic_vector(7 downto 0);
        data_out       : out std_logic_vector(15 downto 0);
        done           : out std_logic;
        reduced_length : out unsigned (7 downto 0)
    );
end entity;

architecture arch of RLE_encoder is
    -- Matrix storage
    type mem_t is array (0 to 63) of std_logic_vector(7 downto 0);
    signal mem : mem_t;

    -- Zigzag order (as provided)
    type zigzag_t is array (0 to 63) of integer range 0 to 63;
    constant zigzag_order : zigzag_t := (
        0, 1, 8,
        16, 9, 2,
        3, 10, 17, 24,
        32, 25, 18, 11, 4,
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

    -- RLE Buffer
    type rle_t is array (0 to 63) of std_logic_vector(15 downto 0);
    signal rle_buffer : rle_t;

    -- FSM states
    type state_t is (idle, mem_fill, mem_compress, output);
    signal state : state_t;

 
    signal memory_fill_counter   : integer range 0 to 63;
    signal zigzag_count    : integer range 0 to 64; 
    signal rle_write_ptr  : integer range 0 to 63;
    signal output_counter    : integer range 0 to 63;
    
  
    signal current_char : std_logic_vector(7 downto 0);
    signal repitition_counter      : unsigned(7 downto 0);
    signal rle_length    : unsigned(7 downto 0);

begin

   
    process(clk, reset)
        variable next_symbol : std_logic_vector(7 downto 0);
    begin
        if (reset = '1') then
       
            state          <=idle;
            memory_fill_counter   <= 0;
            zigzag_count    <= 0;
            rle_write_ptr  <= 0;
            output_counter    <= 0;
            repitition_counter      <= (others => '0');
            current_char <= (others => '0');
            rle_length    <= (others => '0');
            
        elsif (rising_edge(clk)) then
            
            case state is
                when idle =>
                    if (start = '1') then
                       
                        mem(0) <= data_in; 
                        state        <= mem_fill;
                        memory_fill_counter <= 1; 
                        zigzag_count  <= 0;
                        rle_write_ptr <= 0;
                        output_counter  <= 0;
                        rle_length  <= (others => '0');
                    else
                        state <= idle;
                    end if;

                when mem_fill =>
                  
                    mem(memory_fill_counter) <= data_in;
                    
                    if (memory_fill_counter = 63) then
                       
                        state <= mem_compress;
                        
                        current_char <= mem(zigzag_order(0));
                        repitition_counter      <= "00000001";
                        rle_write_ptr  <= 0;
                        zigzag_count   <= 1; 
                    else
                        
                        state        <= mem_fill;
                        memory_fill_counter <= memory_fill_counter + 1;
                    end if;

                when mem_compress =>
                  
                    
                    if (zigzag_count= 64) then
                        
                        rle_buffer(rle_write_ptr) <= std_logic_vector(repitition_counter) & current_char;
                        rle_length <= to_unsigned(rle_write_ptr + 1, 8);
                        
								state <= output;
                        output_counter<= 0; 
                    else
                        
                        next_symbol := mem(zigzag_order(zigzag_count));
                        
                        if (next_symbol = current_char) then
                            
                            repitition_counter <= repitition_counter + 1;
                        else
                          
                            rle_buffer(rle_write_ptr) <= std_logic_vector(repitition_counter) & current_char;
                            
                            current_char <= next_symbol;
                            repitition_counter      <= "00000001";
                            rle_write_ptr  <= rle_write_ptr + 1;
                        end if;
                        
               
                        zigzag_count<= zigzag_count+ 1;
                        state      <= mem_compress;
                    end if;

                when output =>
                    
                    if (output_counter= to_integer(rle_length - 1)) then
                       
                        state <= idle;
                    else
                        
                        state      <= output;
                        output_counter<= output_counter+ 1;
                    end if;
                    
            end case;
        end if;
    end process;


    done     <= '1' when state = output else '0';
    data_out <= rle_buffer(output_counter) when state = output else (others => '0');
	 reduced_length <= rle_length;

end architecture;