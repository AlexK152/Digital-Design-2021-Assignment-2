library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library xil_defaultlib;
use xil_defaultlib.common_pack.all;


entity tb_Bin_BCD is
end;


architecture tb of tb_Bin_BCD is
    
    component Binary_to_BCD is
    port (
        binary_in: in std_logic_vector(9 downto 0);
        BCD_out: out std_logic_vector(11 downto 0);
        i_sig: out integer range 0 to 10;
        maxIndex: out BCD_ARRAY_TYPE(2 downto 0)
        );
    end component;

    FOR BCD1: Binary_to_BCD USE ENTITY work.Bin_BCD(w_array);

    signal clk: std_logic := '0';
    signal binary_in: std_logic_vector(9 downto 0);
    signal BCD_out: std_logic_vector(11 downto 0);
    signal i_sig: integer range 0 to 10;
    signal i_sig_vector: std_logic_vector(3 downto 0);
    signal maxIndex: BCD_ARRAY_TYPE(2 downto 0);

begin

    binary_in <= "0000000000", "1101101011" after 30 ns, "0111001100" after 60 ns, "0100100011" after 100 ns, "1111111111" after 130 ns, "1001101111" after 160 ns;
    clk <= NOT clk AFTER 5 ns WHEN NOW < 3 us ELSE clk;
    i_sig_vector <= std_logic_vector(to_unsigned(i_sig, i_sig_vector'length));

    BCD1: Binary_to_BCD
        port map(
            binary_in => binary_in,
            BCD_out => BCD_out,
            i_sig => i_sig,
            maxIndex => maxIndex
        );
    

    

end tb;
        