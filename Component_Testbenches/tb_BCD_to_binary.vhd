library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library xil_defaultlib;
use xil_defaultlib.common_pack.all;


entity tb_BCD_bin is
end;


architecture tb of tb_BCD_bin is
    
    component BCD_to_binary is
    port (
        numWords_bin: out std_logic_vector(9 downto 0);
        numWords: in BCD_ARRAY_TYPE(2 downto 0)
        );
    end component;

    for BCDbin1: BCD_to_binary use entity work.BCD_bin(simple);

    signal clk: std_logic := '0';
    signal numWords_bin: std_logic_vector(9 downto 0);
    signal numWords: BCD_ARRAY_TYPE(2 downto 0);

begin

    numWords(2) <= "0010" after 30 ns, "1000" after 60 ns, "0101" after 90 ns, "0011" after 120 ns;
    numWords(1) <= "0110" after 30 ns, "1001" after 60 ns, "0111" after 90 ns, "0101" after 120 ns;
    numWords(0) <= "0001" after 30 ns, "0101" after 60 ns, "0000" after 90 ns, "0010" after 120 ns;
    clk <= NOT clk AFTER 5 ns WHEN NOW < 3 us ELSE clk;

    BCDbin1: BCD_to_binary
        port map(
            numWords_bin => numWords_bin,
            numWords => numWords
        );
    

    

end tb;
