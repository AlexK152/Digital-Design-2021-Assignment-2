library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library xil_defaultlib;
use xil_defaultlib.common_pack.all;

--use work.Array_package.all;

entity BCD_bin is
port (
    numWords_bin: out std_logic_vector(9 downto 0);
    numWords: in BCD_ARRAY_TYPE(2 downto 0)
);
end;


architecture simple of BCD_bin is
    begin

    BCD_to_binary: process(numWords)

    variable sum: std_logic_vector(10 downto 0);
    variable product1, product2: unsigned(10 downto 0);
    begin

        product1 := "0001010" * unsigned(numWords(1)); -- second decimal digit multiplied by 10
        product2 := "1100100" * unsigned(numWords(2)); -- first decimal digit multiplied by 100
        sum := std_logic_vector(unsigned(numWords(0)) + product1 + product2);
        numWords_bin <= sum(9 downto 0);
    end process;

end;

