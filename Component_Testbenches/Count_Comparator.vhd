library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity Count_Comparator is
port (
    counter1_out_lock: in integer range 0 to 999;
    numWords_bin: in std_logic_vector(9 downto 0);
    equalTrue: out std_logic
);
end;


architecture simple of Count_Comparator is
    begin

        CountComparator: process(counter1_out_lock, numWords_bin)
    variable result_flag: std_logic;
    variable counter1_out_bin: std_logic_vector(9 downto 0);
    begin
        result_flag := '0';
        counter1_out_bin := std_logic_vector(to_unsigned(counter1_out_lock, counter1_out_bin'length));
        for i in 0 to 9 loop
            if  not (counter1_out_bin(i) = numWords_bin(i)) then
                result_flag := '1';
            end if;
        end loop;
        equalTrue <= not result_flag;
    end process;

end;
