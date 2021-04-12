library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


entity PrimCount is
port (
    resetMemElements: in std_logic;
    clk: in std_logic;
    counter1_enable: in std_logic;
    counter1_out: out integer range 0 to 999
);
end;


architecture simple of PrimCount is
    begin

    primaryCounter: process(resetMemElements, counter1_enable, clk)

    variable count: integer range 0 to 999;
    begin
        if rising_edge(clk) then
            if resetMemElements = '1' then
                count := 0;
            elsif counter1_enable = '1' then
                count := count + 1;
            end if;
        end if;
    counter1_out <= count;
    end process;

end;
