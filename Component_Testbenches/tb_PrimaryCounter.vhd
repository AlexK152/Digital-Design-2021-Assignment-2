library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


entity tb_PrimCount is
end;


architecture tb of tb_PrimCount is
    
    component PrimaryCounter is
    port (
        resetMemElements: in std_logic;
        clk: in std_logic;
        counter1_enable: in std_logic;
        counter1_out: out integer range 0 to 999
        );
    end component;

    for Count1: PrimaryCounter use entity work.PrimCount(simple);

    signal clk: std_logic := '0';
    signal resetMemElements, counter1_enable: std_logic;
    signal counter1_out: integer range 0 to 999;

begin

    resetMemElements <= '1' after 10 ns, '0' after 30 ns;
    clk <= NOT clk AFTER 5 ns WHEN NOW < 3 us ELSE clk;
    counter1_enable <= '1' after 60 ns, '0' after 70 ns, 
                       '1' after 100 ns, '0' after 110 ns, 
                       '1' after 130 ns, '0' after 140 ns,
                       '1' after 150 ns, '0' after 160 ns, 
                       '1' after 180 ns, '0' after 210 ns,
                       '1' after 230 ns, '0' after 240 ns, 
                       '1' after 250 ns, '0' after 260 ns;

    Count1: PrimaryCounter
        port map(
            resetMemElements => resetMemElements,
            clk => clk,
            counter1_enable => counter1_enable,
            counter1_out => counter1_out
        ); 

end tb;
