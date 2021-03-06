library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


entity tb_FSM_logic is
end;


architecture tb of tb_FSM_logic is
    
    component FSM_logic is
    port (
        clk: in std_logic;
        reset: in std_logic;
        start: in std_logic;
        equalTrue: in std_logic;
        counter2_out: in std_logic_vector(1 downto 0);
        ctrl_2_detected: in std_logic;

        counter1_enable: out std_logic;
        counter2_enable: out std_logic;
        shiftReg_enable: out std_logic;
        ctrl_1_change: out std_logic;
        dataReady: out std_logic;
        seqDone: out std_logic;
        resetMemElements: out std_logic
        );
    end component;

    for FSM1: FSM_logic use entity work.FSM_logic(simple);

        signal clk: std_logic := '0';
        signal reset: std_logic;
        signal start: std_logic := '0';
        signal equalTrue: std_logic;
        signal counter2_out: std_logic_vector(1 downto 0) := "00";
        signal ctrl_2_detected: std_logic;

        signal counter1_enable: std_logic;
        signal counter2_enable: std_logic;
        signal shiftReg_enable: std_logic;
        signal ctrl_1_change: std_logic;
        signal dataReady: std_logic;
        signal seqDone: std_logic;
        signal resetMemElements: std_logic;



begin

       

    reset <= '1' after 10 ns, '0' after 30 ns;
    clk <= NOT clk AFTER 5 ns WHEN NOW < 3 us ELSE clk;
    --start <= '1' after 65 ns, '0' after 75 ns;
    process begin
            
            wait until rising_edge(clk);
            wait for 30 ns;
            start <= '1';
            wait for 10 ns;
            start <= '0';
            wait until dataReady = '1';
end process;
process begin
            wait until rising_edge(ctrl_1_change);
            wait until rising_edge(clk);
            wait for 20 ns;
            ctrl_2_detected <= '1';
            wait for 10 ns;
            ctrl_2_detected <= '0'; 
end process;
    Count_bytes: process(counter1_enable,clk, resetMemElements)
     variable byte_count: integer range 0 to 50;
    begin
        if rising_edge(clk) and resetMemElements = '1' then
            byte_count := 0;
        end if;
        if rising_edge(clk) and counter1_enable = '1' then
            byte_count := byte_count + 1;
    
        end if;
    if byte_count > 9 then
        equalTrue <= '1';
    end if;
    end process;

    Secondary: process(counter2_enable,clk, resetMemElements)
     variable secCount: integer range 0 to 50;
    begin
        if rising_edge(clk) and resetMemElements = '1' then
            secCount := 0;
        end if;
        if rising_edge(clk) and counter2_enable = '1' then
            secCount := secCount + 1;
        end if;
        if secCount = 3 then
            counter2_out <= "11";
        end if;
    end process;



    FSM1: FSM_logic
        port map(
        clk => clk,
        reset => reset,
        start => start,
        equalTrue => equalTrue,
        counter2_out => counter2_out,
        ctrl_2_detected => ctrl_2_detected,

        counter1_enable => counter1_enable,
        counter2_enable => counter2_enable,
        shiftReg_enable => shiftReg_enable,
        ctrl_1_change => ctrl_1_change,
        dataReady => dataReady,
        seqDone => seqDone,
        resetMemElements => resetMemElements
        ); 

end tb;
