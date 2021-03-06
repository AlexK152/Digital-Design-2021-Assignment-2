library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library xil_defaultlib;
use xil_defaultlib.common_pack.all;


entity tb_right_combined is
end;
--------------------------------------------------------------------
--Testbench to test the combined BCD-to-binary converter, 
--Counter Comparator Logic, Primary Counter and its value lock, Binary-to-BCD converter
--------------------------------------------------------------------

architecture tb of tb_right_combined is
    
    component PrimaryCounter is
    port (
        resetMemElements: in std_logic;
        clk: in std_logic;
        counter1_enable: in std_logic;
        counter1_out: inout integer range 0 to 999;
        equalTrue: in std_logic;
        counter1_out_lock: out integer range 0 to 999
        );
    end component;

    component BCD_to_binary is
    port (
        numWords_bin: out std_logic_vector(9 downto 0);
        numWords: in BCD_ARRAY_TYPE(2 downto 0)
        );
    end component;

    component Binary_to_BCD is
    port (
        binary_in: in std_logic_vector(9 downto 0);
        BCD_out: out std_logic_vector(11 downto 0);
        i_sig: out integer range 0 to 10;
        maxIndex: out BCD_ARRAY_TYPE(2 downto 0)
        );
    end component;

    component CountCompare is
    port (
        counter1_out_lock: in integer range 0 to 999;
        numWords_bin: in std_logic_vector(9 downto 0);
        equalTrue: out std_logic
    );
    end component;

    for Count1: PrimaryCounter use entity work.PrimCount_w_lock(simple);
    for BCDbin1: BCD_to_binary use entity work.BCD_bin(simple);
    FOR BCD1: Binary_to_BCD USE ENTITY work.Bin_BCD(w_array);
    for Comp1: CountCompare use entity work.Count_Comparator(simple);

    signal clk: std_logic := '0';                         --
    signal resetMemElements, counter1_enable: std_logic;  --
    signal counter1_out: integer range 0 to 999;          --
    signal equalTrue: std_logic;                          --
    signal counter1_out_lock: integer range 0 to 999;     --
    signal numWords_bin: std_logic_vector(9 downto 0);    --
    signal numWords: BCD_ARRAY_TYPE(2 downto 0);          --

    signal binary_in: std_logic_vector(9 downto 0);       --
    signal BCD_out: std_logic_vector(11 downto 0);        --
    signal i_sig: integer range 0 to 10;                  --
    signal maxIndex: BCD_ARRAY_TYPE(2 downto 0);

begin

    binary_in <= std_logic_vector(to_unsigned(counter1_out, binary_in'length));

    resetMemElements <= '1' after 10 ns, '0' after 30 ns;
    clk <= NOT clk AFTER 5 ns WHEN NOW < 3 us ELSE clk;
    counter1_enable <= '1' after 60 ns, '0' after 70 ns, 
                       '1' after 100 ns, '0' after 110 ns, 
                       '1' after 130 ns, '0' after 140 ns,
                       '1' after 150 ns, '0' after 160 ns, 
                       '1' after 180 ns, '0' after 210 ns,
                       '1' after 230 ns, '0' after 240 ns, 
                       '1' after 250 ns, '0' after 260 ns,
                       '1' after 280 ns, '0' after 290 ns,
                       '1' after 300 ns, '0' after 310 ns, 
                       '1' after 320 ns, '0' after 330 ns,
                       '1' after 340 ns, '0' after 350 ns,
                       '1' after 360 ns, '0' after 370 ns, 
                       '1' after 380 ns, '0' after 390 ns,
                       '1' after 400 ns, '0' after 410 ns, 
                       '1' after 420 ns, '0' after 430 ns,
                       '1' after 440 ns, '0' after 450 ns,
                       '1' after 460 ns, '0' after 470 ns, 
                       '1' after 480 ns, '0' after 490 ns;

    numWords(2) <= "0000" after 5 ns;
    numWords(1) <= "0001" after 5 ns;
    numWords(0) <= "0010" after 5 ns;

    Count1: PrimaryCounter
        port map(
            resetMemElements => resetMemElements,
            clk => clk,
            counter1_enable => counter1_enable,
            counter1_out => counter1_out,
            equalTrue => equalTrue,
            counter1_out_lock => counter1_out_lock
        ); 

    BCDbin1: BCD_to_binary
        port map(
            numWords_bin => numWords_bin,
            numWords => numWords
        );

    BCD1: Binary_to_BCD
        port map(
            binary_in => binary_in,
            BCD_out => BCD_out,
            i_sig => i_sig,
            maxIndex => maxIndex
        );

    Comp1: CountCompare
        port map(
            counter1_out_lock => counter1_out_lock,
            numWords_bin => numWords_bin,
            equalTrue => equalTrue
        );
    
end tb;
