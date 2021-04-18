library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
--library xil_defaultlib;
use work.common_pack.all;

entity dataConsume is
port (
                clk: in std_logic;
		reset: in std_logic; -- synchronous reset
		start: in std_logic;		
		numWords_bcd: in BCD_ARRAY_TYPE(2 downto 0);
		ctrlIn: in std_logic;
		ctrlOut: out std_logic; 
		data: in std_logic_vector(7 downto 0);
		dataReady: out std_logic;
		byte: out std_logic_vector(7 downto 0);
		seqDone: out std_logic;
		maxIndex: out BCD_ARRAY_TYPE(2 downto 0);		
		dataResults: out CHAR_ARRAY_TYPE(0 to RESULT_BYTE_NUM-1) -- index 3 holds the peak
);
end;


architecture struct of dataConsume is

---------------------------------------------------------

type state_type is (Idle, ClearMem, CheckCount1, RequestByte, EnableShiftReg, 
                    CommPause, GenPause, IncreaseCount, CheckCount2, EndSeq);
signal curState, nextState: state_type;
signal ctrl_2_delayed, ctrl_2_detected: std_logic;
signal counter2_out: std_logic_vector(1 downto 0);
signal counter1_enable, counter2_enable, shiftReg_enable, ctrl_1_change, resetMemElements: std_logic; -- Signals driven by FSM output logic
signal ctrl_1_not,ctrl_1_int, mux_out: std_logic;
signal counter1_out, counter1_out_lock: integer range 0 to 999;
signal numWords_bin: std_logic_vector(9 downto 0);
signal equalTrue: std_logic;

--signal Input: std_logic_vector(55 downto 0);
signal Input_index: std_logic_vector(9 downto 0);
signal Reg2_enable: std_logic;
signal sub_Output, indexReg_out: unsigned(9 downto 0);
signal output:integer range 0 to 6 := 0;
signal data1: std_logic_vector(7 downto 0);
--signal data3: std_logic_vector(7 downto 0);
signal Output_Reg: std_logic_vector(55 downto 0);
signal data2, Output_bytes: std_logic_vector(55 downto 0);

signal ctrl_2, ctrl_1: std_logic; 

begin
    combi_nextState: process(curState, start, equalTrue, counter2_out, ctrl_2_detected)
        begin
            case curState is
                when Idle =>
                    if start = '1' then
                        nextState <= ClearMem;
                    else
                        nextState <= Idle;
                    end if;
             
                when ClearMem =>
                    nextState <= CheckCount1;

                when CheckCount1 =>
                    if equalTrue = '1' then
                        nextState <= CheckCount2;
                    else
                        nextState <= RequestByte;
                    end if;

                when RequestByte =>
                    nextState <= GenPause;

                when EnableShiftReg =>
                    if equalTrue = '1' then
                        nextState <= IncreaseCount;
                    else
                        nextState <= CommPause;
                    end if;

                when CommPause =>
                    if start = '1' then
                        nextState <= IncreaseCount;
                    else
                        nextState <= CommPause;
                    end if;

                when GenPause =>
                    if ctrl_2_detected = '1' then
                        nextState <= EnableShiftReg;
                    else
                        nextState <= GenPause;
                    end if;

                when IncreaseCount =>
                    if equalTrue = '1' then
                        nextState <= CheckCount2;
                    else
                        nextState <= CheckCount1;
                    end if;

                when CheckCount2 =>
                    if counter2_out = "11" then
                        nextState <= EndSeq;
                    else
                        nextState <= EnableShiftReg;
                    end if;

                when others => -- that also includes the EndSeq state
                    nextState <= Idle;
            end case;
        end process;


    combi_out: process(curState, equalTrue)
    begin
        counter1_enable <= '0';
        counter2_enable <= '0';
        shiftReg_enable <= '0';
        ctrl_1_change <= '0';
        dataReady <= '0';
        seqDone <= '0';
        resetMemElements <= '0';

        if curState = ClearMem then
            resetMemElements <= '1';

        elsif curState = RequestByte then
            ctrl_1_change <= '1';

        elsif curState = EnableShiftReg then
            shiftReg_enable <= '1';

        elsif curState = CommPause then
            dataReady <= '1';

        elsif curState = IncreaseCount then
            counter1_enable <= '1';
            if equalTrue = '1' then
                counter2_enable <= '1';
            end if;

        elsif curState = EndSeq then
            seqDone <= '1';
        end if;
    end process;

    seq_state: process (clk, reset)
    begin
        if rising_edge(clk) then
            if reset = '1' then
                curState <= Idle;
            else
                curState <= nextState;
            end if;
        end if;
    end process;


    delay_ctrl_2: process(clk) -- A flip-flop to store a delayed value of the control signal to detect change when compared to its present state
    begin
        if rising_edge(clk) then
            ctrl_2_delayed <= ctrl_2;
        end if;
    end process;

    ctrl_2_detected <= ctrl_2_delayed xor ctrl_2; -- Detects an event on 'ctrl_2' by comparing its current and delayed versions


    combi_ctrl_1: process(ctrl_1_change, ctrl_1_int, ctrl_1_not) -- A 2x1 mux, working together with a D flip-flop to cause an event on "ctrl_1" when "ctrl_1_change" goes 'high' for one clock cycle
    begin
        if ctrl_1_change = '1' then
            mux_out <= ctrl_1_not;
        else
            mux_out <= ctrl_1_int;
        end if;
    end process;

    seq_ctrl_1: process(clk) -- D flip-flop that stores the current state of "ctrl_1"
    begin
        if rising_edge(clk) then
            if reset = '1' then
                ctrl_1_int <= '0';
            else
                ctrl_1_int <= mux_out;
                ctrl_1_not <= not mux_out;
            end if;
        ctrl_1 <= ctrl_1_int;
        end if;
    end process;


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


    BCD_to_binary: process(numWords_bcd)
    variable sum: std_logic_vector(10 downto 0);
    variable product1, product2: unsigned(10 downto 0);
    begin
        product1 := "0001010" * unsigned(numWords_bcd(1)); -- second decimal digit multiplied by 10
        product2 := "1100100" * unsigned(numWords_bcd(2)); -- first decimal digit multiplied by 100
        sum := std_logic_vector(unsigned(numWords_bcd(0)) + product1 + product2);
        numWords_bin <= sum(9 downto 0);
    end process;


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


    LockCounter1: process(clk, resetMemElements, equalTrue)
    begin
        if rising_edge(clk) then
            if resetMemElements = '1' then
                counter1_out_lock <= 0;
            else
                if not equalTrue = '1' then
                    counter1_out_lock <= counter1_out;
                end if;
            end if;
        end if;
    end process; 


    Binary_to_BCD: process(indexReg_out)
    variable hundreds, tens, ones: unsigned(3 downto 0);
    variable binaryInput: unsigned(9 downto 0);
    begin
        binaryInput := indexReg_out;
        for i in 0 to 9 loop
            
            if to_integer(hundreds) > 4 then
                hundreds := hundreds + 3;
            end if;
            if to_integer(tens) > 4 then
                tens := tens + 3;
            end if;
            if to_integer(ones) > 4 then
                ones := ones + 3;
            end if;

            hundreds := hundreds(2 downto 0) & tens(3);
            tens := tens(2 downto 0) & ones(3);
            ones := ones(2 downto 0) & binaryInput(9-i);

        end loop;
        maxIndex(2) <= std_logic_vector(hundreds);
        maxIndex(1) <= std_logic_vector(tens);
        maxIndex(0) <= std_logic_vector(ones);
        hundreds := (others => '0');
        tens := (others => '0');
        ones := (others => '0');
    end process;


----------------------
    byte <= data;
-----------------------

-- current peak index reg
  process(clk)
  begin
    if rising_edge(clk) then
      if resetMemElements='0' then
        if Reg2_enable='1' then
          indexReg_out<=sub_Output;
        end if;
      elsif resetMemElements='1' then
          indexReg_out<= (others => '0');

      end if;
    end if;
  end process;


--------------------

-- shift reg
  behaviour:process(clk, resetMemElements, shiftReg_enable)
  variable current_bytes:std_logic_vector(55 downto 0):= (others=>'0');
  begin
    --current_bytes:=(others:= "0");
    if rising_edge(clk) then
      if shiftReg_enable='1' then
 
        current_bytes := "00000000" & current_bytes(55 downto 8);
        current_bytes(55 downto 48) := data;
        data1<= current_bytes(31 downto 24);
        Output_Reg<=current_bytes(55 downto 0);

      elsif resetMemElements='1' then
        data1<= (others => '0');
      end if;
    end if;
  end process;

---------------------------

-- current peak register

  process(clk)
  begin
    if rising_edge(clk) then
      if resetMemElements='1' then
        data2<= (others => '0');
      else
        if Reg2_enable= '1' then
          data2<=Output_Reg;
        end if;
      end if;
    end if;
  end process;


-----------------------

-- secondary counter

   -- An enable counter
   process (clk, resetMemElements, counter2_enable)
      variable index:integer range 0 to 3;
   begin
      if (clk'event and clk = '1') then

        if resetMemElements = '1' then
          index := 0;
        elsif resetMemElements = '0' then
          if counter2_enable = '1' then
            index := index + 1;
          end if;
         end if;
      end if;
      counter2_out<=std_logic_vector(to_unsigned(index, counter2_out'length));
   end process;

------------------------

-- subtract 3

sub_Output<= to_unsigned(Counter1_out, sub_Output'length) - 3;



-------------------

-- peak compare
process (data1, data2)
    variable data1_byte: signed(7 downto 0);
    variable data2_byte: signed(7 downto 0);
    begin
      data1_byte := signed(data1(7 downto 0));
      data2_byte := signed(data2(31 downto 24));
      Reg2_enable <= '0';
      if data1_byte>data2_byte then
        Reg2_enable <='1';
      end if  ;
    end process;

--------------------
-- data2 to dataResults
    process (data2)
        begin
            for i in 0 to RESULT_BYTE_NUM-1 loop
                dataResults(i) <= data2((55-(i*8)) downto (48-(i*8)));
            end loop;
    end process;

    ctrl_2 <= ctrlIn;
    ctrlOut <= ctrl_1;

end;
