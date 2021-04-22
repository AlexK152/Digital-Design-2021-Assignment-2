library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
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

type state_type is (Idle, Data_Ready, ClearMem, CheckCount1, RequestByte, EnableShiftReg, 
                    CommPause, GenPause, IncreaseCount2, CheckCount2, EndSeq);
signal curState, nextState: state_type;
signal ctrl_2_delayed, ctrl_2_detected: std_logic;
signal counter2_out: std_logic_vector(1 downto 0);
signal counter1_enable, counter2_enable, shiftReg_enable, ctrl_1_change, resetMemElements: std_logic; -- Signals driven by FSM output logic
signal ctrl_1_not,ctrl_1_int, mux_out: std_logic;
signal counter1_out, counter1_out_lock: integer range 0 to 999;
signal numWords_bin: std_logic_vector(9 downto 0);
signal equalTrue: std_logic;

signal Input_index: std_logic_vector(9 downto 0);
signal Reg2_enable: std_logic;
signal sub_Output, indexReg_out: unsigned(9 downto 0);
signal output:integer range 0 to 6 := 0;
signal data1: std_logic_vector(7 downto 0);

signal Output_Reg: std_logic_vector(55 downto 0);
signal data2, Output_bytes: std_logic_vector(55 downto 0);

begin

------------------------------
-- FSM combinational logic for next state

    combi_nextState: process(curState, start, equalTrue, counter2_out, ctrl_2_detected)
        begin
            case curState is
                when Idle => -- waits for new sequence request, keeps old sequence result values
                    if start = '1' then
                        nextState <= ClearMem;
                    else
                        nextState <= Idle;
                    end if;
             
                when ClearMem => -- post-initiation reset state; resets all memory elements seperate from system reset
                    nextState <= CheckCount1;

                when CheckCount1 => -- checks if the requested number of bytes have been processed
                    if equalTrue = '1' then
                        nextState <= CheckCount2;
                    else
                        nextState <= RequestByte;
                    end if;

                when RequestByte => -- requests a new byte from the Data Generator
                    nextState <= GenPause;

                when EnableShiftReg => -- stores the new byte (into a shift register) after validation from Data Generator
                    if equalTrue = '1' then -- this state is a part of two separate state loops, which is why it branches out depending on 'equalTrue'
                        nextState <= IncreaseCount2;
                    else
                        nextState <= Data_Ready; -- in the state loop for the primary counter, that counter is incremented in the "EnableShiftReg" state
                    end if;

                when Data_Ready => -- the 'dataReady' signal to the Command Processor resides in its own state to ensure it stays 'high' for one clock cycle
                    if start = '1' then
                        nextState <= CheckCount1; -- if 'start' is not asserted 'low', the system will bypass the pause state which waits for the Command Processor
                    else 
                        nextState <= CommPause;
                    end if;

                when CommPause => -- waits for Command Processor's start signal after giving it a new byte
                    if start = '1' or equalTrue = '1' then -- "equalTrue = '1'" accounts for reaching the last byte of the sequence where no new byte should be requested
                        nextState <= CheckCount1;
                    else
                        nextState <= CommPause;
                    end if;

                when GenPause => -- waits for Data Generator to send a new byte
                    if ctrl_2_detected = '1' then
                        nextState <= EnableShiftReg;
                    else
                        nextState <= GenPause;
                    end if;

                when IncreaseCount2 => -- increments the second counter's value
                    nextState <= CheckCount2;

                when CheckCount2 => -- checks if the secondary counter has reached the value of '3' and if so, ends the sequence
                    if counter2_out = "11" then
                        nextState <= EndSeq;
                    else
                        nextState <= EnableShiftReg;
                    end if;

                when others => -- that also includes the EndSeq state
                    nextState <= Idle;
            end case;
        end process;

------------------------------
-- FSM combinational logic for output signals

    combi_out: process(curState, equalTrue)
    begin
        counter1_enable <= '0'; -- default output signal values
        counter2_enable <= '0';
        shiftReg_enable <= '0';
        ctrl_1_change <= '0';
        dataReady <= '0';
        seqDone <= '0';
        resetMemElements <= '0';

        if curState = ClearMem then
            resetMemElements <= '1'; -- internal memory reset signal (synchronous reset)

        elsif curState = RequestByte then
            ctrl_1_change <= '1'; -- output signal to trigger two-phase protocol

        elsif curState = EnableShiftReg then
            shiftReg_enable <= '1';
            if equalTrue = '0' then 
                counter1_enable <= '1'; -- increments primary counter
            end if;

        elsif curState = Data_Ready then
            dataReady <= '1';

        elsif curState = IncreaseCount2 then
            counter1_enable <= '1'; -- increments primary counter regardless of whether it has counted all requested bytes to give an accurate index value of peak byte
            counter2_enable <= '1'; -- increments secondary counter

        elsif curState = EndSeq then
            seqDone <= '1';
        end if;
    end process;

------------------------------
-- FSM sequential logic

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

------------------------------
-- First part of 2-phase-protocol-handling components:
--    Converts event on 'ctrlIn' into a one-clock-cycle-long 'high' output on 'ctrl_2_detected'

-- | Inputs from: Control signal from Data Generator |
-- | Outputs to: FSM next state logic                |

    delay_ctrl_2: process(clk) -- A flip-flop to store a delayed value of the control signal
    begin
        if rising_edge(clk) then
            ctrl_2_delayed <= ctrlIn;
        end if;
    end process;

    ctrl_2_detected <= ctrl_2_delayed xor ctrlIn; -- Detects an event on 'ctrl_2' by comparing its current and delayed versions

------------------------------
-- Second part of 2-phase-protocol-handling components:
--    Converts one-clock-cycle-long 'high' state on 'ctrl_1_change' into and event on 'ctrlOut'

-- | Inputs from: FSM output logic                |
-- | Outputs to: Control signal to Data Generator |

-- A 2x1 multiplexer that switches its output to the 'not' version of the control signal (taken from a flip-flop)
-- just long enough for that flip-flop to store the new value
    combi_ctrl_1: process(ctrl_1_change, ctrl_1_int, ctrl_1_not) 
    begin
        if ctrl_1_change = '1' then
            mux_out <= ctrl_1_not;
        else
            mux_out <= ctrl_1_int;
        end if;
    end process;

    seq_ctrl_1: process(clk) -- D flip-flop that stores the current state of "ctrlOut"
    begin
        if rising_edge(clk) then
            if reset = '1' then -- resets with system reset
                ctrl_1_int <= '0';
            else
                ctrl_1_int <= mux_out;
                ctrl_1_not <= not mux_out;
            end if;
        ctrlOut <= ctrl_1_int;
        end if;
    end process;

------------------------------
-- Converts the "number of bytes" value from the Command Processor from Binary Coded Decimal to binary

-- | Inputs from: 'numWords' signal from Command Processor 
-- | Outputs to: Count Comparator

    BCD_to_binary: process(numWords_bcd)
    variable sum: std_logic_vector(10 downto 0); -- made 11 digits long to avoid product length warnings
    variable product1, product2: unsigned(10 downto 0);
    begin
        product1 := "0001010" * unsigned(numWords_bcd(1)); -- second BCD digit multiplied by 10
        product2 := "1100100" * unsigned(numWords_bcd(2)); -- first BCD digit multiplied by 100
        sum := std_logic_vector(unsigned(numWords_bcd(0)) + product1 + product2);
        numWords_bin <= sum(9 downto 0); -- 11th digit is not necessary for result since the maximum of 1000 bytes 
    end process;                         -- can be expressed using 10 binary digits

------------------------------
-- Compares value of requested bytes (in binary) to the output value of the primary counter, counting the number 
-- of processed bytes

-- | Inputs from: BCD-to-binary converter, Counter Lock               |
-- | Outputs to: FSM next state logic, FSM output logic, Counter Lock |

    CountComparator: process(counter1_out_lock, numWords_bin)
    variable result_flag: std_logic;
    variable counter1_out_bin: std_logic_vector(9 downto 0);
    begin
        result_flag := '0'; -- flag variable starts at '0'
        counter1_out_bin := std_logic_vector(to_unsigned(counter1_out_lock, counter1_out_bin'length));
        for i in 0 to 9 loop
            if  not (counter1_out_bin(i) = numWords_bin(i)) then
                result_flag := '1'; -- flag value becomes '1' if any digit of the compared signals does not match
            end if;
        end loop;
        equalTrue <= not result_flag;
    end process;

------------------------------
-- Component to stop the value of the primary counter, passed to the comparator, from changing when the
-- comparator indicates that its values are equal

-- | Inputs from: Primary Counter, Count Comparator
-- | Outputs to: Count Comparator

    LockCounter1: process(clk, resetMemElements, equalTrue)
    begin
        if rising_edge(clk) then
            if resetMemElements = '1' then -- synchronous reset by FSM logic
                counter1_out_lock <= 0;
            else
                if not equalTrue = '1' then
                    counter1_out_lock <= counter1_out;
                end if;
            end if;
        end if;
    end process; 

------------------------------
-- Converts the stored index of the peak in binary into BCD to give to the Command Processor
-- Heavily inspired by the "Double Dabble" or "Shift-and-add-3" algorithm

-- | Inputs from: Peak Index Register                   |
-- | Outputs to: 'maxIndex' signal to Command Processor |

    Binary_to_BCD: process(indexReg_out)
    variable hundreds, tens, ones: unsigned(3 downto 0); -- store each of the Binary Coded Decimal digits
    begin
        for i in 0 to 9 loop -- iterates as many times as there are digits in the binary input number
            
            if to_integer(hundreds) > 4 then -- first checks if any BCD digit is 5 or greater
                hundreds := hundreds + 3; -- and if so, adds 3 to it to correctly carry BCD digits
            end if;
            if to_integer(tens) > 4 then
                tens := tens + 3;
            end if;
            if to_integer(ones) > 4 then
                ones := ones + 3;
            end if;

            hundreds := hundreds(2 downto 0) & tens(3);   -- shifts the digits of the sequence of hundreds, tens and ones
            tens := tens(2 downto 0) & ones(3);           -- to the left by one and fills the empty space with one of the 
            ones := ones(2 downto 0) & indexReg_out(9-i); -- binary number's digits from left to right each iteration

        end loop;
        maxIndex(2) <= std_logic_vector(hundreds); -- pass each of the BCD digits to the custom BCD array type
        maxIndex(1) <= std_logic_vector(tens);
        maxIndex(0) <= std_logic_vector(ones);
        hundreds := (others => '0'); -- clears the BCD digit variables at the end of a conversion process
        tens := (others => '0');
        ones := (others => '0');
    end process;

------------------------------
-- Passes the Data Generator's input directly to the Command Processor's byte input
    byte <= data;


------------------------------
-- Counts the number of bytes being processed by the data processor

-- | Outputs to: the Subtractor |

    primaryCounter: process(resetMemElements, counter1_enable, clk)
    variable count: integer range 0 to 999; --max range of 1000 bytes
    begin
        if rising_edge(clk) then
            if resetMemElements = '1' then -- synchronous reset by FSM logic
                count := 0;
            elsif counter1_enable = '1' then  -- when counter is enabled, the count value increases by 1 every clock cycle
                count := count + 1;
            end if;
        end if;
    counter1_out <= count; -- Count value assigned as Primary counter's output
    end process;

------------------------------
-- Stores the Current Peak byte's index in the register.

-- | Inputs from: the Subtractor  |
-- | Outputs to: the Binary to BCD|

  IndexRegister: process(clk)
  begin
    if rising_edge(clk) then
      if resetMemElements='0' then
        if Reg2_enable='1' then -- Values are updated as register is enabled
          indexReg_out<=sub_Output;  
        end if;
      elsif resetMemElements='1' then -- synchronous reset by FSM logic
          indexReg_out<= (others => '0');

      end if;
    end if;
  end process;


--------------------

-- Shift register with serial input and parallel output.
-- | Inputs from: the data Generator   |
-- | Outputs to:  Current Peak register|

  ShiftRegister:process(clk, resetMemElements, shiftReg_enable)
  variable current_bytes:std_logic_vector(55 downto 0):= (others=>'0'); -- default output signal values
  begin
    if rising_edge(clk) then
      if shiftReg_enable='1' then  
        -- Each byte is eights bits long
        current_bytes := "00000000" & current_bytes(55 downto 8);
        current_bytes(55 downto 48) := data; -- New input byte is assigned to bit range 55 downto 48
        data1<= current_bytes(31 downto 24); -- Middle byte assigned to data1 
        Output_Reg<=current_bytes(55 downto 0); 
      elsif resetMemElements='1' then -- synchronous reset by FSM logic
        data1<= (others => '0');
      end if;
    end if;
  end process;

---------------------------

-- Current Peak Register
-- Stores the current peak byte as well as the three previous and three following bytes 
-- in a register

-- | Inputs from: Comparator                            |
-- | Outputs to: 'dataResults' signal and the Comparator|

  PeakRegister: process(clk)
  begin
    if rising_edge(clk) then
      if resetMemElements='1' then -- synchronous reset by FSM logic
        data2<= (others => '0');
      else
        if Reg2_enable= '1' then  -- Values are updated as register is enabled
          data2<=Output_Reg;
        end if;
      end if;
    end if;
  end process;


-----------------------

-- Counts till '3' 


-- | Outputs to: the FSM combinational logic for next state|


   SecondaryCounter: process (clk, resetMemElements, counter2_enable)
      variable index:integer range 0 to 3;
   begin
      if (clk'event and clk = '1') then

        if resetMemElements = '1' then -- synchronous reset by FSM logic
          index := 0;
        elsif resetMemElements = '0' then
          if counter2_enable = '1' then  -- when counter is enabled, the count value increases by 1 every clock cycle till '3'
            index := index + 1;
          end if;
         end if;
      end if;
      counter2_out<=std_logic_vector(to_unsigned(index, counter2_out'length));  
   end process;

------------------------

-- Subtractor 
-- Subtracting four from the Primary counter's output.

-- | Inputs from: PrimaryCounter                 |
-- | Outputs to:  Index Register                 |


sub_Output<= to_unsigned(Counter1_out, sub_Output'length) - 4; -- Basic subtraction in 'unsigned std_vector_logic' type



-------------------

-- Compares the current peak byte stored in the Peak register with the new byte read from the shift register

-- | Inputs from: Peak register and Shift register                 |
-- | Outputs to: Current Peak Register                             |

   Comparator: process (data1, data2)
      -- Using type 'signed' as our specification requires us to compare signed
       variable data1_byte: signed(7 downto 0);
       variable data2_byte: signed(7 downto 0);
       begin
         data1_byte := signed(data1(7 downto 0));
         data2_byte := signed(data2(31 downto 24));
         Reg2_enable <= '0';
         if data1_byte>data2_byte then -- If condition met, then Peak register is triggered to be updated
           Reg2_enable <='1';
         end if  ;
       end process;

--------------------
-- Assigning data2 to the 'dataResults' signal

    to_dataResults: process (data2)
        begin
            for i in 0 to RESULT_BYTE_NUM-1 loop -- Iterating through loop 
                dataResults(i) <= data2((55-(i*8)) downto (48-(i*8))); -- Assigning range of 'data2' bits to the relevant 'dataResults' index
            end loop;
    end process;

end;
