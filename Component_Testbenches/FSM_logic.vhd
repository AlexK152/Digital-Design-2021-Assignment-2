library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity FSM_logic is
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
end;


architecture simple of FSM_logic is

    type state_type is (Idle, ClearMem, CheckCount1, RequestByte, EnableShiftReg, 
                    CommPause, GenPause, IncreaseCount, CheckCount2, EndSeq);
    signal curState, nextState: state_type;

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

end;
