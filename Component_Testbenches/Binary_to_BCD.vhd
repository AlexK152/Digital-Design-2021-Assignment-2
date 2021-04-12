library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library xil_defaultlib;
use xil_defaultlib.common_pack.all;

--use work.Array_package.all;

entity Bin_BCD is
port (
    binary_in: in std_logic_vector(9 downto 0);
    BCD_out: out std_logic_vector(11 downto 0);
    i_sig: out integer range 0 to 10;
    maxIndex: out BCD_ARRAY_TYPE(2 downto 0)
);
end;


architecture looped of Bin_BCD is


    begin

    Binary_to_BCD: process(binary_in)

    variable hundreds, tens, ones: unsigned(3 downto 0) := (others => '0');
    variable binaryInput: unsigned(9 downto 0);
    

    begin
        binaryInput := unsigned(binary_in);
        
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

            hundreds := shift_left(hundreds, 1);
            if tens > 7 then hundreds := hundreds + 1; -- greater than "0111" would mean the first bit  would be a 1
            end if;
            tens := shift_left(tens, 1);
            if ones > 7 then tens := tens + 1; -- greater than "0111" would mean the first bit  would be a 1
            end if;
            ones := shift_left(ones, 1);
            if binaryInput(9-i) = '1' then ones := ones + 1;
            end if;

            i_sig <= i;

        end loop;
        BCD_out(11 downto 8) <= std_logic_vector(hundreds);
        BCD_out(7 downto 4) <= std_logic_vector(tens);
        BCD_out(3 downto 0) <= std_logic_vector(ones);  
        hundreds := "0000"; 
        tens := "0000"; 
        ones := "0000";     
    end process;

end;


architecture w_array of Bin_BCD is


    begin
        Binary_to_BCD: process(binary_in)
        variable hundreds, tens, ones: unsigned(3 downto 0) := (others => '0');
        variable binaryInput: unsigned(9 downto 0);

        begin

        binaryInput := unsigned(binary_in);
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

end;
