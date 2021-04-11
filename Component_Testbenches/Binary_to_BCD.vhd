library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

--use work.Array_package.all;

entity Bin_BCD is
port (
    binary_in: in std_logic_vector(9 downto 0);
    BCD_out: out std_logic_vector(11 downto 0)
);
end;


architecture looped of Bin_BCD is
    begin

    Binary_to_BCD: process(binary_in)

    variable hundreds, tens, ones: unsigned(3 downto 0);
    variable binaryInput: unsigned(9 downto 0);

    begin
        binaryInput := unsigned(binary_in);
        for i in 0 to 9 loop
            hundreds := hundreds(2 downto 0) & tens(3);
            tens := tens(2 downto 0) & ones(3);
            ones := ones(2 downto 0) & binaryInput(9-i);
            if to_integer(hundreds) > 4 then
                hundreds := hundreds + 3;
            end if;
            if to_integer(tens) > 4 then
                tens := tens + 3;
            end if;
            if to_integer(ones) > 4 then
                ones := ones + 3;
            end if;
        end loop;
        BCD_out(11 downto 8) <= std_logic_vector(hundreds);
        BCD_out(7 downto 4) <= std_logic_vector(tens);
        BCD_out(3 downto 0) <= std_logic_vector(ones);        
    end process;

end;
