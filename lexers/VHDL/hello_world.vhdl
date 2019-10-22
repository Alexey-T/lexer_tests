
-- hello_world.vhdl  Just output to the screen
--                   This should be independent of whose VHDL you use
--                   When using some vendors GUI, you have a learning curve
--                   Using portable VHDL, it will run on all vendors
--                   with implementations conforming to IEEE Std. 1076-1993


entity hello_world is  -- test bench (top level like "main")
end entity hello_world;

library STD;                            -- you don't need STD, it is automatic
library IEEE;                           -- but may need other libraries
use IEEE.std_logic_1164.all;            -- basic logic types
use STD.textio.all;                     -- basic I/O
use IEEE.std_logic_textio.all;          -- I/O for logic types

architecture test of hello_world is -- where declarations are placed
  subtype word_32 is std_logic_vector(31 downto 0);  -- simple name
  signal four_32 : word_32 := x"00000004";           -- just four in hex
  signal counter : integer := 1;                     -- initialized counter
begin  -- where parallel code is placed
  my_print : process is                  -- a process is parallel
               variable my_line : line;  -- type 'line' comes from textio
             begin
               write(my_line, string'("Hello World"));   -- formatting
               writeline(output, my_line);               -- write to "output"
               write(my_line, string'("four_32 = "));    -- formatting 
               hwrite(my_line, four_32); -- format type std_logic_vector as hex
               write(my_line, string'("  counter= "));
               write(my_line, counter);  -- format 'counter' as integer
               write(my_line, string'(" at time "));
               write(my_line, now);                     -- format time
               writeline(output, my_line);              -- write to display
               wait;
             end process my_print;
end architecture test;


-- compile/analyze this file, then simulate
-- the output on the screen should contain the following lines (without "-- ")

-- Hello World
-- four_32 = 00000004  counter= 1 at time 0 NS

