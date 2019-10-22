-- file_io.vhdl   write and read disk files in VHDL
--                typically used to load RAM or ROM, supply test input data, 
--                record test output (possibly for further analysis)

entity file_io is  -- test bench
end file_io;

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_textio.all;
use STD.textio.all;

architecture test of file_io is
  signal done : std_logic := '0';  -- flag set when simulation finished
begin  -- test of file_io
  done <= '1' after 5 sec;        -- probably set via logic, not time

  read_file:
    process    -- read file_io.in (one time at start of simulation)
      file my_input : TEXT open READ_MODE is "file_io.in";
      variable my_line : LINE;
      variable my_input_line : LINE;
    begin
      write(my_line, string'("reading file"));
      writeline(output, my_line);
      loop
        exit when endfile(my_input);
        readline(my_input, my_input_line);
        -- process input, possibly set up signals or arrays
        writeline(output, my_input_line);  -- optional, write to std out
      end loop;
      wait; -- one shot at time zero,
    end process read_file;

  write_file:
    process (done) is    -- write file_io.out (when done goes to '1')
      file my_output : TEXT open WRITE_MODE is "file_io.out";
      -- above declaration should be in architecture declarations for multiple
      variable my_line : LINE;
      variable my_output_line : LINE;
    begin
      if done='1' then
        write(my_line, string'("writing file"));
        writeline(output, my_line);
        write(my_output_line, string'("output from file_io.vhdl"));
        writeline(my_output, my_output_line);
        write(my_output_line, done);    -- or any other stuff
        writeline(my_output, my_output_line);
      end if;
    end process write_file;
end architecture test; -- of file_io
-- standard output
  -- reading file
  -- line one of file_io.in
  -- just showing that file
  -- can be read.
  -- writing file
-- file_io.out
  -- output from file_io.vhdl
  -- 1




