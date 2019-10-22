-- div_ser.vhdl  division implemented as serial adds (one 32 bit adder)
--               needs component  add32
-- non restoring division  (remainder may need correction - in this case
--                          add divisor, because remainder not same sign
--                          as dividend.)
-- This example uses a signal as a register, modify to suit your needs.

entity div_ser is  -- test bench for divide serial
end div_ser ;

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_textio.all;
use IEEE.std_logic_arith.all;
use STD.textio.all;

architecture schematic of div_ser is
  subtype word is std_logic_vector(31 downto 0);
  --    85 / 7 = 12 with remainder 1 (FFFFFFFA + 00000007 = 00000001)
  signal md      : word := x"00000007";  -- multiplier or divisor
  signal hi      : word := x"00000000";  -- top of dividend (final remainder)
  signal lo      : word := x"00000055";  -- bottom of dividend
  signal cout    : std_logic;            -- adder carry out
  signal divs    : word := x"00000000";  -- adder sum
  signal diva    : word := x"00000000";  -- shifted dividend
  signal divb    : word := x"00000000";  -- multiplexor output
  signal quo     : std_logic := '0';     -- quotient bit
  signal sub_add : std_logic := '1';     -- subtract first (also cin)
  signal clk     : std_logic := '0';     -- system clock
  signal divenb  : std_logic := '1';     -- divide enable
  signal divclk  : std_logic := '0';     -- run division
  signal cntr    : std_logic_vector(5 downto 0) := "000000";  -- counter
begin  -- schematic
  clk <= not clk after 5 ns;    -- 10 ns period
  cntr <= unsigned(cntr)+unsigned'("000001") when clk'event and clk='1';
          -- cntr statement is equivalent to six bit adder and clocked register

  divenb <= '0' when cntr="100001";  -- stop divide
  divclk <= clk and divenb after 50 ps;

  -- divider structure, not a component!

  diva    <= hi(30 downto 0) & lo(31) after 50 ps; -- shift
  divb    <= not md when sub_add='1' else md after 50 ps; -- subtract or add
  adder:entity WORK.add32 port map(diva, divb, sub_add, divs, cout);

  quo     <= not divs(31) after 50 ps; -- quotient bit

  hi      <= divs                  when divclk'event and divclk='1';
  lo      <= lo(30 downto 0) & quo when divclk'event and divclk='1';
  sub_add <= quo                   when divclk'event and divclk='1';

  printout: postponed process(clk)  -- just to see values
              variable my_line : LINE;  -- not part of working circuit
            begin
              if clk='0' then           -- quiet time, falling clock
                if cntr="000000" then
                  write(my_line, string'("divisor="));
                  write(my_line, md);
                  writeline(output, my_line);
                end if;
                write(my_line, string'("at count "));
                write(my_line, cntr);
                write(my_line, string'("  diva="));
                hwrite(my_line, diva);
                write(my_line, string'("  divb="));
                hwrite(my_line, divb);
                write(my_line, string'("  hi="));
                hwrite(my_line, hi);
                write(my_line, string'("  lo="));
                hwrite(my_line, lo);
                write(my_line, string'("  quo="));
                write(my_line, quo);
                writeline(output, my_line);
              end if;
            end process printout;
end architecture schematic;







