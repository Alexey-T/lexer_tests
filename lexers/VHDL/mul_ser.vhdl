-- mul_ser.vhdl  multiplier implemented as serial adds (one 32 bit adder)
-- needs a 32 bit adder called  add32, else fix to use another adder
-- this is for positive operands, use Booth multiplier for two's complement
-- This example uses a signal as a register, modify to suit your needs.

entity mul_ser is                       -- test bench
end mul_ser;

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
use IEEE.std_logic_textio.all;
use STD.textio.all;

architecture schematic of mul_ser is
  subtype word is std_logic_vector(31 downto 0); 
  signal md     : word := x"20010007";  -- multiplier or divisor
  signal hi     : word := x"00000000";  -- top register (final=00000002)
  signal lo     : word := x"00000011";  -- bottom register (final=20110077)
  signal cout   : std_logic;            -- adder carry out
  signal muls   : word := x"00000000";  -- adder sum
  signal mulb   : word := x"00000000";  -- multiplexor output
  signal clk    : std_logic := '0';     -- system clock
  signal mulclk : std_logic := '0';     -- multiplier clock
  signal mulenb : std_logic := '1';     -- enable multiplication
  signal cntr   : std_logic_vector(5 downto 0) := "000000";  -- counter
begin  -- schematic

  clk <= not clk after 5 ns;            -- 10 ns period
  cntr <= unsigned(cntr)+unsigned'("000001") when clk'event and clk='1';
          -- cntr statement is equivalent to six bit adder and clocked register
  
  mulenb <= '0' when (cntr="100001");   -- enable/disable multiply
  mulclk <= clk and mulenb after 50 ps; -- the multipliers "private" clock

  -- multiplier structure, not a component!
  
  mulb  <= md when (lo(0)='1') else x"00000000" after 50 ps; -- mux  md or 0
  adder:entity WORK.add32 port map(hi, mulb, '0', muls, cout); -- 32 bit adder

  hi <= cout & muls(31 downto 1) when mulclk'event and mulclk='1'; --note shift
  lo <= muls(0) & lo(31 downto 1) when mulclk'event and mulclk='1';--note shift

  printout:  postponed process(clk) -- just to see values
               variable my_line : LINE;   -- not part of working circuit
             begin
               if clk='0' then  -- quiet time, falling clock
                 if cntr="000000" then 
                   write(my_line, string'("md="));
                   hwrite(my_line, md);
                   writeline(output, my_line);
                 end if;
                 write(my_line, string'("at count "));
                 write(my_line, cntr);
                 write(my_line, string'("  mulb="));
                 hwrite(my_line, mulb);
                 write(my_line, string'("  muls="));
                 hwrite(my_line, muls);
                 write(my_line, string'("  hi="));
                 hwrite(my_line, hi);
                 write(my_line, string'("  lo="));
                 hwrite(my_line, lo);
                 writeline(output, my_line);
               end if;
             end process printout;
end schematic;

