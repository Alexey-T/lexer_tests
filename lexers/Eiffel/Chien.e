class CHIEN

inherit QUADRUPEDE;

feature {ANY}

   crier is
      do
         std_output.put_string("OUARF");
      end;

end -- CHIEN
