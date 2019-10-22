class CHAT

inherit
   QUADRUPEDE
      redefine reproduction_avec_quadrupede
      end;

feature {ANY}

   reproduction_avec_quadrupede(quadrupede: CHAT): CHAT is
      do
         Result := Current;
      end;

   crier is
      do
         std_output.put_string("MIAOU");
      end;

end -- CHAT
