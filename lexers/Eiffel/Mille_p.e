class MILLE_PATTES

inherit ANIMAL;

feature {ANY}

   crier is
      do
         std_output.put_string("SCOLO");
      end;

end -- MILLE_PATTES
