deferred class ANIMAL

feature {ANY}

   cri: STRING is
      deferred
      end;

   crier2 is
      do
         std_output.put_string(cri);
      end;

   crier is
      deferred
      end;

   reproduction(autre: ANIMAL): ANIMAL is
      require
         autre /= Void
      do
         Result := autre;
      end;

   reproduction_avec_quadrupede(autre: QUADRUPEDE): ANIMAL is
      require
         autre /= Void
      do
         Result := autre;
      end;

   felicitations(autre: ANIMAL) is
      do
         Current.crier2;
         std_output.put_string(" + ");
         autre.crier;
         std_output.put_string(" = ");
         Current.reproduction(autre).crier;
         std_output.put_new_line;
      end;

end -- ANIMAL
