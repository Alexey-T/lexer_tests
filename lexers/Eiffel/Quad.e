deferred class QUADRUPEDE

inherit
   ANIMAL
      redefine reproduction
      end;

feature {ANY}

   reproduction(autre: ANIMAL): ANIMAL is
      do
         Result := autre.reproduction_avec_quadrupede(Current);
      end;

end -- QUADRUPEDE
