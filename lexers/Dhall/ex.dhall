let input =
      { relative = "daughter"
      , movies   = [ "Boss Baby", "Frozen", "Moana" ]
      }

let concatSep = https://prelude.dhall-lang.org/Text/concatSep

    -- Dhall strips leading indentation for you
in  ''
    My ${input.relative} loves to watch ${concatSep ", " input.movies}
    
    How about you?
    ''
    
