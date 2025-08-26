def main(_args: List String): IO UInt32 := do

    _args.forM λ e => println! e

    println! "----------------"

    _args.forM IO.println

    println! "----------------"

    _args.forM (println! .)

    println! "----------------"

    _args |>.forM IO.println

    println! "----------------"

    for e in _args do
      println! e

    return 0
