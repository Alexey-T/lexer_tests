import std/time/timer

// -----------------------------------------------------------------
// Shared
// -----------------------------------------------------------------

alias solution = list<int>

fun is-safe( queen : int, xs : solution, diag : int = 1 ) : bool {
  match(xs) {
    Cons(q,qs) -> (queen != q && queen != (q+diag) && queen != (q - diag) && queen.is-safe(qs,diag+1))
    _ -> True
  }
}

// -----------------------------------------------------------------
// Hand-tuned, tail-recursive solution
// -----------------------------------------------------------------

alias solutions = list<list<int>>

fun append-safe( queen : int, xs : solution, xss : solutions ) : div solutions {
  if (queen <= 0) then xss
  else {
   if (queen.is-safe(xs))
    then append-safe( queen - 1, xs, Cons(Cons(queen,xs),xss) )
    else append-safe( queen - 1, xs, xss )
  }
}

fun extend(queen : int, acc : solutions, xss : solutions ) : div solutions {
  match(xss) {
    Cons(xs,rest) -> extend(queen, append-safe(queen,xs,acc), rest)
    Nil           -> acc
  }
}

fun find-solutions( n : int, i : int ) : div solutions {
  if (i == 0) 
   then [[]]
   else extend(n, [], find-solutions(n, i - 1))  
}

public fun queens( n : int ) : div solutions {
  find-solutions(n,n)
}

// -----------------------------------------------------------------
// Elegant solution with user-defined effects
// -----------------------------------------------------------------

effect choose {
  ctl pick(n : int) : int
  ctl fail<b>()     : b
}

fun choose-all( action ) {
  handle(action){
    return(x)       -> [x]
    ctl fail()      -> []
    rawctl pick(n)  -> list(1,n,fn(x){ rcontext.resume(x) }).concat
  }
}

fun find-solution( n : int, col : int ) : <choose,div> solution {
  if (col==0) then return []
  val sol    = find-solution(n, col - 1)
  val queen  = pick(n)
  if (queen.is-safe(sol)) then Cons(queen,sol) else fail()
}

fun queens-choose(n : int ) : div solutions {
  choose-all{ find-solution(n,n) }
}


// -----------------------------------------------------------------
// Testing
// -----------------------------------------------------------------

fun test(n) {
  print-elapsed({ queens(n) }, "regular").length.println
  print-elapsed({ queens-choose(n) }, "algebraic").length.println
}

fun main() {
  test(12)
}


fun show-solutions( xss : solutions ) : io () {
  println(xss.length.show)
  xss.foreach fn(xs) {
    println(" " ++ xs.show)
  }  
}
