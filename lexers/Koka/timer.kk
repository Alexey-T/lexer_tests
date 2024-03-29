/*----------------------------------------------------------------------------
   Copyright 2012-2021, Microsoft Research, Daan Leijen

   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the LICENSE file at the root of this distribution.
----------------------------------------------------------------------------*/

/* High resolution timer.
*/
module std/time/timer

import std/num/double
import std/num/ddouble
import std/time/duration
import std/time/instant

extern import {
  c  file "timer-inline.c"
  cs file "timer-inline.cs"
  js file "timer-inline.js"
}

// -----------------------------------------------------------
// Ticks
// -----------------------------------------------------------

// Return a high-resolution time stamp in fractional SI seconds.
// The duration is guaranteed to be monotonically increasing
// and have at least millisecond resolution.
public fun ticks() : ndet duration {
  val (secs,frac) = xticks()
  duration(secs.truncate.int, secs.fraction + frac)
}

extern xticks() : ndet (double,double) {
  c  "kk_timer_ticks_tuple"
  cs "_Timer.Ticks"
  js "_ticks"
}

// Return the smallest time difference in seconds that `ticks` can measure.
public fun ticks-resolution() : ndet duration {
  duration(xticks-resolution())
}

// Return the smallest time difference in seconds that `ticks` can measure.
extern xticks-resolution() : ndet double {
  c  "kk_timer_dresolution"
  cs "_Timer.TicksResolution"
  js "_ticks_resolution"
}

// Return the number of fractional seconds that it takes to evaluate `action`.
public fun elapsed( action : () -> <ndet|e> a ) : <ndet|e> (duration,a) {
  val t0 = ticks()
  val x = action()
  val t1 = ticks()
  (t1 - t0, x)
}

// Measure the number of fractional seconds that it takes to evaluate `action`, and print `msg` postfixed with the
// measured time in millisecond resolution.
public fun print-elapsed( action : () -> <ndet,console|e> a, msg : string = "elapsed" ) : <ndet,console|e> a {
  val (t,x) = elapsed(action)
  println( msg ++ " " ++ t.show(3) )
  x
}
