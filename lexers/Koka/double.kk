/*---------------------------------------------------------------------------
  Copyright 2017-2020 Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

/* 64-bit IEEE double floating point numbers.

*/
public module std/num/double

import std/text/parse
import std/num/int32

extern import {
  c file "double-inline.h"
  js file "double-inline.js"
}


//-----------------------------------------
// Constants
//-----------------------------------------


// &pi;
val pi          = 0x1.921FB54442D18p+1 //3.141592653589793238462643383279502884

// &pi;
val dbl-pi      = pi

// 2&pi;
val dbl-twopi   = 0x1.921FB54442D18p+2

// &pi;/2
val dbl-pi2     = 0x1.921FB54442D18p+0

// &pi;/4
val dbl-pi4     = 0x1.921FB54442D18p-1

// 3&pi;/4
val dbl-pi34    =0x1.2D97C7F3321D2p+1

// The [_e_](https://en.wikipedia.org/wiki/E_(mathematical_constant)) constant.
val dbl-e       = 0x1.5BF0A8B145769p+1 // 2.718281828459045235360287471352662498

// The natural logarithm of 2
val dbl-log2    = 0x1.62E42FEFA39EFp-1 // 0.693147180559945309417232121458176568

// The natural logarithm of 10
val dbl-log10   = 0x1.26BB1BBB55516p+1 // 2.302585092994045684017991454684364208

// The base-2 logarithm of _e_.
val dbl-log2e   =  0x1.71547652B82FEp+0 // 1.442695040888963407359924681001892137

// The base-10 logarithm of _e_.
val dbl-log10e  = 0x1.BCB7B1526E50Ep-2 // 0.434294481903251827651128918916605082

// The square-root of 2
val dbl-sqrt2   = 0x1.6A09E667F3BCDp+0 // 1.414213562373095048801688724209698079

// `1.0 / sqrt(2.0)`  (= `sqrt(0.5)`)
val dbl-sqrt12 = 0x1.6A09E667F3BCDp-1 // 0.707106781186547524400844362104849039

// [Euler's constant](https://en.wikipedia.org/wiki/Euler%E2%80%93Mascheroni_constant)
val dbl-euler   = 0x1.2788CFC6FB619p-1 // 0.577215664901532860606512090082402431

// Maximum double value
val dbl-max     = 0x1.FFFFFFFFFFFFFp+1023 // 1.79769313486231570815e+308

// Smallest positive normalized double value
val dbl-min     = 0x1.0p-1022 // 2.22507385850720138309e-308

// Smallest positive subnormal value (i.e. [``DBL_TRUE_MIN``](http://en.cppreference.com/w/cpp/types/climits))
val dbl-true-min= 0x1.0p-1074 // 4.9406564584124654418e-324

// Machine epsilon: the difference between 1.0 and the next representable `:double` value.
val dbl-epsilon = 0x1.0p-52 // 2.2204460492503130808e-16

// maximal precision in decimal digits of a `:double`.
val dbl-max-prec= 15

// Represents a value that is _not a number_ (NaN)
val nan : double    = make-nan()

// Positive infinity
val posinf : double = make-posinf()

// Negative infinity
val neginf : double = make-neginf()


//-----------------------------------------
// NaN, Infinity
//-----------------------------------------

private extern make-nan() : double {
  c  inline "(double)NAN"
  cs inline "double.NaN"
  js inline "NaN"
}

private extern make-posinf() : double {
  c  inline "HUGE_VAL"
  cs inline "double.PositiveInfinity"
  js inline "Infinity"
}

private extern make-neginf() : double {
  c  inline "-HUGE_VAL"
  cs inline "double.NegativeInfinity"
  js inline "-Infinity"
}


// Is this value equal to NaN ?
inline extern is-nan(d:double) : bool {
  c  inline "isnan(#1)"
  cs "double.IsNaN"
  js "isNaN"
}

// Is this value equal to negative or positive infinity ?
extern is-inf(d:double) : bool {
  c  inline "isinf(#1)"
  cs "double.IsInfinity"
  js inline "((#1) === Infinity || (#1) === -Infinity)"
}

// Is this value equal to positive infinity ?
inline extern is-posinf(d:double) : bool {
  c  inline "(isinf(#1) && !signbit(#1))"
  cs "double.IsPositiveInfinity"
  js inline "((#1) === Infinity)"
}

// Is this value equal to negative infinity ?
inline extern is-neginf(d:double) : bool {
  c inline "(isinf(#1) && signbit(#1))"
  cs "double.IsNegativeInfinity"
  js inline "((#1) === -Infinity)"
}

// Is this a finite number (i.e. not `nan` or infinity)
inline extern is-finite( d : double ) : bool {
  c  inline "isfinite(#1)"
  cs inline "(!double.IsNaN(#1) && !double.IsInfinity(#1))"
  js "isFinite"
}

// Is this a negative zero value?
fun is-negzero( d : double ) : bool {
  (d==0.0 && is-neginf(1.0 / d))
}

// Is this a [subnormal](https://en.wikipedia.org/wiki/Denormal_number) value?
// (i.e. `0 < d.abs < dbl-min`)
fun is-subnormal( d :double ) : bool  {
  (d != 0.0 && d.abs < dbl-min)
}


//-----------------------------------------
// Rounding
//-----------------------------------------


// Round a double to its nearest integral value.
// If the value is halfway between two integers, the value is rounded to the even one.
inline extern round(d:double) : double {
  c  "round"              // assume the rounding mode is set correctly by kklib
  cs "Math.Round"
  js "$std_core._double_round"
}

// Return the largest integer equal or less than `d`
inline extern floor(d:double) : double {
  c  "floor"
  cs "Math.Floor"
  js "Math.floor"
}

// Return the smallest integer equal or larger than `d`
inline extern ceiling : (d:double) -> double {
  c  "ceil"
  cs "Math.Ceiling"
  js "Math.ceil"
}

// Return the integral part of a `:double` `d` .
// If `d >= 0.0` , return the largest integer equal or less to `d` ,
// If `d < 0.0` , return the smallest integer equal or larger to `d` .
fun truncate(d : double) : double {
  if (d >= 0.0) then floor(d) else ceiling(d)
}

// Return the fractional part of a `:double` `d`.\
// `d.truncate + d.fraction === d`\
// `(-2.4).fraction === -0.4`
fun fraction(d : double) : double {
  d - d.truncate
}

// Return the 'floored' fraction of `d`, always greater or equal to zero.\
// `d.floor + d.ffraction === d`\
// `(-2.4).ffraction === 0.6`
fun ffraction(d : double) : double {
  d - d.floor
}


// Round a double to a specified precision. Rounds to the  even number in case of a tie.\
// `123.456.round-to-prec(2) == 123.46`\
// `123.456.round-to-prec(-1) == 120.0`\
fun round-to-prec( d : double, prec : int  ) : double {
  if (prec <= 0) then d.round else {
    val p  = exp10(prec.double)
    (d * p).round / p
  }
}

// fused multiply-add. Computes `(x*y)+z` as if to infinite precision
// with only the final result rounded back to a `:double`.
extern fmadd( x : double, y : double, z : double ) : double {
  c  "fma"
  js "_fmadd"
  cs "Math.FusedMultiplyAdd"
}

//-----------------------------------------
// Powers
//-----------------------------------------

// Return the square root of a value `d`
// Returns `nan` if `d == nan`  or if `d`  is negative.
// Returns `inf` if `d == inf` .
inline extern sqrt(d:double) : double {
  c  "sqrt"
  cs "Math.Sqrt"
  js "Math.sqrt"
}


// Return the `d` raised to the power of `p`.
inline extern pow(d:double, p:double) : double  {
  c  "pow"
  cs "Math.Pow"
  js "Math.pow"
}

// Return the natural logarithm (in base _e_)  of a `:double` `d`
inline extern log(d:double) : double  {
  c  "log"
  cs "Math.Log"
  js "Math.log"
}

// Return the logarithm in base 10 of a `:double` `d`.
fun log10( d : double ) : double {
  log(d) / dbl-log10
}

// Return the logarithm in base 2 of a `:double` `d`.
fun log2( d : double ) : double {
  log(d) / dbl-log2
}

// Return _e_ to the power of `p`.
inline extern exp( p : double ) : double {
  c  "exp"
  cs "Math.Exp"
  js "Math.exp"
}

// Return the 10 to the power of `p`.
fun exp10( p : double ) : double {
  pow(10.0,p)
}

// Return the 2 to the power of `p`.
fun exp2( p : double ) : double {
  pow(2.0,p)
}

private fun log2p1( x : double ) : double {
  dbl-log2e * log1p(x)
}

private fun exp2m1( x : double ) : double {
  expm1(dbl-log2 * x)
}

// Returns `log(exp(x) + exp(y))`.
// Avoids overlow/underflow errors.
fun logaddexp( x : double, y : double ) : double {
  if (x==y) then x + dbl-log2 else {
    val z = x - y
    if (z > 0.0) then x + log1p(exp(~z))
                 else y + log1p(exp(z))
  }
}

// Returns `log2( exp2(x) + exp2(y) )`.
// Avoids overlow/underflow errors.
fun logaddexp2( x : double, y : double ) : double {
  if (x==y) then x + 1.0 else {
    val z = x - y
    if (z > 0.0) then x + log2p1(exp2(~z))
      else y + log2p1(exp2(z))
  }
}

// Return if two doubles are nearly equal with respect to some `epsilon` (=`8*dbl-epsilon`).
// The epsilon is the nearest difference for numbers around 1.0. The routine automatically
// scales the epsilon for larger and smaller numbers, and for subnormal numbers.
fun nearly-eq( x : double, y : double, epsilon : double = 8.0*dbl-epsilon ) : bool 
  if (x == y) return True
  val diff = (x - y).abs
  if x==0.0 || y==0.0 || diff < dbl-min then
    // very close to zero, scale the epsilon for denormalized numbers
    (2.0*diff < (epsilon * dbl-min))
  else 
    val sum = x.abs + y.abs
    ((2.0*diff / (if (sum > dbl-max) then dbl-max else sum)) < epsilon)
  

// Return if two doubles are nearly equal with respect to an `epsilon` of `8*dbl-epsilon`.
// See also `nearly-eq` which takes an explicit `epsilon`.
fun (~=)(x : double, y : double ) : bool {
  nearly-eq(x,y)
}

//-----------------------------------------
// Ldexp/Frexp
//-----------------------------------------

// Low-level: return the `(lo,hi)` bits of a 64-bit double.
extern double-to-bits( d : double ) : (int32,int32) {
  c  "kk_double_to_bits"
  cs "Primitive.DoubleToBits"
  js "_double_to_bits"
}


// Low-level: create a `:double` from the given `lo` and `hi` 32-bits.
extern double-from-bits( lo : int32, hi : int32 ) : double {
  c  "kk_double_from_bits"
  cs "Primitive.DoubleFromBits"
  js "_double_from_bits"
}

/*
// Low-level: create a `:double` from the given `lo` and `hi` 32-bits.
fun double-from-bits( lo : int32, hi : int32 ) : double {
  val hifrac = hi & 0xFFFFF.int32
  val exp  = (hi.shr(20.int32) & 0x07FF.int32)
  val is-pos = hi.shr(31.int32).zero?
  val p = if (lo.is-zero && hifrac.is-zero) then {
            if (exp.is-zero) then 0.0
            elif (exp==0x7FF.int32) then posinf
            else exp2( (exp - 1023.int32).double )
          }
          else {
            val frac = lo.double + 0x1.0p32 * hifrac.double
            if (exp.is-zero) then frac * exp2( -1074.0 )  // denormalized number
            elif (exp==0x7FF.int32) then nan
            else (1.0 + frac * 0x1.0p-52) * exp2( (exp - 1023.int32).double )
          }
  if (is-pos) then p else ~p
}
*/

// Calculate 2&middot;^`e`^ for an integer `e`.
// Uses efficient bit conversion for exponents between  -1022 and 1023 and
// otherwise falls back to the regular `exp2` function converting `e` to a double.
fun exp2( e : int ) : double {
  if (e >= -1022 && e <= 1023)
    then double-from-bits(zero, (1023 + e).int32.shl(20.int32))
    else exp2( e.double )
}


// Create a double `d` given a mantissa `man` and exponent `exp`
// such that `man`&middot;2^`exp`^ =  `d` exactly (if it is representable
// by a `:double`). See also `ldexp`.
fun encode( man : int, exp : int ) : double {
  ldexp(man.double,exp)
}

private val one-p1023 = 0x1.0p1023
private val one-m1022 = 0x1.0p-1022   // = dbl-min
private fun mul-exp2( x : double, e : int ) : double {  x * exp2(e) }

// 'Load exponent': returns `x`&middot;2^`e`^ for a `is-finite` `x` and
// otherwise `x` itself. See also `encode` which loads an integer mantissa.
fun ldexp( x : double, e : int ) : double {
  if (!is-finite(x)) then x
  elif (e >= -1022) then {
    if (e <= 1023) then mul-exp2(x,e)  // usually this branch
    elif (e <= 2046) then mul-exp2( x*one-p1023, e - 1023 )
    elif (e <= 3069) then mul-exp2( x*one-p1023*one-p1023, e - 2046 )
    elif (x < 0.0) then neginf else posinf
  }
  else {
    if (e >= -2044) then mul-exp2(x*one-m1022, e + 1022)
    elif (e >= -3066) then mul-exp2(x*one-m1022*one-m1022, e + 2044 )
    elif (x < 0.0) then -0.0 else 0.0
  }
}

// Decode a double `d` into a tuple `(m,e)` of a mantissa `m` and exponent `e`
// such that `m`&middot;2^`e`^ =  `d` exactly. The mantissa `m` is
// always either 0 or in the range [2^52^, 2^53^). See also `frexp`.
fun decode( d : double ) : (int,int) {
  if (d==0.0) then (0,0)
  elif (d.is-subnormal) then decode-normalized(d * 0x1.0p54, -54)
  else decode-normalized(d,0)
}

// decode a normalized double (i.e. not subnormal)
private fun decode-normalized( d : double, e-adjust : int = 0 ) : (int,int) {
  val (lo,hi)  = double-to-bits(d)
  val sign  = if (hi.is-neg) then -1 else 1
  val exp  = and(hi.shr(20.int32),0x7FF.int32).int - 1043
  val man  = and(hi,0xFFFFF.int32).int + 0x100000
  (sign*(man * 0x100000000 + lo.uint), exp - 32 + e-adjust)
}

// 'Fraction/exponent': return the normalized fraction `f` and exponent `exp`
// for a number `x` such that `x == f`&middot;2^`exp`^.
// The absolute value of the fraction `f` is always in the range [0.5, 1.0), or
// one of `0.0`, `-0.0`, `neginf`, `posinf`, or `nan`.
// See also `decode` which  decodes to an integer mantissa.
fun frexp( x : double ) : (double, int) {
  if (!x.is-finite || x.is-negzero) return (x,0)
  val (m,e) = x.decode
  (m.double * 0x1.0p-53, e + 53  )
}


//-----------------------------------------
// Show in hexadecimal
//-----------------------------------------

/* Show a double in [hexadecimal notation](https://books.google.com/books?id=FgMsCwAAQBAJ&pg=PA41).
An advantage of this format is that it precisely represents the `:double` and can
reliably (and efficiently) be parsed back, i.e. `d.show-hex.parse-double == Just(d)`.
The significant is the _hexadecimal_ fraction while the
exponent after the `p` is the _decimal_ power of 2.
 For example, ``0xA.Fp-10`` = (10 + 15/16)&middot;2^-10^  (not 2^-16^!) = 0.01068115234375.
 Equivalently, ``0xA.Fp-10 == 0x5.78p-9 == 0x2.BCp-8 == 0x1.5Ep-7``.
```
> dbl-min.show-hex
"0x1.0p-1022"
> 0.1.show-hex
"0x1.999999999999Ap-4"
> dbl-max.show-hex
"0x1.FFFFFFFFFFFFFp+1023"
> -0.0.show-hex
"-0x0.0p+0"
> nan.show-hex
"NaN"
> 0.01068115234375.show-hex
"0x1.5Ep-7"
```
.
*/
fun show-hex( d : double, width : int = 1, use-capitals : bool = True, pre : string = "0x" ) : string {
  if (!d.is-finite) then d.show else {
    val (m,e)  = d.decode
    val man  = m.abs.show-hex(1,use-capitals,"")
    val exp0  = e + 4*(man.count - 1)
    val exp  = (if (exp0 >= 0) then "+" else "") ++ exp0.show
    val frac  = man.tail.trim-right("0").pad-right(max(1,width),'0')
    val sign  = if (d.is-neg || d.is-negzero) then "-" else ""
    sign ++ pre ++ man.head ++ "." ++ frac ++ "p" ++ exp
  }
}

//-----------------------------------------
// Parse a double
//-----------------------------------------

// Parse a double number. Can be "NaN", "Inf(inity)" (case-insensitive),
// a fix-point number (`1.2`) or in scientific notation (`-2.3e-5`).
// Also allows floats in [hexadecimal notation](https://books.google.com/books?id=FgMsCwAAQBAJ&pg=PA41) (`0xA.Fp-10`) that can
// be represented precisely (and are the preferred _round trip_ format).
fun parse-double( s : string ) : maybe<double> {
  s.trim.to-lower.slice.parse-eof(pdouble).maybe
}

fun pdouble() : parse double {
  val neg = sign()
  val d = [{ phexdouble() }, { pdecdouble() }, { pspecial() }, { 0.0 }].choose
  if (neg) then ~d else d
}

private fun phexdouble() : parse double {
  char('0')
  one-of("xX")
  val man  = hex-digits()
  val frac = optional( "", { char('.'); hex-digits() }).trim-right("0")
  val exp : int = optional( 0, { one-of("pP"); pint() })
  val m : int = (man ++ frac).parse-int(hex=True).default(0)
  val e : int = exp - 4*frac.count
  encode(m,e)
}

private fun pdecdouble() : parse double {
  val cur  = current-input()
  val man  = digits()
  val frac = optional("", { char('.'); digits0() }).trim-right("0")
  val exp : int = optional( 0, { one-of("eE"); pint() })
  //val m : int = (man ++ frac).parse-int-default(0)
  //val e : int = exp - frac.count
  //m.double * exp10(e.double)
  cur.string.prim-parse-double  // more precision than simple multiply
}

private fun pspecial() : parse double {
  [{ pstring("nan"); nan },
   { pstring("infinity"); posinf },
   { pstring("inf"); posinf }
  ].choose
}

// Return `nan` on failure
private extern prim-parse-double( s : string ) : double {
  c  "kk_prim_parse_double"
  cs "Primitive.DoubleParse"
  js "parseFloat"
}



//-----------------------------------------
// Various
//-----------------------------------------

// Return the sum of a list of doubles.
// Uses [Kahan-Babu&scaron;kan-Neumaier summation](https://en.wikipedia.org/wiki/Kahan_summation_algorithm#Further_enhancements)
// to minimize rounding errors. This
// is more precise as Kahan summation and about as fast.\
// `[1.0e3,1.0e97,1.0e3,-1.0e97].sum == 2000.0`\
// A. Neumaier, _Rundungsfehleranalyse einiger Verfahren zur Summation endlicher Summen_.
// Math. Mechanik, 54:39--51, 1974.
fun sum( xs : list<double> ) : double
{
  var total := 0.0
  var comp  := 0.0
  xs.foreach fn(x) {
    val t = total + x
    val c = if (total.abs >= x.abs) then (total - t) + x else (x - t) + total
    comp  := comp + c
    total := t
  }
  total + comp;
}

// The hypotenuse of `x` and `y`: `sqrt(x*x + y*y)`.
// Prevents overflow for large numbers.
fun hypot( x : double, y : double ) : double {
  val xx = abs(x)
  val yy = abs(y)
  val lo = min(xx,yy)
  val hi = max(xx,yy)
  if (hi==0.0) then 0.0 else {
    val z  = lo / hi
    hi * sqrt( 1.0 + z*z )
  }
}

// The square root of the sum of the squares of three doubles.
// Prevents overflow for large numbers.
fun hypot( x : double, y : double, z : double ) : double {
  val xx = abs(x)
  val yy = abs(y)
  val zz = abs(z)
  val hi = max(max(xx,yy),zz)
  if (hi==0.0) then 0.0 else {
    hi * sqrt( sqr(xx / hi) + sqr(yy / hi) + sqr(zz / hi) )
  }
}

// The square root of the sum of squares of a list of doubles.
// Prevents overflow for large numbers and uses Kahan-Babu&scaron;kan-Neumaier summation
// for precision.
fun hypot( xs : list<double> ) : double {
  val hi = xs.abs-max
  if (hi==0.0) then 0.0 else {
    hi * xs.map(fn(x){ sqr(x / hi) }).sum.sqrt
  }
}

// The square of a double
fun sqr(x : double ): double {
  x*x
}

// The maximum of the absolute values.
fun abs-max( x : double, y : double ) : double {
  max(abs(x),abs(y))
}

// The maximum of a list of absolute values.
fun abs-max( xs : list<double> ) : double {
  xs.foldl(0.0,fn(m,x){ max(abs(x),m) })
}

//-----------------------------------------
// Trigonometry
//-----------------------------------------

private val rad2deg : double = 180.0/pi
private val deg2rad : double = pi/180.0

// Convert radians to degrees.
fun deg( rad : double ) : double {
  rad * rad2deg
}

// Convert degrees to radians.
fun rad( deg : double ) : double {
  deg * deg2rad
}


// Return `x` with the sign of `y`.
public fun with-sign-of( x : double, y : double ) : double {
  if (y < 0.0) then ~(x.abs) else x.abs
}


// Return the sine of an angle in radians `d`.
inline extern sin( d : double ) : double {
  c  "sin"
  cs "Math.Sin"
  js "Math.sin"
}

// Return the cosine of an angle in radians `d`.
inline extern cos( d : double ) : double {
  c  "cos"
  cs "Math.Cos"
  js "Math.cos"
}

// Return the tangent of an angle in radians `d`.
inline extern tan( d : double ) : double {
  c  "tan"
  cs "Math.Tan"
  js "Math.tan"
}
// Return the arc-tangent of `d`
inline extern atan( d : double ) : double {
  c  "atan"
  cs "Math.Atan"
  js "Math.atan"
}

// Return the arc-tangent of a point (`x`,`y`).
inline extern atan2( x : double, y : double ) : double {
  c  "atan2"
  cs "Math.Atan2"
  js "Math.atan2"
}

// Return the arc-cosine of `d`
inline extern acos( d : double ) : double {
  c  "acos"
  cs "Math.Acos"
  js "Math.acos"
}

// Return the arc-sine of `d`
inline extern asin( d : double ) : double {
  c  "asin"
  cs "Math.Asin"
  js "Math.asin"
}


// The hyperbolic tangent of `d`
inline extern tanh( d : double ) : double {
  c  "tanh"
  cs "Math.Tanh"
  js "Math.tanh"
}

// The hyperbolic cosine of `d`
inline extern cosh( d : double ) : double {
  c  "cosh"
  cs "Math.Cosh"
  js "Math.cosh"
}

// The hyperbolic sine of `d`
inline extern sinh( d : double ) : double {
  c  "sinh"
  cs "Math.Sinh"
  js "Math.sinh"
}

// Return `log(1.0 + x)`.
// Avoids potential imprecision for small `x` where adding `1.0` explicitly
// may lead to rounding errors.
inline extern log1p( d : double ) : double {
  c  "log1p"
  js "Math.log1p"
}

// Return `exp(x - 1.0)`.
// Avoids rounding errors for values of `x` very close to `1.0`.
inline extern expm1( d : double ) : double {
  c  "log1p"
  js "Math.log1p"
}

// The area hyperbolic tangent of `d`
extern atanh( d : double ) : double {
  c  "atanh"
  js "Math.atanh"
}

// The area hyperbolic cosine of `d`
extern acosh( d : double ) : double {
  c  "acosh"
  js "Math.acosh"
}

// The area hyperbolic sine of `d`
extern asinh( d : double ) : double {
  c "asinh"
  js "Math.asinh"
}

// ------------------------
// explicit implementations
// ------------------------
  
/*  
// Return `log(1.0 + x)`.
// Avoids potential imprecision for small `x` where adding `1.0` explicitly
// may lead to rounding errors.
fun log1p( x : double ) : double {
  if (x.is-posinf) then x else {
    val y = 1.0 + x
    val z = y - 1.0
    if (z==0.0) then x else {
      log(y) * (x / z)
    }
  }
}

// Return `exp(x - 1.0)`.
// Avoids rounding errors for values of `x` very close to `1.0`.
fun expm1( x : double ) : double {
  if (x.is-posinf) then x else {
    val y = exp(x)
    if (y==1.0) then x
    elif (y - 1.0 == -1.0) then -1.0
    else (y - 1.0) * (x / log(y))
  }
}

// The area hyperbolic tangent of `x`
fun atanh( x : double ) : double {
  0.5*log( (1.0 + x) / (1.0 - x))
  /*
  // 0.5*log( (1.0 + x) / (1.0 - x)) = 0.5*log1p( (2.0*x) / (1-x) )
  if (x.abs >= 0.5) then 0.5*log1p(2.0*(x/(1.0 - x))) else {
    val x2 = x + x
    0.5*log1p(x2 + (x2*x)/(1.0 - x))
  }
  */
}

// The area hyperbolic cosine of `x`
fun acosh( x : double ) : double {
  // log(x + sqrt((x - 1.0)*(x + 1.0)));
  if (x > 0x1.0p28) then dbl-log2 + log(x)
  elif (x > 2.0) then {
    log(2.0*x - 1.0/(x + sqrt(x.sqr - 1.0)))
  }
  elif (x <= 1.0) then (if (x < 1.0) then nan else 0.0)
  else {
    val xm1 = x - 1.0
    log1p( xm1 + sqrt(2.0*xm1 + xm1*xm1) )
  }
}

// The area hyperbolic sine of `x`
fun asinh( x : double ) : double {
  //log( x + sqrt(x.sqr + 1.0))
  val xa = x.abs
  if (xa > 0x1.0p28) then (log(xa) + dbl-log2).with-sign-of(x)
  elif (xa >= 2.0) then (log(2.0*xa + 1.0/(xa + sqrt(xa.sqr + 1.0)))).with-sign-of(x)
  elif (xa == 0.0) then 0.0
  else {
    val xa2 = xa.sqr
    log1p( xa + xa2/(1.0 + sqrt( xa2 + 1.0 ))).with-sign-of(x)
  }
}

*/