/*
 *  FloatFun.scala
 *  (ScalaColliderUGens)
 *
 *  Copyright (c) 2008-2013 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth

private[synth] object FloatFun {
   import DoubleFun.LOG2

   // -------- unary ops --------

   @inline def not( f: Float ) : Float          = if( f > 0f ) 0f else 1f
   @inline def neg( f: Float ) : Float          = -f
   @inline def abs( f: Float ) : Float          = math.abs( f )
   @inline def ceil( f: Float ) : Float         = math.ceil( f ).toFloat
   @inline def floor( f: Float ) : Float        = math.floor( f ).toFloat
   @inline def frac( f: Float ) : Float         = (f - math.floor( f )).toFloat // according to jmc
   @inline def signum( f: Float ) : Float       = math.signum( f )
   @inline def squared( f: Float ) : Float      = f * f
   @inline def cubed( f: Float ) : Float        = f * f * f
   @inline def sqrt( f: Float ) : Float         = math.sqrt( f ).toFloat
   @inline def exp( f: Float ) : Float          = math.exp( f ).toFloat
   @inline def reciprocal( f: Float ) : Float   = 1.0f / f
   @inline def midicps( f: Float ) : Float      = (440 * math.pow( 2, (f - 69) / 12 )).toFloat
   @inline def cpsmidi( f: Float ) : Float      = (math.log( f / 440 ) / LOG2 * 12 + 69).toFloat
   @inline def midiratio( f: Float ) : Float    = (math.pow( 2, f / 12 )).toFloat
   @inline def ratiomidi( f: Float ) : Float    = (12 * math.log( f ) / LOG2).toFloat
   @inline def dbamp( f: Float ) : Float        = (math.pow( 10, f * 0.05 )).toFloat
   @inline def ampdb( f: Float ) : Float        = (math.log10( f )* 20).toFloat
   @inline def octcps( f: Float ) : Float       = (440 * math.pow( 2, f - 4.75 )).toFloat
   @inline def cpsoct( f: Float ) : Float       = (math.log( f / 440 ) / LOG2 + 4.75).toFloat
   @inline def log( f: Float ) : Float          = math.log( f ).toFloat
   @inline def log2( f: Float ) : Float         = (math.log( f ) / LOG2).toFloat
   @inline def log10( f: Float ) : Float        = math.log10( f ).toFloat
   @inline def sin( f: Float ) : Float          = math.sin( f ).toFloat
   @inline def cos( f: Float ) : Float          = math.cos( f ).toFloat
   @inline def tan( f: Float ) : Float          = math.tan( f ).toFloat
   @inline def asin( f: Float ) : Float         = math.asin( f ).toFloat
   @inline def acos( f: Float ) : Float         = math.acos( f ).toFloat
   @inline def atan( f: Float ) : Float         = math.atan( f ).toFloat
   @inline def sinh( f: Float ) : Float         = math.sinh( f ).toFloat
   @inline def cosh( f: Float ) : Float         = math.cosh( f ).toFloat
   @inline def tanh( f: Float ) : Float         = math.tanh( f ).toFloat

   @inline def distort( f: Float ) : Float      = f / (1 + math.abs( f ))
   @inline def softclip( f: Float ) : Float = {
      val absx = math.abs( f )
      if( absx <= 0.5f ) f else (absx - 0.25f) / f
   }
   @inline def ramp( f: Float ) : Float         = if( f <= 0 ) 0 else if( f >= 1 ) 1 else f
   @inline def scurve( f: Float ) : Float       = if( f <= 0 ) 0 else if( f > 1 ) 1 else f * f * (3 - 2 * f)

   // -------- binary ops --------

   @inline def +( a: Float, b: Float ) : Float     = a + b
   @inline def -( a: Float, b: Float ) : Float     = a - b
   @inline def *( a: Float, b: Float ) : Float     = a * b
   @inline def div( a: Float, b: Float ) : Int     = (a / b).toInt
   @inline def /( a: Float, b: Float ) : Float     = a / b
   @inline def %( a: Float, b: Float ) : Float     = a % b
   @inline def ===( a: Float, b: Float ) : Int     = if( a == b ) 1 else 0
   @inline def !==( a: Float, b: Float ) : Int     = if( a != b ) 1 else 0
   @inline def <( a: Float, b: Float ) : Boolean   = a < b
   @inline def >( a: Float, b: Float ) : Boolean   = a > b
   @inline def <=( a: Float, b: Float ) : Boolean  = a <= b
   @inline def >=( a: Float, b: Float ) : Boolean  = a >= b
   @inline def min( a: Float, b: Float ) : Float   = math.min( a, b )
   @inline def max( a: Float, b: Float ) : Float   = math.max( a, b )
   @inline def &( a: Float, b: Float ) : Float	   = a.toInt & b.toInt
   @inline def |( a: Float, b: Float ) : Float	   = a.toInt | b.toInt
   @inline def ^( a: Float, b: Float ) : Float	   = a.toInt ^ b.toInt

   // lcm
   // gcd

  @inline def round(a: Float, b: Float) =
    if (b == 0) a else (math.floor(a / b + 0.5f) * b).toFloat

  @inline def roundup(a: Float, b: Float) =
    if (b == 0) a else (math.ceil(a / b) * b).toFloat

  @inline def trunc(a: Float, b: Float) =
    if (b == 0) a else (math.floor(a / b) * b).toFloat

  @inline def atan2(a: Float, b: Float): Float = math.atan2(a, b).toFloat
  @inline def hypot(a: Float, b: Float): Float = math.hypot(a, b).toFloat

  @inline def hypotx(a: Float, b: Float) = {
    val minab = math.min(math.abs(a), math.abs(b))
    (a + b - (math.sqrt(2) - 1) * minab).toFloat
  }

  @inline def pow(a: Float, b: Float): Float = math.pow(a, b).toFloat

  // <<
// >>
// UnsgnRghtShft
// fill

  @inline def ring1(a: Float, b: Float) =
    a * b + a

  @inline def ring2(a: Float, b: Float) =
    a * b + a + b

  @inline def ring3(a: Float, b: Float) =
    a * a * b

  @inline def ring4(a: Float, b: Float) = {
    val ab = a * b
    a * ab - b * ab
  }

  @inline def difsqr(a: Float, b: Float) =
    a * a - b * b

  @inline def sumsqr(a: Float, b: Float) =
    a * a + b * b

  @inline def sqrsum(a: Float, b: Float) = {
    val z = a + b
    z * z
  }

  @inline def sqrdif(a: Float, b: Float) = {
    val z = a - b
    z * z
  }

  @inline def absdif(a: Float, b: Float) = math.abs(a - b)

  @inline def thresh(a: Float, b: Float) =
    if (a < b) 0 else a

  @inline def amclip(a: Float, b: Float) =
    a * 0.5f * (b + math.abs(a))

  @inline def scaleneg(a: Float, b: Float) =
    (math.abs(a) - a) * (0.5f * b + 0.5f) + a

  @inline def clip2(a: Float, b: Float) =
    math.max(math.min(a, b), -b)

  @inline def excess(a: Float, b: Float) =
    a - math.max(math.min(a, b), -b)

  @inline def fold2(a: Float, b: Float) = fold(a, -b, b)

  @inline def wrap2(a: Float, b: Float) = wrap(a, -b, b)

  // -------- n-ary ops --------

  @inline def fold(in: Float, lo: Float, hi: Float): Float = {
    val x = in - lo
    // avoid the divide if possible
    if (in >= hi) {
      val f = hi + hi - in
      if (f >= lo) return f
    } else if (in < lo) {
      val f = lo + lo - in
      if (f < hi) return f
    } else return in

    if (hi == lo) return lo
    // ok do the divide
    val range = hi - lo
    val range2 = range + range
    val c = x - range2 * math.floor(x / range2).toFloat
    lo + (if (c >= range) range2 - c else c)
  }

  @inline def wrap(in: Float, lo: Float, hi: Float): Float = {
    // avoid the divide if possible
    if (in >= hi) {
      val range = hi - lo
      val in2 = in - range
      if (in2 < hi) in2
      else if (hi == lo) lo
      else {
        in2 - range * math.floor((in2 - lo) / range).toFloat
      }
    } else if (in < lo) {
      val range = hi - lo
      val in2 = in + range
      if (in2 >= lo) in2
      else if (hi == lo) lo
      else {
        in2 - range * math.floor((in2 - lo) / range).toFloat
      }
    } else in
  }

  @inline def linlin(in: Float, srcLo: Float, srcHi: Float, dstLo: Float, dstHi: Float): Float =
    (in - srcLo) / (srcHi - srcLo) * (dstHi - dstLo) + dstLo

  @inline def linexp(in: Float, srcLo: Float, srcHi: Float, dstLo: Float, dstHi: Float): Float =
    math.pow(dstHi / dstLo, (in - srcLo) / (srcHi - srcLo)).toFloat * dstLo
}