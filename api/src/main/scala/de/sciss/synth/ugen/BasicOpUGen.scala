/*
 *  BasicOpUGen.scala
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

package de.sciss
package synth
package ugen

import collection.immutable.{IndexedSeq => IIdxSeq}
import de.sciss.synth.{Constant => c}
import annotation.switch

//object MulAdd {
//   def ar( in: GE, mul: GE, add: GE ) : MulAdd = apply( audio, in, mul, add )
//   def kr( in: GE, mul: GE, add: GE ) : MulAdd = apply( control, in, mul, add )
//   def ir( in: GE, mul: GE, add: GE ) : MulAdd = apply( scalar, in, mul, add )
//}

final case class MulAdd(/* rate: MaybeRate, */ in: GE, mul: GE, add: GE)
  extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(IIdxSeq(in.expand, mul.expand, add.expand))

  def rate: MaybeRate = in.rate // XXX correct?

  protected def makeUGen(args: IIdxSeq[UGenIn]): UGenInLike = {
    import BinaryOpUGen.{Plus, Minus, Times}
    import UnaryOpUGen.Neg

    val in0 = args(0)
    val mul0 = args(1)
    val add0 = args(2)
    (mul0, add0) match {
      case (c(0), _)      => add0
      case (c(1), c(0))   => in0
      case (c(1), _)      => Plus .make1(in0, add0)
      case (c(-1), c(0))  => Neg  .make1(in0)
      case (_, c(0))      => Times.make1(in0, mul0)
      case (c(-1), _)     => Minus.make1(add0, in0)
      case _              =>
        if (in0.rate == `audio` ||
           (in0.rate == `control` && (mul0.rate == `control` || mul0.rate == `scalar`) &&
            (add0.rate == `control` || add0.rate == `scalar`))) {
          new UGen.SingleOut(name, in0.rate, args)
        } else {
          Plus.make1(Times.make1(in0, mul0), add0)
        }
    }
  }

  override def toString = s"$in.madd($mul, $add)"
}

/**
 *    Unary operations are generally constructed by calling one of the methods of <code>GE</code>.
 *
 *    @see  GE
 *    @see  BinaryOpUGen
 */
object UnaryOpUGen {
  unop =>

  import synth.{FloatFun => rf}

  object Op {
    def apply(id: Int): Op = (id: @switch) match {
      case Neg        .id => Neg
      case Not        .id => Not
      case Abs        .id => Abs
      case Ceil       .id => Ceil
      case Floor      .id => Floor
      case Frac       .id => Frac
      case Signum     .id => Signum
      case Squared    .id => Squared
      case Cubed      .id => Cubed
      case Sqrt       .id => Sqrt
      case Exp        .id => Exp
      case Reciprocal .id => Reciprocal
      case Midicps    .id => Midicps
      case Cpsmidi    .id => Cpsmidi
      case Midiratio  .id => Midiratio
      case Ratiomidi  .id => Ratiomidi
      case Dbamp      .id => Dbamp
      case Ampdb      .id => Ampdb
      case Octcps     .id => Octcps
      case Cpsoct     .id => Cpsoct
      case Log        .id => Log
      case Log2       .id => Log2
      case Log10      .id => Log10
      case Sin        .id => Sin
      case Cos        .id => Cos
      case Tan        .id => Tan
      case Asin       .id => Asin
      case Acos       .id => Acos
      case Atan       .id => Atan
      case Sinh       .id => Sinh
      case Cosh       .id => Cosh
      case Tanh       .id => Tanh
      case Distort    .id => Distort
      case Softclip   .id => Softclip
      case Ramp       .id => Ramp
      case Scurve     .id => Scurve
    }
  }

  sealed trait Op extends Product {
    op =>

    def id: Int

    final def make(a: GE): GE = a match {
      case c(f) => c(make1(f))
      case _    => new UnaryOpUGen(/* a.rate, */ op, a)
    }

    final protected[synth] def make1(a: UGenIn): UGenIn = a match {
      case c(f) => c(make1(f))
      case _    => new UGenImpl(/* a.rate, */ op, a) // unop.apply( a.rate, this, a )
    }

    protected def make1(a: Float): Float

    override def productPrefix = s"UnaryOp.$plainName"

    def name: String = plainName.capitalize

    private def plainName: String = {
      val cn = getClass.getName
      val sz = cn.length
      val i  = cn.indexOf('$') + 1
      cn.substring(i, if (cn.charAt(sz - 1) == '$') sz - 1 else sz)
    }
  }

  case object Neg extends Op {
    final val id = 0
    protected def make1(a: Float) = -a
  }

  case object Not extends Op {
    final val id = 1
    protected def make1(a: Float) = rf.not(a)
  }

  // case object IsNil       extends Op(  2 )
  // case object NotNil      extends Op(  3 )
  // case object BitNot      extends Op(  4 )
  case object Abs extends Op {
    final val id = 5
    protected def make1(a: Float) = rf.abs(a)
  }

  // case object ToFloat     extends Op(  6 )
  // case object ToInt       extends Op(  7 )
  case object Ceil extends Op {
    final val id = 8
    protected def make1(a: Float) = rf.ceil(a)
  }

  case object Floor extends Op {
    final val id = 9
    protected def make1(a: Float) = rf.floor(a)
  }

  case object Frac extends Op {
    final val id = 10
    protected def make1(a: Float) = rf.frac(a)
  }

  case object Signum extends Op {
    final val id = 11
    protected def make1(a: Float) = math.signum(a)
  }

  case object Squared extends Op {
    final val id = 12
    protected def make1(a: Float) = rf.squared(a)
  }

  case object Cubed extends Op {
    final val id = 13
    protected def make1(a: Float) = rf.cubed(a)
  }

  case object Sqrt extends Op {
    final val id = 14
    protected def make1(a: Float) = rf.sqrt(a)
  }

  case object Exp extends Op {
    final val id = 15
    protected def make1(a: Float) = rf.exp(a)
  }

  case object Reciprocal extends Op {
    final val id = 16
    protected def make1(a: Float) = rf.reciprocal(a)
  }

  case object Midicps extends Op {
    final val id = 17
    protected def make1(a: Float) = rf.midicps(a)
  }

  case object Cpsmidi extends Op {
    final val id = 18
    protected def make1(a: Float) = rf.cpsmidi(a)
  }

  case object Midiratio extends Op {
    final val id = 19
    protected def make1(a: Float) = rf.midiratio(a)
  }

  case object Ratiomidi extends Op {
    final val id = 20
    protected def make1(a: Float) = rf.ratiomidi(a)
  }

  case object Dbamp extends Op {
    final val id = 21
    protected def make1(a: Float) = rf.dbamp(a)
  }

  case object Ampdb extends Op {
    final val id = 22
    protected def make1(a: Float) = rf.ampdb(a)
  }

  case object Octcps extends Op {
    final val id = 23
    protected def make1(a: Float) = rf.octcps(a)
  }

  case object Cpsoct extends Op {
    final val id = 24
    protected def make1(a: Float) = rf.cpsoct(a)
  }

  case object Log extends Op {
    final val id = 25
    protected def make1(a: Float) = rf.log(a)
  }

  case object Log2 extends Op {
    final val id = 26
    protected def make1(a: Float) = rf.log2(a)
  }

  case object Log10 extends Op {
    final val id = 27
    protected def make1(a: Float) = rf.log10(a)
  }

  case object Sin extends Op {
    final val id = 28
    protected def make1(a: Float) = rf.sin(a)
  }

  case object Cos extends Op {
    final val id = 29
    protected def make1(a: Float) = rf.cos(a)
  }

  case object Tan extends Op {
    final val id = 30
    protected def make1(a: Float) = rf.tan(a)
  }

  case object Asin extends Op {
    final val id = 31
    protected def make1(a: Float) = rf.asin(a)
  }

  case object Acos extends Op {
    final val id = 32
    protected def make1(a: Float) = rf.acos(a)
  }

  case object Atan extends Op {
    final val id = 33
    protected def make1(a: Float) = rf.atan(a)
  }

  case object Sinh extends Op {
    final val id = 34
    protected def make1(a: Float) = rf.sinh(a)
  }

  case object Cosh extends Op {
    final val id = 35
    protected def make1(a: Float) = rf.cosh(a)
  }

  case object Tanh extends Op {
    final val id = 36
    protected def make1(a: Float) = rf.tanh(a)
  }

  // class Rand              extends Op( 37 )
  // class Rand2             extends Op( 38 )
  // class Linrand           extends Op( 39 )
  // class Bilinrand         extends Op( 40 )
  // class Sum3rand          extends Op( 41 )
  case object Distort extends Op {
    final val id = 42
    protected def make1(a: Float) = rf.distort(a)
  }

  case object Softclip extends Op {
    final val id = 43
    protected def make1(a: Float) = rf.softclip(a)
  }

  // class Coin              extends Op( 44 )
  // case object DigitValue  extends Op( 45 )
  // case object Silence     extends Op( 46 )
  // case object Thru        extends Op( 47 )
  // case object RectWindow  extends Op( 48 )
  // case object HanWindow   extends Op( 49 )
  // case object WelWindow   extends Op( 50 )
  // case object TriWindow   extends Op( 51 )
  case object Ramp extends Op {
    final val id = 52
    protected def make1(a: Float) = rf.ramp(a)
  }

  case object Scurve extends Op {
    final val id = 53
    protected def make1(a: Float) = rf.scurve(a)
  }

  // Note: only deterministic selectors are implemented!!
  private final class UGenImpl /**/ (/* rate: Rate, */ selector: Op, a: UGenIn /*[ R ]*/)
    extends UGen.SingleOut("UnaryOpUGen", a.rate, IIdxSeq(a)) {

    override def specialIndex = selector.id
  }
}

final case class UnaryOpUGen(/* rate: MaybeRate, */ selector: UnaryOpUGen.Op, a: GE)
  extends UGenSource.SingleOut {

  def rate: MaybeRate = a.rate

  protected def makeUGens: UGenInLike = unwrap(IIdxSeq(a.expand))

  protected def makeUGen(args: IIdxSeq[UGenIn]): UGenInLike = {
    val IIdxSeq(a) = args
    selector.make1(a)
  }

  override def toString = s"$a.${selector.name}"
}

/**
 *    Binary operations are generally constructed by calling one of the methods of <code>GEOps</code>.
 *
 *    @see  GEOps
 *    @see  UnaryOpUGen
 */
object BinaryOpUGen {
  binop =>

  object Op {
    def apply(id: Int): Op = (id: @switch) match {
      case Plus     .id => Plus
      case Minus    .id => Minus
      case Times    .id => Times
      case Div      .id => Div
      case Mod      .id => Mod
      case Eq       .id => Eq
      case Neq      .id => Neq
      case Lt       .id => Lt
      case Gt       .id => Gt
      case Leq      .id => Leq
      case Geq      .id => Geq
      case Min      .id => Min
      case Max      .id => Max
      case BitAnd   .id => BitAnd
      case BitOr    .id => BitOr
      case BitXor   .id => BitXor
      case Round    .id => Round
      case Roundup  .id => Roundup
      case Trunc    .id => Trunc
      case Atan2    .id => Atan2
      case Hypot    .id => Hypot
      case Hypotx   .id => Hypotx
      case Pow      .id => Pow
      case Ring1    .id => Ring1
      case Ring2    .id => Ring2
      case Ring3    .id => Ring3
      case Ring4    .id => Ring4
      case Difsqr   .id => Difsqr
      case Sumsqr   .id => Sumsqr
      case Sqrsum   .id => Sqrsum
      case Sqrdif   .id => Sqrdif
      case Absdif   .id => Absdif
      case Thresh   .id => Thresh
      case Amclip   .id => Amclip
      case Scaleneg .id => Scaleneg
      case Clip2    .id => Clip2
      case Excess   .id => Excess
      case Fold2    .id => Fold2
      case Wrap2    .id => Wrap2
      case Firstarg .id => Firstarg
    }
  }

  import UnaryOpUGen.{Neg, Reciprocal}

  sealed trait Op extends Product {
    op =>

    def id: Int

    //      def make( rate: R, a: GE[ UGenIn[ R ]]) = UnaryOp[ R ]( rate, this, a )
    def make(a: GE, b: GE): GE = (a, b) match {
      case (c(af), c(bf)) => c(make1(af, bf))
      case _              => new Pure(op, a, b)
    }

    protected[synth] def make1(a: UGenIn, b: UGenIn): UGenIn = (a, b) match {
      case (c(af), c(bf)) => c(make1(af, bf))
      case _              => new UGenImpl(op, a, b)
    }

    protected def make1(a: Float, b: Float): Float

    override def productPrefix = s"BinaryOp.$plainName"

    def name: String = plainName.capitalize

    private def plainName: String = {
      val cn = getClass.getName
      val sz = cn.length
      val i  = cn.indexOf('$') + 1
      cn.substring(i, if (cn.charAt(sz - 1) == '$') sz - 1 else sz)
    }
  }

  import synth.{FloatFun => rf}

  case object Plus extends Op {
    final val id = 0
    override val name = "+"

    protected def make1(a: Float, b: Float) = rf.+(a, b)

    override protected[synth] def make1(a: UGenIn, b: UGenIn): UGenIn = (a, b) match {
      case (c(0), _)  => b
      case (_, c(0))  => a
      case _          => super.make1(a, b)
    }
  }

  case object Minus extends Op {
    final val id = 1
    override val name = "-"

    protected def make1(a: Float, b: Float) = rf.-(a, b)

    override protected[synth] def make1(a: UGenIn, b: UGenIn): UGenIn = (a, b) match {
      //         case (c(0), _)       => -b
      case (c(0), _) => Neg.make1(b)
      case (_, c(0)) => a
      case _ => super.make1(a, b)
    }
  }

  case object Times extends Op {
    final val id = 2
    override val name = "*"

    protected def make1(a: Float, b: Float) = rf.*(a, b)

    override protected[synth] def make1(a: UGenIn, b: UGenIn): UGenIn = (a, b) match {
      case (c(0), _)  => a
      case (_, c(0))  => b
      case (c(1), _)  => b
      case (_, c(1))  => a
      case (c(-1), _) => Neg.make1(b) // -b
      case (_, c(-1)) => Neg.make1(a) // -a
      case _          => super.make1(a, b)
    }
  }

  // case object IDiv           extends Op(  3 )
  case object Div extends Op {
    final val id = 4
    override val name = "/"

    protected def make1(a: Float, b: Float) = rf./(a, b)

    override protected[synth] def make1(a: UGenIn, b: UGenIn): UGenIn = (a, b) match {
      case (_, c(1))  => a
      case (_, c(-1)) => Neg.make1(a) // -a
      case _          =>
        if (b.rate == scalar) {
          //               a * b.reciprocal
          Times.make1(a, Reciprocal.make1(b))
        } else {
          super.make1(a, b)
        }
    }
  }

  case object Mod extends Op {
    final val id = 5
    override val name = "%"

    protected def make1(a: Float, b: Float) = rf.%(a, b)
  }

  case object Eq extends Op {
    final val id = 6
    override val name = "sig_=="

    protected def make1(a: Float, b: Float) = if( a == b ) 1 else 0
  }

  case object Neq extends Op {
    final val id = 7
    override val name = "sig_!="

    protected def make1(a: Float, b: Float) = if( a != b ) 1 else 0
  }

  case object Lt extends Op {
    final val id = 8
    override val name = "<"

    protected def make1(a: Float, b: Float) = if (a < b) 1f else 0f // NOT rf.< !
  }

  case object Gt extends Op {
    final val id = 9
    override val name = ">"

    protected def make1(a: Float, b: Float) = if (a > b) 1f else 0f // NOT rf.> !
  }

  case object Leq extends Op {
    final val id = 10
    override val name = "<="

    protected def make1(a: Float, b: Float) = if (a <= b) 1f else 0f // NOT rf.<= !
  }

  case object Geq extends Op {
    final val id = 11
    override val name = ">="

    protected def make1(a: Float, b: Float) = if (a >= b) 1f else 0f // NOT rf.>= !
  }

  case object Min extends Op {
    final val id = 12
    protected def make1(a: Float, b: Float) = rf.min(a, b)
  }

  case object Max extends Op {
    final val id = 13
    protected def make1(a: Float, b: Float) = rf.max(a, b)
  }

  case object BitAnd extends Op {
    final val id = 14
    override val name = "&"

    protected def make1(a: Float, b: Float) = rf.&(a, b)
  }

  case object BitOr extends Op {
    final val id = 15
    override val name = "|"

    protected def make1(a: Float, b: Float) = rf.|(a, b)
  }

  case object BitXor extends Op {
    final val id = 16
    override val name = "^"

    protected def make1(a: Float, b: Float) = rf.^(a, b)
  }

  // case object Lcm            extends Op( 17 )
  // case object Gcd            extends Op( 18 )
  case object Round extends Op {
    final val id = 19
    protected def make1(a: Float, b: Float) = rf.round(a, b)
  }

  case object Roundup extends Op {
    final val id = 20
    protected def make1(a: Float, b: Float) = rf.roundup(a, b)
  }

  case object Trunc extends Op {
    final val id = 21
    protected def make1(a: Float, b: Float) = rf.trunc(a, b)
  }

  case object Atan2 extends Op {
    final val id = 22
    protected def make1(a: Float, b: Float) = rf.atan2(a, b)
  }

  case object Hypot extends Op {
    final val id = 23
    protected def make1(a: Float, b: Float) = rf.hypot(a, b)
  }

  case object Hypotx extends Op {
    final val id = 24
    protected def make1(a: Float, b: Float) = rf.hypotx(a, b)
  }

  case object Pow extends Op {
    final val id = 25
    protected def make1(a: Float, b: Float) = rf.pow(a, b)
  }

  // case object <<             extends Op( 26 )
  // case object >>             extends Op( 27 )
  // case object UnsgnRghtShft  extends Op( 28 )
  // case object Fill           extends Op( 29 )
  case object Ring1 extends Op {
    final val id = 30
    protected def make1(a: Float, b: Float) = rf.ring1(a, b)
  }

  case object Ring2 extends Op {
    final val id = 31
    protected def make1(a: Float, b: Float) = rf.ring2(a, b)
  }

  case object Ring3 extends Op {
    final val id = 32
    protected def make1(a: Float, b: Float) = rf.ring3(a, b)
  }

  case object Ring4 extends Op {
    final val id = 33
    protected def make1(a: Float, b: Float) = rf.ring4(a, b)
  }

  case object Difsqr extends Op {
    final val id = 34
    protected def make1(a: Float, b: Float) = rf.difsqr(a, b)
  }

  case object Sumsqr extends Op {
    final val id = 35
    protected def make1(a: Float, b: Float) = rf.sumsqr(a, b)
  }

  case object Sqrsum extends Op {
    final val id = 36
    protected def make1(a: Float, b: Float) = rf.sqrsum(a, b)
  }

  case object Sqrdif extends Op {
    final val id = 37
    protected def make1(a: Float, b: Float) = rf.sqrdif(a, b)
  }

  case object Absdif extends Op {
    final val id = 38
    protected def make1(a: Float, b: Float) = rf.absdif(a, b)
  }

  case object Thresh extends Op {
    final val id = 39
    protected def make1(a: Float, b: Float) = rf.thresh(a, b)
  }

  case object Amclip extends Op {
    final val id = 40
    protected def make1(a: Float, b: Float) = rf.amclip(a, b)
  }

  case object Scaleneg extends Op {
    final val id = 41
    protected def make1(a: Float, b: Float) = rf.scaleneg(a, b)
  }

  case object Clip2 extends Op {
    final val id = 42
    protected def make1(a: Float, b: Float) = rf.clip2(a, b)
  }

  case object Excess extends Op {
    final val id = 43
    protected def make1(a: Float, b: Float) = rf.excess(a, b)
  }

  case object Fold2 extends Op {
    final val id = 44
    protected def make1(a: Float, b: Float) = rf.fold2(a, b)
  }

  case object Wrap2 extends Op {
    final val id = 45
    protected def make1(a: Float, b: Float) = rf.wrap2(a, b)
  }

  case object Firstarg extends Op {
    final val id = 46
    override def make(/* rate: T, */ a: GE, b: GE): GE = new Impure(this, a, b)

    protected def make1(a: Float, b: Float) = a
  }

  // case object Rrand          extends Op( 47 )
  // case object ExpRRand       extends Op( 48 )

  private final case class Pure(/* rate: MaybeRate, */ selector: Op, a: GE, b: GE)
    extends BinaryOpUGen

  private final case class Impure(/* rate: MaybeRate, */ selector: Op, a: GE, b: GE)
    extends BinaryOpUGen with HasSideEffect {
  }

  // Note: only deterministic selectors are implemented!!
  private final class UGenImpl(selector: Op, a: UGenIn, b: UGenIn)
    extends UGen.SingleOut("BinaryOpUGen", a.rate max b.rate, IIdxSeq(a, b)) {

    override def specialIndex = selector.id
  }
}

// XXX this could become private once the op's make method return type is changed to GE
sealed trait BinaryOpUGen extends UGenSource.SingleOut {
  def selector: BinaryOpUGen.Op
  def a: GE
  def b: GE

  override final def productPrefix = "BinaryOpUGen"

  //  final def productElement(n: Int): Any = (n: @switch) match {
  //    case 0 => selector
  //    case 1 => a
  //    case 2 => b
  //  }

  final def rate: MaybeRate = MaybeRate.max_?( a.rate, b.rate )

  protected final def makeUGens: UGenInLike = unwrap(IIdxSeq(a.expand, b.expand))

  protected final def makeUGen(args: IIdxSeq[UGenIn]): UGenInLike = {
    val a0 = args(0)
    val a1 = args(1)
    selector.make1(a0, a1)
  }

  override def toString = if ((selector.id <= 11) || ((selector.id >= 14) && (selector.id <= 16))) {
    s"($a ${selector.name} $b)"
  } else {
    s"$a.${selector.name}($b)"
  }
}