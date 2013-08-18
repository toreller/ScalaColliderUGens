/*
 *  UGenSpec.scala
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

import collection.immutable
import immutable.{IndexedSeq => Vec}
import collection.breakOut
import impl.{UGenSpecParser => ParserImpl}

object UGenSpec {
  lazy val standardUGens: Map[String, UGenSpec] = {
    val is = ugen.Control.getClass.getResourceAsStream("standard-ugens.xml")
    try {
      val source = xml.Source.fromInputStream(is)
      parseAll(source, docs = true)
    } finally {
      is.close()
    }
  }

  def parseAll(source: xml.InputSource, docs: Boolean = false): Map[String, UGenSpec] =
    ParserImpl.parseAll(source, docs = docs)

  def parse(node: xml.Node, docs: Boolean = false): UGenSpec = ParserImpl.parse(node, docs = docs)

  // ---- UGen attributes ----

  object Attribute {
    sealed trait ImpliesSideEffect extends Attribute
    sealed trait ImpliesIndividual extends Attribute

    case object ReadsBus      extends Attribute.ImpliesIndividual   // cf Specified.txt
    case object ReadsBuffer   extends Attribute.ImpliesIndividual   // cf Specified.txt
    case object ReadsFFT      extends Attribute.ImpliesIndividual   // cf Specified.txt
    case object UsesRandSeed  extends Attribute.ImpliesIndividual
    case object IsIndividual  extends Attribute.ImpliesIndividual

    case object WritesBus     extends Attribute.ImpliesSideEffect with Attribute.ImpliesIndividual
    case object WritesBuffer  extends Attribute.ImpliesSideEffect with Attribute.ImpliesIndividual
    case object WritesFFT     extends Attribute.ImpliesSideEffect with Attribute.ImpliesIndividual
    case object HasSideEffect extends Attribute.ImpliesSideEffect

    case object HasDoneFlag   extends Attribute
  }
  sealed trait Attribute

  // ---- UGen input arguments ----

  final case class Argument(name: String, tpe: ArgumentType,
                            defaults: Map[MaybeRate, ArgumentValue],
                            rates: Map[MaybeRate, RateConstraint]) {
    override def toString = {
      val base = s"${name}: ${tpe}"
      val s1 = defaults.get(UndefinedRate) match {
        case Some(v) => s"${base} = ${v}"
        case _ => base
      }
      val md = defaults - UndefinedRate
      val s2 = if (md.isEmpty) s1 else {
        s"${s1} ${md.mkString("[", ", ", "]")}"
      }
      val s3 = rates.get(UndefinedRate) match {
        case Some(v) => s"${s2} @${v}"
        case _ => s2
      }
      val mr = rates - UndefinedRate
      if (mr.isEmpty) s3 else {
        s"${s3} -> ${mr.mkString("[", ", ", "]")}"
      }
    }
  }

  object ArgumentType {
    case object Int extends ArgumentType
//    sealed trait GELike extends ArgumentType { def shape: SignalShape }
//    case object GEWithDoneFlag extends GELike { def shape: SignalShape = SignalShape.Generic }
//    final case class GE(shape: SignalShape) extends GELike
    final case class GE(shape: SignalShape, scalar: Boolean = false) extends ArgumentType {
      override def toString = {
        val base = shape.toString
        if (scalar) base + " (@init)" else base
      }
    }
  }
  sealed trait ArgumentType

  object RateConstraint {
    case object SameAsUGen extends RateConstraint {
      override def toString = "same-rate-as-ugen"
    }
    final case class Fixed(rate: Rate) extends RateConstraint {
      override def toString = "fixed-rate=" + rate
    }
  }
  sealed trait RateConstraint

  object SignalShape {
    case object Generic     extends SignalShape // aka Float
    case object Int         extends SignalShape
    case object String      extends SignalShape
    case object Bus         extends SignalShape
    case object Buffer      extends SignalShape
    case object FFT         extends SignalShape
    case object Trigger     extends SignalShape // with values `low` and `high`
    case object Switch      extends SignalShape // with values `false` and `true`
    case object Gate        extends SignalShape // with values `closed` and `open`
    case object Mul         extends SignalShape
    case object DoneAction  extends SignalShape
    case object DoneFlag    extends SignalShape
  }
  sealed trait SignalShape /* extends ArgumentType */ // .GELike { def shape = this }

  object ArgumentValue {
    final case class Int(value: scala.Int) extends ArgumentValue {
//      def toGE = /* if (value == scala.Int.MaxValue) Constant(scala.Float.PositiveInfinity) else */ Constant(value)
      override def toString = value.toString
    }
    final case class Float(value: scala.Float) extends ArgumentValue {
//      def toGE = Constant(value)
      override def toString = {
        val s = value.toString
        if (s.contains('.')) s else s + ".0"
      }
    }
    final case class Boolean(value: scala.Boolean) extends ArgumentValue {
//      def toGE = Constant(if (value) 1f else 0f)
      override def toString = value.toString
    }
    final case class String(value: java.lang.String) extends ArgumentValue {
//      def toGE = ???
      override def toString = s""""${value}""""
    }
    case object Inf extends ArgumentValue {
//      def toGE = Constant(scala.Float.PositiveInfinity)
      override def toString = productPrefix.toLowerCase
    }
    final case class DoneAction(peer: synth.DoneAction) extends ArgumentValue {
//      def toGE = Constant(peer.id)
      override def toString = peer.toString
    }
    case object Nyquist extends ArgumentValue {
//      def toGE = ???
      override def toString = productPrefix.toLowerCase
    }
  }
  sealed trait ArgumentValue {
//    def toGE: GE
  }

  final case class Input(arg: String, variadic: Boolean) {
    override def toString = if (variadic) arg + "..." else arg
  }

  // ---- Supported rates ----

  object RateMethod {
    case object Default extends RateMethod
    case class Custom(name: String) extends RateMethod
    case class Alias (name: String) extends RateMethod
    case object None extends RateMethod
  }
  sealed trait RateMethod

  object Rates {
    final case class Implied(rate: Rate, method: RateMethod) extends Rates {
      def set = immutable.Set(rate)

      override def toString = {
        val base = "implied: " + rate
        method match {
          case RateMethod.Default => base
          case _                  => s"${base} (method = ${method})"
        }
      }
    }
    final case class Set(set: immutable.Set[Rate]) extends Rates {
      override def toString = set.mkString("[", ", ", "]")
      def method = RateMethod.Default
    }
  }
  sealed trait Rates {
    def method: RateMethod
    def set: Set[Rate]
  }

  // ---- Outputs ----

//  object Outputs {
//    final case class Argument(name: String) extends Outputs
//    // type = fft
//
//  }
//  sealed trait Outputs

  final case class Output(name: Option[String], shape: SignalShape, variadic: Option[String]) {
    override def toString = {
      val base  = name getOrElse "<out>"
      val s1    = if (shape != SignalShape.Generic) s"${base}: ${shape}" else base
      variadic match {
        case Some(id) => s"${s1}... (${id})"
        case _ => s1
      }
    }
  }

  final case class Doc(body: List[String], args: Map[String, List[String]], outputs: Map[String, List[String]],
                       links: List[String], warnPos: Boolean)
}
final case class UGenSpec(name: String, attr: Set[UGenSpec.Attribute], rates: UGenSpec.Rates,
                          args:    Vec[UGenSpec.Argument],
                          inputs:  Vec[UGenSpec.Input   ],
                          outputs: Vec[UGenSpec.Output  ],
                          doc:     Option [UGenSpec.Doc]) {
  lazy val argMap:   Map[String, UGenSpec.Argument] = args.map  (a => a.name -> a)(breakOut)
  lazy val inputMap: Map[String, UGenSpec.Input   ] = inputs.map(i => i.arg  -> i)(breakOut)

  override def toString = s"${productPrefix}(${name}, attr = ${attr.mkString("[", ", ", "]")}, rates = ${rates}, " +
                          s"args = ${args.mkString("[", ", ", "]")}, inputs = ${inputs.mkString("[", ", ", "]")}, " +
                          s"outputs = ${outputs.mkString("[", ", ", "]")})"
}