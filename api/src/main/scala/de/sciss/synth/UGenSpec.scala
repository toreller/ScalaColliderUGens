/*
 *  Spec.scala
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

import collection.immutable
import immutable.{IndexedSeq => IIdxSeq}
import collection.breakOut
import impl.{UGenSpecParser => ParserImpl}

object UGenSpec {
  lazy val standardUGens: Map[String, UGenSpec] = {
    val is = ugen.Control.getClass.getResourceAsStream("standard-ugens.xml")
    try {
      val source = xml.Source.fromInputStream(is)
      parseAll(source)
    } finally {
      is.close()
    }
  }

  def parseAll(source: xml.InputSource): Map[String, UGenSpec] = ParserImpl.parseAll(source)

  def parse(node: xml.Node): UGenSpec = ParserImpl.parse(node)

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
                            rates: Map[MaybeRate, RateConstraint])

  object ArgumentType {
    case object Int extends ArgumentType
    sealed trait GELike extends ArgumentType { def shape: SignalShape }
    case object GEWithDoneFlag extends GELike { def shape: SignalShape = SignalShape.Generic }
    final case class GE(shape: SignalShape) extends GELike
  }
  sealed trait ArgumentType

  object RateConstraint {
    case object SameAsUGen extends RateConstraint
    final case class Fixed(rate: Rate) extends RateConstraint
  }
  sealed trait RateConstraint

  object SignalShape {
    case object Generic     extends SignalShape // aka Float
    case object Int         extends SignalShape
    case object String      extends SignalShape
    case object Trigger     extends SignalShape // with values `low` and `high`
    case object Switch      extends SignalShape // with values `false` and `true`
    case object Gate        extends SignalShape // with values `closed` and `open`
    case object DoneAction  extends SignalShape
    case object Buffer      extends SignalShape
    case object FFT         extends SignalShape
  }
  sealed trait SignalShape

  object ArgumentValue {
    final case class Int  (value: scala.Int  ) extends ArgumentValue
    final case class Float(value: scala.Float) extends ArgumentValue
    case object Nyquist extends ArgumentValue
  }
  sealed trait ArgumentValue

  final case class Input(arg: String, variadic: Boolean)

  // ---- Supported rates ----

  object Rates {
    final case class Implied(rate: Rate, method: Option[String]) extends Rates
    final case class Set(rates: immutable.Set[Rate]) extends Rates
  }
  sealed trait Rates

  // ---- Outputs ----

//  object Outputs {
//    final case class Argument(name: String) extends Outputs
//    // type = fft
//
//  }
//  sealed trait Outputs

  final case class Output(name: Option[String], shape: SignalShape, variadic: Option[String])
}
final case class UGenSpec(name: String, attr: Set[UGenSpec.Attribute], rates: UGenSpec.Rates,
                          args:    IIdxSeq[UGenSpec.Argument],
                          inputs:  IIdxSeq[UGenSpec.Input   ],
                          outputs: IIdxSeq[UGenSpec.Output  ]) {
  lazy val argMap:   Map[String, UGenSpec.Argument] = args.map  (a => a.name -> a)(breakOut)
  lazy val inputMap: Map[String, UGenSpec.Input   ] = inputs.map(i => i.arg  -> i)(breakOut)
}