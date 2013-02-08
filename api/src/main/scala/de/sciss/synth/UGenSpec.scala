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

import collection.immutable.{IndexedSeq => IIdxSeq}

object UGenSpec {
  // ---- UGen attributes ----

  object Attribute {
    sealed trait ImpliesSideEffect extends Attribute
    sealed trait ImpliesIndividual extends Attribute
  }
  sealed trait Attribute
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

  // ---- UGen input arguments ----

  final case class Argument(name: String, default: Option[ArgumentValue])

  sealed trait ArgumentValue
  final case class IntValue  (value: Int) extends ArgumentValue
  final case class FloatValue(value: Float) extends ArgumentValue
  case object Nyquist extends ArgumentValue

  // --- Supported rates ----

  sealed trait Rates
  final case class ImpliedRate(rate: Rate, method: Option[String]) extends Rates
  final case class RateSet(rates: Set[Rate]) extends Rates
}
final case class UGenSpec(name: String, attr: Set[UGenSpec.Attribute], rates: UGenSpec.Rates,
                          args: IIdxSeq[UGenSpec.Argument])