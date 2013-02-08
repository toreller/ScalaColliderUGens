package de.sciss.synth.ugen

import collection.immutable.{IndexedSeq => IIdxSeq}

object Spec {
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

  final case class Argument(name: String)
}
final case class Spec(name: String, attr: Set[Spec.Attribute], args: IIdxSeq[Spec.Argument])