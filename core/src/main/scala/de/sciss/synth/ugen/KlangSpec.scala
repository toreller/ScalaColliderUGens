package de.sciss.synth
package ugen

import collection.immutable.{IndexedSeq => IIdxSeq}

object KlangSpec {
  def fill    (n: Int)(thunk:      => (GE, GE, GE)): Seq =
    new Seq(IIdxSeq.fill    [(GE, GE, GE)](n)(thunk).map((tup) => KlangSpec(tup._1, tup._2, tup._3)))

  def tabulate(n: Int)(func: (Int) => (GE, GE, GE)): Seq =
    new Seq(IIdxSeq.tabulate[(GE, GE, GE)](n)(func ).map((tup) => KlangSpec(tup._1, tup._2, tup._3)))

  final case class Seq(elems: IIdxSeq[KlangSpec]) extends GE {
    override def productPrefix  = "KlangSpec.Seq"
    def numOutputs              = elems.size * 3
    def rate                    = MaybeRate.reduce(elems.map(_.rate): _*)
    override def toString       = productPrefix + elems.mkString("(", ",", ")")

    def expand: UGenInLike      = UGenInGroup(elems.flatMap(_.expand.outputs))
  }
}

final case class KlangSpec(freq: GE, amp: GE = 1, decay: GE = 0) extends GE {

  def numOutputs          = 3
  def rate                = MaybeRate.reduce(freq.rate, amp.rate, decay.rate)

  def expand: UGenInLike  = UGenInGroup(IIdxSeq(freq.expand, amp.expand, decay.expand))
}
