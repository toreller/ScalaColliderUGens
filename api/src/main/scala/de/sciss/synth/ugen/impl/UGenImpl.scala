package de.sciss.synth.ugen.impl

import de.sciss.synth.{UGenGraph, Rate, UGen, UGenIn}

import scala.collection.immutable.{IndexedSeq => Vec}

final class ZeroOutImpl(val name: String, val rate: Rate, val inputs: Vec[UGenIn], val isIndividual: Boolean)
  extends UGen.ZeroOut

final class SingleOutImpl(val name: String, val rate: Rate, val inputs: Vec[UGenIn], val isIndividual: Boolean,
                          val hasSideEffect: Boolean)
  extends UGen.SingleOut

final class MultiOutImpl(val name: String, val rate: Rate, val outputRates: Vec[Rate], val inputs: Vec[UGenIn],
                         val isIndividual: Boolean, val hasSideEffect: Boolean)
  extends UGen.MultiOut

// side-effect: receiving messages from clients!
// and more importantly: control ugens created from proxies are not wired, so they would
// be eliminated if side-effect was false!!!
object ControlImpl {
  def apply(name: String, rate: Rate, numChannels: Int, specialIndex: Int): ControlImpl = {
    val res = new ControlImpl(name, rate, numChannels = numChannels, specialIndex = specialIndex)
    UGenGraph.builder.addUGen(res)
    res
  }
}
final class ControlImpl private(val name: String, val rate: Rate, numChannels: Int, override val specialIndex: Int)
  extends UGen.MultiOut {

  def outputRates: Vec[Rate] = Vector.fill(numChannels)(rate)

  def isIndividual : Boolean = false
  def hasSideEffect: Boolean = true

  def inputs: Vec[UGenIn] = Vector.empty
}