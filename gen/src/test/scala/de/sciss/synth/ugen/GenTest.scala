package de.sciss.synth
package ugen

object GenTest extends App {
  val cusp  = UGenSpec.standardUGens("CuspN")
  val gen   = new ClassGenerator
  gen.perform1(cusp)
}