package de.sciss.synth
package ugen

object GenTest extends App {
  val cusp  = UGenSpec.standardUGens("Dseries")
  val gen   = new ClassGenerator
  gen.performSpec(cusp)
}