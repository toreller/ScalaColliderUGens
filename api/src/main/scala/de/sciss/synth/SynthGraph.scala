/*
 *  SynthGraph.scala
 *  (ScalaColliderUGens)
 *
 *  Copyright (c) 2008-2014 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth

import collection.immutable.{IndexedSeq => Vec}
import ugen.ControlProxyLike

object SynthGraph {
  trait Builder {
    def addLazy(g: Lazy): Unit

    def addControlProxy(proxy: ControlProxyLike): Unit
  }

  // java.lang.ThreadLocal is around 30% faster than
  // using a synchronized map, plus we don't need
  // to look after its cleaning
  private val builders = new ThreadLocal[Builder] {
    override protected def initialValue = BuilderDummy
  }

  def builder: Builder = builders.get

  def apply(thunk: => Any): SynthGraph = {
    val b   = new BuilderImpl
    val old = builders.get()
    builders.set(b)
    try {
      thunk
      b.build
    } finally {
      builders.set(old) // BuilderDummy
    }
  }

  /** A boolean setting (defaults to `false`) which can help track down
    * bugs with graph elements being added outside a `SynthGraph` context.
    * When this setting is `true`, a warning message is printed to
    * `Console.err` with the graph element added and the stack trace,
    * indicating calls such as `SinOsc.ar` outside a
    * thread local `SynthGraph` builder.
    */
  var warnOutsideContext = false

  private object BuilderDummy extends Builder {
    def build: SynthGraph = sys.error("Out of context")

    private def warn(obj: => String): Unit =
      if (warnOutsideContext) {
        Console.err.println("Warning - adding SynthGraph element outside context: " + obj)
        val e   = new Throwable()
        e.fillInStackTrace()
        val t   = e.getStackTrace
        val n   = t.length
        var i   = 0
        var go  = true
        while (i < n && go) {
          val c = t(i).getClassName
          if ((c.startsWith("de.sciss.synth.") &&
            (c.charAt(15).isUpper || c.startsWith("de.sciss.synth.ugen."))) ||
            c.startsWith("scala.collection.")) {
            i += 1
          } else {
            go = false
          }
        }
        while (i < n) {
          Console.err.println("  at " + t(i))
          i += 1
        }
      }

    def addLazy(g: Lazy): Unit = warn(g.toString)

    def addControlProxy(proxy: ControlProxyLike): Unit = warn(proxy.toString)
  }

  private final class BuilderImpl extends Builder {
    private val lazies          = Vec.newBuilder[Lazy]
    private val controlProxies  = Set.newBuilder[ControlProxyLike]

    override def toString = "SynthGraph.Builder@" + hashCode.toHexString

    def build = SynthGraph(lazies.result(), controlProxies.result())

    def addLazy(g: Lazy): Unit = lazies += g

    def addControlProxy(proxy: ControlProxyLike): Unit = controlProxies += proxy
  }
}

final case class SynthGraph(sources: Vec[Lazy], controlProxies: Set[ControlProxyLike]) {
  def isEmpty : Boolean  = sources.isEmpty && controlProxies.isEmpty
  def nonEmpty: Boolean  = !isEmpty
  def expand(implicit factory: UGenGraph.BuilderFactory): UGenGraph = factory.build(this)
}