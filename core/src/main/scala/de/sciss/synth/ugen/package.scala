package de.sciss.synth

import collection.immutable.{IndexedSeq => IIdxSeq}
import collection.breakOut

package object ugen {
  import language.implicitConversions

  type IIdxSeq[A] = collection.immutable.IndexedSeq[A]

//   private[ugen] def maxInt( is: Int* ) : Int = is.reduceLeft( math.max( _, _ ))
  private[ugen] def stringArg(s: String): IIdxSeq[UGenIn] = {
    val bs = s.getBytes
    Constant(bs.length) +: (bs.map(Constant(_))(breakOut): IIdxSeq[UGenIn])
  }

  //  private[ugen] implicit def mkConst(f: Float): Constant = new Constant(f)

  /* private[ugen] */ final val inf = Float.PositiveInfinity
//  private[ugen] def nyquist: GE = BinaryOp.Times.make(SampleRate.ir, 0.5f)
  private[ugen] def nyquist: GE = Constant(44100) // XXX TODO BinaryOp.Times.make(SampleRate(), 0.5f)

  private[ugen] def replaceZeroesWithSilence(ins: IIdxSeq[UGenIn]): IIdxSeq[UGenIn] = {
    val numZeroes = ins.foldLeft(0)((sum, in) => in match {
      case Constant(0)  => sum + 1
      case _            => sum
    })
    if (numZeroes == 0) {
      ins
    } else {
      // WARNING: Silent has been removed now from scsynth !!!
      //         val silent = Silent.ar( numZeroes ).outputs.iterator
      //         val silent = new UGen.MultiOut( "Silent", audio, IIdxSeq.fill( numZeroes )( audio ), IIdxSeq.empty ).outputs.iterator
      val silent = new UGen.MultiOut("DC", audio, IIdxSeq(audio), IIdxSeq(Constant(0))).outputs.head
      ins.map {
        case Constant(0) => silent // .next()
        case x => x
      }
    }
  }
}