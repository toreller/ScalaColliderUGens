package de.sciss.synth
package impl

import collection.immutable.{IndexedSeq => IIdxSeq}
import collection.breakOut

private[synth] object UGenSpecParser {
  private def DEFAULT_VERIFY = true

  def parseAll(source: xml.InputSource): Map[String, UGenSpec] = {
    val root      = xml.XML.load(source)
    val ugenNodes = root \\ "ugen"
    ugenNodes.map( n => {
      val spec = parse(n, verify = DEFAULT_VERIFY)
      spec.name -> spec
    })(breakOut)
  }

  private val nodeAttrKeys = Set(
    "name",
    "readsbus",  "readsbuf",  "readsfft", "random", "indiv",
    "writesbus", "writesbuf", "writesfft", "sideeffect",
    "doneflag"
  )

  private val argAttrKeys = Set(
    "name",
    "type", "init", "default", "rate", "ugenin", "pos", "variadic"
  )

  private implicit final class RichAttrMap(val map: Map[String, String]) extends AnyVal {
    def string (key: String): String = map.getOrElse(key, sys.error(s"Missing required attribute '${key}'"))
    def boolean(key: String, default: Boolean = false): Boolean = map.get(key).map(_.toBoolean).getOrElse(default)

//    private def getIntAttr(n: xml.Node, name: String, default: Int) =
//      (n \ ("@" + name)).headOption.map(_.text.toInt).getOrElse(default)
  }

  private object IsFloat {
    private val re = """\-?\d+.\d+""".r.pattern
    def unapply(s: String): Option[Float] = {
      if (re.matcher(s).matches()) Some(s.toFloat) else None
    }
  }

  private object IsInt {
    private val re = """\-?\d+""".r.pattern
    def unapply(s: String): Option[Int] = {
      if (re.matcher(s).matches()) Some(s.toInt) else None
    }
  }

  private object IsBoolean {
    def unapply(s: String): Option[Boolean]= s match {
      case "true"   => Some(true)
      case "false"  => Some(false)
      case _        => None
    }
  }

  private object IsDoneAction {
    def unapply(s: String): Option[DoneAction] = PartialFunction.condOpt(s) {
      case doNothing.name         => doNothing
      case pauseSelf.name         => pauseSelf
      case freeSelf.name          => freeSelf
      case freeSelfPred.name      => freeSelfPred
      case freeSelfSucc.name      => freeSelfSucc
      case freeSelfPredAll.name   => freeSelfPredAll
      case freeSelfSuccAll.name   => freeSelfSuccAll
      case freeSelfToHead.name    => freeSelfToHead
      case freeSelfToTail.name    => freeSelfToTail
      case freeSelfPausePred.name => freeSelfPausePred
      case freeSelfPauseSucc.name => freeSelfPauseSucc
      case freeSelfPredDeep.name  => freeSelfPredDeep
      case freeSelfSuccDeep.name  => freeSelfSuccDeep
      case freeAllInGroup.name    => freeAllInGroup
      case freeGroup.name         => freeGroup
    }
  }

  private object IsGate {
    def unapply(s: String): Option[Boolean] = s match {
      case "open"   => Some(true)
      case "closed" => Some(false)
      case _        => None
    }
  }

  private object IsTrigger {
    def unapply(s: String): Option[Boolean] = s match {
      case "high"   => Some(true)
      case "low"    => Some(false)
      case _        => None
    }
  }

  // poor man's type inference
  private def inferArgType(uName: String, aName: String,
                           aAttrs: Map[String, String]): (UGenSpec.ArgumentType, Option[UGenSpec.ArgumentValue]) = {
    import UGenSpec._

    def incompatibleTypeDefault(tpe: Any, df: Any) {
      sys.error(s"Mismatch between argument type and default value for ugen ${uName}, argument ${aName}, type is ${tpe}, default is ${df}")
    }

    val aTypeExp  = aAttrs.get("type").map {
      case "ge"       => SignalShape.Generic
      case "gint"     => SignalShape.Int
      case "gstring"  => SignalShape.String
      case "bus"      => SignalShape.Bus
      case "buf"      => SignalShape.Buffer
      case "fft"      => SignalShape.FFT
      case "trig"     => SignalShape.Trigger
      case "switch"   => SignalShape.Switch
      case "gate"     => SignalShape.Gate
      case "mul"      => SignalShape.Mul
      case "action"   => SignalShape.DoneAction
      case "doneflag" => SignalShape.DoneFlag
      case "int"      => ArgumentType.Int
      case other      => sys.error(s"Unsupported type for ugen ${uName}, argument ${aName}: ${other}")
    }

    aAttrs.get("default") match {
      case Some(default) =>
        val (tpe, df) = default match {
          case IsFloat(f)       =>
            // ---- check correctness ----
            aTypeExp.foreach {
              case SignalShape.Generic =>
              case SignalShape.Mul =>
              case other => incompatibleTypeDefault(other, default)
            }
            aTypeExp.getOrElse(SignalShape.Generic) -> ArgumentValue.Float(f)

          case IsInt(i)         =>
            // ---- check correctness ----
            aTypeExp.foreach {
              case SignalShape.Int =>
              case ArgumentType.Int =>
              case other => incompatibleTypeDefault(other, default)
            }
            aTypeExp.getOrElse(SignalShape.Generic) -> ArgumentValue.Int(i)

          case IsTrigger(b)     =>
            // ---- check correctness ----
            aTypeExp.foreach {
              case SignalShape.Trigger =>
              case other => incompatibleTypeDefault(other, default)
            }
            SignalShape.Trigger -> ArgumentValue.Boolean(value = true)

          case IsBoolean(b)     =>
            // ---- check correctness ----
            aTypeExp.foreach {
              case SignalShape.Switch =>
              case other => incompatibleTypeDefault(other, default)
            }
            SignalShape.Switch -> ArgumentValue.Boolean(value = true)

          case IsGate(b)        =>
            // ---- check correctness ----
            aTypeExp.foreach {
              case SignalShape.Gate =>
              case other => incompatibleTypeDefault(other, default)
            }
            SignalShape.Gate -> ArgumentValue.Boolean(value = true)

          case IsDoneAction(a)  =>
            // ---- check correctness ----
            aTypeExp.foreach {
              case SignalShape.DoneAction =>
              case other => incompatibleTypeDefault(other, default)
            }
            SignalShape.DoneAction -> ArgumentValue.DoneAction(a)

          case "nyquist"        =>
            // ---- check correctness ----
            aTypeExp.foreach {
              case SignalShape.Generic =>
              case SignalShape.Int =>
              case other => incompatibleTypeDefault(other, default)
            }
            aTypeExp.getOrElse(SignalShape.Generic) -> ArgumentValue.Nyquist

          case "inf" =>
            // ---- check correctness ----
            aTypeExp.foreach {
              case SignalShape.Generic =>
              case SignalShape.Int =>
              case other => incompatibleTypeDefault(other, default)
            }
            aTypeExp.getOrElse(SignalShape.Generic) -> ArgumentValue.Inf

          case _ =>
            if (aTypeExp != Some(SignalShape.String)) {
              sys.error(s"Unsupported default value for ugen ${uName}, argument ${aName}: ${default}")
            }
            SignalShape.String -> ArgumentValue.String(default)
        }
        tpe -> Some(df)

      case _ => // no default
        aTypeExp.getOrElse(SignalShape.Generic) -> None
    }
  }

  def parse(node: xml.Node, verify: Boolean = false): UGenSpec = {
    if (node.label != "ugen") throw new IllegalArgumentException(s"Not a 'ugen' node: ${node}")

    import UGenSpec._

    val attrs = node.attributes.asAttrMap
    if (verify) {
      val unknown = attrs.keySet -- nodeAttrKeys
      require(unknown.isEmpty, s"Unsupported ugen attributes: ${unknown.mkString(",")}")
    }

    val uName         = attrs.string ("name")

    // ---- attributes ----

    val readsBus      = attrs.boolean("readsbus")
    val readsBuffer   = attrs.boolean("readsbuf")
    val readsFFT      = attrs.boolean("readsfft")
    val random        = attrs.boolean("random")

    val writesBus     = attrs.boolean("writesbus")
    val writesBuffer  = attrs.boolean("writesbuf")
    val writesFFT     = attrs.boolean("writesfft")

    val sideEffect    = attrs.boolean("sideeffect")
    val doneFlag      = attrs.boolean("doneflag")
    val indiv         = attrs.boolean("indiv")

    var attrB         = Set.newBuilder[Attribute]
    import Attribute._
    if (readsBus)     attrB += ReadsBus
    if (readsBuffer)  attrB += ReadsBuffer
    if (readsFFT)     attrB += ReadsFFT
    if (random)       attrB += UsesRandSeed
    if (indiv)        attrB += IsIndividual

    if (writesBus)    attrB += WritesBus
    if (writesBuffer) attrB += WritesBuffer
    if (writesFFT)    attrB += WritesFFT
    if (sideEffect)   attrB += HasSideEffect

    if (doneFlag)     attrB += HasDoneFlag

    val indSideEffect = writesBus || writesBuffer || writesFFT
    val indIndiv      = readsBus  || readsBuffer  || readsFFT || indSideEffect || random

    // ---- arguments ----

    val argsB         = IIdxSeq.newBuilder[Argument]
    val argMapB       = Map.newBuilder[String, Argument]
    val inputsB       = IIdxSeq.newBuilder[Input]
    val inputMap      = Map.newBuilder[String, Input]

    (node \ "arg").foreach { aNode =>
      val aAttrs      = aNode.attributes.asAttrMap
      if (verify) {
        val unknown = aAttrs.keySet -- argAttrKeys
        require(unknown.isEmpty, s"Unsupported ugen argument attributes: ${unknown.mkString(",")}")
      }
      val aName       = aAttrs.string("name")

      // have to deal with:
      // "type" (ok), "init", "default" (ok), "rate", "ugenin", "pos", "variadic"
      val (aType, aDefaultOpt) = inferArgType(uName = uName, aName = aName, aAttrs)  // handles "type" and "default"

      val aDefaultsB  = Map.newBuilder[MaybeRate, ArgumentValue]
      aDefaultOpt.foreach { df => aDefaultsB += UndefinedRate -> df }
      val aRatesB     = Map.newBuilder[MaybeRate, RateConstraint]

      val arg  = Argument(aName, aType, aDefaultsB.result(), aRatesB.result())
      argsB   += arg
      argMapB += aName -> arg
    }

    UGenSpec(name = uName, attr = attrB.result(), rates = Rates.Set(Set.empty),
      args = argsB.result(), inputs = inputsB.result(), outputs = IIdxSeq.empty)
  }

//  private def booleanAttr(n: xml.Node, name: String, default: Boolean = false) =
//    (n \ ("@" + name)).headOption.map(_.text.toBoolean).getOrElse(default)
//
//  private def getIntAttr(n: xml.Node, name: String, default: Int) =
//    (n \ ("@" + name)).headOption.map(_.text.toInt).getOrElse(default)

}