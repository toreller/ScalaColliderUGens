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
    def intOption(key: String): Option[Int] = map.get(key).map(_.toInt)
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
    import UGenSpec.{SignalShape => Sig, _}
    import ArgumentType.GE

    def incompatibleTypeDefault(tpe: Any, df: Any) {
      sys.error(s"Mismatch between argument type and default value for ugen ${uName}, argument ${aName}, type is ${tpe}, default is ${df}")
    }

    val aTypeExp0 = aAttrs.get("type").map {
      case "ge"       => GE(Sig.Generic)
      case "gint"     => GE(Sig.Int)
      case "gstring"  => GE(Sig.String)
      case "bus"      => GE(Sig.Bus)
      case "buf"      => GE(Sig.Buffer)
      case "fft"      => GE(Sig.FFT)
      case "trig"     => GE(Sig.Trigger)
      case "switch"   => GE(Sig.Switch)
      case "gate"     => GE(Sig.Gate)
      case "mul"      => GE(Sig.Mul)
      case "action"   => GE(Sig.DoneAction)
      case "doneflag" => GE(Sig.DoneFlag)
      case "int"      => ArgumentType.Int
      case other      => sys.error(s"Unsupported type for ugen ${uName}, argument ${aName}: ${other}")
    }

    def errorScalarType(tpe: ArgumentType) {
      sys.error(s"Cannot use scalar declaration with arguments of type ${tpe}, in ${uName}, argument ${aName}")
    }

    val isInit    = aAttrs.boolean("init")
    val aTypeExp  = if (!isInit) aTypeExp0 else aTypeExp0 match {
      case Some(tpe @ GE(sig, _)) =>
        // disallow for Trigger, Gate, Mul, DoneFlag
        sig match {
          case Sig.Generic    =>
          case Sig.Int        =>
          case Sig.String     =>
          case Sig.Bus        =>
          case Sig.Buffer     =>
          case Sig.FFT        =>
          case Sig.Switch     =>
          case Sig.DoneAction =>
          case _ =>
            errorScalarType(tpe)
        }
        Some(GE(sig, scalar = true))

      case Some(tpe @ ArgumentType.Int) =>
        if (!aAttrs.boolean("ugenin")) {
          errorScalarType(tpe)
        }
        aTypeExp0

      case _ => aTypeExp0
    }

    def orGeneric = aTypeExp.getOrElse(GE(Sig.Generic, scalar = isInit))

    aAttrs.get("default") match {
      case Some(default) =>
        val (tpe, df) = default match {
          case IsFloat(f)       =>
            // ---- check correctness ----
            aTypeExp.foreach {
              case GE(Sig.Generic,_) =>
              case GE(Sig.Mul,_) =>
              case other => incompatibleTypeDefault(other, default)
            }
            orGeneric -> ArgumentValue.Float(f)

          case IsInt(i) =>
            // ---- check correctness ----
            aTypeExp.foreach {
              case GE(Sig.Int,_) =>
              case ArgumentType.Int =>
              case other => incompatibleTypeDefault(other, default)
            }
            orGeneric -> ArgumentValue.Int(i)

          case IsTrigger(b)     =>
            // ---- check correctness ----
            aTypeExp.foreach {
              case GE(Sig.Trigger,_) =>
              case other => incompatibleTypeDefault(other, default)
            }
            val tpe = GE(Sig.Trigger)
            if (isInit) errorScalarType(tpe)
            tpe -> ArgumentValue.Boolean(value = true)

          case IsBoolean(b)     =>
            // ---- check correctness ----
            aTypeExp.foreach {
              case GE(Sig.Switch,_) =>
              case other => incompatibleTypeDefault(other, default)
            }
            GE(Sig.Switch, scalar = isInit) -> ArgumentValue.Boolean(value = true)

          case IsGate(b)        =>
            // ---- check correctness ----
            aTypeExp.foreach {
              case GE(Sig.Gate,_) =>
              case other => incompatibleTypeDefault(other, default)
            }
            val tpe = GE(Sig.Gate)
            if (isInit) errorScalarType(tpe)
            tpe -> ArgumentValue.Boolean(value = true)

          case IsDoneAction(a)  =>
            // ---- check correctness ----
            aTypeExp.foreach {
              case GE(Sig.DoneAction,_) =>
              case other => incompatibleTypeDefault(other, default)
            }
            GE(Sig.DoneAction, scalar = isInit) -> ArgumentValue.DoneAction(a)

          case "nyquist"        =>
            // ---- check correctness ----
            aTypeExp.foreach {
              case GE(Sig.Generic,_) =>
              case GE(Sig.Int,_) =>
              case other => incompatibleTypeDefault(other, default)
            }
            orGeneric -> ArgumentValue.Nyquist

          case "inf" =>
            // ---- check correctness ----
            aTypeExp.foreach {
              case GE(Sig.Generic,_) =>
              case GE(Sig.Int,_) =>
              case other => incompatibleTypeDefault(other, default)
            }
            orGeneric -> ArgumentValue.Inf

          case _ =>
            aTypeExp match {
              case Some(GE(Sig.String,_)) =>
              case _ =>
                sys.error(s"Unsupported default value for ugen ${uName}, argument ${aName}: ${default}")
            }
            GE(Sig.String, scalar = isInit) -> ArgumentValue.String(default)
        }
        tpe -> Some(df)

      case _ => // no default
        orGeneric -> None
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

    var uAttr         = Set.empty[Attribute]
    import Attribute._
    if (readsBus)     uAttr += ReadsBus
    if (readsBuffer)  uAttr += ReadsBuffer
    if (readsFFT)     uAttr += ReadsFFT
    if (random)       uAttr += UsesRandSeed
    if (indiv)        uAttr += IsIndividual

    if (writesBus)    uAttr += WritesBus
    if (writesBuffer) uAttr += WritesBuffer
    if (writesFFT)    uAttr += WritesFFT
    if (sideEffect)   uAttr += HasSideEffect

    if (doneFlag)     uAttr += HasDoneFlag

//    val indSideEffect = writesBus || writesBuffer || writesFFT
//    val indIndiv      = readsBus  || readsBuffer  || readsFFT || indSideEffect || random

    // ---- arguments ----

    var args          = IIdxSeq.empty[Argument]
    var argsOrd       = Map.empty[Int, Argument]
    var argMap        = Map.empty[String, Argument]
    var inputs        = IIdxSeq.empty[Input]
    var inputMap      = Map.empty[String, Input]

    (node \ "arg").foreach { aNode =>
      val aAttrs      = aNode.attributes.asAttrMap
      if (verify) {
        val unknown = aAttrs.keySet -- argAttrKeys
        require(unknown.isEmpty, s"Unsupported ugen argument attributes: ${unknown.mkString(",")}")
      }
      val aName       = aAttrs.string("name")

      // have to deal with:
      // "type" (ok), "init" (ok), "default" (ok), "rate", "ugenin" (ok), "pos" (ok), "variadic" (ok)
      val (aType, aDefaultOpt) = inferArgType(uName = uName, aName = aName, aAttrs)   // handles "type", "default", "init"

      val isInput = aType match {
        case ArgumentType.GE(_, _) => true
        case ArgumentType.Int => aAttrs.boolean("ugenin")                             // handles "ugenin"
      }

      if (isInput) {
        val variadic = aAttrs.boolean("variadic")                                     // handles "variadic"
        val in = Input(aName, variadic = variadic)
        inputMap += aName -> in
        inputs :+= in
      }

      val aDefaultsB  = Map.newBuilder[MaybeRate, ArgumentValue]
      aDefaultOpt.foreach { df => aDefaultsB += UndefinedRate -> df }
      val aRatesB     = Map.newBuilder[MaybeRate, RateConstraint]

      def errorMixPos() {
        sys.error(s"Cannot mix positional and non-positional arguments, in ugen ${uName}, argument ${aName}")
      }

      val arg = Argument(aName, aType, aDefaultsB.result(), aRatesB.result())
      aAttrs.intOption("pos") match {                                                 // handles "pos"
        case Some(pos) =>
          if (args.nonEmpty) errorMixPos()
          if (argsOrd.contains(pos)) {
            sys.error(s"Duplicate argument input position, in ugen ${uName}, argument ${aName}")
          }
          argsOrd += pos -> arg

        case _ =>
          if (argsOrd.nonEmpty) errorMixPos()
          args :+= arg
      }
      argMap += aName -> arg
    }

    if (argsOrd.nonEmpty) {
      val sq = argsOrd.keys.toIndexedSeq.sorted
      if (sq != (0 until sq.size)) sys.error(s"Non contiguous argument positions, in ugen ${uName}: ${sq.mkString(",")}")
      args = sq.map(argsOrd)
    }

    if (verify) {
      val hasV = inputs.filter(_.variadic)
      if (hasV.nonEmpty) {
        if (hasV.size > 1) sys.error(s"Can only have one variadic argument, in ugen ${uName}: ${hasV.map(_.arg).mkString(",")}")
        if (hasV.head != inputs.last) sys.error(s"Variadic input must come last, in ugen ${uName}, argument ${hasV.head.arg}")
      }
    }

    UGenSpec(name = uName, attr = uAttr, rates = Rates.Set(Set.empty),
      args = args, inputs = inputs, outputs = IIdxSeq.empty)
  }

//  private def booleanAttr(n: xml.Node, name: String, default: Boolean = false) =
//    (n \ ("@" + name)).headOption.map(_.text.toBoolean).getOrElse(default)
//
//  private def getIntAttr(n: xml.Node, name: String, default: Int) =
//    (n \ ("@" + name)).headOption.map(_.text.toInt).getOrElse(default)

}