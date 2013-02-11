package de.sciss.synth
package impl

import collection.immutable.{IndexedSeq => IIdxSeq}
import collection.breakOut
import annotation.switch

private[synth] object UGenSpecParser {
  private def DEFAULT_VERIFY = true

  def parseAll(source: xml.InputSource, docs: Boolean = false): Map[String, UGenSpec] = {
    val root      = xml.XML.load(source)
    val ugenNodes = root \\ "ugen"
    ugenNodes.map( n => {
      val spec = parse(n, docs = docs, verify = DEFAULT_VERIFY)
      spec.name -> spec
    })(breakOut)
  }

  private val nodeAttrKeys = Set(
    "name", // required
    "readsbus",  "readsbuf",  "readsfft", "random", "indiv",
    "writesbus", "writesbuf", "writesfft", "sideeffect",
    "doneflag"
  )

  private val nodeChildKeys = Set(
    "rate", "no-outputs", "output", "arg", "doc"
  )

  private val argAttrKeys = Set(
    "name", // required
    "type", "init", "default", "rate", "ugenin", "pos", "variadic"
  )

  private val outputAttrKeys = Set(
    "name", "type", "variadic"
  )

  private val rateAttrKeys = Set(
    "name"  // required
  )

  private val impliedRateAttrKeys = rateAttrKeys ++ Set(
    "method", "methodalias", "implied"
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

          case IsGate(b) =>
            // ---- check correctness ----
            aTypeExp.foreach {
              case GE(Sig.Gate,_) =>
              case other => incompatibleTypeDefault(other, default)
            }
            val tpe = GE(Sig.Gate)
            if (isInit) errorScalarType(tpe)
            tpe -> ArgumentValue.Boolean(value = true)

          case IsDoneAction(a) =>
            // ---- check correctness ----
            aTypeExp.foreach {
              case GE(Sig.DoneAction,_) =>
              case other => incompatibleTypeDefault(other, default)
            }
            GE(Sig.DoneAction, scalar = isInit) -> ArgumentValue.DoneAction(a)

          case "nyquist" =>
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

  private val DEFAULT_OUTPUTS =
    Vector(UGenSpec.Output(name = None, shape = UGenSpec.SignalShape.Generic, variadic = None))

  private def trimDoc(docText: String): List[String] = {
    val trim0 = docText.lines.map(_.trim).toIndexedSeq
    val trim1 = trim0.dropWhile(_.isEmpty)
    val idx   = trim1.lastIndexWhere(_.isEmpty) // why there's no dropRightWhile?
    val trim  = if (idx >= 0) trim1.dropRight(trim1.size - idx) else trim1
    val b     = List.newBuilder[String]
    val sb    = new StringBuilder

    def flush() {
      if (!sb.isEmpty) {
        b += sb.toString
        sb.clear()
      }
    }

    trim.foreach { line =>
      if (line.isEmpty) flush() else {
        sb.append(' ')
        sb.append(line)
      }
    }
    b.result()
  }

  private def mkDoc(node: xml.Node, argDocs: Map[String, List[String]],
                    outputDocs: Map[String, List[String]]): Option[UGenSpec.Doc] = {
    val docOpt = (node \ "doc").headOption
    docOpt.flatMap { dNode =>
      val dSees: List[String] = (dNode \ "see").map(_.text)(breakOut)
      val dAttr     = dNode.attributes.asAttrMap
      val dWarnPos  = dAttr.boolean("warnpos")
      val dText     = (dNode \ "text").text
      val hasAny    = !dText.isEmpty || dSees.nonEmpty || dWarnPos || argDocs.nonEmpty || outputDocs.nonEmpty
      if (hasAny) {
        val dTextT  = trimDoc(dText)
        val doc     = UGenSpec.Doc(body = dTextT, args = argDocs, outputs = Map.empty, links = dSees, warnPos = dWarnPos)
        Some(doc)
      } else {
        None
      }
    }
  }

  def parse(node: xml.Node, docs: Boolean = false, verify: Boolean = false): UGenSpec = {
    if (node.label != "ugen") throw new IllegalArgumentException(s"Not a 'ugen' node: ${node}")

    import UGenSpec.{SignalShape => Sig, _}

    val attrs = node.attributes.asAttrMap
    val uName = attrs.string ("name")

    if (verify) {
      val unknown = attrs.keySet -- nodeAttrKeys
      require(unknown.isEmpty, s"Unsupported ugen attributes in ${uName}: ${unknown.mkString(",")}")

      val unknownN = (node.child.collect({ case e: xml.Elem => e.label })(breakOut): Set[String]) -- nodeChildKeys
      require(unknownN.isEmpty, s"Unsupported ugen child nodes in ${uName}: ${unknownN.mkString(",")}")
    }

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

    val indSideEffect = writesBus || writesBuffer || writesFFT
    val indIndiv      = readsBus  || readsBuffer  || readsFFT || indSideEffect || random

    val aNodes = (node \ "arg")

    // ---- rates ----

    def getRate(map: Map[String, String]): Rate = {
      map.get("name").getOrElse(sys.error(s"Missing rate name in ugen ${uName}")) match {
        case "audio"    => audio
        case "control"  => control
        case "scalar"   => scalar
        case "demand"   => demand
        case other      => sys.error(s"Invalid rate in ugen ${uName}: ${other}")
      }
    }

    val rNodes = (node \ "rate")
    if (rNodes.isEmpty) sys.error(s"No rates specified for ${uName}")
    val impliedRate = (rNodes.size == 1) && rNodes.head.attributes.asAttrMap.boolean("implied")
    val (rates, rateArgMap) = if (impliedRate) {
      val rNode   = rNodes.head
      val rAttr   = rNode.attributes.asAttrMap
      val r       = getRate(rAttr)

      if (verify) {
        val unknown = rAttr -- impliedRateAttrKeys
        if (unknown.nonEmpty)
          sys.error(s"Unsupported ugen rate attributes, in ugen ${uName}, rate ${r}: ${unknown.mkString(",")}")
      }

      val rMethod = (rAttr.get("method"), rAttr.get("methodalias")) match {
        case (None, None)     => RateMethod.Default
        case (Some(""), None) =>
          if (aNodes.nonEmpty || indIndiv)
            sys.error(s"Cannot produce singleton for ugen ${uName} that has arguments or is individual")
          RateMethod.None
        case (Some(m), None)  => RateMethod.Custom(m)
        case (None, Some(m))  => RateMethod.Alias(m)
        case other            =>
          sys.error(s"Cannot use both method and methodalias attributes, in ugen ${uName}, rate ${r}")
      }
      Rates.Implied(r, rMethod) -> Map(r -> (rNode \ "arg"))

    } else {
      val map: Map[Rate, Seq[xml.Node]] = rNodes.map( rNode => {
        val rAttr   = rNode.attributes.asAttrMap
        val r       = getRate(rAttr)

        if (verify) {
          val unknown = rAttr -- rateAttrKeys
          if (unknown.nonEmpty)
            sys.error(s"Unsupported ugen rate attributes, in ugen ${uName}, rate ${r}: ${unknown.mkString(",")}")
        }
        r -> (rNode \ "arg")
      })(breakOut)

      Rates.Set(map.keySet) -> map
    }

    // ---- arguments ----

    var args          = Vector.empty: IIdxSeq[Argument]
    var argsOrd       = Map.empty[Int, Argument]
    var argMap        = Map.empty[String, Argument]
    var inputs        = Vector.empty[Input]
    var inputMap      = Map.empty[String, Input]
    var argDocs       = Map.empty[String, List[String]]

    aNodes.foreach { aNode =>
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

      var aDefaults   = Map.empty[MaybeRate, ArgumentValue]
      aDefaultOpt.foreach { df => aDefaults += UndefinedRate -> df }
      var aRates      = Map.empty[MaybeRate, RateConstraint]

      def checkRate(a: Map[String, String], r: MaybeRate) {
        aAttrs.get("rate").foreach {
          case "ugen"   => aRates += r -> RateConstraint.SameAsUGen
          case "audio"  => aRates += r -> RateConstraint.Fixed(audio)
          case other =>
            sys.error(s"Invalid rate constraint, in ugen ${uName}, argument ${aName}, rate ${other}")
        }
      }

      checkRate(aAttrs, UndefinedRate)

      def errorMixPos() {
        sys.error(s"Cannot mix positional and non-positional arguments, in ugen ${uName}, argument ${aName}")
      }

      val arg = Argument(aName, aType, aDefaults, aRates)
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

      if (docs) {
        val aDoc  = (aNode \ "doc").text
        if (!aDoc.isEmpty) {
          val aDocT = trimDoc(aDoc)
          if (aDocT.nonEmpty) argDocs += aName -> aDocT
        }
      }
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

    // ---- outputs ----
    var outputs     = Vector.empty[Output]
    var outputDocs  = Map.empty[String, List[String]]

    val oNodes  = (node \ "output")
    val zeroOut = (node \ "no-outputs").nonEmpty
    if (zeroOut && oNodes.nonEmpty) sys.error(s"Cannot mix no-outputs and output nodes, in ugen ${uName}")

    if (!zeroOut && oNodes.isEmpty) {
      outputs = DEFAULT_OUTPUTS
    } else oNodes.foreach { oNode =>
      val oAttrs = oNode.attributes.asAttrMap
      if (verify) {
        val unknown = oAttrs.keySet -- outputAttrKeys
        require(unknown.isEmpty, s"Unsupported ugen outputs attributes: ${unknown.mkString(",")}")
      }

      val oName   = oAttrs.get("name")
      val oShape0 = oAttrs.get("type").map {
        case "ge"       => Sig.Generic
        case "gint"     => Sig.Int
        case "bus"      => Sig.Bus
        case "buf"      => Sig.Buffer
        case "fft"      => Sig.FFT
        case "trig"     => Sig.Trigger
        case "switch"   => Sig.Switch
        case "gate"     => Sig.Gate
        case other      =>
          sys.error(s"Unsupported type, in ugen ${uName}, output ${oName}: ${other}")
      }
      val oShape  = oShape0 getOrElse Sig.Generic

      val variadic  = oAttrs.get("variadic")
      variadic.foreach { id =>
        if (!argMap.contains(id))
          sys.error(s"Variadic output refers to unknown argument, in ugen ${uName}, output ${oName}, ref ${id}")
      }

      if (variadic.isDefined) oShape match {
        case Sig.Generic  =>
        case Sig.Int      =>
        case Sig.Trigger  =>
        case Sig.Switch   =>
        case Sig.Gate     =>
        case other        =>
          sys.error(s"Variadic output has unsupported type, in ugen ${uName}, output ${oName}: ${other}")
      }

      val out = Output(oName, shape = oShape, variadic = variadic)
      outputs :+= out

      if (docs) {
        val oDoc  = (oNode \ "doc").text
        if (!oDoc.isEmpty) {
          oName match {
            case Some(n) =>
              val oDocT = trimDoc(oDoc)
              if (oDocT .nonEmpty) outputDocs += n -> oDocT
            case _ =>
              sys.error(s"Documented outputs must be named, in ${uName}")
          }
        }
      }
    }

    // ---- wrap up ----

    val uDoc = if (docs) mkDoc(node, argDocs = argDocs, outputDocs = outputDocs) else None

    UGenSpec(name = uName, attr = uAttr, rates = rates, args = args, inputs = inputs, outputs = outputs, doc = uDoc)
  }
}