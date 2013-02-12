package de.sciss.synth
package ugen

import tools.nsc.symtab.Flags
import tools.refactoring
import refactoring.Refactoring
import refactoring.util.CompilerProvider
import collection.breakOut
import collection.immutable.{IndexedSeq => IIdxSeq}
import refactoring.transformation.TreeFactory
import refactoring.common.{CompilerAccess, Tracing}
import tools.nsc.io.AbstractFile
import java.io.File
import UGenSpec.{SignalShape => Sig, _}
import ArgumentType.GE
import annotation.tailrec

final class ClassGenerator
  extends Refactoring with Tracing with CompilerProvider with CompilerAccess with TreeFactory {

  import global._

  def typeName(name: String) = newTypeName(name)
  def ident   (name: String) = new Ident(newTermName(name))

  def compilationUnitOfFile(f: AbstractFile) = global.unitOfFile.get(f)

//  private def trimDoc(docText: String): List[String] = {
//
////    val trim0 = docText.lines.map(_.trim).toIndexedSeq
////    val trim1 = trim0.dropWhile(_.isEmpty)
////    val idx = trim1.lastIndexWhere(_.isEmpty) // why the fuck there's no dropRightWhile?
////    if (idx >= 0) trim1.dropRight(trim1.size - idx) else trim1
//  }

//  private def ensureEmptyTail(text: List[String]): List[String] = if (text.nonEmpty) text :+ "" else text

  private def linesWrap(para: String, width: Int): List[String] = {
    val sz = para.length
    if (sz <= width ) return para :: Nil

    val b     = List.newBuilder[String]
    val sb    = new StringBuilder

    def flush() {
      if (!sb.isEmpty) {
        b += sb.toString().trim
        sb.clear()
      }
    }

    @tailrec def loop(off: Int) {
      val j0  = para.indexOf(' ', off)
      val j   = if (j0 >= 0) j0 + 1 else sz
      val ln  = j - off
      if (ln > 0) {
        if (sb.length + ln > width) flush()
        sb.append(para.substring(off, j))
        loop(j)
      }
    }

    loop(0)
    flush()
    b.result()
  }

  private def linesFromParagraphs(paras: List[String], width: Int = 80): List[String] = {
    val b = List.newBuilder[String]
    @tailrec def loop(xs: List[String], first: Boolean) {
      xs match {
        case head :: tail =>
          val headL = linesWrap(head, width)
          if (!first) b += ""
          b ++= headL
          loop(tail, first = false)

        case Nil =>
      }
    }
    loop(paras, first = true)
    b.result()
  }

  private def wrapDoc(tree: Tree, indent: Int, docText: List[String] = Nil, sees: List[String] = Nil,
                      docWarnPos: Boolean = false, argDocs: List[(String, String)] = Nil): Tree = {
    val hasAny = docText != "" || sees.nonEmpty || docWarnPos || argDocs.nonEmpty
    if (!hasAny) return tree

    val docLines = linesFromParagraphs(docText)

    val app1 = if (!docWarnPos) docLines else {
      docLines ++ List("", "'''Warning''': The argument order is different from its sclang counterpart.")
    }

    val app2: List[String] = if (argDocs.isEmpty) app1 else {
      val argLines = argDocs.flatMap { case (aName, aDoc) =>
        val aDocL = linesWrap(aDoc, 40)
        aDocL.zipWithIndex.map { case (ln, idx) =>
          val pre = if (idx == 0) ("@param " + aName).substring(0, 21) else ""
          val tab = pre + (" " * (23 - pre.length))
          tab + ln
        }
      }
      app1 ++ argLines
    }

    val app3 = if (sees.isEmpty) app2 else {
      app2 ++ ("" :: sees.map(link => s"@see [[de.sciss.synth.${link}]]"))
    }

    DocDef(DocComment(app3.mkString("/**\n * ", "\n * ", "\n */\n"), NoPosition), tree)
  }

  //  private def joinParagraphs(paragraphs: List[String], wrap: Int = 40): String = {
//    val sb = new StringBuilder
//    @tailrec def loop(xs: List[String]) {
//      xs match {
//        case Nil =>
//        case para :: tail =>
//
//      }
//  }

  /*
   * Collects argument documentation in the form of a list of tuples argName -> joinedDoc. If there aren't
   * docs, returns `Nil`.
   */
  private def collectMethodDocs(spec: UGenSpec, args: List[Argument]): List[(String, String)] =
    for {
      a     <- args
      doc   <- spec.doc
      aDoc  <- doc.args.get(a.name)
    } yield
      a.name -> aDoc.mkString(" ")

  def perform( specs: Seq[UGenSpec], dir: File) {

  }

  private implicit final class RichArgumentValue(val peer: ArgumentValue) /* extends AnyVal */ {
    import ArgumentValue._

    def toTree: Tree = peer match {
      case Int(i)         => Literal(Constant(i))
      case Float(f)       => Literal(Constant(f))
      case Boolean(b)     => Literal(Constant(if (b) 1 else 0)) // currently no type class for GE | Switch
      case String(s)      => Apply(Ident("stringArg"), Literal(Constant(s)) :: Nil)
      case Inf            => Ident("inf")
      case DoneAction(a)  => Ident(a.name)
      case Nyquist        => Ident("nyquist")
    }
  }

  private implicit final class RichRateMap[A](val peer: Map[MaybeRate, A]) {
    def getWithDefault(r: MaybeRate): Option[A] = peer.get(r).orElse(peer.get(UndefinedRate))
  }

  private implicit final class RichArgument(val peer: UGenSpec.Argument) /* extends AnyVal */ {
    def typeString: String = peer.tpe match {
      case GE(Sig.String,_)   => "String" // currently no type class for GE | String
      case GE(Sig.DoneFlag,_) => "GE with HasDoneFlag"
      case GE(_,_)            => "GE"
      case ArgumentType.Int   => "Int"
    }

    def defaultTree(rate: MaybeRate = UndefinedRate): Tree = peer.defaults.getWithDefault(rate) match {
      case Some(df) =>
        df.toTree
      case _ => EmptyTree
    }
  }

   // filter gets applied with filename (without extension) and ugen name
  def perform1(spec: UGenSpec) {
    import spec._

    val strApply   = "apply"
    val identApply = ident(strApply)

    val traitSideEffect = TypeDef(Modifiers(Flags.TRAIT), typeName("HasSideEffect"), Nil, EmptyTree)
    val traitDoneFlag = TypeDef(Modifiers(Flags.TRAIT), typeName("HasDoneFlag"), Nil, EmptyTree)
    val traitRandom = TypeDef(Modifiers(Flags.TRAIT), typeName("UsesRandSeed"), Nil, EmptyTree)
    val traitIndiv = TypeDef(Modifiers(Flags.TRAIT), typeName("IsIndividual"), Nil, EmptyTree)
    val traitWritesBuffer = TypeDef(Modifiers(Flags.TRAIT), typeName("WritesBuffer"), Nil, EmptyTree)
    val traitWritesFFT = TypeDef(Modifiers(Flags.TRAIT), typeName("WritesFFT"), Nil, EmptyTree)
    val traitWritesBus = TypeDef(Modifiers(Flags.TRAIT), typeName("WritesBus"), Nil, EmptyTree)
    val strIIdxSeq = "IIdxSeq"
    val identIIdxSeq = ident(strIIdxSeq)
    val strMakeUGens = "makeUGens"
    val strMakeUGen = "makeUGen"
    val identMakeUGen = ident(strMakeUGen)
    val strExpand = "expand"
    val strUArgs = "_args"
    val identUArgs = ident(strUArgs)
    val identUnwrap = ident("unwrap")
    val strRatePlaceholder = "Rate" // "R"
    val strMaybeRate = "MaybeRate"
    val strMaybeResolve = "?|"

    val identName = ident("name")

    val strRateArg = "rate"
    val strRateMethod = "rate"
    val identRateArg = ident(strRateArg)

    val impliedRate = rates match {
      case Rates.Implied(r, _) => Some(r)
      case _                   => None
    }

    // whether the case class should contain default values or not
    val hasApply = rates.method match {
      case RateMethod.Custom(`strApply`) => true
      case RateMethod.Alias (`strApply`) => true
      case _                             => false
    }

    // option for a binary operator expansion (synthetic mul-input)
    val expandBin = {
      val muls = args.filter(_.tpe match {
        case GE(Sig.Mul, _)  => true
        case _               => false
      })
      require(muls.size <= 1, s"Can only have one expandBin (${name})")
      muls.headOption
    }

    // this is to ensure for makeUGen that when expandBin is defined, we simply do not pass in
    // _args to the UGen constructor, simplifying the process of filtering out the argument
    // which corresponds to expandBin!
    require(expandBin.isEmpty || inputs.size == 1, s"Currently `mul` input must be sole input (${name})")

    // XXX TODO: correct?
    val argsIn: List[Argument] = args.collect({ case a if inputMap.contains(a.name) => a })(breakOut)

    // a `MaybeRate` is used when no rate is implied and an input generally uses a same-as-ugen constraint.
    // `maybeRateRef` contains the arguments which resolve the undefined rate at expansion time
    val maybeRateRef = if (impliedRate.isEmpty) {
      argsIn.filter(_.rates.get(UndefinedRate) == Some(RateConstraint.SameAsUGen))
    } else IIdxSeq.empty

    val sortedRates = rates.set.toList.sorted
    val objectMethodDefs = sortedRates.flatMap { rate =>
      // e.g. `freq: GE, phase: GE = 0f`
      val objectMethodArgs: List[ValDef] = argsIn.map { uArgInfo =>
        ValDef(
          Modifiers(Flags.PARAM),
          uArgInfo.name,
          ident(uArgInfo.typeString),
          uArgInfo.defaultTree(rate)
        )
      }

      // either a parenthesised arg list, or no parens if there aren't any args
      val objectMethodArgsList: List[List[ValDef]] =
        if (objectMethodArgs.nonEmpty) objectMethodArgs :: Nil else Nil

      // e.g. `apply(audio, freq, phase)
      val methodBody = {
        val argIdents = argsIn.map(i => Ident(i.name))
        // prepend the rate argument if necessary
        val applyArgs = if (impliedRate.isEmpty)
          Ident(rate.name) :: argIdents
        else
          argIdents

        Apply(identApply, applyArgs)
      }

      val argDocs = collectMethodDocs(spec, argsIn)
      val methodNames = rates.method match {
        case RateMethod.Default     => rate.methodName :: Nil
        case RateMethod.Custom(m)   => m :: Nil
        case RateMethod.Alias(m)    => rate.methodName :: m :: Nil
        case RateMethod.None        => Nil
      }

      // e.g. `def ar(freq: GE, phase: GE = 0f): SinOsc`
      // e.g. `def apply(chain: GE, winType: GE = 0.0f, winSize: GE = 0.0f): IFFT`
      val fullMethods = methodNames.map { mName =>
        val df = DefDef(
          NoMods withPosition(Flags.METHOD, NoPosition),
          mName: TermName,
          Nil,                   // tparams
          objectMethodArgsList,  // vparamss

          // Note: to help faster compilation of use site code, always produce the return type annotation
          // if (mName != strApply) EmptyTree else
            TypeDef(NoMods, name: TypeName, Nil, EmptyTree),
          methodBody             // rhs
        )
        wrapDoc(df, 1, argDocs = argDocs)
      }

      // whether the number of arguments is non-zero and there are defaults for all of them
      val allArgsHaveDefaults = objectMethodArgs.nonEmpty && !objectMethodArgs.exists(_.rhs.isEmpty)

      // ...in that case, we generate overloaded methods without parentheses
      if (allArgsHaveDefaults && methodNames.nonEmpty) {
        fullMethods.zip(methodNames).flatMap { case (fullMethod, mName) =>
          // e.g. `kr()`
          val overloadedBody    = Apply(Ident(mName), Nil)
          // e.g. `def kr: SinOsc = kr()
          val overloadedMethod  = DefDef(
            NoMods withPosition(Flags.METHOD, NoPosition),
            mName: TermName,
            Nil, // tparams
            Nil, // vparams
            TypeDef(NoMods, name: TypeName, Nil, EmptyTree),
            overloadedBody // rhs
          )
          overloadedMethod :: fullMethod :: Nil
        }
      } else {
        fullMethods
      }
    }

    val testDef = PackageDef(Select(Select(ident("de"), "sciss"), "synth"), PackageDef(Ident("ugen"), objectMethodDefs) :: Nil)
    println(createText(testDef))

//     val objectDef = if (objectMethodDefs.nonEmpty) {
//       val mod = ModuleDef(
//         NoMods,
//         name,
//         Template(
//           EmptyTree :: Nil, // parents
//           emptyValDef, // self
//           objectMethodDefs // body
//         )
//       )
//       wrapDoc(mod, 0, docText, docSees, docWarnPos) :: Nil
//     } else Nil
//
//     val caseClassConstrArgs = {
//       val args0 = argsInS map {
//         uArgInfo => (NoMods, uArgInfo.name, {
//           val typ0 = Ident(uArgInfo.argDefault.typ.toString)
//           if (hasApply) uArgInfo.defaultTree() match {
//             case EmptyTree => typ0
//             case t => Assign(typ0, t)
//           } else typ0
//         })
//       }
//       if (impliedRate.isDefined) args0
//       else {
//         (NoMods, strRateArg + ": " + (/*if( rateTypes ) (*/
//           /* if( expandBin.isDefined ) "T" else */
//           if (maybeRateRef.nonEmpty) strMaybeRate else strRatePlaceholder /*) else "Rate" */), EmptyTree) :: args0
//       }
//     }
//
//     val caseClassTypeParam = Nil // if( expandBin.isDefined ) ugenCaseClassTypeParam ::: typExpandBinDf else ugenCaseClassTypeParam
//
//     val caseCommonParents: List[TypeDef] = {
//       val p1 = if (doneFlag) (traitDoneFlag :: Nil) else Nil
//       val p2 = if (random) (traitRandom :: p1) else p1
//       val p3 = if (writesBus) (traitWritesBus :: p2) else p2
//       val p4 = if (writesBuffer) (traitWritesBuffer :: p3) else p3
//       val p5 = if (writesFFT) (traitWritesFFT :: p4) else p4
//       val p6 = if (indiv && !indIndiv) (traitIndiv :: p5) else p5
//
//       impliedRate.map(r => TypeDef(NoMods, typeName(r.traitTyp), Nil, EmptyTree) :: p6).getOrElse(p6)
//     }
//
//     /*
//      * `protected def makeUGens: UGenInLike = ...`
//      */
//     val makeUGensDef = {
//       val methodBody: Tree = {
//         val args0 = argsOut // .filterNot( _.expandBin.isDefined )
//         val argsApp = args0.map(a => {
//             val id = Ident(a.name)
//             if (a.isGE) {
//               val sel = Select(id, strExpand)
//               if (a.multi) Select(sel, "outputs") else sel
//             } else if (a.isString) {
//               Apply(Ident("stringArg"), id :: Nil)
//             } else if (a.isInt) {
//               Apply(Ident("Constant"), id :: Nil) // e.g. MFCC numCoeffs
//             } else id
//           })
//
//         def split(as: IIdxSeq[UGenArgInfo], ts: IIdxSeq[Tree]): IIdxSeq[Tree] = {
//           require(as.size == ts.size)
//           var from = 0
//           var keep = false
//           var res = IIdxSeq.empty[Tree]
//           while (from < as.size) {
//             val n = as.segmentLength(u => (u.multi || u.isString) == keep, from)
//             if (n > 0) {
//               val slice = ts.slice(from, from + n)
//               res = if (keep) res ++ slice else res :+ Apply(identIIdxSeq, slice.toList)
//               from += n
//             }
//             keep = !keep
//           }
//           res
//         }
//
//         val argsApp2 = split(args0.toIndexedSeq, argsApp.toIndexedSeq)
//         argsApp2.lastOption match {
//           case None =>
//             Apply(identMakeUGen, Select(identIIdxSeq, "empty") :: Nil) // shortcut, works because we do not use type apply
//           case Some(l) =>
//             Apply(identUnwrap, argsApp2.init.foldRight(l)((a, t) => Apply(Select(a, "++"), t :: Nil)) :: Nil)
//         }
//       }
//
//       DefDef(
//         NoMods withPosition(Flags.PROTECTED, NoPosition) withPosition(Flags.METHOD, NoPosition),
//         stringToTermName(strMakeUGens),
//         Nil, // tparams
//         Nil, // vparamss
//         TypeDef(NoMods, typeName(outputs.resName), Nil, EmptyTree), // TypeTree( NoType ), // tpt -- empty for testing
//         methodBody // rhs
//       )
//     }
//
//     /*
//      * `protected def makeUGen(_args: IIdxSeq[UGenIn]): UGenInLike = ...`
//      */
//     val makeUGenDef = {
//       val methodBody: Tree = {
//         val (preBody, outUGenArgs) = {
//           val strResolvedRateArg = if (maybeRateRef.nonEmpty) "_rate" else strRateArg
//           val args1 = {
//             val args0 = if ((sideEffect || indSideEffect) && (outputs != ZeroOutputs)) {
//               Literal(Constant(true)) :: Nil
//             } else Nil
//
//             if (indiv || indIndiv) {
//               Literal(Constant(true)) :: args0
//             } else if (args0.nonEmpty) {
//               Literal(Constant(false)) :: args0
//             } else args0
//           }
//
//           val args2 = outputs match {
//             case m: MultiOutputLike =>
//               val tree = {
//                 m match {
//                   case fm: FixedMultiOutput => fm.tree
//                   case am@ArgMultiOutput(a) => if (a.isGE) {
//                     require(!argsOut.exists(_.isString), "Currently strings not supported for arg-multioutput")
//                     val numFixedArgs = argsOut.size - 1
//                     val selSz = Select(identUArgs, "size")
//                     if (numFixedArgs == 0) selSz else Apply(Select(selSz, "-"), Literal(Constant(numFixedArgs)) :: Nil)
//                   } else am.tree
//                 }
//               }
//               Apply(Apply(Select(identIIdxSeq, "fill"), tree :: Nil),
//                 Ident(impliedRate.map(_.typ).getOrElse(strResolvedRateArg)) :: Nil) :: Nil
//             case _ => Nil
//           }
//           // might need some more intelligent filtering eventually
//           val args3 = (args2 :+ (if (expandBin.isDefined) Select(identIIdxSeq, "empty") else identUArgs)) ++ args1
//           val args4 = identName /* Literal( Constant( name )) */ :: Ident(impliedRate.map(_.typ).getOrElse(strResolvedRateArg)) :: args3
//
//           val preBody = maybeRate.map(ua => {
//             val aPos = argsOut.indexOf(ua)
//             require(argsOut.take(aPos).forall(a => a.isGE && !a.multi), "Cannot resolve MaybeRate ref after multi args")
//             // val _rate = rate |? _args(<pos>).rate
//             ValDef(
//               NoMods,
//               strResolvedRateArg,
//               EmptyTree,
//               Apply(Select(identRateArg, strMaybeResolve), Select(Apply(identUArgs, Literal(Constant(aPos)) :: Nil), strRateMethod) :: Nil)
//             )
//           })
//
//           (preBody, args4 :: Nil) // no currying
//         }
//         val app0a = TypeDef(NoMods, typeName(outputs.typ), Nil, EmptyTree)
//         val app1 = New(app0a, outUGenArgs)
//
//         val app2 = expandBin.map(binSel => {
//           val a = argsInS.find(_.expandBin.isDefined).get
//           require(!argsOut.exists(a => a.multi || a.isString), "Mixing binop with multi args is not yet supported")
//           val aPos = argsOut.indexOf(a)
//           assert(aPos >= 0)
//           Apply(Select(Select(Ident("BinaryOp"), binSel), "make1"), app1 :: Apply(identUArgs, Literal(Constant(aPos)) :: Nil) :: Nil)
//         }).getOrElse(app1)
//         preBody match {
//           case Some(tree) => Block(tree, app2)
//           case None => app2
//         }
//       }
//
//       val methodArgs = List(List(ValDef(
//         Modifiers(Flags.PARAM),
//         strUArgs,
//         TypeDef(NoMods, typeName(strIIdxSeq), TypeDef(NoMods, typeName("UGenIn"), Nil, EmptyTree) :: Nil, EmptyTree),
//         EmptyTree
//       )))
//
//       DefDef(
//         NoMods withPosition(Flags.PROTECTED, NoPosition) withPosition(Flags.METHOD, NoPosition),
//         stringToTermName(strMakeUGen),
//         Nil, // tparams
//         methodArgs, // vparamss
//         TypeDef(NoMods, typeName(outputs.resName), Nil, EmptyTree), // TypeTree( NoType ), // tpt -- empty for testing
//         methodBody // rhs
//       )
//     }
//
//     val caseClassMethods = {
//       val m1 = makeUGensDef :: makeUGenDef :: Nil
//       m1
//     }
//
//     val caseClassSuperArgs = Nil
//
//     val caseClassDef0 = mkCaseClass(
//       NoMods withPosition(Flags.FINAL, NoPosition),
//       name,
//
//       caseClassTypeParam, // tparams
//       caseClassConstrArgs :: (
//         Nil),
//       caseClassMethods, {
//         // parents
//         val p4 = if (sideEffect && !indSideEffect) (traitSideEffect :: caseCommonParents) else caseCommonParents
//         TypeDef(NoMods, typeName(outputs.sourceName), {
//           Nil
//         }, EmptyTree) :: p4
//
//       },
//       Literal(Constant(name)) :: caseClassSuperArgs // super args
//     )
//     val caseClassDef = wrapDoc(caseClassDef0, 0, docText, docSees, docWarnPos, collectMethodDocs(argsIn))
//
//     val classes0 = objectDef ::: (caseClassDef :: Nil)
//
//     classes0
   }
}