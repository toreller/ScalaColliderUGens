/*
 *  ClassGenerator.scala
 *  (ScalaColliderUGens)
 *
 *  Copyright (c) 2008-2013 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

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
    @tailrec def loop(xs: List[String], feed: Boolean) {
      xs match {
        case head :: tail =>
          val isPre = head.startsWith("{{{")
          val headL = if (isPre) {
            head.split('\n').toList
          } else {
            if (feed) b += ""
            linesWrap(head, width)
          }
          b ++= headL
          loop(tail, feed = !isPre)

        case Nil =>
      }
    }
    loop(paras, feed = false)
    b.result()
  }

  private def wrapDoc(spec: UGenSpec, tree: Tree, body: Boolean, args: Boolean): Tree = {
    if (spec.doc.isEmpty) return tree
    val doc     = spec.doc.get
    val bodyDoc = if (body) doc.body  else Nil
    val linkDocs= if (body) doc.links else Nil
    val argDocs = if (args) collectMethodDocs(spec) else Nil
    val warnPos = body && doc.warnPos

    val hasAny = doc.body.nonEmpty || doc.links.nonEmpty || argDocs.nonEmpty || warnPos
    if (!hasAny) return tree

    def feed(pre: List[String], post: List[String]): List[String] =
      if (pre.isEmpty) post else pre ::: "" :: post

    val bodyLines = linesFromParagraphs(bodyDoc)

    val bodyAndWarn = if (!warnPos) bodyLines else {
      feed(bodyLines, "'''Warning''': The argument order is different from its sclang counterpart." :: Nil)
    }

    val bodyAndArgs = if (argDocs.isEmpty) bodyAndWarn else {
      val argLines = argDocs.flatMap { case (aName, aDoc) =>
        val aDocL = linesWrap(aDoc, 57) // 80 - 23 = 57 columns
        aDocL.zipWithIndex.map { case (ln, idx) =>
          val pre = if (idx == 0) ("@param " + aName).take(21) else ""
          val tab = pre + (" " * (23 - pre.length))
          tab + ln
        }
      }
      feed(bodyAndWarn, argLines)
    }

    val all = if (linkDocs.isEmpty) bodyAndArgs else {
      val linkLines = linkDocs.map(link => s"@see [[de.sciss.synth.${link}]]")
      feed(bodyAndArgs, linkLines)
    }

    DocDef(DocComment(all.mkString("/**\n * ", "\n * ", "\n */\n"), NoPosition), tree)
  }

  /*
   * Collects argument documentation in the form of a list of tuples argName -> joinedDoc. If there aren't
   * docs, returns `Nil`.
   */
  private def collectMethodDocs(spec: UGenSpec): List[(String, String)] = spec.doc match {
    case Some(doc) =>
      spec.args.flatMap( a => {
        val aDoc = doc.args.get(a.name)
        aDoc.map(d => a.name -> d.mkString(" "))
      })(breakOut)

    case _ => Nil
  }

  def perform( specs: Seq[UGenSpec], dir: File) {
    ???
  }

  private implicit final class RichRate(val peer: Rate) {
    def traitTypeString = peer.name.capitalize + "Rated"
  }

  private implicit final class RichArgumentValue(val peer: ArgumentValue) /* extends AnyVal */ {
    import ArgumentValue._

    def toTree: Tree = peer match {
      case Int(i)         => Literal(Constant(i))
      case Float(f)       => Literal(Constant(f))
      case Boolean(b)     => Literal(Constant(if (b) 1 else 0)) // currently no type class for GE | Switch
      case String(s)      => Literal(Constant(s))               // currently no type class for GE | String
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
      case Some(df) => df.toTree
      case _        => EmptyTree
    }

    def typeIsString: Boolean = peer.tpe match {
      case GE(Sig.String,_) => true
      case _                => false
    }
  }

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
    val identVector = ident("Vector")
    val strMakeUGens = "makeUGens"
    val strMakeUGen = "makeUGen"
    val identMakeUGen = ident(strMakeUGen)
    val strExpand = "expand"
    val strUArgs = "_args"
    val identUArgs = ident(strUArgs)
    val identUnwrap = ident("unwrap")
    val strRatePlaceholder = "Rate" // "R"
    val identMaybeRate  = Ident("MaybeRate")
    val identRate       = Ident("Rate")
    val strMaybeResolve = "?|"
    val identConstant = Ident("Constant")
    val strOutputs  = "outputs"

    val identName = ident("name")

    val strRateArg = "rate"
    val strRateMethod = "rate"
    val identRateArg = ident(strRateArg)
    val identStringArg = Ident("stringArg")
    val strEmpty      = "empty"
    val strPlusPlus   = "++"

    val impliedRate = rates match {
      case Rates.Implied(r, _) => Some(r)
      case _                   => None
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

    val argsIn  = args.toList
    // val argsOut = args.filter(a => inputMap.contains(a.name))
    val argsOut = inputs.flatMap(i => argMap.get(i.arg)) // important: must be sorted according to `inputs`

    val sortedRates = rates.set.toList.sorted
    // the companion object's methods
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
        wrapDoc(spec, df, /* indent = 1, */ body = false, args = true)
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
            Nil,            // tparams
            Nil,            // vparams
            TypeDef(NoMods, name: TypeName, Nil, EmptyTree),
            overloadedBody  // rhs
          )
          overloadedMethod :: fullMethod :: Nil
        }
      } else {
        fullMethods
      }
    }

    // the complete companion object. this is given as a List[Tree],
    // because there might be no object (in that case objectDef is Nil).
    val objectDef = if (objectMethodDefs.nonEmpty) {
      val mod = ModuleDef(
        NoMods,
        name,
        Template(
          EmptyTree :: Nil, // parents
          emptyValDef,      // self
          objectMethodDefs  // body
        )
      )
      wrapDoc(spec, mod, /* indent = 0, */ body = true, args = false) :: Nil
    } else Nil

    // whether the case class should contain default values or not
    val hasApply = rates.method match {
      case RateMethod.Custom(`strApply`) => true
      case RateMethod.Alias (`strApply`) => true
      case _                             => false
    }

    // a `MaybeRate` is used when no rate is implied and an input generally uses a same-as-ugen constraint.
    // `maybeRateRef` contains the arguments which resolve the undefined rate at expansion time
    val maybeRateRef = if (impliedRate.isEmpty) {
      argsIn.filter(_.rates.get(UndefinedRate) == Some(RateConstraint.SameAsUGen))
    } else IIdxSeq.empty

    val caseClassConstrArgs = {
      // for each argument the tuple (NoMods, argName, type or type-and-default)
      val argTuples = argsIn map { arg =>
        val tpe     = Ident(arg.typeString)
        val tpeRHS  = if (!hasApply) tpe else arg.defaultTree() match {
          case EmptyTree  => tpe
          case t          => Assign(tpe, t)
        }
        (NoMods, arg.name, tpe)
      }

      // if a rate is implied, we're done, otherwise prepend the rate argument
      if (impliedRate.isDefined) argTuples else {
        val tpe = if (maybeRateRef.nonEmpty) identMaybeRate else identRate
        (NoMods, strRateArg, tpe) :: argTuples
      }
    }

    // attributes

    val a = spec.attr
    import Attribute._

    val readsBus      = a contains ReadsBus
    val readsBuf      = a contains ReadsBuffer
    val readsFFT      = a contains ReadsFFT
    val random        = a contains UsesRandSeed
    val indiv         = a contains IsIndividual

    val writesBus     = a contains WritesBus
    val writesBuf     = a contains WritesBuffer
    val writesFFT     = a contains WritesFFT
    val sideEffect    = a contains HasSideEffect

    val doneFlag      = a.contains(HasDoneFlag)

    val indSideEffect = writesBus || writesBuf || writesFFT
    val indIndiv      = readsBus  || readsBuf  || readsFFT || random || indSideEffect

    // generates the mixins such as `HasDoneFlag`, `IsIndividual`, and in the
    // case of an implied rate `AudioRated` etc.
    val caseClassMixins: List[TypeDef] = {

      val mixin1 = if (doneFlag)                    (traitDoneFlag   :: Nil)    else Nil
      val mixin2 = if (indiv      || indIndiv)      (traitIndiv      :: mixin1) else mixin1
      val mixin3 = if (sideEffect || indSideEffect) (traitSideEffect :: mixin2) else mixin2

      impliedRate match {
        case Some(r)  => TypeDef(NoMods, r.traitTypeString: TypeName, Nil, EmptyTree) :: mixin3
        case _        => mixin3
      }
    }

    val expandResultStr = if (outputs.isEmpty) "Unit" else "UGenInLike"

    // `protected def makeUGens: UGenInLike = ...`
    val makeUGensDef = {
      val methodBody: Tree = {
        val argsApp = argsOut.map { a =>
          val id = Ident(a.name)
          a.tpe match {
            case GE(Sig.String,_) => Apply(identStringArg, id :: Nil)
            case GE(_,_) =>
              val sel = Select(id, strExpand)
              if (inputMap(a.name).variadic) Select(sel, strOutputs) else sel

            case ArgumentType.Int => id // Apply(identConstant, id:: Nil)
          }
        }

        def split(as: IIdxSeq[Argument], ts: IIdxSeq[Tree]): IIdxSeq[Tree] = {
          var from  = 0
          var keep  = false
          var b     = Vector.empty[Tree]
          val sz    = as.size
          while (from < sz) {
            val n = as.segmentLength(u => {
              val isMulti = inputMap(u.name).variadic || u.typeIsString
              isMulti == keep
            }, from)
            if (n > 0) {
              val slice = ts.slice(from, from + n)
              if (keep)
                b ++= slice
              else
                b :+= Apply(identVector, slice.toList)
              from += n
            }
            keep = !keep
          }
          b
        }

        val argsApp2 = split(argsOut, argsApp)
        argsApp2.lastOption match {
          case None =>
            Apply(identMakeUGen, Select(identIIdxSeq, strEmpty) :: Nil) // shortcut, works because we do not use type apply
          case Some(l) =>
            Apply(identUnwrap, argsApp2.init.foldRight(l)((a, t) => Apply(Select(a, strPlusPlus), t :: Nil)) :: Nil)
        }
      }

      DefDef(
        NoMods withPosition(Flags.PROTECTED, NoPosition) withPosition(Flags.METHOD, NoPosition),
        stringToTermName(strMakeUGens),
        Nil, // tparams
        Nil, // vparamss
        TypeDef(NoMods, expandResultStr: TypeName, Nil, EmptyTree), // tpt
        methodBody // rhs
      )
    }

    val multiOut = outputs.size > 1 || outputs.exists(_.variadic.isDefined)

    // `protected def makeUGen(_args: IIdxSeq[UGenIn]): UGenInLike = ...`
    val makeUGenDef = {
      val methodBody: Tree = {
        val (preBody, outUGenArgs) = {
          val strResolvedRateArg = if (maybeRateRef.nonEmpty) "_rate" else strRateArg

          // args1 are the last two args (isIndividual, hasSideEffect)
          val args1 = {
            // the last argument to UGen.SingleOut and UGen.MultiOut is `hasSideEffect`.
            // it has a default value of `false`, so we need to add this argument only
            // if there is a side effect and the UGen is not zero-out.
            val args0 = if ((sideEffect || indSideEffect) && (outputs.nonEmpty)) {
              Literal(Constant(true)) :: Nil
            } else Nil

            // the preceeding argument is `isIndividual` for all UGens
            if (indiv || indIndiv) {
              Literal(Constant(true)) :: args0
            } else if (args0.nonEmpty) {
              Literal(Constant(false)) :: args0
            } else args0
          }

//          val args2 = if (multiOut) {
//            val tree = {
//              outputs match {
//                case fm: FixedMultiOutput => fm.tree
//                case am@ArgMultiOutput(a) => if (a.isGE) {
//                  require(!argsOut.exists(_.isString), "Currently strings not supported for arg-multioutput")
//                  val numFixedArgs = argsOut.size - 1
//                  val selSz = Select(identUArgs, "size")
//                  if (numFixedArgs == 0) selSz else Apply(Select(selSz, "-"), Literal(Constant(numFixedArgs)) :: Nil)
//                } else am.tree
//              }
//            }
//            Apply(Apply(Select(identIIdxSeq, "fill"), tree :: Nil),
//              Ident(impliedRate.map(_.typ).getOrElse(strResolvedRateArg)) :: Nil) :: Nil
//
//          } else {
//            Nil
//          }
//
//          // might need some more intelligent filtering eventually
//          val args3 = (args2 :+ (if (expandBin.isDefined) Select(identIIdxSeq, "empty") else identUArgs)) ::: args1
//          val args4 = identName :: Ident(impliedRate.map(_.typ).getOrElse(strResolvedRateArg)) :: args3
//
//          val preBody = maybeRate.map(ua => {
//            val aPos = argsOut.indexOf(ua)
//            require(argsOut.take(aPos).forall(a => a.isGE && !a.multi), "Cannot resolve MaybeRate ref after multi args")
//            // val _rate = rate |? _args(<pos>).rate
//            ValDef(
//              NoMods,
//              strResolvedRateArg,
//              EmptyTree,
//              Apply(Select(identRateArg, strMaybeResolve), Select(Apply(identUArgs, Literal(Constant(aPos)) :: Nil), strRateMethod) :: Nil)
//            )
//          })
//
//          (preBody, args4 :: Nil) // no currying
          ???
        }
//        val app0a = TypeDef(NoMods, typeName(outputs.typ), Nil, EmptyTree)
//        val app1 = New(app0a, outUGenArgs)
//
//        val app2 = expandBin.map(binSel => {
//          val a = argsIn.find(_.expandBin.isDefined).get
//          require(!argsOut.exists(a => a.multi || a.isString), "Mixing binop with multi args is not yet supported")
//          val aPos = argsOut.indexOf(a)
//          assert(aPos >= 0)
//          Apply(Select(Select(Ident("BinaryOp"), binSel), "make1"), app1 :: Apply(identUArgs, Literal(Constant(aPos)) :: Nil) :: Nil)
//        }).getOrElse(app1)
//        preBody match {
//          case Some(tree) => Block(tree, app2)
//          case None => app2
//        }
        ???
      }

      val methodArgs = List(List(ValDef(
        Modifiers(Flags.PARAM),
        strUArgs,
        TypeDef(NoMods, typeName(strIIdxSeq), TypeDef(NoMods, typeName("UGenIn"), Nil, EmptyTree) :: Nil, EmptyTree),
        EmptyTree
      )))

      DefDef(
        NoMods withPosition(Flags.PROTECTED, NoPosition) withPosition(Flags.METHOD, NoPosition),
        stringToTermName(strMakeUGen),
        Nil, // tparams
        methodArgs, // vparamss
        TypeDef(NoMods, expandResultStr: TypeName, Nil, EmptyTree), // tpt
        methodBody // rhs
      )
    }

    val caseClassMethods = {
      val m1 = makeUGensDef /* :: makeUGenDef */ :: Nil
      m1
    }

    val caseClassDef: Tree = {
      val outputsPrefix = if (outputs.isEmpty)
        "Zero"
      else if (multiOut)
        "Multi"
      else
        "Single"
      val outputsTypeString = s"UGenSource.${outputsPrefix}Out"

      val plain = mkCaseClass(
        NoMods withPosition(Flags.FINAL, NoPosition),
        name,

        Nil, // tparams
        caseClassConstrArgs :: Nil,
        caseClassMethods,
        TypeDef(NoMods, outputsTypeString, Nil, EmptyTree) :: caseClassMixins,  // parents
        Nil // super args
      )
      wrapDoc(spec, plain, body = true, args = true)
    }

    val classes = objectDef ::: (caseClassDef :: Nil)

    val testDef = PackageDef(Select(Select(ident("de"), "sciss"), "synth"), PackageDef(Ident("ugen"), classes) :: Nil)
    println(createText(testDef))
  }
}