/*
 *  Rate.scala
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

import collection.immutable.{IndexedSeq => IIdxSeq}

object MaybeRate {
  def max_?(rates: MaybeRate*): MaybeRate = {
    if (rates.isEmpty) return UndefinedRate
    var res: Rate = scalar
    rates.foreach {
      case UndefinedRate => return UndefinedRate
      case r: Rate => if (r.id > res.id) res = r
    }
    res
  }

  def min_?(rates: MaybeRate*): MaybeRate = {
    if (rates.isEmpty) return UndefinedRate
    var res: Rate = demand
    rates.foreach {
      case UndefinedRate => return UndefinedRate
      case r: Rate => if (r.id < res.id) res = r
    }
    res
  }

  def reduce(rates: MaybeRate*): MaybeRate = {
    rates.headOption match {
      case Some(r) => if (r == UndefinedRate || rates.exists(_ != r)) UndefinedRate else r
      case None => UndefinedRate
    }
  }
}

sealed abstract class MaybeRate extends Product {
  final def name: String = productPrefix
  def toOption: Option[Rate]
  def ?|(r: => Rate): Rate
}

case object UndefinedRate extends MaybeRate {
  val toOption: Option[Rate] = None
  def ?|(r: => Rate): Rate = r
}

object Rate {
  implicit val ordering = Ordering.ordered[Rate]
}
/**
 *    The calculation rate of a UGen or a UGen output.
 */
sealed abstract class Rate extends MaybeRate with Ordered[Rate] {
  def id: Int
  def methodName: String

  final val toOption: Option[Rate] = Some(this)
  final val toIndexedSeq: IIdxSeq[Rate] = IIdxSeq(this)

  final def ?|(r: => Rate): Rate = this
  final def min(that: Rate) = if (id < that.id) this else that
  final def max(that: Rate) = if (id > that.id) this else that
  final def compare(that: Rate): Int = id - that.id
}

case object scalar extends Rate {
  final val id = 0
  final val methodName = "ir"
}

case object control extends Rate {
  final val id = 1
  final val methodName = "kr"
}

case object audio extends Rate {
  final val id = 2
  final val methodName = "ar"
}

case object demand extends Rate {
  final val id = 3
  final val methodName = "dr"
}

trait ScalarRated  { final def rate: Rate = scalar  }
trait ControlRated { final def rate: Rate = control }
trait AudioRated   { final def rate: Rate = audio   }
trait DemandRated  { final def rate: Rate = demand  }
