/*
 *  AddAction.scala
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

/** Add-actions are used by the server to determine where to place a node with
  * respect to other nodes. They form an enumeration of integers which are
  * represented by case objects being subclasses of this abstract class.
  *
  * The following actions are known:
  *
  *  - `0` - [[de.sciss.synth.addToHead addToHead]]
  *  - `1` - [[de.sciss.synth.addToTail addToTail]]
  *  - `2` - [[de.sciss.synth.addBefore addBefore]]
  *  - `3` - [[de.sciss.synth.addAfter addAfter]]
  *  - `4` - [[de.sciss.synth.addReplace addReplace]]
  */
sealed trait AddAction { def id: Int }

/** An `AddAction` with id `0`, indicating that a node should be add to the head of
  * of a target group.
  */
case object addToHead extends AddAction { final val id = 0 }
/** An `AddAction` with id `1`, indicating that a node should be add to the tail of
  * of a target group.
  */
case object addToTail extends AddAction { final val id = 1 }
/** An `AddAction` with id `2`, indicating that a node should be added to the same
  * group as the target node, right before it.
  */
case object addBefore extends AddAction { final val id = 2 }
/** An `AddAction` with id `3`, indicating that a node should be added to the same
  * group as the target node, right after it.
  */
case object addAfter extends AddAction { final val id = 3 }
/** An `AddAction` with id `4`, indicating that a node should replace an existing
  * node, that is take the target node's exact position in the tree.
  */
case object addReplace extends AddAction { final val id = 4 }
