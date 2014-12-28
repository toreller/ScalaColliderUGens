/*
 *  ChannelProxy.scala
 *  (ScalaColliderUGens)
 *
 *  Copyright (c) 2008-2015 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth
package ugen

final case class ChannelProxy(elem: GE, index: Int) extends GE.Lazy {
  def rate = elem.rate

  override def toString = s"$elem.\\($index)"

  def makeUGens: UGenInLike = {
    val _elem = elem.expand
    _elem.unwrap(index)
  }
}