/*
 *  Constant.scala
 *  ControlDesc
 *
 *  Copyright (c) 2008-2009 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is free software; you can redistribute it and/or
 *	modify it under the terms of the GNU General Public License
 *	as published by the Free Software Foundation; either
 *	version 2, june 1991 of the License, or (at your option) any later version.
 *
 *	This software is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *	General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public
 *	License (gpl.txt) along with this software; if not, write to the Free Software
 *	Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 *
 *
 *  Changelog:
 */

package de.sciss.tint.sc

import collection.immutable.{ IndexedSeq => IIdxSeq }
import math._

/**
 *    @version	0.12, 14-Apr-10
 */
case class ControlName( name: String ) {
   def ir : ControlDesc = ir( Vector( 0f ))
   def ir( value: Float ) : ControlDesc = ir( Vector( value ))
   def kr : ControlDesc = kr( Vector( 0f ))
   def kr( value: Float ) : ControlDesc = kr( Vector( value ))

   def ir( values: IIdxSeq[ Float ]) = new ControlDesc( Some( name ), scalar, values, None )
   def kr( values: IIdxSeq[ Float ]) = new ControlDesc( Some( name ), control, values, None )

   def kr( values: Tuple2[ GE, IIdxSeq[ Float ]]) : ControlDesc = {
      val lags = values._1.outputs
      val inits = values._2
      val numCh = max( lags.size, inits.size )
      new ControlDesc( Some( name ), control, wrapExtend( values._2, numCh ), Some( wrapExtend( lags, numCh )))
   }
  
   private def wrapExtend[T]( coll: IIdxSeq[T], size: Int ) : IIdxSeq[T] = {
      if( coll.size == size ) coll
      else if( coll.size > size ) coll.take( size )
      else {
         var result = coll
         while( result.size < size ) {
            val diff = size - result.size
            result ++= (if( diff >= coll.size ) coll else coll.take( diff ))
         }
         result
      }
   }
}

class ControlDesc( val name: Option[ String ], val rate: Rate, val initValues: IIdxSeq[ Float ],
                   val lag : Option[ IIdxSeq[ UGenIn ]])
extends RatedGE
{
   var ugen: UGen = null            // XXX mutable
   var ugenOutputIndex : Int = 0    // XXX mutable 

   // ---- constructor ----
   {
      addToSynth
   }

   def outputs /*: Seq[ ControlProxy ]*/ = (0 until initValues.size).map( new ControlProxy( this, _ ))
   override def numOutputs	= initValues.size
//   def toUGenIns	= outputs

   private def addToSynth {
      SynthDef.builder.addControlDesc( this )
   }
}

class ControlProxy( desc: ControlDesc, channel: Int )
extends UGenIn with UGenProxy {
   def rate          = desc.rate
   def source        = desc.ugen
   def outputIndex   = desc.ugenOutputIndex + channel
    
//  def writeInputSpec( dos: DataOutputStream, synthDef: SynthDef ) : Unit = {
////      val ugenIndex = synthDef.getUGenIndex( desc.ugen )
//		val ugenIndex = desc.ugen.synthIndex
//      if( SynthDef.verbose ) println( "  ControlProxy.writeInputSpec. ugenIndex = " + ugenIndex + "; ugenOutputIndex = " + desc.ugenOutputIndex + "; channel = " + channel )
//      if( ugenIndex == -1 ) throw new IOException( "UGen not listed in synth def : " + source )
//      dos.writeShort( ugenIndex )
//      dos.writeShort( desc.ugenOutputIndex + channel )
//  }
}