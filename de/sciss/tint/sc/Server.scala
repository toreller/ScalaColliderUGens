/*
 *  Server.scala
 *  Tintantmare
 *
 *  Copyright (c) 2008-2010 Hanns Holger Rutz. All rights reserved.
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

import _root_.de.sciss.scalaosc.{ OSCChannel, OSCBundle, OSCClient, OSCMessage }
import _root_.java.awt.event.{ ActionEvent, ActionListener }
import _root_.java.net.{ ConnectException, InetAddress, InetSocketAddress, SocketAddress }
import _root_.java.io.{ BufferedInputStream, File, IOException, InputStream }
import _root_.javax.swing.{ Timer => SwingTimer }
import _root_.scala.collection.mutable.{ HashSet, ListBuffer }
import _root_.scala.math._

/**
 *	@author		Hanns Holger Rutz
 * 	@version	0.12, 19-Jan-10
 */
object Server {
//  var default: Option[Server] = None //	= new Server( "local" );
  var default: Server = null
  val all = new HashSet[ Server ]()
//  val statusMsg = OSCStatusMessage
  
  def printError( name: String, t: Throwable ) {
    println( name + " : " )
    t.printStackTrace
  }

  case object Running
  case object Booting
  case object Offline
  case class Counts( c: OSCStatusReplyMessage )
  private case object Terminating
  private case object NoPending
}

class Server( val name: String, val options: ServerOptions = new ServerOptions, val clientID: Int = 0 )
extends Model
{
  import Server._

  private val   syncBootThread						= new AnyRef
  private var	aliveThread: Option[StatusWatcher]	= None
  private var	bootThread: Option[BootThread]		= None
  private var	countsObj							= new OSCStatusReplyMessage( 0, 0, 0, 0, 0f, 0f, 0.0, 0.0 )
//  private val	listeners							= new ListBuffer[ (Server, Symbol) => Any ]()
  private val	collBootCompletion					= new ListBuffer[ (Server) => Unit ]()
  private var	conditionVar: AnyRef 				= Offline
  protected[sc] var	pendingCondition: AnyRef		= NoPending
  private var   bufferAllocator : ContiguousBlockAllocator = null
  val rootNode      = new Group( this, 0 )
  val defaultGroup  = new Group( this, 1 )
  
  var latency = 0.2f

  // ---- constructor ----
  all += this
  if( default == null ) default = this
  private val host = InetAddress.getByName( options.host.value )
  val addr = new InetSocketAddress( host, options.port.value )
  /* XXX private */ val c = OSCClient( options.protocol.value, 0, host.isLoopbackAddress, ServerCodec )
  c.bufferSize = 0x10000
  
  val nodeMgr = new NodeManager( this )
  private val multi	= new OSCMultiResponder( this )
  c.target = addr

//  private val host = addr.getAddress
//  if( host == null ) throw new IOException( "Server.new : unresolved network address " + addr );
  val isLocal = host.isLoopbackAddress || host.isSiteLocalAddress

  createNewAllocators
//  resetBufferAutoInfo
    
  def isConnected = c.isConnected
  def isRunning = conditionVar == Running
  def isBooting = conditionVar == Booting
  def isOffline = conditionVar == Offline
  def getBufferAllocator = bufferAllocator

//  try { c.connect }
  
  object nodes {
    private var allocator : NodeIDAllocator = _
    
    reset
    
    def nextID = { allocator.alloc }
    def reset = { allocator = new NodeIDAllocator( clientID, options.initialNodeID )}
  }
  
  object busses {
    protected var controlAllocator : ContiguousBlockAllocator = _
    protected var audioAllocator : ContiguousBlockAllocator = _
    
    reset
    
    def reset = {
    	controlAllocator	= new ContiguousBlockAllocator( options.controlBusChannels.value )
    	audioAllocator		= new ContiguousBlockAllocator( options.audioBusChannels.value, options.firstPrivateBus )
    }
    
    def allocControl( numChannels: Int ) = controlAllocator.alloc( numChannels )
    def allocAudio( numChannels: Int ) = audioAllocator.alloc( numChannels )
    def freeControl( index: Int ) = controlAllocator.free( index )
    def freeAudio( index: Int ) = audioAllocator.free( index )
  }
  
  def sendMsg( cmd: String, p: Any* ) {
    c.send( OSCMessage( cmd, p:_* )) 
//	addr.sendMsg( p: _* )
  }
  
  def sendMsg( msg: OSCMessage ) {
//    println( "sending msg " + msg.getName )
//    for( i <- (0 until msg.getArgCount) ) {
//      println( "arg#" + i + " = " + msg.getArg( i ))
//    }
    c.send( msg )
  }

//  def sendBundle( bndl: OSCBundle ) : Server = {
//    c.send( bndl )
//    this
//  }
  
	def sendBundle( delta: Double, msgs: OSCMessage* ) {
	  // XXX eventually use logical clock
	  c.send(
	      if( delta >= 0 ) {
	    	  val absSecs			= System.currentTimeMillis * 0.001 + delta
	    	  val secsSince1900	= absSecs.toLong + 2208988800L;
	    	  val secsFractional	= ((absSecs % 1.0) * 0x100000000L).toLong
	    	  val raw				= (secsSince1900 << 32) | secsFractional;
	    	  OSCBundle( raw, msgs:_* )
	      } else {
	     	  OSCBundle( msgs:_* )
	      }
	  )
	}
  
//	def sync( condition: Cond = new Cond, bundles: List[ List[ _ ]], latency : Float ) : Unit = { // array of bundles that cause async action
//		if( bundles.isEmpty ) {
//			val id = makeSyncResponder( condition )
//			sendBundle( latency, List( "/sync", id ))
//			condition.wait
//		} else {
//			// not sure what the exact size is, but its about 20000
//			// this relates to what _NetAddr_SendBundle can send
//			if( bundles.bundleSize > 20000/*65515*/) { // 65515 = 65535 - 16 - 4 (sync msg size)
//				bundles.clumpBundles.foreach( item => {
//					val id = makeSyncResponder( condition )
//					sendBundle( latency, (item ++ List( List( "/sync", id ))) :_* )
//					if( latency != null ) { latency = latency + 1e-9 }
//					condition.wait
//				})
//			} else {
//				val id = makeSyncResponder( condition )
//				sendBundle( latency, (bundles ++ List( List( "/sync", id ))) :_* )
//				condition.wait
//			}
//		}
//		// maybe needed: a timeout
//	}

  	def sync( delta: Double = -1, bundles: Seq[ OSCMessage ] = Nil ) {
  		val id		= UniqueID.next
  		var bndl2	= bundles ++ List( new OSCSyncMessage( id ))
  		val cond	= new AnyRef
  		val resp	= new OSCResponderNode( this, "/synced", (msg, addr, when) =>
  			(if( msg( 0 ) == id ) cond.synchronized { cond.notify })
  		)
  		cond.synchronized {
  	  		resp.add
  	  		sendBundle( delta, bndl2 :_* )
  	  		cond.wait
  	  	}
  	}

//   def syncMsg( id: Int ) : OSCMessage =
//      new OSCMessage( "/sync", id ) with Async
  
//  def listSendMsg( p: Seq[ Any ]) {
//    c.send( new OSCMessage( cmd, p.map( _.asInstanceOf[ AnyRef ]).toArray )) 
////	addr.listSendMsg( p )
//  }

//  def listSendBundle( time: Option[Double], bndl: Seq[ Seq[ Any ]]) {
//    addr.listSendBundle( time, bndl )
//  }
  
  def counts = countsObj
  protected[sc] def counts_=( newCounts: OSCStatusReplyMessage ) {
    countsObj = newCounts
    dispatch( Counts( newCounts ))
  }
  
  def dumpTree : Unit = dumpTree( false )

  def dumpTree( controls: Boolean ) {
    rootNode.dumpTree( controls )
  }
  
  def condition = conditionVar
  protected[sc] def condition_=( newCondition: AnyRef ) {
    if( newCondition != conditionVar ) {
        conditionVar = newCondition
        if( newCondition == Offline ) {
//            if( pendingCondition == Terminating ) {
                pendingCondition = NoPending
//            }
        } else if( newCondition == Running ) {
            if( pendingCondition == Booting ) {
                pendingCondition = NoPending
                collBootCompletion.foreach( _.apply( this ))
                collBootCompletion.clear
            }
        }
        dispatch( newCondition )
      }
  }
  
//  def pendingCondition = pendingConditionVar
//  def pendingCondition_=( newCondition: AnyRef ) {
//    println( "pending now " + newCondition )
//    pendingConditionVar = newCondition
//  }
  
  protected[sc] def getMultiResponder = multi
  
  def startAliveThread( delay: Float = 0.25f, period: Float = 0.25f, deathBounces: Int = 25 ) {
//    synchronized( syncBootThread ) {
      if( aliveThread.isEmpty ) {
        val statusWatcher = new StatusWatcher( this, delay, period, deathBounces )
        aliveThread = Some( statusWatcher )
        statusWatcher.start
      }
//    }
  }

  def stopAliveThread {
//  synchronized( syncBootThread ) {
    aliveThread.foreach( _.stop )
    aliveThread = None
//  }
  }

/*
  def setRunning( serverRunning: Boolean ) : Server = {
//    synchronized( syncBootThread ) {
      if( this.serverRunning != serverRunning ) {
        this.serverRunning = serverRunning;
        if( !serverRunning ) {
// XXX          dispatch( ServerEvent.STOPPED );
          if( bootThread.isDefined ) {
            try {
              bootThread.get.keepRunning = false
              syncBootThread.wait( 4000 );
            }
            catch { case e: InterruptedException => }
          }
        } else {
//          while( !collBootCompletion.isEmpty() ) {
//            ((CompletionAction) collBootCompletion.remove( 0 )).completion( this );
//          }
// XXX          dispatch( ServerEvent.RUNNING );
        }
      }
//    }
      this
  }
*/
  def queryCounts {
    sendMsg( OSCStatusMessage )
  }
  
//  private def dispatch( what: Symbol ) : Server = {
//    listeners.foreach { func => func.apply( this, what )}
//    this
//  }
//
//  def addListener( func: (Server, Symbol) => Any ) : Server = {
//    listeners += func
//    this
//  }
//
//  def removeListener( func: (Server, Symbol) => Any ) : Server = {
//    listeners -= func
//    this
//  }

   // note: this is called register instead of
   // notify to avoid conflict with notify in java.lang.Object
   def register( onOff: Boolean = true ) {
      sendMsg( notifyMsg( onOff ))
   }

   def notifyMsg( onOff: Boolean = true ) : OSCMessage =
      OSCMessage( "/notify", if( onOff ) 1 else 0 )

  def dumpOSC( mode: Int = OSCChannel.DUMP_TEXT ) {
     c.dumpIncomingOSC( mode, filter = _ match {
        case m: OSCStatusReplyMessage => false
        case _ => true
     })
     c.dumpOutgoingOSC( mode, filter = _ match {
// fucking match doesn not work, need case object here eventually!
//        case m: OSCStatusMessage.type => false
        case OSCMessage( "/status" ) => false
        case _ => true
     })
  }

  def boot: Unit = boot( true )
  def boot( startAliveThread: Boolean ) {
    if( pendingCondition != NoPending ) {
      println( "Server:boot - ongoing operations" )
      return
    }
    if( conditionVar == Running ) {
      println( "Server:boot - already running" )
      return
    }
    
    if( !isLocal ) throw new IllegalStateException( "Server.boot : only allowed for local servers!" )
      
    val whenBooted = (s: Server) => { 
      try {
//      s.setBooting( false );
//        if( s.oscDumpMode != 'off ) {
//          s.dumpOSC( s.getDumpMode() );
//        }
//        if( s.isNotified() ) {
          println( "notification is on" )
          s.register()
//        } else {
//          println( "notification is off" );
//        }
        s.initTree	// XXX inefficient since it re-created the node allocator
      }
      catch { case e: IOException => printError( "Server.boot", e )}
    }

    pendingCondition = Booting
    condition = Booting

    try {
      createNewAllocators
// XXX resetBufferAutoInfo
			
      addDoWhenBooted( whenBooted )
      bootServerApp( startAliveThread )
    }
    catch { case e /*: IOException*/ => {

      removeDoWhenBooted( whenBooted )
      try {
        stopAliveThread
      }
      catch { case e /*: IOException*/ => printError( "Server.boot", e )}
//      setBooting( false )
      condition = Offline // pendingCondition = NoPending
      throw e
    }}
  }
  
  protected[sc] def bootThreadTerminated {
    bootThread = None
    stopAliveThread
    condition = Offline
  }
  
  // note: we do _not_ reset the nodeIDallocator here
  def initTree {
//    println( "initTree" )
    sendMsg( defaultGroup.newMsg( rootNode, addToHead ))
//    sendMsg( OSCMessage( "/g_new", 1, 0, 0 ))
  }
  
  def addDoWhenBooted( action: (Server) => Unit ) {
    collBootCompletion += action
  }
  
  def removeDoWhenBooted( action: (Server) => Unit ) {
    collBootCompletion -= action
  }
 
  private def createNewAllocators {
    nodes.reset
    busses.reset
    bufferAllocator = new ContiguousBlockAllocator( options.audioBuffers.value )
  }

  private def bootServerApp( startAliveThread: Boolean ) {
    if( bootThread.isEmpty ) {
//    println( "about to boot " + this.name + "; '" + this.options.program.value + "'" )
      var thread	= new BootThread( this, startAliveThread )
      bootThread	= Some( thread )
//    condition		= 'booting
      thread.start
    }
  }
  
  def quit {
    sendMsg( quitMsg )
    println( "/quit sent" )
    cleanUpAfterQuit
  }

  def quitMsg = OSCMessage( "/quit" )

  private def cleanUpAfterQuit {
    try {
      stopAliveThread
//    dumpMode		= 0;
//    setBooting( false )
//    setRunning( false )
//      condition = 'offline
//createNewAllocators
// XXX resetBufferAutoInfo
      pendingCondition = Terminating
    }
    catch { case e: IOException => printError( "Server.cleanUpAfterQuit", e )}
  }
  
  def start {
    c.start
  }

  override def toString = "Server( " + name + " )"
}

private class BootThread( server: Server, startAliveThread: Boolean )
extends Thread {
  var keepRunning = true
  
//  private val folder = new File( "/Users/rutz/Documents/devel/fromSVN/SuperCollider3/build" )
  private val program = server.options.program.value
  println( "Booting '" + program + "'" )
  private val file = new File( program )
  private val processArgs = server.options.toProcessArgs.toArray
  private val pb = new ProcessBuilder( processArgs: _* )
    .directory( file.getParentFile )
    .redirectErrorStream( true )
  
  // ---- constructor ----
//  start

  override def run {
    var cStarted = false
    var pRunning = true
    val inBuf	= new Array[Byte](128)
    try {
      val p			= pb.start
      val inStream	= new BufferedInputStream( p.getInputStream )
      while( keepRunning && pRunning ) {
        if( !cStarted ) {
          try {
            server.start
            cStarted = true
            // note that we optimistically assume that if we boot the server, it
            // will not die (exhausting deathBounces). if it crashes, the boot
            // thread's process will know anyway. this way we avoid stupid
            // server offline notifications when using slow asynchronous commands
            if( startAliveThread ) server.startAliveThread( 1.0f, 0.25f, Int.MaxValue )
          }
          // thrown when in TCP mode and socket not yet available
          catch { case e: ConnectException => }
        }
        try {
          Thread.sleep( 500 )   // a kind of cheesy way to wait for the program to end
        }
        catch { case e: InterruptedException => }
        
        handleConsole( inStream, inBuf )
//      handleConsole( errStream, errBuf )
        try {
          val resultCode	= p.exitValue
          pRunning			= false
//        p					= null
          println( "scsynth terminated (" + resultCode +")" )
        }
        // gets thrown if we call exitValue() while sc still running
        catch { case e: IllegalThreadStateException => }
      } // while( keepScRunning && pRunning )
    }
    catch { case e: IOException => Server.printError( "BootThread.run", e )}
    finally {
      server.bootThreadTerminated  // ! must be before setRunning !
    }
  }
  
  // redirect console
  private def handleConsole( stream: InputStream, buf: Array[Byte] ) : Unit = {
    try {
      while( stream.available > 0 ) {
        val i = min( buf.length, stream.available )
        stream.read( buf, 0, i )
//      printStream.write( buf, 0, i )
        print( new String( buf, 0, i ))
      }
    }
    catch { case e: IOException => } // ignored XXX
  }
}

private class StatusWatcher( server: Server, delay: Float, period: Float, deathBounces: Int )
extends Object /* with OSCListener */ with ActionListener {
  import Server._

  private var	alive			= deathBounces
  private val	delayMillis		= (delay * 1000).toInt  
  private val	periodMillis	= (period * 1000).toInt
  private val	resp			= new OSCResponderNode( server, "/status.reply", messageReceived )
  private val	timer			= new SwingTimer( periodMillis, this )
  
  // ---- constructor ----
  timer.setInitialDelay( delayMillis )
  
  def start {
    resp.add
    timer.restart
  }

  def stop {
    timer.stop
    resp.remove
  }
		
  def actionPerformed( e: ActionEvent ) {
    if( (server.pendingCondition == Booting) && (server.options.protocol.value == 'tcp) &&
        !server.isConnected ) {
      try {
        server.start
      }
      catch { case e: IOException => Server.printError( "Server.status", e )}
    } else {
      alive -= 1
      if( alive < 0 ) {
        server.condition = Offline
      }
      try {
        server.queryCounts
      }
      catch { case e: IOException => Server.printError( "Server.status", e )}
    }
  }
		
  // XXX create specific osc message decoder
  private def messageReceived( msg: OSCMessage, sender: SocketAddress, time: Long ) {
	  msg match {
	 	  case statusReply: OSCStatusReplyMessage => {
              alive = deathBounces
              // note: put the counts before running
              // because that way e.g. the sampleRate
              // is instantly available
	 	 	  server.counts = statusReply
	 	 	  server.condition = Running
	 	  }
	 	  case _ =>
	  }
	  
//    if( msg.length < 9 ) return
//    
//    alive = deathBounces
//    
//    try {
//      // msg.at( 0 ) == 1
//      server.counts = Counts(
//        msg( 1 ).asInstanceOf[Number].intValue,
//        msg( 2 ).asInstanceOf[Number].intValue,
//        msg( 3 ).asInstanceOf[Number].intValue,
//        msg( 4 ).asInstanceOf[Number].intValue,
//        msg( 5 ).asInstanceOf[Number].floatValue,
//        msg( 6 ).asInstanceOf[Number].floatValue,
//        msg( 7 ).asInstanceOf[Number].doubleValue,
//        msg( 8 ).asInstanceOf[Number].doubleValue
//      )
//    }
//    catch { case e: ClassCastException => Server.printError( "StatusWatcher.messageReceived", e )}
  }
}

//case class Counts( numUGens: Int, numSynths: Int, numGroups: Int, numSynthDefs: Int,
//                   avgCPU: Float, peakCPU: Float, sampleRate: Double, actualSampleRate: Double );

object UniqueID { // XXX needs sync
	private var id = 1000
	def next : Int = { var result = id; id += 1; result }
}
