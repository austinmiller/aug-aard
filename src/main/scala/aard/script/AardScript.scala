package aard.script

import java.awt.Color

import aard.adventure.{Campaign, Quest, Targeter}
import aard.map.{Path, Room, Zone}
import aard.player.{Player, Prompt}
import aug.profile.{ProfileEvent, ProfileEventListener, ScriptClose, ScriptInit, TelnetGMCP}
import aug.script.{Alias, Game}

import scala.util.{Random, Try}

class AardScript extends ProfileEventListener {

  override def event(event: ProfileEvent, data: Option[String]): Unit = {
    event match {
      case ScriptInit => init
      case TelnetGMCP => data map(handleGmcp(_))
      case ScriptClose => close
      case _ =>
    }
  }

  def handleGmcp(gmcp: String) = {
    println(gmcp)
    Gmcp.deserialize(gmcp) match {
      case GmcpUnknown(s) => println(s"UNKNOWN GMCP $s")
      case q: GmcpQuest =>
        Game.header("quest info")
        Game.echo(s"$q\n")
        Quest.onGmcp(q)
      case r: GmcpRoom =>
        Game.echo(s"<${r.num}> ")
        Room.setRoom(r)
      case p: GmcpChar => Player.onGmcp(p)
      case GmcpTick => Game.send("q")
      case GmcpRepop(_) =>
        Room.onRepop()
      case _ =>
    }
  }

  def init = {
    Game.info("loaded aard script")
    Room.load
    Zone.load
    Path.load
    Game.info(s"loaded ${Room.rooms.size}")
    Quest.load
    Campaign.load
    Prompt.load
    Targeter.load
    Player.load
    Discover.load
  }

  def close: Unit = {
    Path.close
    Discover.close
  }

}

object Shortcuts {
  private val random = new Random()
  def rand[A](items: Iterable[A])(implicit manifest: Manifest[A]) : Option[A] = {
    if(items.isEmpty) None else {
      val sz = items.size
      val arr = items.toArray
      Some(arr(random.nextInt(sz)))
    }
  }
  def error(s: String) = Game.echo(s"\nERROR: $s\n",Some(Color.RED))
  def info(s: String) = Game.echo(s"\nINFO: $s\n",Some(Color.YELLOW))
  def echo(s: String) = Game.echo(s"$s\n",Some(Color.YELLOW))

  def bench[A](op: String="operation", report: String => Unit = println)(f: =>A) : A = {
    val time = System.currentTimeMillis()
    val rt = f
    val diff = System.currentTimeMillis() - time
    report(s"'$op' took $diff ms")
    rt
  }
}


object AardUtil {

  trait ColorState
  case object Stream extends ColorState
  case object ExpectColor extends ColorState

  def removeColors(line : String) : String = {

    val b = List.newBuilder[Char]
    var s : ColorState = Stream
    for(ch<-line) {
      s match {
        case Stream => if(ch=='@') s = ExpectColor else b+=ch
        case ExpectColor =>
          s = Stream
          if(ch=='@') b += ch
      }
    }

    b.result().mkString("")
  }
}

object Discover {
  import Shortcuts._
  private var thread = Option[Thread](null)

  def start = {
    thread foreach (_.interrupt)
    thread = Some(new Thread(new Runnable {
      override def run(): Unit = {
        for(i<-1 to 1000) {
          if(Thread.interrupted()) return
          Room.aliasNextRoom
          info(s"discover step: $i")
          Try {
            Thread.sleep(500)
          }
        }
        info("discover over")
      }}))
    thread.foreach(_.start)
  }

  def close = {
    thread.foreach(_.interrupt)
    thread = None
  }

  def load = {
    Alias.alias("discover",m=>{
      start
    })

    Alias.alias("discover stop",m=>{
      close
    })
  }
}