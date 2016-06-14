package aard.script

import aard.map.Room
import aug.profile.{ProfileEvent, ProfileEventListener, ScriptInit, TelnetGMCP}
import aug.script.Game
import org.mongodb.scala.{Completed, Document, MongoClient, MongoCollection, Observable, Observer}

class AardScript extends ProfileEventListener {
  override def event(event: ProfileEvent, data: Option[String]): Unit = {
    event match {
      case ScriptInit => init
      case TelnetGMCP => data foreach { gmcp =>
        Gmcp.deserialize(gmcp) match {
          case GmcpUnknown(s) => println(s"UNKNOWN GMCP $s")
          case r: GmcpRoom =>
            Game.echo(s"<${r.num}> ")
            Room.setRoom(r)
          case _ =>
        }
      }
      case _ =>
    }
  }

  def init = {
    Game.info("loaded aard script")
    Room.load
    Game.info(s"loaded ${Room.rooms.size}")
    addAliases
  }

  def addAliases = {
  }
}
