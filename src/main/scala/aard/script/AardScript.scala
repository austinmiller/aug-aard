package aard.script

import aard.adventure.{Campaign, Quest}
import aard.map.{Room, Zone}
import aard.player.Prompt
import aug.profile.{ProfileEvent, ProfileEventListener, ScriptInit, TelnetGMCP}
import aug.script.Game

class AardScript extends ProfileEventListener {
  override def event(event: ProfileEvent, data: Option[String]): Unit = {
    event match {
      case ScriptInit => init
      case TelnetGMCP => data map(handleGmcp(_))
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
      case r: GmcpRoom =>
        Game.echo(s"<${r.num}> ")
        Room.setRoom(r)
      case _ =>
    }
  }

  def init = {
    Game.info("loaded aard script")
    Room.load
    Zone.load
    Game.info(s"loaded ${Room.rooms.size}")
    Quest.load
    Campaign.load
    Prompt.load
  }
}
