package aard.map

import java.util.regex.Matcher

import aard.db.Store
import aug.script.{Alias, Game, Trigger}

import scala.util.{Failure, Try}

object Zone {

  var zones = Set[Zone]()

  val whereTriggerPattern = "You are in area : (.*)"
  val whereTrigger = Trigger.trigger(whereTriggerPattern,(m: Matcher) => {
    Room.forRoom() { r =>
      val long = m.group(1)
      val cur = r.zoneName
      val nz = Zone(cur, long)
      if (!(zones.contains(nz))) {
        zones = zones.filter(_.name != cur) + nz
        save
      }
    }
  })

  def load = {
    Try {
      zones = Store.load[Set[Zone]]("zones")
    } match {
      case Failure(e) => Game.handleException(e)
      case _ =>
    }

    Room.rooms.values.map(r=>r.zoneName).toSet[String].foreach(zn=>register(zn))

    Alias.alias("z list",m=>aliasList)
  }

  def byLong(long: String) : Option[Zone] = zones.filter(_.long == long).headOption
  def apply(name: String) : Option[Zone] = zones.filter(_.name == name).headOption

  def register(zoneName: String) = synchronized {
    Zone(zoneName) match {
      case None =>
        zones = zones + Zone(zoneName,"")
        save
      case _ =>
    }
  }

  def save = synchronized {
    Store.save("zones",zones)
  }

  def aliasList = {
    Game.header("zone list")
    zones.toList.sortBy(_.name).foreach {z=>
      Game.echo(f"${z.name}%-15s: ${z.long}%s\n")
    }
  }


}

case class Zone(name: String, long: String)