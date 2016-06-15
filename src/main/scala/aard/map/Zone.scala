package aard.map

import java.util.regex.Matcher

import aard.db.Store
import aug.script.{Game, Trigger}

import scala.util.{Failure, Try}

object Zone {

  var zones = Set[Zone]()

  val whereTriggerPattern = "You are in area : (.*)"
  val whereTrigger = Trigger.trigger(whereTriggerPattern,(m: Matcher) => {
    val long = m.group(1)
    val cur = Room.current.zoneName
    val nz = Zone(cur,long)
    if(!(zones.contains(nz))) {
      zones = zones.filter(_.name != cur) + nz
      save
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

}

case class Zone(name: String, long: String)