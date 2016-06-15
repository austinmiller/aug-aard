package aard.map

import aard.db.Store

object Zone {

  var zones = Set[Zone]()

  def load = {
    zones = Store.load[Set[Zone]]("zones")
  }

  def save = {
    Store.save("zones",zones)
  }

}

case class Zone(name: String, long: String)