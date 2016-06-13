package aard.map

import aard.db.MDB


trait Storable[A] {
  def collection: String
}

object Zone {
  val collection = MDB.db.getCollection("zone")

  val save = {
  }
}

case class Zone(id: Long) extends Storable[Long] {
  override def collection: String = "zone"

}


object Room {
  val collection = MDB.db.getCollection("room")
}

/**
  * Created by austin on 6/11/2016.
  */
case class Room(id: Long,
  terrain: String

               ) extends Storable[Long] {

  override def collection = "room"

}
