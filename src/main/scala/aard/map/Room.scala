package aard.map

import aard.db.MDB
import aard.script.GmcpRoom
import org.mongodb.scala.bson.collection.immutable.Document

import scala.collection.mutable

object Room {
  val collection = MDB.db.getCollection("room")

  private val rooms = mutable.Map[Long,Room]()
  private var currentRoom : Room = null

  def apply(id: Long) = rooms(id)

  def current : Room = synchronized { currentRoom }

  def loadAll = {
    collection.find.collect().subscribe((results:  Seq[Document]) => {
      val rooms = results.map { doc => fromDocument(doc) }
    })
  }

  def fromDocument(document: Document): Room = {
    Room(document.get("id").get.asInt64(),
      document.get("terrain").get.asString(),
      document.get("zoneName").get.asString(),
      document.get("terrain").get.asString()
    )
  }

  def setRoom(gmcp: GmcpRoom) : Unit = synchronized {
    currentRoom = rooms.get(gmcp.num).getOrElse {
      val r = fromGmcp(gmcp)
      rooms(gmcp.num) = r
      collection.insertOne(r.document)
      r
    }
  }

  def fromGmcp(gmcp: GmcpRoom) : Room = {
    val ex = gmcp.exits
    val exits = Set.newBuilder[Exit]
    if(ex.n > 0) exits += Exit("n",gmcp.num,ex.n)
    if(ex.e > 0) exits += Exit("n",gmcp.num,ex.e)
    if(ex.s > 0) exits += Exit("n",gmcp.num,ex.s)
    if(ex.w > 0) exits += Exit("n",gmcp.num,ex.w)
    if(ex.d > 0) exits += Exit("n",gmcp.num,ex.d)
    if(ex.u > 0) exits += Exit("n",gmcp.num,ex.u)
    val exitMap : Map[String,Exit] = exits.result.map { e => e.name -> e }.toMap

    Room(
      gmcp.num,
      gmcp.terrain,
      gmcp.zone,
      exitMap,
      gmcp.name,
      Coords(gmcp.coord.x,gmcp.coord.y)
    )
  }

}

case class Coords(y: Int, x: Int)

case class Room(id: Long,
                terrain: String,
                zoneName: String,
                exits: Map[String,Exit],
                name: String,
                coords: Coords,
                recallable: Boolean = true,
                portalable: Boolean = true
               ) {

  lazy val document = {
    val exitDocs = exits.values.map {_.document}.toList
    Document("id" -> id, "terrain" -> terrain, "zoneName" -> zoneName, "exits" -> exitDocs,
      "x" -> coords.x, "y"->coords.y, "recallable" -> recallable, "portalable" -> portalable)
  }

}

object Exit {
  def fromDocument(document: Document): Exit = {
    Exit(document.get("name").get.asString(),
      document.get("fromId").get.asInt64(),
      document.get("toId").get.asInt64(),
      document.get("maze").get.asBoolean(),
      document.get("door").get.asBoolean(),
      document.get("locked").get.asBoolean(),
      document.get("pathable").get.asBoolean(),
      document.get("weight").get.asInt64()
    )
  }

}

case class Exit(name: String, fromId: Long, toId: Long, maze: Boolean = false, door: Boolean = false,
                locked: Boolean = false, pathable: Boolean = false, weight: Int = 1) {
  def from = Room(fromId)
  def to = Room(toId)
  lazy val document = {
    Document("name" -> name, "fromId" -> fromId, "toId" -> toId,
      "maze" -> maze, "door" -> door, "locked" -> locked, "pathable" -> pathable, "weight" -> weight)
  }
}