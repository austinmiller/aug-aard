package aard.map

import java.io.File
import java.nio.charset.StandardCharsets
import java.util.regex.Matcher

import aard.db.Store
import aard.script.GmcpRoom
import aug.script.{Alias, Game}
import aug.util.JsonUtil
import com.google.common.cache.{CacheBuilder, CacheLoader, LoadingCache}
import org.apache.commons.io.FileUtils

import scala.collection.mutable

case class RunList(run: String ="", amt: Int = 0, last: String = "") {
  def +(exit: Exit): RunList = {
    if(exit.name == last) {
      RunList(run,amt+1,last)
    } else {
      RunList(runString,1,exit.name)
    }
  }

  private def runString : String = {
    if(amt == 1) {
      run+last
    } else if(amt > 1) {
      run+amt+last
    } else {
      run
    }
  }

  def isEmpty = if(run == "" && amt == 0) true else false

  def command : String = s"run $runString"
}

case class Path(val exits: List[Exit]) {
  lazy val weight = exits.map {_.weight}.sum
  def + (exit: Exit) : Path = Path(exits :+ exit)

  def runTo = {
    val cmds = List.newBuilder[String]
    var run = RunList()

    def addRunTo = {
      if(!run.isEmpty) {
        cmds += run.command
        run = RunList()
      }
    }

    exits.foreach {exit=>
      if(exit.door) {
        addRunTo
        if(exit.locked) cmds += s"unlock ${exit.name}"
        cmds += s"open ${exit.name}"
      }
      run += exit
    }

    addRunTo
    cmds.result.foreach { cmd => Game.send(cmd) }
  }
}

class Pather(val room: Room, val rooms: Iterable[Room]) {
  private case class Link(prev: Option[Room], exit: Option[Exit], dist: Int = Integer.MAX_VALUE)

  private val links = mutable.Map[Room,Link]()



  val time = {
    val t = System.currentTimeMillis()
    val unvisited = mutable.Set[Room]()

    rooms.foreach{r=>
      links.put(r,Link(None,None))
      unvisited.add(r)
    }

    links.put(room,Link(None,None,0))

    def nextToVisit: Room = {
      val r: Room = unvisited.reduceLeft { (a, b) =>
        if (links(a).dist < links(b).dist) a else b
      }
      unvisited.remove(r)
      r
    }

    while (!unvisited.isEmpty) {
      val r = nextToVisit

      val dist = links(r).dist

      for (exit <- r.exits.values if exit.to.isDefined && exit.pathable) {
        val to = exit.to.get
        if(rooms.exists{_ == to}) {
          val alt = dist + exit.weight
          val p = links(to)

          if (alt < p.dist) links(to) = Link(Some(r), Some(exit), alt)
        }
      }
    }

    System.currentTimeMillis() - t
  }

  Game.echo(s"pather created for $room in $time ms\n")

  def pathTo(target: Room) : Path = {
    val seq = List.newBuilder[Exit]
    val link = links.get(target)

    def addPath(link: Link): Unit = {
      link.prev.foreach {p => if(p != room) addPath(links(p)) }
      link.exit.foreach {e=> seq += e}
    }

    link.foreach {l => addPath(l)}
    Path(seq.result())
  }


}

object Room {

  val dataDir = new File(Store.dataDir,"room")
  val ext = ".room"

  val rooms = mutable.Map[Long,Room]()
  private var currentRoom : Room = null
  private val charset = StandardCharsets.UTF_8

  def apply(id: Long) = rooms.get(id)

  def current : Room = synchronized { currentRoom }

  val patherCache: LoadingCache[Room, Pather] = CacheBuilder.newBuilder().maximumSize(1000).build(new CacheLoader[Room,Pather]() {
    override def load(key: Room): Pather = new Pather(key,roomsByZone(key.zoneName))
  })

  private def loadAll = {
    for(file <- dataDir.listFiles if file.getName.endsWith(ext)) {
      val s = FileUtils.readFileToString(file,charset)
      val r = JsonUtil.fromJson[Room](s)
      rooms(r.id) = r
    }
  }

  def setRoom(gmcp: GmcpRoom) : Unit = synchronized {
    currentRoom = rooms.get(gmcp.num).getOrElse {
      val r = fromGmcp(gmcp)
      rooms(gmcp.num) = r
      save(r)
      r
    }
  }

  def save(room: Room) = synchronized {
    patherCache.invalidate(room)
    val file = new File(dataDir,s"${room.id}$ext")
    FileUtils.writeStringToFile(file,JsonUtil.toJson(room),charset)
  }

  def fromGmcp(gmcp: GmcpRoom) : Room = {
    val ex = gmcp.exits
    val exits = Set.newBuilder[Exit]
    if(ex.n > 0) exits += Exit("n",gmcp.num,ex.n)
    if(ex.e > 0) exits += Exit("e",gmcp.num,ex.e)
    if(ex.s > 0) exits += Exit("s",gmcp.num,ex.s)
    if(ex.w > 0) exits += Exit("w",gmcp.num,ex.w)
    if(ex.d > 0) exits += Exit("d",gmcp.num,ex.d)
    if(ex.u > 0) exits += Exit("u",gmcp.num,ex.u)
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

  def roomsByZone(zoneName: String) = rooms.values.filter {_.zoneName == zoneName}

  def load = {
    loadAll
    Alias.alias("r info",(m: Matcher) => aliasInfo)
    Alias.alias("r list",(m: Matcher) => aliasList)
    Alias.alias("r next", (m: Matcher) => aliasNextRoom)   // TODO
    Alias.alias("g ([0-9]*)", (m: Matcher) => aliasGoto(m.group(1).toLong))
  }

  def aliasGoto(id: Long): Unit = {

  }

  def aliasInfo = {
    Game.echo(s"current room: ${currentRoom.id}\n")
  }

  def aliasList = {
    roomsByZone(currentRoom.zoneName).foreach { r=> Game.echo(s"$r\n") }
  }

  def aliasNextRoom = {
    val pather = patherCache.get(current)

    val rooms = pather.rooms.filter { r=> r.exits.values.exists { e=> e.to.isEmpty }}

    if(rooms.isEmpty) {
      Game.echo("There are no rooms with undiscovered exits.\n")
    } else {
      val target = rooms.reduce { (a, b) =>
        val ap = pather.pathTo(a)
        val bp = pather.pathTo(b)
        if (ap.weight < bp.weight) a else b
      }
      val exit = target.exits.values.filter { e => e.to.isEmpty }.head
      val path = pather.pathTo(target) + exit
      path.runTo
    }
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
               )

case class Exit(name: String, fromId: Long, toId: Long, maze: Boolean = false, door: Boolean = false,
                locked: Boolean = false, pathable: Boolean = true, weight: Int = 1) {
  def from = Room(fromId)
  def to = Room(toId)

}