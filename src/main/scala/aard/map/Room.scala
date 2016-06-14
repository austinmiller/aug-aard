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
  val weight = exits.map {_.weight}.sum
  def + (exit: Exit) : Path = Path(exits :+ exit)
  def + (path: Path) : Path = Path(exits ::: path.exits)

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

  override def toString: String = {
    weight + "++" + exits.map {_.name}.mkString(",")
  }
}

object InfinitePath extends Path(List[Exit]()) {
  override val weight: Int = Int.MaxValue
}

class ZonePather(val room: Room) {

  val links = mutable.Map[Exit,Path]()

  val time = {
    val t = System.currentTimeMillis()
    val unvisited = mutable.Set[Exit]()

    def nextToVisit: Exit = {
      val e : Exit = unvisited.reduceLeft { (a, b) =>
        if (links(a).weight < links(b).weight) a else b
      }
      unvisited.remove(e)
      e
    }

    def addToUnvisited(exit: Exit): Unit = {
      if(unvisited.contains(exit)) return
      unvisited.add(exit)
      links(exit) = InfinitePath
      Room.zoneLinks(exit.to.get.zoneName).foreach {e=>addToUnvisited(e) }
    }

    Room.zoneLinks(room.zoneName).foreach {e=>
      addToUnvisited(e)
      room.pather.pathTo(e.from.get).foreach(path=>links(e) = path + e)
    }

    while(!unvisited.isEmpty) {
      val e = nextToVisit

      val basePath = links(e)
      val from = e.to.get
      val pather = Room.patherCache.get(from)
      val zls = Room.zoneLinks(from.zoneName)

      for(zl <- zls) {
        val to = zl.from.get
        val pathToTarget = pather.pathTo(to)

        if(pathToTarget.isDefined) {
          // if weight == 0 then to is unreachable unless from == to
          if (pathToTarget.get.weight > 0 || to == from) {
            val newPath = basePath + pathToTarget.get + zl // the path to the new zone goes through the zl
            if (newPath.weight < links(zl).weight) links(zl) = newPath
          }
        }
      }
    }
    System.currentTimeMillis() - t
  }

  Game.echo(s"zone pather created for room ${room.id} in $time ms\n")

  def pathsTo(zoneName: String) : Iterable[Path] = links.filter{ x => x._1.to.get.zoneName == zoneName}.values

  def shortest(paths: Iterable[Path]) = Some(paths).filter(_.nonEmpty).map(_.minBy(_.weight))

  def pathTo(zoneName: String) : Option[Path] = shortest(pathsTo(zoneName))

  def pathTo(target: Room) : Option[Path] = {
    shortest(pathsTo(target.zoneName).flatMap { path =>
      path.exits.last.to.get.pather.pathTo(target).map(path + _)
    } ++ room.pather.pathTo(target))
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
      if (dist < Integer.MAX_VALUE) {

        for (exit <- r.exits.values if exit.to.isDefined && exit.pathable) {
          val to = exit.to.get
          if (rooms.exists {
            _ == to
          }) {
            val alt = dist + exit.weight
            val p = links(to)

            if (alt < p.dist) links(to) = Link(Some(r), Some(exit), alt)
          }
        }
      }
    }

    System.currentTimeMillis() - t
  }

  Game.echo(s"pather created for ${room.id} in $time ms\n")

  def pathTo(target: Room) : Option[Path] = {
    val seq = List.newBuilder[Exit]
    val link = links.get(target)
    var i = 0

    def addPath(link: Link): Unit = {
      link.prev.foreach {p => if(p != room) addPath(links(p)) }
      link.exit.foreach {e=> seq += e}
    }

    if(link.isEmpty) return None
    if(link.get.dist == Int.MaxValue) return None

    link.foreach {l => addPath(l)}
    Some(Path(seq.result()))
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
    override def load(key: Room): Pather = new Pather(key,zoneRooms(key.zoneName))
  })

  val zonePatherCache: LoadingCache[Room, ZonePather] = CacheBuilder.newBuilder().maximumSize(1000).build(new CacheLoader[Room,ZonePather]() {
    override def load(key: Room): ZonePather = new ZonePather(key)
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
    zoneRooms(room.zoneName).foreach {r=>patherCache.invalidate(room)}
    zonePatherCache.invalidateAll()
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

  def zoneRooms(zoneName: String = currentRoom.zoneName): Iterable[Room] = rooms.values.filter {_.zoneName == zoneName}
  def zoneLinks(zoneName: String = currentRoom.zoneName): Iterable[Exit] = zoneRooms(zoneName).flatMap { _.externalExits }

  def load = {
    loadAll
    Alias.alias("r info",(m: Matcher) => aliasInfo)
    Alias.alias("r list",(m: Matcher) => aliasList)
    Alias.alias("r next", (m: Matcher) => aliasNextRoom)   // TODO
    Alias.alias("g ([0-9]*)", (m: Matcher) => aliasGoto(m.group(1).toLong))
    Alias.alias("r zls", (m: Matcher) => aliasZoneLinks)
    Alias.alias("r zp", (m: Matcher) => aliasZonePather)
    Alias.alias("gz (.*)", (m: Matcher) => aliasGotoZone(m.group(1)))
    Alias.alias("r zones", (m: Matcher) => aliasZones)
  }

  def aliasGotoZone(zoneName: String): Unit = {
    val shortestPath = zonePatherCache.get(currentRoom).pathTo(zoneName)
    if(shortestPath.isEmpty) {
      Game.echo(s"no path to $zoneName\n")
    } else {
      shortestPath.get.runTo
    }
  }


  def aliasZones(): Unit = {
    Game.header("zone names")
    rooms.values.map{_.zoneName}.toSet[String].foreach { s=>
      Game.echo(s"$s\n")
    }
  }

  def aliasZonePather(): Unit = {
    Game.header(s"zone pather cache from room ${current.id}")
    zonePatherCache.get(currentRoom).links.foreach { x =>
      Game.echo(s"${x._1.to.get.zoneName} -> ${x._2}\n")
    }
  }

  def aliasGoto(id: Long): Unit = {
    rooms.get(id) match {
      case Some(r) =>
        currentRoom.pathTo(r) match {
          case Some(p) => p.runTo
          case None => Game.echo(s"\nNo path to <$id>.\n")
        }
      case None => Game.echo(s"\nRoom <$id> isn't known\n")
    }
  }

  def aliasInfo = {
    val r = currentRoom
    Game.header(s"room <${r.id}>")
    val s = JsonUtil.prettyJson(r)
    Game.echo(s"$s\n")
  }

  def aliasList = {
    zoneRooms(currentRoom.zoneName).foreach { r=> Game.echo(s"$r\n") }
  }

  def aliasNextRoom = {
    //TODO refactor to be less inefficient
    val pather = patherCache.get(current)

    pather.rooms.filter { r=> r.exits.values.exists { e=> e.to.isEmpty }} match {
      case Nil => Game.echo("There are no rooms with undiscovered exits.\n")
      case rooms =>
        Some(rooms.flatMap(pather.pathTo(_))).filter(_.nonEmpty).map(_.minBy(_.weight)) match {
          case Some(p) =>
            val path = p + p.exits.last.to.get.unknownExits.head
            path.runTo
          case None => Game.echo(s"No path found to any rooms with undiscovered exits.\n")
        }
    }
  }



  def aliasZoneLinks = {
    Game.header("zone links")
    zoneLinks().foreach { zl =>
      Game.echo(s"room ${zl.from.get.id} -> ${zl.to.get.zoneName}\n")
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
               ) {

  def externalExits: Iterable[Exit] = {
    exits.values.filter { e=>
      e.to.exists { to=>
        to.zoneName != zoneName
      } && e.pathable
    }
  }

  def pather = Room.patherCache.get(this)
  def zonePather = Room.zonePatherCache.get(this)

  def pathTo(target: Room) = zonePather.pathTo(target)
  def unknownExits = exits.values.filter(_.to.isEmpty)

}

case class Exit(name: String, fromId: Long, toId: Long, maze: Boolean = false, door: Boolean = false,
                locked: Boolean = false, pathable: Boolean = true, weight: Int = 1) {
  def from = Room(fromId)
  def to = Room(toId)

}