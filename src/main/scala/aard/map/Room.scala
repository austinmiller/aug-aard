package aard.map

import java.util.regex.Matcher

import aard.db.Store
import aard.script.GmcpRoom
import aug.script.{Alias, Game}
import aug.util.JsonUtil
import com.google.common.cache.{CacheBuilder, CacheLoader, LoadingCache}

import scala.collection.{Set, mutable}

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

object Path {
  val recall = 32418
  val quest = 32458
  val campaign = 32611

  def shortest(paths: Iterable[Path]) : Option[Path] = Some(paths).filter(_.nonEmpty).map(_.minBy(_.weight))

  def recallTo(zoneName : String) = {
    Room(recall).flatMap{_.zonePather.pathTo(zoneName)}.map { p=> Path(Exit("recall",-1,-1,weight=10) :: p.exits)}
  }

  def recallTo(target: Room): Option[Path] = {
    Room(recall).flatMap{_.zonePather.pathTo(target)}.map { p=> Path(Exit("recall",-1,-1,weight=10) :: p.exits)}
  }

  def to(zoneName: String) = {
    shortest(
      List(Room.current.zonePather.pathTo(zoneName),
        recallTo(zoneName)).flatten
    )
  }

  def to(target: Room) = {
    shortest(
      List(Room.current.zonePather.pathTo(target),
        recallTo(target)).flatten
    )
  }
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
        run += exit
      } else if(exit.name.length > 1) {
        addRunTo
        cmds += exit.name
      } else {
        run += exit
      }
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
  override def toString : String = "infpath"
}

class ZonePather(val room: Room) {

  val links = mutable.Map[Exit,Path]()

  private def setPath(exit: Exit,path: Path) = {
    path.exits match {
      case Nil =>
      case l =>
        val fromId = l.head.fromId
        if(fromId != room.id) throw new Exception(s"$fromId != ${room.id}")
        links(exit) = path
    }
  }

  private def printLinks = {
    Game.header("links")
    links.foreach {x=>
      val zfrom =x._1.from.get.zoneName
      val zto =x._1.to.get.zoneName
      val p = x._2
      Game.echo(s"zone ${zfrom}->$zto via $p\n")
    }
    Game.echo("\n")
  }

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
      room.pather.pathTo(e.from.get).foreach(path=>setPath(e,path + e))
    }



    while(!unvisited.isEmpty) {
      val e = nextToVisit

      links(e) match {
        case InfinitePath =>
        case basePath =>
          val from = e.to.get
          val zls = Room.zoneLinks(from.zoneName)

          for(zl <- zls) {
            val to = zl.from.get
            from.pather.pathTo(to) match {
              case None =>
              case pathToTarget =>
                if (pathToTarget.get.weight > 0 || to == from) {
                  val newPath = basePath + pathToTarget.get + zl // the path to the new zone goes through the zl
                  if (newPath.weight < links(zl).weight) {
                    setPath(zl,newPath)
                  }
                }
            }
          }
      }
    }
    System.currentTimeMillis() - t
  }

  if(time>1000) Game.echo(s"zone pather created for room ${room.id} in $time ms\n")

  def pathsTo(zoneName: String) : Iterable[Path] = links.filter{
    x => x._1.to.get.zoneName == zoneName && x._2.weight != InfinitePath.weight
  }.values

  def pathTo(zoneName: String) : Option[Path] = Path.shortest(pathsTo(zoneName))

  def pathTo(target: Room) : Option[Path] = {
    Path.shortest(pathsTo(target.zoneName).flatMap { path =>
      path.exits.last.to.get.pather.pathTo(target).map(path + _)
    } ++ room.pather.pathTo(target))
  }

}

class Pather(val room: Room, val rooms: Set[Room]) {
  private val paths = mutable.Map[Room,Option[Path]]()

  private def printPaths = {
    Game.header("links")
    paths.foreach {x=>
      val r = x._1
      val ps = x._2.map(_.toString).getOrElse {"Empty"}
      Game.echo(s"<${r.id}> $ps\n")
    }
    Game.echo("\n")
  }

  val time = {
    val t = System.currentTimeMillis()
    val unvisited = mutable.Set[Room]()

    rooms.foreach{r=>
      paths.put(r,None)
      unvisited.add(r)
    }

    paths.put(room,Some(Path(List[Exit]())))

    def nextToVisit: Option[Room] = {
      Some(unvisited.filter(paths(_).isDefined)).filter(_.nonEmpty).map(_.minBy(paths(_).get.weight)) map { r=>
        unvisited.remove(r)
        r
      }
    }

    while (!unvisited.isEmpty) {
      nextToVisit match {
        case None => unvisited.clear()
        case Some(r) =>
          val bp = paths(r).get
          r.exits.values.filter(e=>e.to.isDefined && e.pathable).foreach { e=>
            val to = e.to.get
            if(rooms.contains(to)) {
              val newPath = bp + e
              val op = paths(to)
              if(op.isEmpty || newPath.weight < op.get.weight) {
                paths(to) = Some(newPath)
              }
            }
          }
      }
    }

    System.currentTimeMillis() - t
  }

  if(time>1000) Game.echo(s"pather created for ${room.id} in $time ms\n")

  def pathTo(target: Room) : Option[Path] = paths.get(target).flatten
}

case class RList(rooms: Array[Room]) {

  def runTo(i: Int): Unit = {
    rooms match {
      case Array() => Game.echo("\nThe rlist is empty.\n")
      case x =>
        if(i < 0 || i >= rooms.size) {
          Game.echo(s"\nERROR: $i is out of bounds [0,${rooms.size-1}]\n")
        } else {
          Game.echo(s"\nTrying to run to <${x(i).id}>\n")
          Path.to(x(i)) match {
            case Some(p) => p.runTo
            case None =>
          }
        }
    }

  }

  def print(begin: Int = 0, end: Int = 20) = {
    Game.header("room list")
    for(i <- begin until end if i < rooms.size) {
      val r = rooms(i)
      Game.echo(f"$i%3d] [${r.zoneName}%15s] ${r.name}%s\n")
    }
  }
}

object Room {

  val path = "room"
  val ext = ".room"
  val rooms = mutable.Map[Long,Room]()
  val roomsByName = mutable.Map[String,Iterable[Room]]()

  private var currentRoom : Room = null
  private var rlist : Option[RList] = None
  private var specialExit = Option[SpecialExit](null)

  def apply(id: Long) = rooms.get(id)

  def current : Room = synchronized { currentRoom }

  val patherCache: LoadingCache[Room, Pather] = CacheBuilder.newBuilder().maximumSize(1000).build(new CacheLoader[Room,Pather]() {
    override def load(key: Room): Pather = new Pather(key,zoneRooms(key.zoneName).toSet)
  })

  val zonePatherCache: LoadingCache[Room, ZonePather] = CacheBuilder.newBuilder().maximumSize(1000).build(new CacheLoader[Room,ZonePather]() {
    override def load(key: Room): ZonePather = new ZonePather(key)
  })


  private def loadAll = {
    Store.loadAll[Room](path,ext).foreach(r=>rooms(r.id)=r)
  }

  def setRoom(gmcp: GmcpRoom) : Unit = synchronized {
    specialExit foreach {se=>
      if(gmcp.num != se.from.id) {
        val newexits = se.from.exits + ((se.name)->Exit(se.name,se.from.id,gmcp.num))
        val nr = se.from.copy(exits = newexits)
        save(nr)
      }
    }

    currentRoom = rooms.getOrElse(gmcp.num, {
      val r = fromGmcp(gmcp)
      save(r)
      r
    })
  }

  def setRList(rooms: Seq[Room]) = synchronized {
    rlist = Some(RList(rooms.toSet.toArray))
  }

  def printRList(begin: Int = 0, end: Int = 20) = rlist map (_.print(begin,end))

  def save(room: Room) = synchronized {
    zoneRooms(room.zoneName).foreach {r=>patherCache.invalidate(room)}
    zonePatherCache.invalidateAll()
    rooms(room.id) = room

    val nm = rooms.values.groupBy(_.name)

    roomsByName ++= rooms.values.groupBy(_.name)

    Zone.register(room.zoneName)
    Store.save(s"$path/${room.id}.room",room)
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
    Alias.alias("r info",(m: Matcher) => aliasInfo(currentRoom.id))
    Alias.alias("r info ([0-9]*)",(m: Matcher) => aliasInfo(m.group(1).toLong))
    Alias.alias("r path ([0-9]*) ([0-9]*)",(m: Matcher) => aliasPath(m.group(1).toLong,m.group(2).toLong))
    Alias.alias("r list",(m: Matcher) => aliasList)
    Alias.alias("rn", (m: Matcher) => aliasNextRoom)
    Alias.alias("r exit (.*)", (m: Matcher) => aliasSpecialExit(m.group(1)))
    Alias.alias("r cancelexit", (m: Matcher) => specialExit = None)
    Alias.alias("r door (.*)", (m: Matcher) => aliasAddDoor(m.group(1)))
    Alias.alias("r zls", (m: Matcher) => aliasZoneLinks)
    Alias.alias("r zp", (m: Matcher) => aliasZonePather)
    Alias.alias("r zones", (m: Matcher) => aliasZones)
    Alias.alias("r find (.*)", (m: Matcher) => aliasFind(m.group(1)))

    Alias.alias("g recall", (m: Matcher) => runTo(Path.recall))
    Alias.alias("g quest", (m: Matcher) => aliasGotoQuest)
    Alias.alias("g zone (.*)", (m: Matcher) => aliasGotoZone(m.group(1)))
    Alias.alias("g #([0-9]*)", (m: Matcher) => aliasGoto(m.group(1).toLong))
    Alias.alias("g ([0-9]*)", (m: Matcher) => aliasRListGoto(m.group(1).toInt))

    roomsByName ++= rooms.values.groupBy(_.name)
  }

  def withRoom(id: Long, f: Room => Unit) = {
    Room(id) match {
      case None => Game.echo(s"\nCould not locate room <${id}>\n")
      case Some(room) => f(room)
    }
  }

  def runTo(id: Long) : Unit = {
    withRoom(id,r=>runTo(r))
  }

  def runTo(r: Room) : Unit= {
    Path.to(r) match {
      case None => Game.echo(s"\nNo path to <${r.id}>\n")
      case Some(p) => p.runTo
    }
  }

  def runTo(zoneName: String) : Unit = {
    Path.to(zoneName) match {
      case None => Game.echo(s"\nNo path to <$zoneName>\n")
      case Some(p) => p.runTo
    }
  }

  def aliasAddDoor(exitName: String) = {
    current.withExit(exitName) { e=>
      val ne = e.copy(door = true)
      val nm = current.exits + ((exitName)->ne)
      val nr = current.copy(exits=nm)

      Game.echo(s"\nAdded door to $ne\n")
      currentRoom = nr
      save(current)
    }
  }

  def aliasGotoQuest = runTo(Path.quest)

  def aliasSpecialExit(exitName: String): Unit = {
    current.exits.get(exitName) match {
      case Some(_) => Game.echo(s"\nExit $exitName is already defined\n")
      case _ =>
        Game.send(exitName)
        specialExit= Some(SpecialExit(current,exitName))
    }
  }

  def aliasPath(rid1: Long, rid2: Long) = {
    rooms.get(rid1) match {
      case None => Game.echo(s"\nDidn't find room <$rid1>.\n")
      case Some(r1) =>
        rooms.get(rid2) match {
          case None => Game.echo(s"\nDidn't find room <$rid2>.\n")
          case Some(r2) =>
            r1.zonePather.pathTo(r2) match {
              case None => Game.echo("\nno path found.\n")
              case Some(path) => Game.echo(s"\npath is $path\n")
            }
        }
    }
  }

  def aliasRListGoto(index: Int) = rlist map (_.runTo(index))

  def aliasFind(sub: String) = synchronized {
    rlist = Some(RList(rooms.values.filter(_.name.toLowerCase.contains(sub)).toArray))
    rlist map (_.print())
  }

  def aliasGotoZone(zoneName: String): Unit = runTo(zoneName)

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
        Path.to(r) match {
          case Some(p) => p.runTo
          case None => Game.echo(s"\nNo path to <$id>.\n")
        }
      case None => Game.echo(s"\nRoom <$id> isn't known\n")
    }
  }

  def aliasInfo(id: Long) = {
    rooms.get(id) match {
      case None => Game.echo(s"\nRoom <$id> was not found.\n")
      case Some(room) =>
        Game.header(s"room <${room.id}>")
        val s = JsonUtil.prettyJson(room)
        Game.echo(s"$s\n")
        Game.header("exits")
        room.exits.values.foreach {e=>
          Game.echo(s"${e.name} - ${e.toId} - ${e.to.isDefined}\n")
        }

    }
  }

  def aliasList = {
    zoneRooms(currentRoom.zoneName).foreach { r=> Game.echo(s"$r\n") }
  }

  def aliasNextRoom = {
    val pather = patherCache.get(current)

    pather.rooms.filter { r=> r.exits.values.exists { e=> e.to.isEmpty }}.toSeq match {
      case Seq() => Game.echo("There are no rooms with undiscovered exits.\n")
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

case class SpecialExit(from: Room, name: String)

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
  def unknownExits = exits.values.filter(_.to.isEmpty)
  def zone = Zone(zoneName).get // if this doesn't exist, we fucked up

  def withExit(name: String)(f: Exit => Unit) = {
    exits.get(name) match {
      case None => Game.echo(s"\nExit $name not found on room <$id>\n")
      case Some(exit) => f(exit)
    }
  }
}

case class Exit(name: String, fromId: Long, toId: Long, maze: Boolean = false, door: Boolean = false,
                locked: Boolean = false, pathable: Boolean = true, weight: Int = 1) {
  def from = Room(fromId)
  def to = Room(toId)
}