package aard.map

import java.awt.Color
import java.util.regex.Matcher

import aard.db.Store
import aard.script.GmcpRoom
import aug.script.{Alias, Game, Trigger}
import aug.util.JsonUtil
import com.google.common.cache.{CacheBuilder, CacheLoader, LoadingCache}

import scala.collection.mutable


case class RList(rooms: Array[Room]) {

  var nextIndex = 0

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

  def exhausted = nextIndex >= rooms.size

  def pathToNext : Option[Path] = {
    if(nextIndex >= rooms.size) {
      None
    } else {
      val p = Path.to(rooms(nextIndex))
      nextIndex += 1
      p
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
  val ext = ".zone"
  val rooms = mutable.Map[Long,Room]()
  val roomsByName = mutable.Map[String,Iterable[Room]]()
  private var current : Option[Room] = None
  private var rlist : Option[RList] = None
  private var specialExit = Option[Exit](null)
  private var mazeExit = Option[Exit](null)

  def apply(id: Long) = rooms.get(id)

  val patherCache: LoadingCache[Room, Pather] = CacheBuilder.newBuilder().maximumSize(1000).build(new CacheLoader[Room,Pather]() {
    override def load(key: Room): Pather = new Pather(key,zoneRooms(key.zoneName).toSet)
  })

  val zonePatherCache: LoadingCache[Room, ZonePather] = CacheBuilder.newBuilder().maximumSize(1000).build(new CacheLoader[Room,ZonePather]() {
    override def load(key: Room): ZonePather = new ZonePather(key)
  })

  val portalBlockTrigger = Trigger.trigger("Magic walls bounce you back.",m=> {
    withRoom() { r =>
      current = Some(r.copy(portalable=false))
    }
  })


  private def loadAll = {
    Store.loadAll[List[Room]](path,ext).flatten.foreach(r=>rooms(r.id)=r)
  }

  def onRepop(): Unit = {
    deleteMazeExits()
  }

  def clearCaches() = {
    zonePatherCache.invalidateAll()
    patherCache.invalidateAll()
    Game.echo("\ncaches cleared\n",Some(Color.RED))
  }

  def deleteMazeExits(): Unit = {
    zoneRooms().foreach {r=>
      if(r.hasMazeExits) {
        save(r.deleteMazeExits())
      }
    }
    patherCache.invalidateAll()
  }

  def setRoom(gmcp: GmcpRoom) : Unit = synchronized {
    specialExit foreach {se=>
      se.from.foreach { from=>
        if(gmcp.num != from.id) {
          val ne = se.copy(toId = gmcp.num)
          val nr = from.addExit(ne)
          save(nr)
          specialExit = None
        }
      }

    }

    mazeExit foreach {me=>
      forRoom(Some(me.fromId)) {r=>
        save(r.addExit(me.copy(toId = gmcp.num)))
      }
      mazeExit = None
    }

    rooms.get(gmcp.num) match {
      case Some(room) => current = Some(room)
      case None =>
        val r = fromGmcp(gmcp)
        current = Some(r)
        save(r)
    }

    if(List[Option[Boolean]](
      addMazeExit("n",gmcp.exits.n),
      addMazeExit("e",gmcp.exits.e),
      addMazeExit("s",gmcp.exits.s),
      addMazeExit("w",gmcp.exits.w),
      addMazeExit("u",gmcp.exits.u),
      addMazeExit("d",gmcp.exits.d)
    ).exists(_ == Some(true))) {
      save(current.get)
    }
  }

  def addMazeExit(name: String, id: Long): Option[Boolean] = {
    withRoom() {r=>
      if(!r.hasExit(name) && id == -1) {
        current = Some(r.addExit(Exit(name,r.id,-1,maze=true)))
        true
      } else {
        false
      }
    }
  }

  def withRList()(f: RList => Unit) = {
    rlist match {
      case None => Game.echo("\nrlist is undefined.\n")
      case Some(rl) => f(rl)
    }
  }

  def setRList(rooms: Seq[Room]) : Unit = synchronized {
    rlist = Some(RList(rooms.toSet.toArray))
  }

  def setRList(name: String) : Unit = synchronized { setRList(roomsByName.getOrElse(name,List[Room]()).toSeq) }

  def printRList(begin: Int = 0, end: Int = 20) = rlist map (_.print(begin,end))

  def save(room: Room) : Unit = synchronized {

    if(room.id < 0) {
      Game.echo("\nrefusing to save, room.id is negative\n")
      return
    }

    rooms(room.id) = room

    val zn = room.zoneName
    val roomsToSave = zoneRooms(zn)

    zoneRooms.foreach {r=>patherCache.invalidate(room)}
    zonePatherCache.invalidateAll()

    roomsByName ++= rooms.values.groupBy(_.name)

    Zone.register(room.zoneName)
    Store.save(s"$path/$zn.zone",roomsToSave)
  }

  def saveAll = {
    rooms.values.groupBy(_.zoneName).map(x=> x._2.head).foreach(save(_))
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

  def zoneRooms(): List[Room] = withRoom() { r=>zoneRooms(r.zoneName)}.getOrElse(List[Room]())
  def zoneRooms(zoneName: String): List[Room] = rooms.values.filter {_.zoneName == zoneName}.toList
  def zoneLinks() : List[Exit] = withRoom() {r=>zoneLinks(r.zoneName)}.getOrElse(List[Exit]())
  def zoneLinks(zoneName: String): List[Exit] = zoneRooms(zoneName).flatMap { _.externalExits }


  def forRoom(id: Option[Long] = None)(f: Room => Unit) : Unit = {
    id.map(rid=>rooms.get(rid)).getOrElse(current) match {
      case None => Game.echo(s"\nCould not locate room <${id}>\n")
      case Some(room) => f(room)
    }
  }

  def withRoom[A](id: Option[Long] = None)(f: Room => A) : Option[A] = {
    id.map(rid=>rooms.get(rid)).getOrElse(current) match {
      case None =>
        Game.echo(s"\nCould not locate room <${id}>\n")
        None
      case Some(room) => Some(f(room))
    }
  }

  def runTo(id: Long) : Unit = {
    withRoom(Some(id))(r=>runTo(r))
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

  def load = {
    loadAll
    Alias.alias("r info",(m: Matcher) => aliasInfo(None))
    Alias.alias("r info ([0-9]*)",(m: Matcher) => aliasInfo(Some(m.group(1).toLong)))
    Alias.alias("r path ([0-9]*) ([0-9]*)",(m: Matcher) => aliasPath(m.group(1).toLong,m.group(2).toLong))
    Alias.alias("r list",(m: Matcher) => aliasList)
    Alias.alias("rn", (m: Matcher) => aliasNextRoom)
    Alias.alias("r exit (.*)", (m: Matcher) => aliasSpecialExit(m.group(1)))
    Alias.alias("r cancelexit", (m: Matcher) => specialExit = None)
    Alias.alias("r delete (.*)", (m: Matcher) => aliasDeleteExit(m.group(1)))
    Alias.alias("r door (.*)", (m: Matcher) => aliasAddDoor(m.group(1)))
    Alias.alias("r zls", (m: Matcher) => aliasZoneLinks)
    Alias.alias("r zp", (m: Matcher) => aliasZonePather)
    Alias.alias("r zones", (m: Matcher) => aliasZones)
    Alias.alias("r find (.*)", (m: Matcher) => aliasFind(m.group(1)))
    Alias.alias("r save all", m=> saveAll)
    Alias.alias("r nopath (.*)", m=> aliasNoPath(m.group(1)))
    Alias.alias("r hidden (w|n|e|s|u|d) (.*)",m=>aliasHiddenExit(m.group(1),m.group(2)))

    Alias.alias("cache clear",m=> clearCaches())

    Alias.alias("maze",m=>aliasMaze)
    Alias.alias("maze clear",m=>deleteMazeExits())

    Alias.alias("gr", (m: Matcher) => runTo(Path.recall))
    Alias.alias("gq", (m: Matcher) => aliasGotoQuest)
    Alias.alias("gcp", (m: Matcher) => runTo(Path.campaign))
    Alias.alias("gz (.*)", (m: Matcher) => aliasGotoZone(m.group(1)))
    Alias.alias("g #([0-9]*)", (m: Matcher) => aliasGoto(m.group(1).toLong))
    Alias.alias("g ([0-9]*)", (m: Matcher) => rlistGoto(m.group(1).toInt))
    Alias.alias("g vid",m=>{
      Path.forPath(Path.vidblain) {p=>
        p.runTo
        Game.send("enter hole")
      }
    })

    Alias.alias("g sendhia",m=>runTo(Path.sendhia))

    Alias.alias("qr",m=> {
      runTo(Path.quest)
      Game.send("quest request")
    })

    Alias.alias("qc",m=> {
      runTo(Path.quest)
      Game.send("quest complete")
    })

    Alias.alias("cr",m=> {
      runTo(Path.campaign)
      Game.send("campaign request")
    })

    Alias.alias("rt (.*)",m=>{
      runTo(Path.recall)
      Game.send(s"rt ${m.group(1)}")
      Game.send("where") // collect zone name, otherwise why are we using rt?
    })

    roomsByName ++= rooms.values.groupBy(_.name)
  }

  def aliasHiddenExit(name: String, hidden: String) = {
    forRoom() {r=>
      r.exits.get(name) match {
        case Some(_) => Game.echo(s"\nAlready has a '${name}' exit.\n")
        case None =>
          Game.send(s"open $hidden")
          Game.send(name)
          specialExit = Some(Exit(name, r.id,-1, hidden = hidden))
      }

    }
  }

  def aliasGameRunTo(arg: String) {

  }

  def aliasNoPath(s: String): Unit = {
    forRoom() {r=>
      r.withExit(s){ e=>
        val nr = r.addExit(e.copy(pathable = false))
        current = Some(nr)
        save(nr)
        Game.echo(s"\nSet exit <$s> to unpathable.\n")
      }
    }
  }

  def aliasMaze : Unit = {
    withRoom() { r=>
      if(r.hasUnknownMazeExits) {
        val me = r.unknownMazeExits.head
        Game.send(me.name)
        mazeExit = Some(me)
      } else {
        val mazeRooms: List[Path] = r.pather.paths.filter(_._1.hasUnknownMazeExits).values.flatten.toList
        mazeRooms match {
          case Nil => Game.echo("\nNo maze rooms are pathable from this room\n")
          case list => Path.shortest(mazeRooms).foreach {_.runTo}
        }
      }
    }

  }

  def aliasNextRoom = {
    forRoom() { cur=>
      cur.unknownExits match {
        case head :: xs => head.take
        case Nil =>
          cur.pather.paths.filter(_._1.hasUnknownExits).values.flatten.toList match {
            case Nil => Game.echo("\nNo paths found to rooms with unknown exists in this zone.\n")
            case list => Path.shortest(list).foreach {_.runTo}
          }
      }
    }

  }

  def aliasDeleteExit(exitName: String) = {
    forRoom() { cur =>
      cur.withExit(exitName) { e =>
        val nm = cur.exits - exitName
        val nr = cur.copy(exits = nm)
        current = Some(nr)
        Game.echo(s"\nDeleted exit $exitName\n")
        save(nr)
      }
    }
  }

  def aliasAddDoor(exitName: String) = {
    forRoom() { cur =>
      cur.withExit(exitName) { e =>
        val ne = e.copy(door = true)
        val nm = cur.exits + ((exitName) -> ne)
        val nr = cur.copy(exits = nm)

        Game.echo(s"\nAdded door to $ne\n")
        current = Some(nr)
        save(nr)
      }
    }
  }

  def aliasGotoQuest = runTo(Path.quest)

  def aliasSpecialExit(exitName: String): Unit = {
    forRoom() { cur =>
      cur.exits.get(exitName) match {
        case Some(_) => Game.echo(s"\nExit $exitName is already defined\n")
        case _ =>
          Game.send(exitName)
          specialExit = Some(Exit(exitName, cur.id,-1))
      }
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

  def rlistGoto(index: Int) = rlist map (_.runTo(index))

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
    forRoom() { cur =>
      Game.header(s"zone pather cache from room ${cur.id}")
      cur.zonePather.paths.foreach { x =>
        Game.echo(s"${x._1.to.get.zoneName} -> ${x._2}\n")
      }
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

  def aliasInfo(id: Option[Long]) = {
    withRoom(id) { room=>
      Game.header(s"room <${room.id}>")
      val s = JsonUtil.prettyJson(room)
      Game.echo(s"$s\n")

      Game.header("exits")
      room.exits.values.foreach {e=>
        Game.echo(s"${e.name} - ${e.toId} - ${e.to.isDefined}\n")
      }

      Game.header("external exits")
      room.externalExits.foreach {e=>
        Game.echo(s"${e.name} - ${e.toId} - ${e.to.isDefined}\n")
      }
    }
  }

  def aliasList = {
    forRoom() {r=> zoneRooms(r.zoneName).foreach { r=> Game.echo(s"$r\n") } }
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
  def hasUnknownExits: Boolean = exits.values.exists(e=>e.to.isEmpty && e.pathable && !e.maze)
  def unknownExits: List[Exit] = exits.values.filter(e=>e.to.isEmpty && e.pathable && !e.maze).toList

  def hasMazeExits: Boolean = exits.exists(_._2.maze)
  def hasUnknownMazeExits: Boolean = exits.exists(x=>x._2.maze && x._2.toId == -1)
  def mazeExits: List[Exit] = exits.values.filter(_.maze).toList
  def unknownMazeExits: List[Exit] = exits.values.filter(e=>e.maze && e.toId == -1).toList

  def externalExits: Iterable[Exit] = {
    exits.values.filter { e=>
      e.to.exists { to=>
        to.zoneName != zoneName
      } && e.pathable
    }
  }

  def pather = Room.patherCache.get(this)
  def zonePather = Room.zonePatherCache.get(this)
  def zone = Zone(zoneName).get // if this doesn't exist, we fucked up

  def withExit(name: String)(f: Exit => Unit) = {
    exits.get(name) match {
      case None => Game.echo(s"\nExit $name not found on room <$id>\n")
      case Some(exit) => f(exit)
    }
  }

  def removeExit(name: String) = copy(exits = exits.filter(_._2.name == name))
  def addExit(exit: Exit) = copy(exits = exits.filter(_._1 != name) + (exit.name->exit))
  def hasExit(name: String) = exits.get(name).isDefined
  def deleteMazeExits() = copy(exits=exits.filter(_._2.maze == false))
}

case class Exit(name: String, fromId: Long, toId: Long, maze: Boolean = false, door: Boolean = false,
                locked: Boolean = false, pathable: Boolean = true, weight: Int = 1, hidden: String = "") {
  def from = Room(fromId)
  def to = Room(toId)

  def commands : List[String] = {
    val xs = List.newBuilder[String]
    if(hidden != "") xs += "open hidden"
    if(door) xs += s"open $name"
    xs += name
    xs.result()
  }

  def take = {
    commands.foreach(Game.send)
  }
}