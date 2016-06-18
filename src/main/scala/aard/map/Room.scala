package aard.map

import java.util.regex.Matcher

import aard.db.Store
import aard.player.{Player, Portal}
import aard.script.GmcpRoom
import aug.script.{Alias, Game, Trigger}
import aug.util.JsonUtil
import com.google.common.cache.{CacheBuilder, CacheLoader, LoadingCache}

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

object Path {
  val recall = 32418
  val quest = 32458
  val campaign = 32614

  val recallExit = Exit("recall",-1,-1,weight=500)

  def shortest(paths: Iterable[Path]) : Option[Path] = Some(paths).filter(_.nonEmpty).map(_.minBy(_.weight))

  def recallTo(zoneName : String) = {
    Room(recall).flatMap{_.zonePather.pathTo(zoneName)}.map { p=> Path(recallExit :: p.exits)}
  }

  def recallTo(target: Room): Option[Path] = {
    Room(recall).flatMap{_.zonePather.pathTo(target)}.map { p=> Path(recallExit :: p.exits)}
  }

  def portalTo(target: Room): Option[Path] = {
    toPortalable.flatMap { bp=>
      val addPath = Player.current.flatMap { player=>
        shortest(player.availablePortals.map { p=>
          p.to.get.zonePather.pathTo(target).map(path=>Path(path.exits,Some(p)))
        }.toList.flatten)
      }
      addPath.map(ap=> ap.copy(prepath=bp.exits))
    }
  }

  def portalTo(zoneName: String): Option[Path] = {
    Player.current.flatMap { player=>
      shortest(player.availablePortals.map { p=>
        p.to.get.zonePather.pathTo(zoneName).map(path=>Path(path.exits,Some(p)))
      }.toList.flatten)
    }
  }

  def to(zoneName: String) : Option[Path] = {
    Room.withRoom() { r =>
      shortest(
        List(r.zonePather.pathTo(zoneName),
          recallTo(zoneName),
          portalTo(zoneName)).flatten
      )
    }.flatten
  }

  def to(target: Room) : Option[Path] = {
    Room.withRoom() { r =>
      shortest(
        List(r.zonePather.pathTo(target),
          recallTo(target),
          portalTo(target)).flatten
      )
    }.flatten
  }

  def toPortalable : Option[Path] = {
    Room.withRoom[Option[Path]]() {cur=>
      val rv: Option[Path] = shortest(cur.pather.paths.filter{ x=>
        x._1.portalable
      }.values.toList.flatten)
      rv
    }.flatten
  }
}

case class Path(val exits: List[Exit], portal: Option[Portal]=None, prepath: List[Exit] = List.empty) {
  val weight = prepath.map{_.weight}.sum + exits.map {_.weight}.sum + portal.map(p=>5).getOrElse(0)
  def + (exit: Exit) : Path = Path(exits :+ exit,portal)
  def + (path: Path) : Path = Path(exits ::: path.exits,portal)

  private def mapCmds(exits: List[Exit]) : List[String] = {
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
    cmds.result
  }

  def runTo = {
    mapCmds(prepath).foreach(Game.send(_))

    portal foreach { portal=>
      Player.forPlayer(){_.usePortal(portal.id) }
    }

    mapCmds(exits).foreach(Game.send(_))
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

  val paths = mutable.Map[Exit,Path]()

  private def setPath(exit: Exit,path: Path) = {
    path.exits match {
      case Nil =>
      case l =>
        val fromId = l.head.fromId
        if(fromId != room.id) throw new Exception(s"$fromId != ${room.id}")
        paths(exit) = path
    }
  }

  private def printLinks = {
    Game.header("links")
    paths.foreach { x=>
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
        if (paths(a).weight < paths(b).weight) a else b
      }
      unvisited.remove(e)
      e
    }

    def addToUnvisited(exit: Exit): Unit = {
      if(unvisited.contains(exit)) return
      unvisited.add(exit)
      paths(exit) = InfinitePath
      Room.zoneLinks(exit.to.get.zoneName).foreach {e=>addToUnvisited(e) }
    }

    Room.zoneLinks(room.zoneName).foreach {e=>
      addToUnvisited(e)
      room.pather.pathTo(e.from.get).foreach(path=>setPath(e,path + e))
    }



    while(!unvisited.isEmpty) {
      val e = nextToVisit

      paths(e) match {
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
                  if (newPath.weight < paths(zl).weight) {
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

  def pathsTo(zoneName: String) : Iterable[Path] = paths.filter{
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
  val paths = mutable.Map[Room,Option[Path]]()

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
  private var specialExit = Option[SpecialExit](null)
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

  def deleteMazeExits(): Unit = {
    zoneRooms().foreach {r=>
      if(r.hasMazeExits) {
        save(r.deleteMazeExits())
      }
    }
  }

  def setRoom(gmcp: GmcpRoom) : Unit = synchronized {
    specialExit foreach {se=>
      if(gmcp.num != se.from.id) {
        val newexits = se.from.exits + ((se.name)->Exit(se.name,se.from.id,gmcp.num))
        val nr = se.from.copy(exits = newexits)
        save(nr)
        specialExit = None
      }
    }

    mazeExit foreach {me=>
      if(gmcp.num != me.fromId) {
        forRoom(Some(me.fromId)) {r=>
          save(r.addExit(me.copy(toId = gmcp.num)))
        }
        mazeExit = None
      }
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

    Alias.alias("maze",m=>aliasMaze)
    Alias.alias("maze clear",m=>deleteMazeExits())

    Alias.alias("g recall", (m: Matcher) => runTo(Path.recall))
    Alias.alias("g q", (m: Matcher) => aliasGotoQuest)
    Alias.alias("g cp", (m: Matcher) => runTo(Path.campaign))
    Alias.alias("g zone (.*)", (m: Matcher) => aliasGotoZone(m.group(1)))
    Alias.alias("g #([0-9]*)", (m: Matcher) => aliasGoto(m.group(1).toLong))
    Alias.alias("g ([0-9]*)", (m: Matcher) => rlistGoto(m.group(1).toInt))

    roomsByName ++= rooms.values.groupBy(_.name)
  }

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
          specialExit = Some(SpecialExit(cur, exitName))
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

  def aliasNextRoom = {
    forRoom() { cur=>
      val pather = cur.pather

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
  def unknownExits = exits.values.filter(_.to.isEmpty)
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
                locked: Boolean = false, pathable: Boolean = true, weight: Int = 1) {
  def from = Room(fromId)
  def to = Room(toId)
}