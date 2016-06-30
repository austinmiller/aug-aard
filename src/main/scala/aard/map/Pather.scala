package aard.map

import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}

import aard.db.Store
import aard.player.{Player, Portal}
import aug.script.Game

import scala.collection.mutable
import aard.script.Shortcuts._

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}


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
  val sendhia = 20286
  val vidblain = 11910
  val amulet = 29364

  val recallExit = Exit("recall",-1,-1,weight=500)

  val empty = Path(Nil)

  @volatile
  private[map] var zonePaths = new ZonePaths(List.empty)
  @volatile
  private var version = 0
  private var running = new AtomicBoolean(true)
  private val thread = new Thread(new Runnable() {
    override def run(): Unit = {
      compileZonePaths
    }
  })

  def dirty = version += 1

  private def compileZonePaths = {

    Try {
      ZonePaths.load
    } match {
      case Success(zp) =>
        info("ZonePaths loaded")
        zonePaths = zp
      case Failure(e) => Game.handleException(e)
    }

    var compiledVersion = 0
    while(running.get) {
      if(version > compiledVersion) {
        compiledVersion = version
        val zp = ZonePaths.construct(Room.allRooms)
        zonePaths = zp
        ZonePaths.save(zp)
      }

      Try {
        Thread.sleep(1000)
      }
    }
  }

  def load = {
    thread.start
  }

  def close = {
    running.set(false)
    thread.interrupt()
  }

  def shortest(paths: Iterable[Path]) : Option[Path] = Some(paths).filter(_.nonEmpty).map(_.minBy(_.weight))

  def path(room: Room, target: Room) = zonePaths.path(room,target)

  def recallTo(zoneName : String) = {
    Room(recall).flatMap{zonePaths.path(_,zoneName)}.map { p=> Path(recallExit :: p.exits)}
  }

  def recallTo(target: Room): Option[Path] = {
    Room(recall).flatMap{zonePaths.path(_,target)}.map { p=> Path(recallExit :: p.exits)}
  }

  def portalTo(target: Room): Option[Path] = {
    toPortalable.flatMap { bp=>
      val addPath = Player.current.flatMap { player=>
        shortest(player.availablePortals.map { p=>
          zonePaths.path(p.to.get,target).map(path=>Path(path.exits,Some(p)))
        }.toList.flatten)
      }
      addPath.map(ap=> ap.copy(prepath=bp.exits))
    }
  }

  def portalTo(zoneName: String): Option[Path] = {
    Player.current.flatMap { player=>
      shortest(player.availablePortals.map { p=>
        zonePaths.path(p.to.get,zoneName).map(path=>Path(path.exits,Some(p)))
      }.toList.flatten)
    }
  }

  def to(zoneName: String) : Option[Path] = {
    Room.withRoom() { r =>
      shortest(
        List(zonePaths.path(r,zoneName),
          recallTo(zoneName),
          portalTo(zoneName)).flatten
      )
    }.flatten
  }

  def to(target: Room) : Option[Path] = {
    Room.withRoom() { r =>
      shortest(
        List(zonePaths.path(r,target),
          r.to(target),
          recallTo(target),
          portalTo(target)).flatten
      )
    }.flatten
  }

  def toPortalable : Option[Path] = {
    Room.withRoom[Option[Path]]() {cur=>
      val rv: Option[Path] = shortest(cur.paths.filter{ x=>
        x._1.portalable
      }.values.toList.flatten)
      rv
    }.flatten
  }

  def forPath(target: Room)(f: Path=>Unit): Unit = {
    to(target) match {
      case None => Game.echo(s"\nNo path to <${target.id}>\n")
      case Some(path) => f(path)
    }
  }

  def forPath(id: Long)(f: Path=>Unit): Unit = {
    Room.forRoom(Some(id)) { r =>forPath(r)(f) }
  }
}

case class Path(val exits: List[Exit], portal: Option[Portal]=None, prepath: List[Exit] = List.empty) {
  def <(path: Path) = weight < path.weight

  val weight = prepath.map{_.weight}.sum + exits.map {_.weight}.sum + portal.map(p=>5).getOrElse(0)
  def + (exit: Exit) : Path = Path(exits :+ exit,portal)
  def + (path: Path) : Path = Path(exits ::: path.exits,portal)
  val last = exits.lastOption
  val head = exits.headOption
  val startRoom : Option[Room] = exits.headOption.flatMap{_.from}
  val endRoom : Option[Room] = exits.lastOption.flatMap{_.to}

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
      if(exit.hidden != null && exit.hidden.length > 0) {
        addRunTo
        cmds += s"open ${exit.hidden}"
        run += exit
      } else if(exit.door) {
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

//  if(time>1000)
    println(s"pather created for ${room.id} in $time ms")

  def pathTo(target: Room) : Option[Path] = paths.get(target).flatten
}

class AStarPather(val room: Room, heuristic: (Room,Room)=>Int) {

  def to(target: Room) : Option[Path] = {
    (target.zoneName == room.zoneName) match {
      case false => None
      case true =>
        val time = System.currentTimeMillis()
        val rv = calc(target)
        val diff = System.currentTimeMillis() - time
        if(diff > 50) println(s"A* (${room.id}->${target.id}) took $diff ms")
        rv
    }
  }

  private def calc(target: Room) : Option[Path] = {
    val closed = mutable.Set[Room]()
    val open = mutable.Set(room)
    val paths = mutable.Map[Room,Path]()
    paths(room) = Path(List.empty)
    val estimates = mutable.Map[Room,Int]()
    estimates(room) = heuristic(room,target)

    while(open.nonEmpty) {
      val cur: Room = open.minBy(r=>estimates(r))
      if(cur.id == target.id) return Some(paths(target))
      open.remove(cur)
      closed.add(cur)
      cur.exits.values.filter { e=>
        e.to.isDefined && e.to.get.zoneName == room.zoneName
      }.foreach{e=>
        val neighbour = e.to.get
        if(!closed.contains(neighbour)) {
          val tentativePath = paths(cur).copy(exits=paths(cur).exits :+ e)
          if(!open.contains(neighbour)) {
            open.add(neighbour)
            estimates(neighbour) = heuristic(neighbour,target)
            paths(neighbour) = tentativePath
          } else if(tentativePath < paths(neighbour)  ) {
            paths(neighbour) = tentativePath
            estimates(neighbour) = tentativePath.weight + heuristic(neighbour,target)
          }
        }
      }
    }
    None
  }

}

object ZonePaths {
  val savePath = "zonePaths.paths"

  case class Link(start: Long, end: Long)
  def construct(rooms: Iterable[Room]) : ZonePaths = {
    val time = System.currentTimeMillis()
    val zoneLinks: Set[Exit] = rooms.flatMap {_.externalExits}.toSet
    val zlsByZone = zoneLinks.groupBy(_.from.get.zoneName)
    val edges: Set[Path] = zoneLinks.flatMap { e=>
      val to = e.to.get
      val rv: Set[Path] = zlsByZone.get(to.zoneName).map{
        _.filter(_.fromId != e.toId).flatMap { ex=> to.to(ex.from.get).map(_ + ex)}
      }.getOrElse(Set.empty)
      rv ++ Some(e.toPath)
    }


    val links = mutable.Map[Link,Path]()

    zoneLinks.foreach{e=>
      links(Link(e.fromId,e.toId)) = e.toPath
    }

    edges.foreach{edge=>
      links(Link(edge.head.get.fromId,edge.last.get.toId)) = edge
    }

    val keys = links.keys.flatMap(l=>List(l.start,l.end))

    def weight(link: Link) = links.get(link).map(_.weight).getOrElse(Int.MaxValue/2 - 1)

    keys.toList.zipWithIndex.foreach{ x =>
      val (k,i) = x
      for(i<-keys;j<-keys) {
        if(i != j) {
          val ij = Link(i, j)
          val ik = Link(i, k)
          val kj = Link(k, j)
          if (weight(ij) > weight(ik) + weight(kj)) {
            val np = links(ik) + links(kj)
            val fz = np.last.get.to.get.zoneName
//            if(np.exits.filter(_.to.get.zoneName == fz).size == 1)
              links(ij) = np
          }
        }
      }
      if(i%50==0) println(f"percent %%${(i * 100.0)/keys.size}%2.1f")
    }

    val diff = System.currentTimeMillis() - time
    val paths = links.values.toList.filter(p=>p.endRoom.get.coords.cont == false && !ddEnd(p.exits))

    println(s"Construction of zone paths took $diff ms with ${paths.size} paths")
    new ZonePaths(paths)
  }

  def load() : ZonePaths = {
    val lists: List[List[Exit]] = Store.loadOrElse[List[List[Exit]]](savePath,List[List[Exit]]())
    new ZonePaths(lists.map{le=>Path(le)})
  }

  // is the penultimate element the same zn as the final element?
  @tailrec
  def ddEnd(exits: List[Exit]) : Boolean = {
    exits match {
      case Nil => false
      case List(a,b,c) => a.to.get.zoneName == c.to.get.zoneName
      case x :: xs => ddEnd(xs)
    }
  }

  def save(zonePaths: ZonePaths) = {
    val pathsToSave: List[List[Exit]] = zonePaths.paths.map(_.exits)
    Store.save(savePath,pathsToSave)
  }

  def main(args: Array[String]) : Unit = {



    Room.load
    Zone.load
    val zp = ZonePaths.construct(Room.allRooms)
  }
}

class ZonePaths(val paths: List[Path]) {

  private val map: Map[(String, String), List[Path]] = paths.groupBy { p=>
    (p.head.get.from.get.zoneName,p.last.get.to.get.zoneName)
  }

  def path(start: Room, target: Room) : Option[Path] = {
    val time = System.currentTimeMillis()
    val xs: List[Path] = paths(start.zoneName,target.zoneName).flatMap { zonePath=>
      start.to(zonePath.startRoom.get) match {
        case None =>
          println("no starting path")
          None
        case Some(startPath) => zonePath.endRoom.get.to(target) match {
          case None =>
            println("no ending path")
            None
          case Some(endPath) => Some(startPath + zonePath + endPath)
        }
      }
    }
    val diff = System.currentTimeMillis() - time

    println(s"Path resolution took $diff ms")
    Path.shortest(xs)
  }

  def path(start: Room, zoneName: String) : Option[Path] = {
    val time = System.currentTimeMillis()

    val xs: List[Path] = paths.filter{ p=>
      p.endRoom.get.zoneName == zoneName &&
      p.startRoom.get.zoneName == start.zoneName
    }.flatMap { zonePath=>
      start.to(zonePath.startRoom.get) match {
        case None => None
        case Some(startPath) => Some(startPath + zonePath)
      }
    }
    val diff = System.currentTimeMillis() - time

    println(s"Path resolution took $diff ms")
    Path.shortest(xs)
  }


  def paths(start: String,end: String) : List[Path] = map.getOrElse((start,end),Nil)
}