package aard.map

import aard.player.{Player, Portal}
import aug.script.Game

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
  val sendhia = 20286
  val vidblain = 11910

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