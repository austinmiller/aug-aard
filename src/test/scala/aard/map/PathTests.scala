package aard.map

import org.scalatest._

import scala.annotation.tailrec

object PathTestUtils {
  def continuous(path: Path): Boolean = {
    @tailrec
    def go(exits: List[Exit]) : Boolean = {
      exits match {
        case Nil => true
        case List(a) => true
        case x :: xs => x.toId == xs.head.fromId && go(xs)
      }
    }
    go(path.exits)
  }

  def mockRoom(id: Long,zn: String, exits: Exit*): Unit = {
    val r = Room(id,"",zn,exits.map(e=>(e.name)->e).toMap,"",Coords(0,0,0,false))
    Room.rooms(r.id) = r
  }
}

class PathTests extends FunSuite {
  import PathTestUtils._

  test("empty path should be continuous") {
    assert(continuous(Path.empty))
  }

  test("single element path should be continuous") {
    assert(continuous(Path(List(Exit("",1,1)))))
  }

  test("double element path should be continuous") {
    val path = Path(List(
      Exit("",1,2),
      Exit("",2,3)
    ))
    assert(continuous(path))
  }

  test("bad path should not be continuous") {
    val path = Path(List(
      Exit("",1,2),
      Exit("",1,2)
    ))
    assert(continuous(path)==false)
  }

  test("triple element path should be continuous") {
    val path = Path(List(
      Exit("",1,2),
      Exit("",2,3),
      Exit("",3,4)
    ))
    assert(continuous(path))
  }


}

class MockZonePathTests extends FunSuite with BeforeAndAfterEach {
  import PathTestUtils._


  override def afterEach() {
    Room.rooms.clear
  }

  test("no rooms should have no paths") {
    val zp = ZonePaths.construct(Set.empty)
    assert(zp.paths.size == 0)
  }

  test("one room with no exits has no paths") {
    mockRoom(1,"z1")
    val zp = ZonePaths.construct(Room.rooms.values.toSet)
    assert(zp.paths.size == 0)
  }

  test("two rooms, one linked to the other") {
    mockRoom(1,"z1",Exit("s",1,2))
    mockRoom(2,"z2")
    val zp = ZonePaths.construct(Room.rooms.values.toSet)
    assert(zp.paths.size == 1)
    assert(zp.paths("z1","z2").size == 1)
    assert(zp.paths("z1","z2").head.exits.head.name == "s")
  }

  test("two rooms, linked to each other") {
    mockRoom(1,"z1",Exit("s",1,2))
    mockRoom(2,"z2",Exit("n",2,1))
    val zp = ZonePaths.construct(Room.rooms.values.toSet)
    assert(zp.paths.size == 2)
    assert(zp.paths("z1","z2").size == 1)
    assert(zp.paths("z1","z2").head.exits.head.name == "s")
    assert(zp.paths("z2","z1").size == 1)
    assert(zp.paths("z2","z1").head.exits.head.name == "n")
  }

  test("two rooms, linked to each other with extra exits") {
    mockRoom(1,"z1",Exit("s",1,2),Exit("n",1,2))
    mockRoom(2,"z2",Exit("n",2,1),Exit("w",2,2),Exit("q",2,5))
    val zp = ZonePaths.construct(Room.rooms.values.toSet)
    assert(zp.paths.size == 2)
    assert(zp.paths("z1","z2").size == 1)
    assert(zp.paths("z2","z1").size == 1)
    assert(zp.paths("z2","z1").head.exits.head.name == "n")
  }

  test("three rooms") {
    mockRoom(1,"z1",Exit("s",1,2),Exit("n",1,2))
    mockRoom(2,"z2",Exit("n",2,1),Exit("w",2,2),Exit("q",2,5))
    mockRoom(5,"z2")
    val zp = ZonePaths.construct(Room.rooms.values.toSet)
    assert(zp.paths.size == 2)
    assert(zp.paths("z1","z2").size == 1)
    assert(zp.paths("z2","z1").size == 1)
    assert(zp.paths("z2","z1").head.exits.head.name == "n")
  }


  test("three zones") {
    mockRoom(1,"z1",Exit("s",1,2),Exit("n",1,2))
    mockRoom(2,"z2",Exit("n",2,1),Exit("w",2,3),Exit("q",2,5))
    mockRoom(3,"z2")
    mockRoom(5,"z3")
    val zp = ZonePaths.construct(Room.rooms.values.toSet)
    assert(zp.paths.size == 4)
    assert(zp.paths("z1","z2").size == 1)
    assert(zp.paths("z2","z1").size == 1)
    assert(zp.paths("z2","z1").head.exits.head.name == "n")
    assert(zp.paths("z2","z3").size == 1)
    assert(zp.paths("z1","z3").size == 1)
  }

  test("ddEnd works correctly") {
    mockRoom(1,"z1",Exit("s",1,2),Exit("n",1,2))
    mockRoom(2,"z2",Exit("n",2,1),Exit("w",2,3),Exit("q",2,5))
    mockRoom(5,"z3")
    assert(ZonePaths.ddEnd(Path(List(Exit("s",1,2),Exit("s",1,2))).exits))
    assert(ZonePaths.ddEnd(Path(List(Exit("s",1,2),Exit("s",2,5))).exits)==false)
  }

}

class ZonePathTests extends FunSuite {
  import PathTestUtils._

  val time = System.currentTimeMillis()
  Room.load
  Zone.load
  val diff = System.currentTimeMillis() - time
  println(s"load time $diff")

  val zp = Path.zonePaths
  val paths = zp.paths

  test("all paths in ZonePaths should be continuous") {
    paths.foreach {path=>
      assert(continuous(path))
    }
  }

  def pathsBetween(start: String, end: String): List[Path] = {
    paths.filter{p=>
      p.exits.size > 2 &&
        p.head.get.from.get.zoneName == start &&
        p.last.get.to.get.zoneName == end
    }
  }

  def reachable(start: String, end: String) : Boolean = pathsBetween(start,end).nonEmpty

  test("sample of area pairs should be reachable") {
    assert(reachable("aylor","legend"))
    assert(reachable("aylor","underdark"))
    assert(reachable("aylor","zoo"))
  }

  test("every terminal link should be a zonelink") {
    paths.filter(_.exits.nonEmpty).foreach { p=>
      val le = p.last.get
      assert(le.from.get.zoneName != le.to.get.zoneName)
    }
  }

  test("recall to beer via zonepath") {
    val op = zp.path(Room(Path.recall).get,Room(20062).get)
    assert(op.isDefined)
    assert(op.get.weight < 20)
  }

}

class CoordPatherTests extends FunSuite {
  Room.load
  Zone.load

  test("mesolar to beer") {
    assert(Room(12664).get.to(Room(12642).get).isDefined)
  }
}
