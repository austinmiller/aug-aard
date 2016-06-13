package aard.script

import aug.profile.{ProfileEvent, ProfileEventListener, ScriptInit, TelnetGMCP}
import aug.script.Game
import org.mongodb.scala.{Completed, Document, MongoClient, MongoCollection, Observable, Observer}

class AardScript extends ProfileEventListener {
  override def event(event: ProfileEvent, data: Option[String]): Unit = {
    event match {
      case ScriptInit => Game.info("loaded aard script")
      case TelnetGMCP => data foreach { gmcp =>
        Gmcp.deserialize(gmcp) match {
          case GmcpUnknown(s) => println(s"UNKNOWN GMCP $s")
          case r: GmcpRoom => Game.echo(s"coord id == ${r.coord.id}")
          case _ =>
        }
      }
      case _ =>
    }

  }
}

object MdbTest {
  def main(args: Array[String]) : Unit = {
    val client = MongoClient()
    val db = client.getDatabase("aard")

    db.createCollection("room")

    val rooms : MongoCollection[Document] = db.getCollection("room")

    val d = Document("id"->0)

    val insertObservable: Observable[Completed] = rooms.insertOne(d)



    insertObservable.subscribe(new Observer[Completed] {
      override def onNext(result: Completed): Unit = println(s"onNext: $result")
      override def onError(e: Throwable): Unit = println(s"onError: $e")
      override def onComplete(): Unit = println("onComplete")
    })

    println("here")

    Thread.sleep(10000)

  }
}
