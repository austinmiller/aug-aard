package aard.db

import org.mongodb.scala.MongoClient

object MDB {

  val client = MongoClient()

  val db = client.getDatabase("aard")
}
