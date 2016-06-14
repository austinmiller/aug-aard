package aard.script

import aug.util.{JsonUtil, Util}

sealed trait GmcpMessage

case class GmcpRoom(num: Long, name: String, zone: String, terrain: String, details: String, exits: GmcpRoomExits, coord: GmcpRoomCoords) extends GmcpMessage
case class GmcpRoomExits(n: Long, s: Long, u: Long, d: Long, e: Long, w: Long) extends GmcpMessage
case class GmcpRoomCoords(id: Long, x: Int, y: Int, cont: Int) extends GmcpMessage
case object GmcpTick extends GmcpMessage
case class GmcpVitals(hp: Int, mana: Int, moves: Int) extends GmcpMessage
case class GmcpChannel(chan: String, msg: String, player: String) extends GmcpMessage
case class GmcpRepop(zone: String) extends GmcpMessage
case class GmcpUnknown(gmcp: String) extends GmcpMessage
case class GmcpQuest(action: String, status: String) extends GmcpMessage
case class GmcpStatus(level: Int, tnl: Int, hunger: Int, thirst: Int, align: Int, state: Int, pos: String, enemy: String) extends GmcpMessage
case class GmcpStats(str: Int, int: Int, wis: Int, dex: Int, con: Int, luck: Int, hr: Int, dr: Int, saves: Int) extends GmcpMessage
case class GmcpMaxStats(maxhp: Int, maxmana: Int, maxmoves: Int, maxstr: Int, maxint: Int, maxwis: Int, maxdex: Int, maxcon: Int, maxluck: Int) extends GmcpMessage
case class GmcpWorth(gold: Long, bank: Long, qp: Int, tp: Int, trains: Int, pracs: Int) extends GmcpMessage
case class GmcpChar(name: String, gameClass: String, subClass: String, race: String, clan: String, pretitle: String, perlevel: Int, tier:Int, remotes: Int, redos: Int) extends GmcpMessage
case class GmcpGroup(groupname: String, reason: String) extends GmcpMessage

object Gmcp {

  def deserialize(gmcp: String) : GmcpMessage = {
    val tokens = Util.removeColors(gmcp).split(" ",2)
    if(tokens.size < 2) throw new Exception("tokens isn't 2")
    tokens(0) match {
      case "room.info" => JsonUtil.fromJson[GmcpRoom](tokens(1))
      case "comm.tick" => GmcpTick
      case "comm.channel" => JsonUtil.fromJson[GmcpChannel](tokens(1))
      case "comm.repop" => JsonUtil.fromJson[GmcpRepop](tokens(1))
      case "char.vitals" => JsonUtil.fromJson[GmcpVitals](tokens(1))
      case "comm.quest" => JsonUtil.fromJson[GmcpQuest](tokens(1))
      case "char.status" => JsonUtil.fromJson[GmcpStatus](tokens(1))
      case "char.stats" => JsonUtil.fromJson[GmcpStats](tokens(1))
      case "char.maxstats" => JsonUtil.fromJson[GmcpMaxStats](tokens(1))
      case "char.worth" => JsonUtil.fromJson[GmcpWorth](tokens(1))
      case "char.base" => JsonUtil.fromJson[GmcpChar](tokens(1))
      case "group" => JsonUtil.fromJson[GmcpGroup](tokens(1))
      case _ => GmcpUnknown(gmcp)
    }
  }

}
