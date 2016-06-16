package aard.adventure

import java.util.regex.Matcher

import aard.map.Room
import aard.script.GmcpQuest
import aug.script.{Alias, Game}

object Quest {
  def load = {
    Alias.alias("q gmcp",(m: Matcher)=>Game.sendGmcp("request quest"))
  }

  def onGmcp(gmcp: GmcpQuest): Unit = {
    if(gmcp.room != null && gmcp.room.length > 0) {
      Room.setRList(gmcp.room)
      Room.printRList()
    }
  }
}

class Quest {

}
