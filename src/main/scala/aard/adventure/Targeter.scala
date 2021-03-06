package aard.adventure

import java.util.regex.Matcher

import aard.map.Room
import aard.player.{Prompt, PromptCallback}
import aug.script.{Alias, Game, Trigger, TriggerOptions}
import aard.script.Shortcuts._

object Targeter {

  var target = Option[String](null)

  val whereRooms = Seq.newBuilder[String]
  val whereTriggerPattern ="(.*)   (.*)"
  val whereTrigger = Trigger.trigger(whereTriggerPattern,(m: Matcher)=> {
    whereRooms += m.group(2)
  },false, TriggerOptions(disableAtPrompt = true))
  val wherePromptCallback = PromptCallback(() => whereOnPrompt)
  def whereOnPrompt : Unit = {

    Prompt.unregister(wherePromptCallback)

    Room.forRoom() {r=>
      val map: Map[String, Iterable[Room]] = Room.zoneRooms(r.zoneName).groupBy(_.name)
      val rooms = whereRooms.result.flatMap(rn=>map.get(rn)).flatten
      whereRooms.clear
      Room.setRList(rooms.sortBy(_.id))
      Room.printRList()
    }

  }

  def kill = target map (t=>Game.send(s"kill $t"))

  def load = {
    Alias.alias("x (.*)",(m: Matcher) => {
      target = Some(m.group(1).trim)
      whereTrigger.enabled = true
      Prompt.register(wherePromptCallback)
      Game.send(s"where ${target.get}")
    })

    Alias.alias("x",(m: Matcher) => {
      target match {
        case None => error("No target loaded, use 'x <target>'")
        case Some(t) => Game.send(s"kill $t")
      }
    })

    Alias.alias("xn",m=> {
      Room.withRList() { rl=>
        if(rl.exhausted) {
          info("No more rooms to cycle")
        } else {
          rl.pathToNext match {
            case None => error("No path to next room")
            case Some(p) =>
              p.runTo
              kill
          }
        }
      }
    })

    Alias.alias("wh (.*) ([0-9]+)",m=> {
      val count = m.group(2).toInt
      val t = m.group(1)
      for(i <- 1 to count) Game.send(s"where $i.$t")
    })
  }

}
