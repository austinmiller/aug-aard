package aard.adventure

import java.util.regex.Matcher

import aard.map.Room
import aard.player.{Prompt, PromptCallback}
import aug.script.{Alias, Game, Trigger, TriggerOptions}

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

    val map: Map[String, Iterable[Room]] = Room.zoneRooms(Room.current.zoneName).groupBy(_.name)
    val rooms = whereRooms.result.flatMap(rn=>map.get(rn)).flatten
    whereRooms.clear
    Room.setRList(rooms)
    Room.printRList()
  }

  def load = {
    Alias.alias("x (.*)",(m: Matcher) => {
      target = Some(m.group(1).trim)
      whereTrigger.enabled = true
      Prompt.register(wherePromptCallback)
      Game.send(s"where ${target.get}")
    })

    Alias.alias("x",(m: Matcher) => {
      target match {
        case None => Game.echo("\nNo target loaded, used 'x=<target>'\n")
        case Some(t) => Game.send(s"kill $t")
      }
    })
  }

}
