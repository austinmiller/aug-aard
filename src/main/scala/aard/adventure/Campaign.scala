package aard.adventure

import java.util.regex.Matcher

import aard.map.Room
import aug.script.{Alias, Game, Trigger, TriggerOptions}

import scala.collection.mutable.ListBuffer


object Campaign {
  val campaignCheckTriggerPattern = "You still have to kill \\* (.*) \\((.*)\\)"
  val campaignCheckNoteTriggerPattern = "Note: One or more target names in this campaign might be slightly scrambled."
  var campaignCheckBuilder = ListBuffer[CampaignMob]()
  val campaignCheckTrigger = Trigger.trigger(campaignCheckTriggerPattern, (m: Matcher)=> {
    campaignCheckBuilder += CampaignMob(m.group(1),m.group(2))
  }, false)
  val campaignCheckNoteTrigger = Trigger.trigger(campaignCheckNoteTriggerPattern, (m: Matcher) =>{
    campaignCheckTrigger.enabled = false
    current = Some(Campaign(campaignCheckBuilder.result))
    current map(c=>Game.echo(s"$c\n"))
  }, false, TriggerOptions(fireOnce = true))

  var current : Option[Campaign] = None

  def load = {
    Alias.alias("cc",(m: Matcher) => {
      Game.send("campaign check")
      campaignCheckTrigger.enabled = true
      campaignCheckNoteTrigger.enabled = true
      campaignCheckBuilder.clear()
    })
  }
}

case class CampaignMob(name: String, hint: String)

case class Campaign(mobs: List[CampaignMob]) {

  val zone = mobs.exists(cm => Room.zones.get(cm.hint).isDefined)

  def next = {
    mobs match {
      case Nil => campaignComplete
      case head :: xs =>
        if(zone) {
          if(Room.current.zoneName != head.hint) {
            Room.current.zonePather
          }
        } else {

        }
    }
  }

  def campaignComplete = {

  }


}
