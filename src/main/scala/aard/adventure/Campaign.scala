package aard.adventure

import java.util.regex.Matcher

import aard.map.{Path, Room, Zone}
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

  def campaignCheck = {
    Game.send("campaign check")
    campaignCheckTrigger.enabled = true
    campaignCheckNoteTrigger.enabled = true
    campaignCheckBuilder.clear()
  }

  var lastMobSlain = Option[String](null)
  val lastMobSlainTriggerPattern = "(.*) is slain by .*"
  val lastMobSlainTrigger = Trigger.trigger(lastMobSlainTriggerPattern,(m: Matcher) => {
    lastMobSlain = Some(m.group(1))
  })

  val campaignMobKilledTriggerPattern = "Congratulations, that was one of your CAMPAIGN mobs!"
  val campaignMobKilledTrigger = Trigger.trigger(campaignMobKilledTriggerPattern, (m: Matcher) => {
    campaignCheck
  })

  def load = {
    Alias.alias("cc",(m: Matcher) => campaignCheck)

    Alias.alias("cn",(m: Matcher) => {
      current match {
        case None => campaignCheck
        case Some(campaign) => campaign.next
      }
    })
  }
}

case class CampaignMob(name: String, hint: String)

case class Campaign(mobs: List[CampaignMob]) {

  val zone = mobs.exists(cm => Zone.byLong(cm.hint).isDefined)

  def next : Unit = {
    mobs match {
      case Nil => campaignComplete
      case _ => if(zone) zoneNext else roomNext
    }
  }

  def zoneNext = {
    if(mobs.isEmpty) campaignComplete

    mobs.filter(cm=>Zone.byLong(cm.hint).isDefined) match {
      case Nil =>
      case head :: xs =>
        val zone = Zone.byLong(head.hint).get
        Room.forRoom() {r=>
          if(r.zone != zone) {
            Path.to(zone.name) match {
              case None => Game.echo(s"\nNo path to $zone\n")
              case Some(path) =>
                path.runTo
                Game.send(s"gt next target is $head")
            }
          } else {
            Game.echo(s"\nYou're in the zone for ${head.name}\n")
          }
        }

    }
  }

  def roomNext = {
    Game.echo("\nRoom campaigns not implemented\n")
  }

  def campaignComplete = {
    Game.echo("\nCampaign complete not implemented\n")
  }


}
