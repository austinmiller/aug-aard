package aard.player

import java.util.regex.Pattern

import aard.db.Store
import aard.map.Room
import aard.script.GmcpChar
import aug.script.{Alias, Game, Trigger}
import aug.util.{JsonUtil, Util}

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

object InvHeader {
  val ObjectId = 0
  val Level = 1
  val ItemTypeName = 2
  val Value = 3
  val Weight = 4
  val WearLoc = 5
  val Flags = 6
  val Owner = 7
  val ClanName = 8
  val Timer = 9
  val Unknown1 = 10
  val Unknown2 = 11
  val ItemScore = 13
}

object InvMonAction {
  val Removed = 1
  val Worn = 2
  val Dropped = 3
  val Gained = 4
  val LeaveContainer = 5
  val EnterContainer = 6
  val Consumed = 7
  val EnterVault = 9
  val LeaveVault = 10
  val EnterKeyRing = 11
  val LeaveKeyRing = 12
}

object Player {

  var current = Option[Player](null)
  val dir = "player"
  val ext = ".player"

  val items = mutable.Map[Long,Item]()

  def onGmcp(gmcp: GmcpChar) = {
    current.filter(_.name == gmcp.name  ) match {
      case None =>
        current = Some(Store.loadOrElse[Player](s"$dir/${gmcp.name}$ext",newPlayer(gmcp)))
      case _ =>
    }
  }

  def save(player: Player) = {
    Store.save(s"$dir/${player.name}$ext",player)
  }

  def newPlayer(gmcp: GmcpChar) : Player = {
    val p = Player(gmcp.name)
    save(p)
    p
  }

  def withPlayer()(f: Player => Unit) : Unit = {
    current match {
      case None => Game.echo("\nplayer is not currently loaded.\n")
      case Some(player) => f(player)
    }
  }

  def load = {
    Alias.alias("p gmcp",m=> Game.sendGmcp("request char"))
    Alias.alias("p info",m=> {
      withPlayer() { p =>
        Game.header("player info")
        val s = JsonUtil.prettyJson(p)
        Game.echo(s"$s\n")
      }
    })

    Alias.alias("i invdata",m=>{
      containerId = -1
      setItems(Set[Item]())
      Game.send("invdata")
      Game.send("eqdata")
    })

    Alias.alias("i list",m=>{
      withPlayer(){p=>
        Game.header(s"inventory [${p.items.size}]")
        p.items.foreach {i=>
          Game.echo(s"$i\n")
        }
      }
    })

    Alias.alias("i worn",m=>{
      withPlayer(){p=>
        Game.header(s"worn items")
        p.items.filter(_.wearLoc != Unworn).toList.sortBy(_.wearLoc.name).foreach {i=>
          val name = Util.removeColors(i.name)
          Game.echo(s"${i.wearLoc} :: $name\n")
        }
      }
    })

    Alias.alias("i contained",m=>{
      withPlayer(){p=>
        Game.header(s"items in containers")
        p.items.filter(_.containerId != -1).foreach {i=>
          Game.echo(s"$i\n")
        }
      }
    })

    Alias.alias("i portals",m=>{
      withPlayer(){p=>
        Game.header(s"portals")
        p.portals.foreach {portal=>
          Game.echo(s"$portal\n")
        }
      }
    })

    Alias.alias("i best",m=> {
      withPlayer() {_.wearBest()}
    })

    Game.sendGmcp("request char")
  }

  val invDataStartTriggerPattern = "(\\{invdata\\}|\\{eqdata\\})"
  val invDataStartTrigger = Trigger.trigger(invDataStartTriggerPattern,m=>{
    invDataLineTrigger.enabled = true
    invDataStopTrigger.enabled = true
    containerId = -1
  })

  val invDataContainerStartTriggerPattern = "\\{invdata (\\d+)\\}"
  val invDataContainerStartTrigger = Trigger.trigger(invDataContainerStartTriggerPattern,m=>{
    invDataLineTrigger.enabled = true
    invDataStopTrigger.enabled = true
    containerId = m.group(1).toLong
  })

  val invDataLineTriggerPattern = "(\\d+,.*,.*,.*,.*,.*)"
  val invDataLineTrigger = Trigger.trigger(invDataLineTriggerPattern,m=>invItem(m.group()),false)

  def invItem(s: String) : Unit = {
    Item.from(s,containerId).map{item=>
      if(containerId != -1) {
        Game.send(s"get ${item.id} $containerId")
        Game.send(s"invdetails ${item.id}")
        Game.send(s"put ${item.id} $containerId")
      } else Game.send(s"invdetails ${item.id}")
      addItem(item)

      if(item.itemType == Container) Game.send(s"invdata ${item.id}")
    }
  }

  val invDataStopTriggerPattern = "(\\{/invdata\\}|\\{/eqdata\\})"
  val invDataStopTrigger = Trigger.trigger(invDataStopTriggerPattern,m=> stopCollectingInvData(),false)

  def stopCollectingInvData() : Unit = {
    invDataLineTrigger.enabled = false
    invDataStopTrigger.enabled = false
  }

  val invDetailsStartTriggerPattern = Pattern.quote("{invdetails}")
  val invDetailsStartTrigger = Trigger.trigger(invDetailsStartTriggerPattern,m=>{
    invDetailsHeaderTrigger.enabled = true
    invDetailsStopTrigger.enabled = true
  })

  val invDetailsHeaderTriggerPattern = Pattern.quote("{invheader}")+"(.*)"
  val invDetailsHeaderTrigger = Trigger.trigger(invDetailsHeaderTriggerPattern,m=>{
    import InvHeader._
    val tokens = m.group(1).split(Pattern.quote("|"))
    val id = tokens(ObjectId).toLong
    withPlayer() {p=>
      p.withItem(id) { item=>
        addItem(item.copy(
          score = tokens(ItemScore).toInt,
          weight = tokens(Weight).toInt,
          wornAt = tokens(WearLoc)
        ))
      }
    }
  },false)

  val invDetailsStopTriggerPattern = Pattern.quote("{/invdetails}")
  val invDetailsStopTrigger = Trigger.trigger(invDetailsStopTriggerPattern,m=> stopCollectingInvDetails(),false)

  def stopCollectingInvDetails() : Unit = {
    invDetailsHeaderTrigger.enabled = false
    invDetailsStopTrigger.enabled = false
  }

  val invItemTriggerPattern = Pattern.quote("{invitem}") + "(.*)"
  val invItemTrigger = Trigger.trigger(invItemTriggerPattern,m=>invItem(m.group(1)))

  var containerId : Long = -1

  val invMonTriggerPattern = Pattern.quote("{invmon}") + "(.*)"
  val invMonTrigger = Trigger.trigger(invMonTriggerPattern,m=>{
    import InvMonAction._
    val tokens = m.group(1).split(",")
    require(tokens.size == 4)
    val action = tokens(0).toInt
    val id = tokens(1).toLong
    val containerId = tokens(2).toLong
    val wearLocId = tokens(3).toInt

    withPlayer() { p =>
      action match {
        case Dropped | Consumed =>
          removeItem(id)
        case Removed =>
          p.withItem(id) {i=>
            addItem(i.copy(containerId = containerId,wearLocId = Unworn.id))
          }
        case Worn =>
          p.withItem(id) {i=>
            addItem(i.copy(containerId = containerId,wearLocId = wearLocId))
          }
        case _ =>
          p.withItem(id) {i=>
            addItem(i.copy(containerId = containerId))
          }

      }
    }

  })

  def removeItem(id: Long) = {
    withPlayer() { p=>
      current = Some(p.copy(items = p.items.filter(_.id != id)))
      save(p)
    }
  }

  def addItem(item: Item) = {
    withPlayer() { p=>

      if(item.itemType == Portal) {
        if(p.portals.find(_.id == item.id).isEmpty) {
          current = Some(p.copy(portals = p.portals + Portal(item.id)))
        }
      }

      current = Some(p.copy(items = p.items.filter(_.id != item.id) + item))
      save(p)
    }
  }

  def setItems(items: Set[Item]) = {
    withPlayer() { p=>
      current = Some(p.copy(items=items))
      save(p)
    }
  }
}

sealed abstract class ItemType(val id: Int)

case object Light extends ItemType(1)
case object Scroll extends ItemType(2)
case object Wand extends ItemType(3)
case object Stave extends ItemType(4)
case object Weapon extends ItemType(5)
case object Treasure extends ItemType(6)
case object Armor extends ItemType(7)
case object Potion extends ItemType(8)
case object Furniture extends ItemType(9)

case object Trash extends ItemType(10)
case object Container extends ItemType(11)
case object DrinkContainer extends ItemType(12)
case object Key extends ItemType(13)
case object Food extends ItemType(14)
case object Boat extends ItemType(15)
case object MobCorpse extends ItemType(16)
case object PlayerCorpse extends ItemType(17)
case object Fountain extends ItemType(18)
case object Pill extends ItemType(19)

case object Portal extends ItemType(20)
case object Beacon extends ItemType(21)
case object GiftCard extends ItemType(22)
case object UnusedItemType extends ItemType(23)
case object RawMaterial extends ItemType(24)
case object Campfire extends ItemType(25)
case object Forge extends ItemType(26)
case object Runestone extends ItemType(27)

sealed abstract class WearLoc(val id: Int,val name: String)

case object Unworn extends WearLoc(-1,"")
case object WearLight extends WearLoc(0, "light")
case object WearHead extends WearLoc(1, "head")
case object WearEyes extends WearLoc(2, "eyes")
case object WearLEar extends WearLoc(3,"lear")
case object WearREar extends WearLoc(4, "rear")
case object WearNeck1 extends WearLoc(5,"neck1")
case object WearNeck2 extends WearLoc(6,"neck2")
case object WearBack extends WearLoc(7,"back")
case object WearMedal1 extends WearLoc(8,"medal1")
case object WearMedal2 extends WearLoc(9,"medal2")

case object WearMedal3 extends WearLoc(10,"medal3")
case object WearMedal4 extends WearLoc(11,"medal4")
case object WearTorso extends WearLoc(12,"torso")
case object WearBody extends WearLoc(13,"body")
case object WearWaist extends WearLoc(14,"waist")
case object WearArms extends WearLoc(15,"arms")
case object WearLWrist extends WearLoc(16,"lwrist")
case object WearRWrist extends WearLoc(17,"rwrist")
case object WearHands extends WearLoc(18,"hands")
case object WearLFinger extends WearLoc(19,"lfinger")

case object WearRFinger extends WearLoc(20,"rfinger")
case object WearLegs extends WearLoc(21,"legs")
case object WearFeet extends WearLoc(22,"feet")
case object WearShield extends WearLoc(23,"shield")
case object WearWielded extends WearLoc(24,"wielded")
case object WearSecond extends WearLoc(25,"second")
case object WearHold extends WearLoc(26,"hold")
case object WearFloat extends WearLoc(27,"float")
case object WearUnknown1 extends WearLoc(28,"")
case object WearUnknown2 extends WearLoc(29,"")

case object WearAbove extends WearLoc(30,"above")
case object WearUnknown3 extends WearLoc(31,"")
case object WearSleeping extends WearLoc(32,"sleeping")

object Item {
  val idToItemType = Map(
    1->Light,
    2->Scroll,
    3->Wand,
    4->Stave,
    5->Weapon,
    6->Treasure,
    7->Armor,
    8->Potion,
    9->Furniture,

    10->Trash,
    11->Container,
    12->DrinkContainer,
    13->Key,
    14->Food,
    15->Boat,
    16->MobCorpse,
    17->PlayerCorpse,
    18->Fountain,
    19->Pill,

    20->Portal,
    21->Beacon,
    22->GiftCard,
    23->UnusedItemType,
    24->RawMaterial,
    25->Campfire,
    26->Forge,
    27->Runestone)

  val idToWearLoc = Map(
    -1->Unworn,
    0->WearLight,
    1->WearHead,
    2->WearEyes,
    3->WearLEar,
    4->WearREar,
    5->WearNeck1,
    6->WearNeck2,
    7->WearBack,
    8->WearMedal1,
    9->WearMedal2,

    10->WearMedal3,
    11->WearMedal4,
    12->WearTorso,
    13->WearBody,
    14->WearWaist,
    15->WearArms,
    16->WearLWrist,
    17->WearRWrist,
    18->WearHands,
    19->WearLFinger,

    20->WearRFinger,
    21->WearLegs,
    22->WearFeet,
    23->WearShield,
    24->WearWielded,
    25->WearSecond,
    26->WearHold,
    27->WearFloat,
    28->WearUnknown1,
    29->WearUnknown2,

    30->WearAbove,
    31->WearUnknown3,
    32->WearSleeping
  )

  val nameToWearLoc: Map[String, WearLoc] = idToWearLoc.map(x=>(x._2.name,x._2))

  def from(line: String, containerId: Long) : Option[Item] = {
    Try {
      val tokens = line.split(",")
      require(tokens.size == 8)

      val objectId = tokens(0).toLong
      val flags = tokens(1)
      val name = tokens(2)
      val level = tokens(3).toInt
      val itemType = tokens(4).toInt
      val unique = if(tokens(5).toInt == 1) true else false
      val wearLocId = tokens(6).toInt
      val timer = tokens(7).toInt
      Item(objectId,flags,name,level,itemType,unique,wearLocId,timer,containerId=containerId)
    } match {
      case Failure(e) =>
        Game.handleException(e)
        None
      case Success(id) => Some(id)
    }
  }

}

case class Portal(id: Long, toId: Long = -1, pathable: Boolean = true) {
  def to = Room(toId)
}

case class Item(id: Long, flags: String = "", name: String = "", level: Int = 0, itemTypeId: Int = Light.id,
                unique: Boolean = false, wearLocId: Int = Unworn.id, timer: Int= -1, score: Int = 0,
                containerId: Long = -1, wornAt: String = "", weight: Int = 1) {
  def itemType = Item.idToItemType(itemTypeId)
  def wearLoc = Item.idToWearLoc(wearLocId)
  def kept : Boolean = flags.contains("K")
  def worn : Boolean = wearLoc != Unworn

  def contained : Boolean = containerId != -1

  def getFromContainer = if(contained) Game.send(s"get ${id} ${containerId}")
  def remove = if(worn) Game.send(s"remove ${id}")
}


case class Player(name: String, items: Set[Item] = Set.empty, portals : Set[Portal] = Set.empty) {
  def item(id: Long) = items.find(_.id == id)

  def withItem(id: Long)(f: Item=>Unit) = {
    item(id) match {
      case None => Game.echo(s"\nitem <${id}> not found.\n")
      case Some(item) => f(item)
    }
  }

  def dualWield : Boolean = false

  def availablePortals = {
    val keys = items.map(_.id)
    portals.filter(p=> keys.contains(p.id) && p.pathable && p.to.isDefined)
  }

  def wornItem(wearLoc: WearLoc) : Option[Item] = items.find(_.wearLoc == wearLoc)

  def best(wornAt: String, num: Int=1) : List[Item] = items.filter(i=>i.wornAt == wornAt && i.kept).toList
    .sortBy(_.score).take(num)

  def wearBestPair(wearAt: String, wearLoc1: WearLoc, wearLoc2: WearLoc) = {
    best(wearAt,2) match {
      case Nil =>Game.echo("nil!\n")
      case List(a) =>
        Game.echo("one!\n")
        wear(a,wearLoc1)
      case List(a,b) =>
        Game.echo(s"$a\n$b\n")
        wear(a,wearLoc1)
        wear(b,wearLoc2)
      case _ => Game.echo("ruh roh!\n")
    }
  }

  def wearBest(): Unit = {
    best("light").foreach { i=>wear(i,WearLight) }
    best("head").foreach { i=>wear(i,WearHead) }
    best("eyes").foreach { i=>wear(i,WearEyes) }
    best("back").foreach { i=>wear(i,WearBack) }
    best("torso").foreach { i=>wear(i,WearTorso) }
    best("body").foreach { i=>wear(i,WearBody) }
    best("waist").foreach { i=>wear(i,WearWaist) }
    best("arms").foreach { i=>wear(i,WearArms) }
    best("hands").foreach { i=>wear(i,WearHands) }
    best("legs").foreach { i=>wear(i,WearLegs) }
    best("feet").foreach { i=>wear(i,WearFeet) }
    best("float").foreach { i=>wear(i,WearFloat) }
    best("above").foreach { i=>wear(i,WearAbove) }

    wearBestPair("ear",WearLEar,WearREar)
    wearBestPair("neck",WearNeck1,WearNeck2)
    wearBestPair("wrist",WearLWrist,WearRWrist)
    wearBestPair("finger",WearLFinger,WearRFinger)

    if(dualWield) {
      ???
    } else {
      best("shield").foreach { i=>wear(i,WearShield) }
      best("held").foreach { i=>wear(i,WearHold) }
    }

  }



  def wear(item: Item, wearLoc: WearLoc) : Unit = {
    if(!item.worn) {
      item.getFromContainer
      item.remove
      Game.send(s"wear ${item.id} ${wearLoc.name}")
    }
  }
}


