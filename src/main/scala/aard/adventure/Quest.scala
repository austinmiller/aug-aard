package aard.adventure

import java.util.regex.Matcher

import aug.script.{Alias, Game}

object Quest {
  def load = {
    Alias.alias("q gmcp",(m: Matcher)=>Game.sendGmcp("request quest"))
  }
}

class Quest {

}
