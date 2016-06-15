package aard.adventure

import java.util.regex.Matcher

import aug.script.{Alias, Game}

object Targeter {

  var target = Option[String](null)

  def load = {
    Alias.alias("x=(.*)",(m: Matcher) => {
      target = Some(m.group(1))
      Game.send("where")
    })

    Alias.alias("x",(m: Matcher) => {
      target match {
        case None => Game.echo("\nNo target loaded, used 'x=<target>'\n")
        case Some(t) => Game.send(s"kill $t")
      }
    })
  }

}
