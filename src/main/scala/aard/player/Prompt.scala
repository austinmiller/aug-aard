package aard.player

import java.util.regex.Matcher

import aug.script.{Trigger, TriggerOptions}

import scala.collection.mutable

object Prompt {

  private val defaultPromptRegex = ".*\\[\\d+/\\d+hp \\d+/\\d+mn \\d+/\\d+mv \\d+qt \\d+tnl\\].*"
  private var prompt = Option[Prompt](null)
  private val callbacks = mutable.Set[PromptCallback]()

  def set(promptPattern: String): Unit = synchronized {
    prompt map (_.unregister)
    prompt = Some(Prompt(promptPattern))
  }

  def onPrompt = synchronized {
    callbacks.foreach(_.callback())
    Trigger.disablePromptTriggers
  }

  def load = {
    set(defaultPromptRegex)
  }

  def register(callback: PromptCallback) = synchronized { callbacks += callback }
  def unregister(callback: PromptCallback) = synchronized { callbacks -= callback }
}

case class PromptCallback(callback: () => Unit)

case class Prompt(val promptRegex: String) {
  private val trigger = Trigger.trigger(promptRegex,(m: Matcher) => Prompt.onPrompt,true,TriggerOptions(fragmentTrigger = true))
  def unregister = Trigger.unregister(trigger)
}
