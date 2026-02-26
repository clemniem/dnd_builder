package dndbuilder.screens

import tyrian.Html.*
import tyrian.*

object StepIndicator:

  private val steps = List("Race", "Class", "Background", "Abilities", "Skills", "Review")

  def apply[Msg](currentStep: Int): Html[Msg] =
    val items = steps.zipWithIndex.flatMap { case (label, idx) =>
      val stepNum = idx + 1
      val cls =
        if stepNum < currentStep then "step-item step-item--done"
        else if stepNum == currentStep then "step-item step-item--active"
        else "step-item"
      val connectorCls =
        if stepNum < currentStep then "step-connector step-connector--done"
        else "step-connector"

      val item = div(`class` := cls)(
        span(`class` := "step-number")(text(stepNum.toString)),
        span(text(label))
      )
      if stepNum < steps.size then List(item, div(`class` := connectorCls)())
      else List(item)
    }
    div(`class` := "step-indicator")(items*)
