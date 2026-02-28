package dndbuilder.screens

import tyrian.Html.*
import tyrian.*

object StepIndicator {

  private val baseSteps = List("Race", "Class", "Background", "Abilities", "Skills", "Equipment")
  private val casterSteps = baseSteps :+ "Class Features" :+ "Spells" :+ "Languages" :+ "Review"
  private val nonCasterSteps = baseSteps :+ "Class Features" :+ "Languages" :+ "Review"

  def apply[Msg](currentStep: Int, isSpellcaster: Boolean): Html[Msg] = {
    val steps = if isSpellcaster then casterSteps else nonCasterSteps
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
  }
}
