package dndbuilder.screens

import tyrian.Html.*
import tyrian.*

object StepIndicator {

  private val baseSteps = List("Race", "Class", "Background", "Abilities", "Skills")
  private val casterSteps = baseSteps :+ "Features" :+ "Equipment" :+ "Spells" :+ "Review"
  private val nonCasterSteps = baseSteps :+ "Features" :+ "Equipment" :+ "Review"

  /** Canonical step list (0-indexed); used for progress bar and nav labels. */
  def steps(isSpellcaster: Boolean): List[String] =
    if isSpellcaster then casterSteps else nonCasterSteps

  /** Back button label: previous step name, or "< Home" when currentStep <= 1. (currentStep is 1-based.) */
  def backLabel(currentStep: Int, isSpellcaster: Boolean): String =
    if currentStep <= 1 then "< Home"
    else "< " + steps(isSpellcaster)(currentStep - 2)

  /** Next button label: "Next: <next step> >", or "Save Character" when on last step. (currentStep is 1-based.) */
  def nextLabel(currentStep: Int, isSpellcaster: Boolean): String = {
    val st = steps(isSpellcaster)
    if currentStep >= st.size then "Save Character"
    else "Next: " + st(currentStep) + " >"
  }

  def apply[Msg](currentStep: Int, isSpellcaster: Boolean): Html[Msg] = {
    val st = steps(isSpellcaster)
    val items = st.zipWithIndex.flatMap { case (label, idx) =>
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
      if stepNum < st.size then List(item, div(`class` := connectorCls)())
      else List(item)
    }
    div(`class` := "step-indicator")(items*)
  }
}
