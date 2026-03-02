package dndbuilder.screens

import dndbuilder.dnd.*
import tyrian.Html.*
import tyrian.*

/** Renders class features split into Actionable and Informative; optional character for description resolution. */
object FeatureListSplit {

  def apply[Msg](features: List[Feature], ch: Option[Character]): Html[Msg] = {
    val (actionable, informative) = FeatureDisplay.splitActionableInformative(features)
    def featureItem(f: Feature): Html[Msg] = {
      val desc = ch.fold(FeatureDisplay.generalDescription(f))(c => FeatureDisplay.resolvedDescription(f, c))
      div(`class` := "feature-item")(
        div(`class` := "feature-name")(text(f.name)),
        div(`class` := "feature-desc")(text(desc))
      )
    }
    div(
      (if actionable.nonEmpty then
        div(
          div(`class` := "section-title", style := "font-size: 0.9rem; margin-top: 0.5rem;")(text("Actionable")),
          div(`class` := "feature-list")(actionable.map(featureItem)*)
        )
      else div()),
      (if informative.nonEmpty then
        div(
          div(`class` := "section-title", style := "font-size: 0.9rem; margin-top: 0.5rem;")(text("Informative")),
          div(`class` := "feature-list")(informative.map(featureItem)*)
        )
      else div())
    )
  }
}
