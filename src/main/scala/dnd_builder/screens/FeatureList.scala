package dndbuilder.screens

import dndbuilder.dnd.*
import tyrian.Html.*
import tyrian.*

/** Renders class features as a flat list; optional character for description resolution. */
object FeatureList {

  def apply[Msg](features: List[Feature], ch: Option[Character]): Html[Msg] = {
    def featureItem(f: Feature): Html[Msg] = {
      val desc = ch.fold(FeatureDisplay.generalDescription(f))(c => FeatureDisplay.resolvedDescription(f, c))
      div(`class` := "feature-item")(
        div(`class` := "feature-name")(text(f.name)),
        div(`class` := "feature-desc")(text(desc))
      )
    }
    div(`class` := "feature-list")(features.map(featureItem)*)
  }
}
