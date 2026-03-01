package dndbuilder.screens

import dndbuilder.dnd.{Ability, Skill}
import tyrian.Html
import tyrian.Html.*

/** Shared polymorphic view components for class feature and level-up choices. */
object ChoiceWidgets {

  /** Single-select card grid: title, items (value, label, optional description), selected value, onSelect. */
  def cardPicker[Msg, A](
      title: String,
      items: List[(A, String, Option[String])],
      selected: Option[A],
      onSelect: A => Msg
  ): Html[Msg] =
    div(style := "margin-bottom: 1.5rem;")(
      h2(`class` := "about-heading")(text(title)),
      div(`class` := "card-grid")(
        items.map { case (value, label, descOpt) =>
          val isSelected = selected.contains(value)
          div(
            `class` := (if isSelected then "card card--selected" else "card"),
            onClick(onSelect(value))
          )(
            div(`class` := "card-title")(text(label)),
            descOpt match {
              case Some(desc) => div(`class` := "card-desc")(text(desc))
              case None       => div()
            }
          )
        }*
      )
    )

  /** Multi-select skill list: title, optional intro, pool, chosen set, max count, onToggle. */
  def skillPicker[Msg](
      title: String,
      intro: Option[String],
      pool: List[Skill],
      chosen: Set[Skill],
      max: Int,
      onToggle: Skill => Msg
  ): Html[Msg] = {
    val introNode: Html[Msg] =
      intro match {
        case Some(s) => p(`class` := "screen-intro")(text(s))
        case None    => div()
      }
    div(style := "margin-bottom: 1.5rem;")(
      h2(`class` := "about-heading")(text(title)),
      introNode,
      div(`class` := "points-pool", style := "margin-bottom: 0.5rem;")(
        text("Chosen: "),
        span(`class` := "points-pool-value")(text(s"${chosen.size} / $max"))
      ),
      div(
        pool.sortBy(_.label).map { skill =>
          val isChosen = chosen.contains(skill)
          val clsName  = if isChosen then "skill-item skill-item--selected" else "skill-item"
          div(`class` := clsName, onClick(onToggle(skill)))(
            div(`class` := "skill-checkbox")(text(if isChosen then "*" else "")),
            span(`class` := "skill-label")(text(skill.label)),
            span(`class` := "skill-ability-tag")(text(skill.ability.abbreviation))
          )
        }*
      )
    )
  }

  /** Section title + multi-select skill list grouped by ability (for extra skills at level-up). */
  def skillPickerGroupedByAbility[Msg](
      title: String,
      chosenCount: Int,
      maxCount: Int,
      byAbility: List[(Ability, List[Skill])],
      chosen: Set[Skill],
      onToggle: Skill => Msg
  ): Html[Msg] =
    div(style := "margin-bottom: 1.5rem;")(
      div(`class` := "section-title")(text(title)),
      div(`class` := "points-pool", style := "margin-bottom: 0.5rem;")(text(s"Chosen: $chosenCount / $maxCount")),
      div(
        byAbility.flatMap { case (ability, skills) =>
          if skills.isEmpty then Nil
          else
            List(
              div(`class` := "skill-group")(
                div(`class` := "skill-group-title")(text(ability.label)),
                div(
                  skills.map { skill =>
                    val isChosen = chosen.contains(skill)
                    val clsName  = if isChosen then "skill-item skill-item--selected" else "skill-item"
                    div(`class` := clsName, onClick(onToggle(skill)))(
                      div(`class` := "skill-checkbox")(text(if isChosen then "*" else "")),
                      span(`class` := "skill-label")(text(skill.label)),
                      span(`class` := "skill-ability-tag")(text(ability.abbreviation))
                    )
                  }*
                )
              )
            )
        }*
      )
    )
}
