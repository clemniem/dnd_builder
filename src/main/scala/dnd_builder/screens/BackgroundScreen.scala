package dndbuilder.screens

import cats.effect.IO
import dndbuilder.{NavigateNext, Screen, ScreenId, ScreenOutput}
import dndbuilder.dnd.*
import tyrian.Html.*
import tyrian.*

object BackgroundScreen extends Screen:
  type Model = BackgroundModel
  type Msg   = BackgroundMsg | NavigateNext

  val screenId: ScreenId = ScreenId.BackgroundId

  def init(previous: Option[ScreenOutput]): (Model, Cmd[IO, Msg]) =
    val (species, cls) = previous match
      case Some(ScreenOutput.ClassChosen(sp, c)) => (sp, c)
      case _ => (Human, Barbarian)
    (BackgroundModel(species, cls, None), Cmd.None)

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) =
    case BackgroundMsg.SelectBackground(bg) =>
      (model.copy(selectedBackground = Some(bg)), Cmd.None)

    case BackgroundMsg.Next =>
      model.selectedBackground match
        case Some(bg) =>
          val output = ScreenOutput.BackgroundChosen(model.species, model.dndClass, bg)
          (model, Cmd.Emit(NavigateNext(ScreenId.AbilitiesId, Some(output))))
        case None =>
          (model, Cmd.None)

    case BackgroundMsg.Back =>
      val output = ScreenOutput.SpeciesChosen(model.species)
      (model, Cmd.Emit(NavigateNext(ScreenId.ClassSelectId, Some(output))))

    case _: NavigateNext =>
      (model, Cmd.None)

  def view(model: Model): Html[Msg] =
    div(`class` := "screen-container")(
      StepIndicator(3),
      h1(`class` := "screen-title")(text("Choose Your Background")),
      p(`class` := "screen-intro")(text("Your background determines ability bonuses, skill proficiencies, and your origin feat.")),
      div(`class` := "card-grid--2col card-grid")(
        Background.all.map { bg =>
          val isSel = model.selectedBackground.contains(bg)
          div(
            `class` := (if isSel then "card card--selected" else "card"),
            onClick(BackgroundMsg.SelectBackground(bg))
          )(
            div(`class` := "card-title")(text(bg.name)),
            div(`class` := "card-desc")(text(bg.description)),
            div(`class` := "flex-row", style := "margin-top: 0.5rem; gap: 0.4rem;")(
              span(`class` := "badge badge--feat")(text(bg.feat.name)),
              span(`class` := "badge")(text(bg.skillProficiencySet.map(_.label).mkString(", ")))
            ),
            div(style := "margin-top: 0.3rem; font-size: 0.8rem; color: var(--color-text-dim);")(
              text(s"Abilities: ${bg.abilityOptionsList.map(_.abbreviation).mkString(", ")}"),
              text(s" | Tools: ${bg.toolProficiency}")
            ),
            div(style := "margin-top: 0.2rem; font-size: 0.8rem; color: var(--color-text-dim);")(
              text(s"Starting gold: ${bg.startingGold} GP")
            )
          )
        }*
      ),
      selectedBackgroundDetail(model),
      div(`class` := "nav-row")(
        button(`class` := "btn-ghost", onClick(BackgroundMsg.Back))(text("< Class")),
        button(
          `class` := (if model.selectedBackground.isDefined then "btn-primary btn-lg" else "btn-primary btn-lg btn-disabled"),
          onClick(BackgroundMsg.Next)
        )(text("Next: Abilities >"))
      )
    )

  private def selectedBackgroundDetail(model: Model): Html[Msg] =
    model.selectedBackground match
      case Some(bg) =>
        div(style := "margin-top: 1rem;")(
          div(`class` := "section-title")(text(s"${bg.name} Details")),
          div(`class` := "feature-list")(
            div(`class` := "feature-item")(
              div(`class` := "feature-name")(text("Origin Feat")),
              div(`class` := "feature-desc")(text(s"${bg.feat.name}: ${bg.feat.description}"))
            ),
            div(`class` := "feature-item")(
              div(`class` := "feature-name")(text("Skill Proficiencies")),
              div(`class` := "feature-desc")(text(bg.skillProficiencySet.map(_.label).mkString(", ")))
            ),
            div(`class` := "feature-item")(
              div(`class` := "feature-name")(text("Ability Score Bonuses")),
              div(`class` := "feature-desc")(
                text(s"Choose from ${bg.abilityOptionsList.map(_.label).mkString(", ")}: either +2/+1 or +1/+1/+1")
              )
            ),
            div(`class` := "feature-item")(
              div(`class` := "feature-name")(text("Tool Proficiency")),
              div(`class` := "feature-desc")(text(bg.toolProficiency))
            )
          )
        )
      case None => div()

final case class BackgroundModel(
    species: Species,
    dndClass: DndClass,
    selectedBackground: Option[Background])

enum BackgroundMsg:
  case SelectBackground(bg: Background)
  case Next
  case Back
