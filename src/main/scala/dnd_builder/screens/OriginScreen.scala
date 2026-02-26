package dndbuilder.screens

import cats.effect.IO
import dndbuilder.{NavigateNext, Screen, ScreenId, ScreenOutput}
import dndbuilder.dnd.*
import tyrian.Html.*
import tyrian.*

object OriginScreen extends Screen:
  type Model = OriginModel
  type Msg   = OriginMsg | NavigateNext

  val screenId: ScreenId = ScreenId.OriginId

  def init(previous: Option[ScreenOutput]): (Model, Cmd[IO, Msg]) =
    val cls = previous match
      case Some(ScreenOutput.ClassChosen(c)) => c
      case _ => Barbarian
    (OriginModel(cls, None, None, None), Cmd.None)

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) =
    case OriginMsg.SelectTemplate(t) =>
      (model.copy(
        selectedTemplate = Some(t),
        selectedSpecies = Some(t.defaultInstance),
        selectedBackground = model.selectedBackground
      ), Cmd.None)

    case OriginMsg.SelectSpecies(sp) =>
      (model.copy(selectedSpecies = Some(sp)), Cmd.None)

    case OriginMsg.SelectBackground(bg) =>
      (model.copy(selectedBackground = Some(bg)), Cmd.None)

    case OriginMsg.Next =>
      (model.selectedSpecies, model.selectedBackground) match
        case (Some(sp), Some(bg)) =>
          val output = ScreenOutput.OriginChosen(model.dndClass, sp, bg)
          (model, Cmd.Emit(NavigateNext(ScreenId.AbilitiesId, Some(output))))
        case _ =>
          (model, Cmd.None)

    case OriginMsg.Back =>
      (model, Cmd.Emit(NavigateNext(ScreenId.ClassSelectId, None)))

    case _: NavigateNext =>
      (model, Cmd.None)

  def view(model: Model): Html[Msg] =
    div(`class` := "screen-container")(
      StepIndicator(2),
      h1(`class` := "screen-title")(text("Choose Your Origin")),
      p(`class` := "screen-intro")(text("Pick a species and background for your character.")),
      speciesSection(model),
      backgroundSection(model),
      div(`class` := "nav-row")(
        button(`class` := "btn-ghost", onClick(OriginMsg.Back))(text("< Class")),
        button(
          `class` := (
            if model.selectedSpecies.isDefined && model.selectedBackground.isDefined
            then "btn-primary btn-lg"
            else "btn-primary btn-lg btn-disabled"
          ),
          onClick(OriginMsg.Next)
        )(text("Next: Abilities >"))
      )
    )

  private def speciesSection(model: Model): Html[Msg] =
    div(
      h2(text("Species")),
      div(`class` := "card-grid", style := "margin-top: 0.75rem; margin-bottom: 1.25rem;")(
        Species.allTemplates.map { t =>
          val isSel = model.selectedTemplate.contains(t)
          div(
            `class` := (if isSel then "card card--selected" else "card"),
            onClick(OriginMsg.SelectTemplate(t))
          )(
            div(`class` := "card-title")(text(t.name))
          )
        }*
      ),
      subChoiceSection(model)
    )

  private def subChoiceSection(model: Model): Html[Msg] =
    model.selectedTemplate match
      case Some(t) if t.needsSubchoice =>
        val options: List[(String, Species)] = t match
          case Species.DragonbornTemplate =>
            DragonAncestry.values.toList.map(a => (a.label, DragonbornOf(a)))
          case Species.ElfTemplate =>
            ElvenLineage.values.toList.map(l => (l.label, Elf(l)))
          case Species.GnomeTemplate =>
            GnomishLineage.values.toList.map(l => (l.label, Gnome(l)))
          case Species.GoliathTemplate =>
            GiantAncestry.values.toList.map(a => (a.label, Goliath(a)))
          case Species.TieflingTemplate =>
            FiendishLegacy.values.toList.map(l => (l.label, Tiefling(l)))
          case _ => Nil

        div(`class` := "field-block")(
          label(`class` := "label-block")(text(s"${t.name} Lineage / Ancestry")),
          div(`class` := "flex-row", style := "flex-wrap: wrap; gap: 0.5rem;")(
            options.map { case (lbl, sp) =>
              val isSel = model.selectedSpecies.contains(sp)
              button(
                `class` := (if isSel then "btn-primary btn-sm" else "btn-ghost btn-sm"),
                onClick(OriginMsg.SelectSpecies(sp))
              )(text(lbl))
            }*
          )
        )
      case _ =>
        div()

  private def backgroundSection(model: Model): Html[Msg] =
    div(
      h2(text("Background")),
      div(`class` := "card-grid--2col card-grid", style := "margin-top: 0.75rem;")(
        Background.all.map { bg =>
          val isSel = model.selectedBackground.contains(bg)
          div(
            `class` := (if isSel then "card card--selected" else "card"),
            onClick(OriginMsg.SelectBackground(bg))
          )(
            div(`class` := "card-title")(text(bg.name)),
            div(`class` := "card-desc")(text(bg.description)),
            div(`class` := "flex-row", style := "margin-top: 0.5rem; gap: 0.4rem;")(
              span(`class` := "badge")(text(bg.feat.name)),
              span(`class` := "badge")(text(bg.skillProficiencySet.map(_.label).mkString(", ")))
            ),
            div(style := "margin-top: 0.3rem; font-size: 0.8rem; color: var(--color-text-dim);")(
              text(s"Abilities: ${bg.abilityOptionsList.map(_.abbreviation).mkString(", ")}")
            )
          )
        }*
      )
    )

final case class OriginModel(
    dndClass: DndClass,
    selectedTemplate: Option[Species.SpeciesTemplate],
    selectedSpecies: Option[Species],
    selectedBackground: Option[Background])

enum OriginMsg:
  case SelectTemplate(t: Species.SpeciesTemplate)
  case SelectSpecies(sp: Species)
  case SelectBackground(bg: Background)
  case Next
  case Back
