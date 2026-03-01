package dndbuilder.screens

import cats.effect.IO
import dndbuilder.{NavigateNext, Screen, ScreenId, ScreenOutput}
import dndbuilder.dnd.*
import tyrian.Html.*
import tyrian.*

object SpeciesScreen extends Screen {
  type Model = SpeciesModel
  type Msg   = SpeciesMsg | NavigateNext

  val screenId: ScreenId = ScreenId.SpeciesId

  def init(previous: Option[ScreenOutput]): (Model, Cmd[IO, Msg]) =
    (SpeciesModel(None, None), Cmd.None)

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = {
    case SpeciesMsg.SelectTemplate(t) =>
      (model.copy(
        selectedTemplate = Some(t),
        selectedSpecies = Some(t.defaultInstance)
      ), Cmd.None)

    case SpeciesMsg.SelectSpecies(sp) =>
      (model.copy(selectedSpecies = Some(sp)), Cmd.None)

    case SpeciesMsg.Next =>
      model.selectedSpecies match {
        case Some(sp) =>
          val draft = CharacterDraft.empty.copy(species = Some(sp))
          (model, Cmd.Emit(NavigateNext(ScreenId.ClassSelectId, Some(ScreenOutput.Draft(draft)))))
        case None =>
          (model, Cmd.None)
      }

    case SpeciesMsg.Back =>
      (model, Cmd.Emit(NavigateNext(ScreenId.HomeId, None)))

    case _: NavigateNext =>
      (model, Cmd.None)
  }

  def view(model: Model): Html[Msg] =
    div(`class` := "screen-container")(
      StepIndicator(1, false),
      StepNav(StepIndicator.backLabel(1, false), SpeciesMsg.Back, StepIndicator.nextLabel(1, false), SpeciesMsg.Next, model.selectedSpecies.isDefined),
      h1(`class` := "screen-title")(text("Choose Your Race")),
      p(`class` := "screen-intro")(text("Select a species to define your character's heritage and innate traits.")),
      div(`class` := "card-grid")(
        Species.allTemplates.map { t =>
          val isSel = model.selectedTemplate.contains(t)
          val sp = if isSel then model.selectedSpecies else Some(t.defaultInstance)
          div(
            `class` := (if isSel then "card card--selected" else "card"),
            onClick(SpeciesMsg.SelectTemplate(t))
          )(
            div(`class` := "card-title")(text(t.name)),
            sp.map { s =>
              div(`class` := "card-desc")(
                div(`class` := "flex-row", style := "gap: 0.4rem; margin-bottom: 0.3rem;")(
                  span(`class` := "badge")(text(s.size.label)),
                  span(`class` := "badge")(text(s"${s.speed}ft")),
                  s.darkvision.map(d => span(`class` := "badge")(text(s"Darkvision ${d}ft"))).getOrElse(span())
                ),
                div(style := "font-size: 0.8rem; color: var(--color-text-muted);")(
                  text(s.traits.take(3).mkString(", "))
                )
              )
            }.getOrElse(div())
          )
        }*
      ),
      subChoiceSection(model),
      selectedTraitsSection(model)
    )

  private def subChoiceSection(model: Model): Html[Msg] =
    model.selectedTemplate match {
      case Some(t) if t.needsSubchoice =>
        val options: List[(String, Species)] = t match {
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
        }

        div(`class` := "field-block", style := "margin-top: 1rem;")(
          label(`class` := "label-block")(text(s"${t.name} Lineage / Ancestry")),
          div(`class` := "flex-row", style := "flex-wrap: wrap; gap: 0.5rem;")(
            options.map { case (lbl, sp) =>
              val isSel = model.selectedSpecies.contains(sp)
              button(
                `class` := (if isSel then "btn-primary btn-sm" else "btn-ghost btn-sm"),
                onClick(SpeciesMsg.SelectSpecies(sp))
              )(text(lbl))
            }*
          )
        )
      case _ =>
        div()
    }

  private def selectedTraitsSection(model: Model): Html[Msg] =
    model.selectedSpecies match {
      case Some(sp) =>
        div(style := "margin-top: 1rem;")(
          div(`class` := "section-title")(text(s"${sp.name} Traits${sp.subLabel.map(s => s" ($s)").getOrElse("")}")),
          div(`class` := "feature-list")(
            sp.traits.map { t =>
              div(`class` := "feature-item")(
                div(`class` := "feature-name")(text(t))
              )
            }*
          )
        )
      case None => div()
    }
}

final case class SpeciesModel(
    selectedTemplate: Option[Species.SpeciesTemplate],
    selectedSpecies: Option[Species])

enum SpeciesMsg {
  case SelectTemplate(t: Species.SpeciesTemplate)
  case SelectSpecies(sp: Species)
  case Next
  case Back
}
