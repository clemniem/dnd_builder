package dndbuilder.screens

import cats.effect.IO
import dndbuilder.{NavigateNext, Screen, ScreenId, ScreenOutput}
import dndbuilder.dnd.*
import tyrian.Html.*
import tyrian.*

object ClassSelectScreen extends Screen {
  type Model = ClassSelectModel
  type Msg   = ClassSelectMsg | NavigateNext

  val screenId: ScreenId = ScreenId.ClassSelectId

  def init(previous: Option[ScreenOutput]): (Model, Cmd[IO, Msg]) = {
    val draft = previous match {
      case Some(ScreenOutput.Draft(d)) => d
      case _ => CharacterDraft.empty
    }
    (ClassSelectModel(draft, draft.dndClass), Cmd.None)
  }

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = {
    case ClassSelectMsg.SelectClass(cls) =>
      (model.copy(selected = Some(cls)), Cmd.None)
    case ClassSelectMsg.Next =>
      model.selected match {
        case Some(cls) =>
          val updated = model.draft.copy(dndClass = Some(cls))
          (model, Cmd.Emit(NavigateNext(ScreenId.BackgroundId, Some(ScreenOutput.Draft(updated)))))
        case None =>
          (model, Cmd.None)
      }
    case ClassSelectMsg.Back =>
      (model, Cmd.Emit(NavigateNext(ScreenId.SpeciesId, None)))
    case _: NavigateNext =>
      (model, Cmd.None)
  }

  def view(model: Model): Html[Msg] =
    div(`class` := "screen-container")(
      StepIndicator(2, false),
      StepNav("< Race", ClassSelectMsg.Back, "Next: Background >", ClassSelectMsg.Next, model.selected.isDefined),
      h1(`class` := "screen-title")(text("Choose Your Class")),
      p(`class` := "screen-intro")(text("Select a class to define your character's abilities and role.")),
      div(`class` := "card-grid")(
        DndClass.all.map(cls => classCard(cls, model.selected.contains(cls)))*
      )
    )

  private def classCard(cls: DndClass, isSelected: Boolean): Html[Msg] = {
    val cardClass = if isSelected then "card card--selected" else "card"
    div(
      `class` := cardClass,
      onClick(ClassSelectMsg.SelectClass(cls))
    )(
      div(`class` := "card-title")(text(cls.name)),
      div(`class` := "card-subtitle")(
        span(`class` := "badge")(text(s"d${cls.hitDie.sides}")),
        text(" "),
        span(text(cls.primaryAbilities.map(_.abbreviation).mkString(", ")))
      ),
      div(`class` := "card-desc")(text(cls.description))
    )
  }
}

final case class ClassSelectModel(draft: CharacterDraft, selected: Option[DndClass])

enum ClassSelectMsg {
  case SelectClass(cls: DndClass)
  case Next
  case Back
}
