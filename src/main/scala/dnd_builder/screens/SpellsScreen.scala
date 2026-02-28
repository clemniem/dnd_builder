package dndbuilder.screens

import cats.effect.IO
import dndbuilder.{NavigateNext, Screen, ScreenId, ScreenOutput}
import dndbuilder.dnd.*
import tyrian.Html.*
import tyrian.*

object SpellsScreen extends Screen {
  type Model = SpellsModel
  type Msg   = SpellsMsg | NavigateNext

  val screenId: ScreenId = ScreenId.SpellsId

  private def spellInfo(draft: CharacterDraft): (DndClass, Int, Int, Int, Int) = {
    val cls = draft.dndClass.getOrElse(Wizard)
    val lvl = draft.level.getOrElse(1)
    val prog = SpellProgression.forClass(cls, lvl)
    val cantrips = prog.map(_.cantrips).getOrElse(0)
    val prepared = prog.map(_.preparedSpells).getOrElse(0)
    val spellbook = if cls == Wizard then SpellProgression.wizardSpellbookSize(lvl) else 0
    (cls, cantrips, prepared, spellbook, SpellProgression.maxSpellLevelForSlots(cls, lvl))
  }

  def init(previous: Option[ScreenOutput]): (Model, Cmd[IO, Msg]) = {
    val draft = previous match {
      case Some(ScreenOutput.Draft(d)) => d
      case _ => CharacterDraft.empty
    }
    val (cls, cantrips, _, _, _) = spellInfo(draft)
    val phase = if cantrips > 0 then Phase.Cantrips else Phase.Prepared
    (SpellsModel(draft, draft.chosenCantrips, draft.preparedSpells, draft.spellbookSpells, phase), Cmd.None)
  }

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = {
    case SpellsMsg.ToggleCantrip(spell) =>
      val (_, cantrips, _, _, _) = spellInfo(model.draft)
      if model.chosenCantrips.exists(_.name == spell.name) then
        (model.copy(chosenCantrips = model.chosenCantrips.filterNot(_.name == spell.name)), Cmd.None)
      else if model.chosenCantrips.size < cantrips then
        (model.copy(chosenCantrips = model.chosenCantrips :+ spell), Cmd.None)
      else (model, Cmd.None)

    case SpellsMsg.ToggleSpellbook(spell) =>
      val (_, _, _, spellbookSize, _) = spellInfo(model.draft)
      if model.spellbookSpells.exists(_.name == spell.name) then {
        val newBook = model.spellbookSpells.filterNot(_.name == spell.name)
        val newPrepared = model.preparedSpells.filter(s => newBook.exists(_.name == s.name))
        (model.copy(spellbookSpells = newBook, preparedSpells = newPrepared), Cmd.None)
      }
      else if model.spellbookSpells.size < spellbookSize then
        (model.copy(spellbookSpells = model.spellbookSpells :+ spell), Cmd.None)
      else (model, Cmd.None)

    case SpellsMsg.TogglePrepared(spell) =>
      val (_, _, prepared, _, _) = spellInfo(model.draft)
      if model.preparedSpells.exists(_.name == spell.name) then
        (model.copy(preparedSpells = model.preparedSpells.filterNot(_.name == spell.name)), Cmd.None)
      else if model.preparedSpells.size < prepared then
        (model.copy(preparedSpells = model.preparedSpells :+ spell), Cmd.None)
      else (model, Cmd.None)

    case SpellsMsg.SetPhase(phase) =>
      (model.copy(phase = phase), Cmd.None)

    case SpellsMsg.Next =>
      val (_, cantrips, prepared, spellbookSize, _) = spellInfo(model.draft)
      val cantripsReady = model.chosenCantrips.size == cantrips
      val preparedReady = model.preparedSpells.size == prepared
      val spellbookReady = spellbookSize == 0 || model.spellbookSpells.size == spellbookSize

      model.phase match {
        case Phase.Cantrips if cantripsReady =>
          if spellbookSize > 0 then
            (model.copy(phase = Phase.Spellbook), Cmd.None)
          else if prepared > 0 then
            (model.copy(phase = Phase.Prepared), Cmd.None)
          else
            emitNext(model)
        case Phase.Spellbook if spellbookReady =>
          if prepared > 0 then
            (model.copy(phase = Phase.Prepared), Cmd.None)
          else emitNext(model)
        case Phase.Prepared if preparedReady =>
          emitNext(model)
        case _ => (model, Cmd.None)
      }

    case SpellsMsg.Back =>
      val (_, cantrips, _, spellbookSize, _) = spellInfo(model.draft)
      model.phase match {
        case Phase.Prepared if spellbookSize > 0 =>
          (model.copy(phase = Phase.Spellbook), Cmd.None)
        case Phase.Prepared if cantrips > 0 =>
          (model.copy(phase = Phase.Cantrips), Cmd.None)
        case Phase.Spellbook if cantrips > 0 =>
          (model.copy(phase = Phase.Cantrips), Cmd.None)
        case _ =>
          val updated = model.draft.copy(
            chosenCantrips = model.chosenCantrips,
            preparedSpells = model.preparedSpells,
            spellbookSpells = model.spellbookSpells
          )
          (model, Cmd.Emit(NavigateNext(ScreenId.ClassFeaturesId, Some(ScreenOutput.Draft(updated)))))
      }

    case _: NavigateNext => (model, Cmd.None)
  }

  private def emitNext(model: SpellsModel): (SpellsModel, Cmd[IO, SpellsMsg | NavigateNext]) = {
    val updated = model.draft.copy(
      chosenCantrips = model.chosenCantrips,
      preparedSpells = model.preparedSpells,
      spellbookSpells = model.spellbookSpells
    )
    (model, Cmd.Emit(NavigateNext(ScreenId.LanguagesId, Some(ScreenOutput.Draft(updated)))))
  }

  def view(model: Model): Html[Msg] = {
    val cls = model.draft.dndClass.getOrElse(Wizard)
    div(`class` := "screen-container")(
      StepIndicator(9, cls.isSpellcaster),
      model.phase match {
        case Phase.Cantrips  => cantripsView(model)
        case Phase.Spellbook => spellbookView(model)
        case Phase.Prepared  => preparedView(model)
      }
    )
  }

  private def cantripsView(model: SpellsModel): Html[Msg] = {
    val (cls, cantrips, prepared, spellbookSize, _) = spellInfo(model.draft)
    val available = Spell.cantripsForClass(cls)
    val remaining = cantrips - model.chosenCantrips.size
    val nextLabel = if spellbookSize > 0 then "Next: Spellbook >"
      else if prepared > 0 then "Next: Spells >"
      else "Next: Review >"

    div(
      StepNav("< Class Features", SpellsMsg.Back, nextLabel, SpellsMsg.Next, remaining == 0),
      h1(`class` := "screen-title")(text("Choose Cantrips")),
      p(`class` := "screen-intro")(
        text(s"Select $cantrips cantrips from the ${cls.name} spell list.")
      ),
      div(`class` := "points-pool", style := "margin-bottom: 1rem;")(
        text("Remaining: "),
        span(`class` := "points-pool-value")(text(remaining.toString))
      ),
      spellListGrouped(available, model.chosenCantrips, SpellsMsg.ToggleCantrip.apply)
    )
  }

  private def spellbookView(model: SpellsModel): Html[Msg] = {
    val (cls, _, _, spellbookSize, maxSpellLvl) = spellInfo(model.draft)
    val available = (1 to maxSpellLvl).flatMap(l => Spell.forClass(cls, l)).toList
    val remaining = spellbookSize - model.spellbookSpells.size

    div(
      StepNav("< Cantrips", SpellsMsg.Back, "Next: Prepared Spells >", SpellsMsg.Next, remaining == 0),
      h1(`class` := "screen-title")(text("Build Your Spellbook")),
      p(`class` := "screen-intro")(
        text(s"Choose $spellbookSize spells to inscribe in your spellbook.")
      ),
      div(`class` := "points-pool", style := "margin-bottom: 1rem;")(
        text("Remaining: "),
        span(`class` := "points-pool-value")(text(remaining.toString))
      ),
      spellListGrouped(available, model.spellbookSpells, SpellsMsg.ToggleSpellbook.apply)
    )
  }

  private def preparedView(model: SpellsModel): Html[Msg] = {
    val (cls, cantrips, prepared, spellbookSize, maxSpellLvl) = spellInfo(model.draft)
    val available =
      if spellbookSize > 0 then model.spellbookSpells
      else (1 to maxSpellLvl).flatMap(l => Spell.forClass(cls, l)).toList
    val remaining = prepared - model.preparedSpells.size
    val backLabel =
      if spellbookSize > 0 then "< Spellbook"
      else if cantrips > 0 then "< Cantrips"
      else "< Class Features"

    val title =
      if spellbookSize > 0 then "Prepare Spells from Spellbook"
      else "Choose Prepared Spells"
    val intro =
      if spellbookSize > 0 then
        s"Choose $prepared spells from your spellbook to prepare."
      else
        s"Select $prepared spells from the ${cls.name} spell list."

    div(
      StepNav(backLabel, SpellsMsg.Back, "Next: Languages >", SpellsMsg.Next, remaining == 0),
      h1(`class` := "screen-title")(text(title)),
      p(`class` := "screen-intro")(text(intro)),
      div(`class` := "points-pool", style := "margin-bottom: 1rem;")(
        text("Remaining: "),
        span(`class` := "points-pool-value")(text(remaining.toString))
      ),
      spellListGrouped(available, model.preparedSpells, SpellsMsg.TogglePrepared.apply)
    )
  }

  private def spellListGrouped(
      available: List[Spell],
      selected: List[Spell],
      toggleMsg: Spell => SpellsMsg
  ): Html[Msg] = {
    val bySchool = available.groupBy(_.school).toList.sortBy(_._1.label)
    div(
      bySchool.map { case (school, spells) =>
        div(`class` := "skill-group")(
          div(`class` := "skill-group-title")(text(school.label)),
          div(
            spells.sortBy(_.name).map { spell =>
              val isChosen = selected.exists(_.name == spell.name)
              val clsName = if isChosen then "skill-item skill-item--selected" else "skill-item"
              val ritualTag = if spell.ritual then " (R)" else ""
              div(`class` := clsName, onClick(toggleMsg(spell)))(
                div(`class` := "skill-checkbox")(text(if isChosen then "*" else "")),
                span(`class` := "skill-label")(text(s"${spell.name}$ritualTag")),
                span(`class` := "skill-ability-tag")(text(spell.school.label.take(4)))
              )
            }*
          )
        )
      }*
    )
  }
}

enum Phase {
  case Cantrips, Spellbook, Prepared
}

final case class SpellsModel(
    draft: CharacterDraft,
    chosenCantrips: List[Spell],
    preparedSpells: List[Spell],
    spellbookSpells: List[Spell],
    phase: Phase)

enum SpellsMsg {
  case ToggleCantrip(spell: Spell)
  case ToggleSpellbook(spell: Spell)
  case TogglePrepared(spell: Spell)
  case SetPhase(phase: Phase)
  case Next
  case Back
}
