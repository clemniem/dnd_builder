package dndbuilder.screens

import cats.effect.IO
import dndbuilder.{NavigateNext, Screen, ScreenId, ScreenOutput}
import dndbuilder.dnd.*
import tyrian.Html.*
import tyrian.*

object SpellsScreen extends Screen:
  type Model = SpellsModel
  type Msg   = SpellsMsg | NavigateNext

  val screenId: ScreenId = ScreenId.SpellsId

  def init(previous: Option[ScreenOutput]): (Model, Cmd[IO, Msg]) =
    val base = previous match
      case Some(ScreenOutput.EquipmentChosen(s, c, b, sc, bn, sk, ar, sh, wp)) =>
        SpellsModel(c, s, b, sc, bn, sk, ar, sh, wp, Nil, Nil, Nil, Phase.Cantrips)
      case Some(ScreenOutput.SpellsChosen(s, c, b, sc, bn, sk, ar, sh, wp, ct, ps, sb)) =>
        val phase = if c.cantripsKnown > 0 then Phase.Cantrips else Phase.Prepared
        SpellsModel(c, s, b, sc, bn, sk, ar, sh, wp, ct, ps, sb, phase)
      case _ =>
        SpellsModel(Wizard, Human, Acolyte, AbilityScores.default,
          BackgroundBonus.ThreePlusOnes(Ability.Strength, Ability.Dexterity, Ability.Constitution),
          Set.empty, None, false, Nil, Nil, Nil, Nil, Phase.Cantrips)
    val model =
      if base.dndClass.cantripsKnown == 0 then base.copy(phase = Phase.Prepared)
      else base
    (model, Cmd.None)

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) =
    case SpellsMsg.ToggleCantrip(spell) =>
      if model.chosenCantrips.exists(_.name == spell.name) then
        (model.copy(chosenCantrips = model.chosenCantrips.filterNot(_.name == spell.name)), Cmd.None)
      else if model.chosenCantrips.size < model.dndClass.cantripsKnown then
        (model.copy(chosenCantrips = model.chosenCantrips :+ spell), Cmd.None)
      else (model, Cmd.None)

    case SpellsMsg.ToggleSpellbook(spell) =>
      if model.spellbookSpells.exists(_.name == spell.name) then
        val newBook = model.spellbookSpells.filterNot(_.name == spell.name)
        val newPrepared = model.preparedSpells.filter(s => newBook.exists(_.name == s.name))
        (model.copy(spellbookSpells = newBook, preparedSpells = newPrepared), Cmd.None)
      else if model.spellbookSpells.size < model.dndClass.spellbookSize then
        (model.copy(spellbookSpells = model.spellbookSpells :+ spell), Cmd.None)
      else (model, Cmd.None)

    case SpellsMsg.TogglePrepared(spell) =>
      if model.preparedSpells.exists(_.name == spell.name) then
        (model.copy(preparedSpells = model.preparedSpells.filterNot(_.name == spell.name)), Cmd.None)
      else if model.preparedSpells.size < model.dndClass.numPreparedSpells then
        (model.copy(preparedSpells = model.preparedSpells :+ spell), Cmd.None)
      else (model, Cmd.None)

    case SpellsMsg.SetPhase(phase) =>
      (model.copy(phase = phase), Cmd.None)

    case SpellsMsg.Next =>
      val cantripsReady = model.chosenCantrips.size == model.dndClass.cantripsKnown
      val preparedReady = model.preparedSpells.size == model.dndClass.numPreparedSpells
      val spellbookReady = model.dndClass.spellbookSize == 0 || model.spellbookSpells.size == model.dndClass.spellbookSize

      model.phase match
        case Phase.Cantrips if cantripsReady =>
          if model.dndClass.spellbookSize > 0 then
            (model.copy(phase = Phase.Spellbook), Cmd.None)
          else if model.dndClass.numPreparedSpells > 0 then
            (model.copy(phase = Phase.Prepared), Cmd.None)
          else
            emitNext(model)
        case Phase.Spellbook if spellbookReady =>
          if model.dndClass.numPreparedSpells > 0 then
            (model.copy(phase = Phase.Prepared), Cmd.None)
          else emitNext(model)
        case Phase.Prepared if preparedReady =>
          emitNext(model)
        case _ => (model, Cmd.None)

    case SpellsMsg.Back =>
      model.phase match
        case Phase.Prepared if model.dndClass.spellbookSize > 0 =>
          (model.copy(phase = Phase.Spellbook), Cmd.None)
        case Phase.Prepared if model.dndClass.cantripsKnown > 0 =>
          (model.copy(phase = Phase.Cantrips), Cmd.None)
        case Phase.Spellbook if model.dndClass.cantripsKnown > 0 =>
          (model.copy(phase = Phase.Cantrips), Cmd.None)
        case _ =>
          val output = ScreenOutput.EquipmentChosen(
            model.species, model.dndClass, model.background,
            model.baseScores, model.backgroundBonus, model.chosenSkills,
            model.equippedArmor, model.equippedShield, model.equippedWeapons
          )
          (model, Cmd.Emit(NavigateNext(ScreenId.EquipmentId, Some(output))))

    case _: NavigateNext => (model, Cmd.None)

  private def emitNext(model: SpellsModel): (SpellsModel, Cmd[IO, SpellsMsg | NavigateNext]) =
    val output = ScreenOutput.SpellsChosen(
      model.species, model.dndClass, model.background,
      model.baseScores, model.backgroundBonus, model.chosenSkills,
      model.equippedArmor, model.equippedShield, model.equippedWeapons,
      model.chosenCantrips, model.preparedSpells, model.spellbookSpells
    )
    (model, Cmd.Emit(NavigateNext(ScreenId.ReviewId, Some(output))))

  def view(model: Model): Html[Msg] =
    div(`class` := "screen-container")(
      StepIndicator(7, model.dndClass.isSpellcaster),
      model.phase match
        case Phase.Cantrips  => cantripsView(model)
        case Phase.Spellbook => spellbookView(model)
        case Phase.Prepared  => preparedView(model)
    )

  private def cantripsView(model: SpellsModel): Html[Msg] =
    val available = Spell.cantripsForClass(model.dndClass)
    val remaining = model.dndClass.cantripsKnown - model.chosenCantrips.size
    val nextLabel = if model.dndClass.spellbookSize > 0 then "Next: Spellbook >"
      else if model.dndClass.numPreparedSpells > 0 then "Next: Spells >"
      else "Next: Review >"

    div(
      StepNav("< Equipment", SpellsMsg.Back, nextLabel, SpellsMsg.Next, remaining == 0),
      h1(`class` := "screen-title")(text("Choose Cantrips")),
      p(`class` := "screen-intro")(
        text(s"Select ${model.dndClass.cantripsKnown} cantrips from the ${model.dndClass.name} spell list.")
      ),
      div(`class` := "points-pool", style := "margin-bottom: 1rem;")(
        text("Remaining: "),
        span(`class` := "points-pool-value")(text(remaining.toString))
      ),
      spellListGrouped(available, model.chosenCantrips, SpellsMsg.ToggleCantrip.apply)
    )

  private def spellbookView(model: SpellsModel): Html[Msg] =
    val available = Spell.level1ForClass(model.dndClass)
    val remaining = model.dndClass.spellbookSize - model.spellbookSpells.size

    div(
      StepNav("< Cantrips", SpellsMsg.Back, "Next: Prepared Spells >", SpellsMsg.Next, remaining == 0),
      h1(`class` := "screen-title")(text("Build Your Spellbook")),
      p(`class` := "screen-intro")(
        text(s"Choose ${model.dndClass.spellbookSize} level 1 spells to inscribe in your spellbook.")
      ),
      div(`class` := "points-pool", style := "margin-bottom: 1rem;")(
        text("Remaining: "),
        span(`class` := "points-pool-value")(text(remaining.toString))
      ),
      spellListGrouped(available, model.spellbookSpells, SpellsMsg.ToggleSpellbook.apply)
    )

  private def preparedView(model: SpellsModel): Html[Msg] =
    val available =
      if model.dndClass.spellbookSize > 0 then model.spellbookSpells
      else Spell.level1ForClass(model.dndClass)
    val remaining = model.dndClass.numPreparedSpells - model.preparedSpells.size
    val backLabel =
      if model.dndClass.spellbookSize > 0 then "< Spellbook"
      else if model.dndClass.cantripsKnown > 0 then "< Cantrips"
      else "< Equipment"

    val title =
      if model.dndClass.spellbookSize > 0 then "Prepare Spells from Spellbook"
      else "Choose Prepared Spells"
    val intro =
      if model.dndClass.spellbookSize > 0 then
        s"Choose ${model.dndClass.numPreparedSpells} spells from your spellbook to prepare."
      else
        s"Select ${model.dndClass.numPreparedSpells} level 1 spells from the ${model.dndClass.name} spell list."

    div(
      StepNav(backLabel, SpellsMsg.Back, "Next: Review >", SpellsMsg.Next, remaining == 0),
      h1(`class` := "screen-title")(text(title)),
      p(`class` := "screen-intro")(text(intro)),
      div(`class` := "points-pool", style := "margin-bottom: 1rem;")(
        text("Remaining: "),
        span(`class` := "points-pool-value")(text(remaining.toString))
      ),
      spellListGrouped(available, model.preparedSpells, SpellsMsg.TogglePrepared.apply)
    )

  private def spellListGrouped(
      available: List[Spell],
      selected: List[Spell],
      toggleMsg: Spell => SpellsMsg
  ): Html[Msg] =
    val bySchool = available.groupBy(_.school).toList.sortBy(_._1.label)
    div(
      bySchool.map { case (school, spells) =>
        div(`class` := "skill-group")(
          div(`class` := "skill-group-title")(text(school.label)),
          div(
            spells.sortBy(_.name).map { spell =>
              val isChosen = selected.exists(_.name == spell.name)
              val cls = if isChosen then "skill-item skill-item--selected" else "skill-item"
              val ritualTag = if spell.ritual then " (R)" else ""
              div(`class` := cls, onClick(toggleMsg(spell)))(
                div(`class` := "skill-checkbox")(text(if isChosen then "*" else "")),
                span(`class` := "skill-label")(text(s"${spell.name}$ritualTag")),
                span(`class` := "skill-ability-tag")(text(spell.school.label.take(4)))
              )
            }*
          )
        )
      }*
    )

enum Phase:
  case Cantrips, Spellbook, Prepared

final case class SpellsModel(
    dndClass: DndClass,
    species: Species,
    background: Background,
    baseScores: AbilityScores,
    backgroundBonus: BackgroundBonus,
    chosenSkills: Set[Skill],
    equippedArmor: Option[Armor],
    equippedShield: Boolean,
    equippedWeapons: List[Weapon],
    chosenCantrips: List[Spell],
    preparedSpells: List[Spell],
    spellbookSpells: List[Spell],
    phase: Phase)

enum SpellsMsg:
  case ToggleCantrip(spell: Spell)
  case ToggleSpellbook(spell: Spell)
  case TogglePrepared(spell: Spell)
  case SetPhase(phase: Phase)
  case Next
  case Back
