package dndbuilder.screens

import cats.effect.IO
import dndbuilder.{NavigateNext, Screen, ScreenId, ScreenOutput, StorageKeys, StoredCharacter}
import dndbuilder.common.LocalStorageUtils
import dndbuilder.dnd.*
import tyrian.Html.*
import tyrian.*

object ChangeSpellsScreen extends Screen {
  type Model = ChangeSpellsModel
  type Msg   = ChangeSpellsMsg | NavigateNext

  val screenId: ScreenId = ScreenId.ChangeSpellsId

  def init(previous: Option[ScreenOutput]): (Model, Cmd[IO, Msg]) = {
    val sc = previous match {
      case Some(ScreenOutput.ChangeSpells(c)) => c
      case _ =>
        StoredCharacter("?", Character(
          "Unknown", Human, List(ClassLevel(DndClass.Barbarian, 1)), Acolyte,
          AbilityScores.default,
          BackgroundBonus.ThreePlusOnes(Ability.Intelligence, Ability.Wisdom, Ability.Charisma),
          Set.empty, None, false, Nil, Nil, Nil, Nil, ClassFeatureSelections.empty, None, Human.languages, Coins.empty, Nil
        ))
    }
    val ch = sc.character
    (ChangeSpellsModel(sc, ch.preparedSpells, false, Nil), Cmd.None)
  }

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = {
    case ChangeSpellsMsg.TogglePrepared(spell) =>
      val ch = model.storedCharacter.character
      val maxPrepared = ch.spellProgression.map(_.preparedSpells).getOrElse(0)
      val alwaysNames = alwaysPreparedNames(ch)
      if alwaysNames.contains(spell.name) then (model, Cmd.None)
      else if model.preparedSpells.exists(_.name == spell.name) then
        (model.copy(preparedSpells = model.preparedSpells.filterNot(_.name == spell.name)), Cmd.None)
      else if model.preparedSpells.size < maxPrepared then
        (model.copy(preparedSpells = model.preparedSpells :+ spell), Cmd.None)
      else (model, Cmd.None)

    case ChangeSpellsMsg.Save =>
      val ch = model.storedCharacter.character
      val updated = ch.copy(preparedSpells = model.preparedSpells)
      val updatedStored = StoredCharacter(model.storedCharacter.id, updated)
      (model.copy(saving = true),
        LocalStorageUtils.loadList[StoredCharacter, Msg](StorageKeys.characters)(
          existing => ChangeSpellsMsg.Loaded(existing, updatedStored),
          _ => ChangeSpellsMsg.Loaded(Nil, updatedStored),
          (msg, _) => ChangeSpellsMsg.Error(msg)
        ))

    case ChangeSpellsMsg.Loaded(existing, updatedChar) =>
      val newList = existing.map { sc =>
        if sc.id == updatedChar.id then updatedChar else sc
      }
      (model,
        LocalStorageUtils.saveList(StorageKeys.characters, newList)(
          _ => ChangeSpellsMsg.Saved(updatedChar),
          (msg, _) => ChangeSpellsMsg.Error(msg)
        ))

    case ChangeSpellsMsg.Saved(updatedChar) =>
      (model, Cmd.Emit(NavigateNext(ScreenId.DetailId, Some(ScreenOutput.ViewCharacter(updatedChar)))))

    case ChangeSpellsMsg.Error(msg) =>
      (model.copy(saving = false, errors = List(msg)), Cmd.None)

    case ChangeSpellsMsg.Cancel =>
      (model, Cmd.Emit(NavigateNext(ScreenId.DetailId,
        Some(ScreenOutput.ViewCharacter(model.storedCharacter)))))

    case _: NavigateNext => (model, Cmd.None)
  }

  def view(model: Model): Html[Msg] = {
    val ch = model.storedCharacter.character
    val cls = ch.primaryClass
    val lvl = ch.primaryClassLevel
    val prog = ch.spellProgression
    val maxPrepared = prog.map(_.preparedSpells).getOrElse(0)
    val maxSpellLvl = SpellProgression.maxSpellLevelForSlots(cls, lvl)
    val isWizard = cls.usesSpellbook
    val available =
      if isWizard then ch.spellbookSpells
      else (1 to maxSpellLvl).flatMap(l => Spell.forClass(cls, l)).toList
    val alwaysNames = alwaysPreparedNames(ch)
    val remaining = maxPrepared - model.preparedSpells.size
    val canSave = remaining == 0

    div(`class` := "screen-container")(
      div(`class` := "screen-container-inner")(
        h2(`class` := "screen-title")(text("Change Prepared Spells")),
        p(`class` := "screen-intro")(
          text(
            if isWizard then s"Choose $maxPrepared spells from your spellbook to prepare."
            else s"Choose $maxPrepared spells from the ${cls.name} spell list."
          )
        ),
        (if alwaysNames.nonEmpty then
          div(style := "margin-bottom: 1rem; padding: 0.5rem; background: var(--color-bg-muted); border-radius: 4px; font-size: 0.85rem;")(
            text(s"Always prepared: ${alwaysNames.mkString(", ")}")
          )
        else div()),
        div(`class` := "points-pool", style := "margin-bottom: 1rem;")(
          text("Remaining: "),
          span(`class` := "points-pool-value")(text(remaining.toString))
        ),
        spellListGrouped(available, model.preparedSpells, alwaysNames),
        (if model.errors.nonEmpty then
          div(`class` := "error-box")(model.errors.map(e => div(text(e)))*)
        else div()),
        StepNav(
          "Cancel",
          ChangeSpellsMsg.Cancel,
          if model.saving then "Saving..." else "Save",
          ChangeSpellsMsg.Save,
          canSave && !model.saving
        )
      )
    )
  }

  private def alwaysPreparedNames(ch: Character): Set[String] = {
    val subSpells = ch.subclass.toList.flatMap { sub =>
      (1 to ch.primaryClassLevel).flatMap(l => sub.alwaysPreparedByLevel.getOrElse(l, Nil))
    }
    subSpells.map(_.name).toSet
  }

  private def spellListGrouped(
      available: List[Spell],
      selected: List[Spell],
      alwaysNames: Set[String]
  ): Html[Msg] = {
    val bySchool = available.groupBy(_.school).toList.sortBy(_._1.label)
    div(
      bySchool.map { case (school, spells) =>
        div(`class` := "skill-group")(
          div(`class` := "skill-group-title")(text(school.label)),
          div(
            spells.sortBy(_.name).map { spell =>
              val isAlways = alwaysNames.contains(spell.name)
              val isChosen = selected.exists(_.name == spell.name) || isAlways
              val clsName =
                if isAlways then "skill-item skill-item--selected"
                else if isChosen then "skill-item skill-item--selected"
                else "skill-item"
              val ritualTag = if spell.ritual then " (R)" else ""
              val lockTag = if isAlways then " (always)" else ""
              div(
                `class` := clsName,
                onClick(ChangeSpellsMsg.TogglePrepared(spell))
              )(
                div(`class` := "skill-checkbox")(text(if isChosen then "*" else "")),
                span(`class` := "skill-label")(text(s"${spell.name}$ritualTag$lockTag")),
                span(`class` := "skill-ability-tag")(text(spell.school.label.take(4)))
              )
            }*
          )
        )
      }*
    )
  }
}

final case class ChangeSpellsModel(
    storedCharacter: StoredCharacter,
    preparedSpells: List[Spell],
    saving: Boolean,
    errors: List[String]
)

enum ChangeSpellsMsg {
  case TogglePrepared(spell: Spell)
  case Save
  case Cancel
  case Loaded(existing: List[StoredCharacter], updatedChar: StoredCharacter)
  case Saved(updatedChar: StoredCharacter)
  case Error(msg: String)
}
