package dndbuilder.screens

import cats.effect.IO
import dndbuilder.{NavigateNext, Screen, ScreenId, ScreenOutput, StorageKeys, StoredCharacter}
import dndbuilder.common.LocalStorageUtils
import dndbuilder.dnd.*
import tyrian.Html.*
import tyrian.*

object LevelUpScreen extends Screen {
  type Model = LevelUpModel
  type Msg   = LevelUpMsg | NavigateNext

  val screenId: ScreenId = ScreenId.LevelUpId

  def init(previous: Option[ScreenOutput]): (Model, Cmd[IO, Msg]) = {
    val sc = previous match {
      case Some(ScreenOutput.LevelUp(c)) => c
      case _ =>
        StoredCharacter("?", Character(
          "Unknown", Human, List(ClassLevel(Barbarian, 1)),
          Acolyte, AbilityScores.default,
          BackgroundBonus.ThreePlusOnes(Ability.Intelligence, Ability.Wisdom, Ability.Charisma),
          Set.empty, None, false, Nil, Nil, Nil, Nil, ClassFeatureSelections.empty, Human.languages
        ))
    }
    (LevelUpModel(sc, false, Nil), Cmd.None)
  }

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = {
    case LevelUpMsg.Confirm =>
      val ch = model.storedCharacter.character
      val newClassLevels = ch.classLevels match {
        case head :: tail => ClassLevel(head.dndClass, head.classLevel + 1) :: tail
        case Nil          => Nil
      }
      val updated = ch.copy(classLevels = newClassLevels)
      val updatedStored = StoredCharacter(model.storedCharacter.id, updated)
      (model.copy(saving = true),
        LocalStorageUtils.loadList[StoredCharacter, Msg](StorageKeys.characters)(
          existing => LevelUpMsg.Loaded(existing, updatedStored),
          _ => LevelUpMsg.Loaded(Nil, updatedStored),
          (msg, _) => LevelUpMsg.Error(msg)
        ))

    case LevelUpMsg.Loaded(existing, updatedChar) =>
      val newList = existing.map { sc =>
        if sc.id == updatedChar.id then updatedChar else sc
      }
      (model,
        LocalStorageUtils.saveList(StorageKeys.characters, newList)(
          _ => LevelUpMsg.Saved(updatedChar),
          (msg, _) => LevelUpMsg.Error(msg)
        ))

    case LevelUpMsg.Saved(updatedChar) =>
      (model, Cmd.Emit(NavigateNext(ScreenId.DetailId, Some(ScreenOutput.ViewCharacter(updatedChar)))))

    case LevelUpMsg.Error(msg) =>
      (model.copy(saving = false, errors = List(msg)), Cmd.None)

    case LevelUpMsg.Cancel =>
      (model, Cmd.Emit(NavigateNext(ScreenId.DetailId,
        Some(ScreenOutput.ViewCharacter(model.storedCharacter)))))

    case _: NavigateNext =>
      (model, Cmd.None)
  }

  def view(model: Model): Html[Msg] = {
    val ch = model.storedCharacter.character
    val cls = ch.primaryClass
    val currentLevel = ch.characterLevel
    val nextLevel = currentLevel + 1
    val nextClassLevel = ch.primaryClassLevel + 1
    val gain = ClassProgression.atLevel(cls, nextClassLevel)
    val conMod = ch.modifier(Ability.Constitution)
    val hpGain = cls.hitDie.avgGain + conMod.toInt + ch.species.hpBonusPerLevel
    val oldProf = ch.proficiencyBonus
    val newProf = nextLevel match {
      case l if l <= 4  => 2
      case l if l <= 8  => 3
      case l if l <= 12 => 4
      case l if l <= 16 => 5
      case _            => 6
    }
    val oldProg = ch.spellProgression
    val newProg = SpellProgression.forClass(cls, nextClassLevel)

    div(`class` := "screen-container")(
      div(`class` := "screen-header")(
        h1(`class` := "screen-title")(text(s"Level Up: ${ch.name}")),
        button(`class` := "btn-ghost", onClick(LevelUpMsg.Cancel))(text("< Back"))
      ),
      div(`class` := "flex-row", style := "margin-bottom: 1.5rem; gap: 0.5rem;")(
        span(`class` := "badge")(text(s"${cls.name} ${ch.primaryClassLevel}")),
        span(style := "font-size: 1.2rem; font-weight: bold; color: var(--color-text-muted);")(text("->")),
        span(`class` := "badge", style := "font-weight: bold;")(text(s"${cls.name} $nextClassLevel"))
      ),
      div(`class` := "section-title")(text("Stats Comparison")),
      combatTable(ch, hpGain, oldProf.toInt, newProf, nextLevel, cls),
      spellTable(oldProg, newProg),
      (if gain.features.nonEmpty then
        div(
          div(`class` := "section-title")(text("New Features")),
          div(`class` := "feature-list")(
            gain.features.map { f =>
              div(`class` := "feature-item")(
                div(`class` := "feature-name")(text(f.name)),
                div(`class` := "feature-desc")(text(f.description))
              )
            }*
          )
        )
      else div()),
      errorsView(model.errors),
      div(style := "margin-top: 1.5rem; display: flex; gap: 1rem;")(
        button(`class` := "btn-ghost", onClick(LevelUpMsg.Cancel))(text("Cancel")),
        button(
          `class` := (if model.saving then "btn-primary btn-lg btn-disabled" else "btn-primary btn-lg"),
          onClick(LevelUpMsg.Confirm)
        )(text(if model.saving then "Saving..." else s"Confirm Level Up to $nextLevel"))
      )
    )
  }

  private def combatTable(
      ch: Character,
      hpGain: Int,
      oldProf: Int,
      newProf: Int,
      nextLevel: Int,
      cls: DndClass
  ): Html[Msg] = {
    val oldHp = ch.maxHitPoints
    val newHp = oldHp + hpGain
    val oldHitDice = ch.hitDiceString
    val newHitDice = s"${nextLevel}d${cls.hitDie.sides}"

    val rows: List[Html[Msg]] = List(
      compareRow("Level", ch.characterLevel.toString, nextLevel.toString, ch.characterLevel != nextLevel),
      compareRow("Hit Points", oldHp.toString, s"$newHp (+$hpGain)", true),
      compareRow("Hit Dice", oldHitDice, newHitDice, oldHitDice != newHitDice),
      compareRow("Proficiency Bonus", s"+$oldProf", s"+$newProf", oldProf != newProf)
    )

    table(`class` := "compare-table")(
      thead(tr(th(text("")), th(text("Current")), th(text("New")))),
      tbody(rows*)
    )
  }

  private def spellTable(
      old: Option[SpellSlotRow],
      next: Option[SpellSlotRow]
  ): Html[Msg] =
    (old, next) match {
      case (Some(o), Some(n)) =>
        val rows: List[Html[Msg]] = List(
          if o.cantrips > 0 || n.cantrips > 0 then
            Some(compareRow("Cantrips Known", o.cantrips.toString, n.cantrips.toString, o.cantrips != n.cantrips))
          else None,
          if o.preparedSpells > 0 || n.preparedSpells > 0 then
            Some(compareRow("Prepared Spells", o.preparedSpells.toString, n.preparedSpells.toString, o.preparedSpells != n.preparedSpells))
          else None
        ).flatten ++ slotRows(o.slots, n.slots)

        if rows.isEmpty then div()
        else div(
          div(`class` := "section-title")(text("Spellcasting")),
          table(`class` := "compare-table")(
            thead(tr(th(text("")), th(text("Current")), th(text("New")))),
            tbody(rows*)
          )
        )
      case (None, Some(n)) =>
        val rows: List[Html[Msg]] = List(
          if n.cantrips > 0 then Some(compareRow("Cantrips Known", "-", n.cantrips.toString, true)) else None,
          if n.preparedSpells > 0 then Some(compareRow("Prepared Spells", "-", n.preparedSpells.toString, true)) else None
        ).flatten ++ n.slots.zipWithIndex.collect { case (cnt, i) if cnt > 0 =>
          compareRow(s"Level ${i + 1} Slots", "-", cnt.toString, true)
        }
        if rows.isEmpty then div()
        else div(
          div(`class` := "section-title")(text("Spellcasting (New!)")),
          table(`class` := "compare-table")(
            thead(tr(th(text("")), th(text("Current")), th(text("New")))),
            tbody(rows*)
          )
        )
      case _ => div()
    }

  private def slotRows(oldSlots: List[Int], newSlots: List[Int]): List[Html[Msg]] =
    oldSlots.zipAll(newSlots, 0, 0).zipWithIndex.collect {
      case ((ov, nv), i) if ov > 0 || nv > 0 =>
        compareRow(s"Level ${i + 1} Slots", ov.toString, nv.toString, ov != nv)
    }

  private def compareRow(label: String, oldVal: String, newVal: String, changed: Boolean): Html[Msg] =
    tr(
      td(text(label)),
      td(`class` := "val-old")(text(oldVal)),
      td(`class` := (if changed then "val-changed" else "val-new"))(text(newVal))
    )

  private def errorsView(errors: List[String]): Html[Msg] =
    if errors.isEmpty then div()
    else div(`class` := "error-box")(
      errors.map(e => div(text(e)))*
    )
}

final case class LevelUpModel(
    storedCharacter: StoredCharacter,
    saving: Boolean,
    errors: List[String])

enum LevelUpMsg {
  case Confirm
  case Cancel
  case Loaded(existing: List[StoredCharacter], updatedChar: StoredCharacter)
  case Saved(updatedChar: StoredCharacter)
  case Error(msg: String)
}
