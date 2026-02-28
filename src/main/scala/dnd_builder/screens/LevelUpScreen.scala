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
    val currentLevel = ch.characterLevel
    val nextLevel = currentLevel + 1
    val cls = ch.primaryClass
    val nextClassLevel = ch.primaryClassLevel + 1
    val gain = ClassProgression.atLevel(cls, nextClassLevel)
    val conMod = ch.modifier(Ability.Constitution)
    val hpGain = cls.hitDie.avgGain + conMod + ch.species.hpBonusPerLevel
    val newProf = nextLevel match {
      case l if l <= 4  => 2
      case l if l <= 8  => 3
      case l if l <= 12 => 4
      case l if l <= 16 => 5
      case _            => 6
    }
    val oldProf = ch.proficiencyBonus
    val profChanged = newProf != oldProf

    val newProg = SpellProgression.forClass(cls, nextClassLevel)
    val oldProg = ch.spellProgression

    div(`class` := "screen-container")(
      div(`class` := "screen-header")(
        h1(`class` := "screen-title")(text(s"Level Up: ${ch.name}")),
        button(`class` := "btn-ghost", onClick(LevelUpMsg.Cancel))(text("< Back"))
      ),
      div(`class` := "flex-row", style := "margin-bottom: 1rem; gap: 0.5rem;")(
        span(`class` := "badge")(text(s"${cls.name} ${ch.primaryClassLevel}")),
        span(`class` := "badge", style := "font-weight: bold;")(text(s"-> Level $nextLevel"))
      ),
      div(`class` := "section-title")(text("You Gain")),
      div(`class` := "stat-block", style := "margin-bottom: 1rem;")(
        statBox("HP", s"+$hpGain", s"d${cls.hitDie.sides} + CON"),
        statBox("Hit Dice", s"${nextLevel}d${cls.hitDie.sides}", ""),
        (if profChanged then statBox("Prof. Bonus", s"+$newProf", s"was +$oldProf") else div())
      ),
      spellChanges(oldProg, newProg),
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
      div(style := "margin-top: 1.5rem;")(
        button(
          `class` := (if model.saving then "btn-primary btn-lg btn-disabled" else "btn-primary btn-lg"),
          onClick(LevelUpMsg.Confirm)
        )(text(if model.saving then "Saving..." else s"Confirm Level Up to $nextLevel"))
      )
    )
  }

  private def spellChanges(
      old: Option[SpellSlotRow],
      next: Option[SpellSlotRow]
  ): Html[Msg] =
    (old, next) match {
      case (Some(o), Some(n)) =>
        val cantripDiff = n.cantrips - o.cantrips
        val preparedDiff = n.preparedSpells - o.preparedSpells
        val slotChanges = n.slots.zip(o.slots).zipWithIndex.collect {
          case ((nv, ov), i) if nv != ov => s"Lv${i + 1}: $ov -> $nv"
        }
        if cantripDiff == 0 && preparedDiff == 0 && slotChanges.isEmpty then div()
        else {
          val lines: List[Html[Msg]] =
            (if cantripDiff > 0 then List(div(text(s"Cantrips: ${o.cantrips} -> ${n.cantrips} (+$cantripDiff)"))) else Nil) ++
            (if preparedDiff > 0 then List(div(text(s"Prepared: ${o.preparedSpells} -> ${n.preparedSpells} (+$preparedDiff)"))) else Nil) ++
            slotChanges.map(s => div(text(s)))
          div(
            div(`class` := "section-title")(text("Spellcasting Changes")),
            div(style := "font-size: 0.85rem; color: var(--color-text-muted); margin-bottom: 1rem;")(
              lines*
            )
          )
        }
      case (None, Some(n)) =>
        div(
          div(`class` := "section-title")(text("Spellcasting")),
          div(style := "font-size: 0.85rem; color: var(--color-text-muted); margin-bottom: 1rem;")(
            text(s"Cantrips: ${n.cantrips}, Prepared: ${n.preparedSpells}")
          )
        )
      case _ => div()
    }

  private def errorsView(errors: List[String]): Html[Msg] =
    if errors.isEmpty then div()
    else div(`class` := "error-box")(
      errors.map(e => div(text(e)))*
    )

  private def statBox(label: String, value: String, sub: String): Html[Msg] =
    div(`class` := "stat-box")(
      div(`class` := "stat-box-label")(text(label)),
      div(`class` := "stat-box-value")(text(value)),
      (if sub.nonEmpty then div(`class` := "stat-box-sub")(text(sub)) else div())
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
