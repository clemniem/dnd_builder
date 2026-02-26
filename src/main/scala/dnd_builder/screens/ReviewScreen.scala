package dndbuilder.screens

import cats.effect.IO
import dndbuilder.{NavigateNext, Screen, ScreenId, ScreenOutput, StorageKeys, StoredCharacter}
import dndbuilder.common.LocalStorageUtils
import dndbuilder.dnd.*
import tyrian.Html.*
import tyrian.*

import scala.scalajs.js

object ReviewScreen extends Screen:
  type Model = ReviewModel
  type Msg   = ReviewMsg | NavigateNext

  val screenId: ScreenId = ScreenId.ReviewId

  def init(previous: Option[ScreenOutput]): (Model, Cmd[IO, Msg]) =
    val (cls, sp, bg, scores, bonus, skills) = previous match
      case Some(ScreenOutput.SkillsChosen(s, c, b, sc, bn, sk)) => (c, s, b, sc, bn, sk)
      case _ => (Barbarian, Human, Acolyte, AbilityScores.default,
        BackgroundBonus.ThreePlusOnes(Ability.Strength, Ability.Dexterity, Ability.Constitution), Set.empty[Skill])
    (ReviewModel(cls, sp, bg, scores, bonus, skills, "", Nil, false), Cmd.None)

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) =
    case ReviewMsg.SetName(n) =>
      (model.copy(name = n), Cmd.None)

    case ReviewMsg.Save =>
      val result = CharacterValidator.validate(
        model.name, model.species, model.dndClass, model.background,
        model.baseScores, model.backgroundBonus, model.chosenSkills, 1
      )
      result match
        case Left(errors) =>
          (model.copy(errors = errors.map(_.message)), Cmd.None)
        case Right(character) =>
          val stored = StoredCharacter(
            js.Dynamic.global.crypto.randomUUID().asInstanceOf[String],
            character
          )
          (model.copy(errors = Nil, saving = true),
            LocalStorageUtils.loadList[StoredCharacter, Msg](StorageKeys.characters)(
              existing => ReviewMsg.Loaded(existing, stored),
              _ => ReviewMsg.Loaded(Nil, stored),
              (msg, _) => ReviewMsg.Error(msg)
            ))

    case ReviewMsg.Loaded(existing, newChar) =>
      val updated = existing :+ newChar
      (model,
        LocalStorageUtils.saveList(StorageKeys.characters, updated)(
          _ => ReviewMsg.Saved,
          (msg, _) => ReviewMsg.Error(msg)
        ))

    case ReviewMsg.Saved =>
      (model, Cmd.Emit(NavigateNext(ScreenId.GalleryId, None)))

    case ReviewMsg.Error(msg) =>
      (model.copy(errors = List(msg), saving = false), Cmd.None)

    case ReviewMsg.Back =>
      val output = ScreenOutput.AbilitiesChosen(
        model.species, model.dndClass, model.background,
        model.baseScores, model.backgroundBonus
      )
      (model, Cmd.Emit(NavigateNext(ScreenId.SkillsId, Some(output))))

    case _: NavigateNext =>
      (model, Cmd.None)

  def view(model: Model): Html[Msg] =
    val character = Character(
      if model.name.trim.isEmpty then "Unnamed Hero" else model.name,
      model.species, model.dndClass, model.background,
      model.baseScores, model.backgroundBonus, model.chosenSkills, 1
    )

    div(`class` := "screen-container")(
      StepIndicator(6),
      StepNav("< Skills", ReviewMsg.Back, if model.saving then "Saving..." else "Save Character", ReviewMsg.Save, !model.saving),
      h1(`class` := "screen-title")(text("Review & Save")),
      div(`class` := "field-block")(
        label(`class` := "label-block")(text("Character Name")),
        input(`type` := "text", placeholder := "Enter name...", value := model.name,
          onInput(ReviewMsg.SetName.apply))
      ),
      errorsView(model.errors),
      characterSummary(character)
    )

  private def errorsView(errors: List[String]): Html[Msg] =
    if errors.isEmpty then div()
    else div(`class` := "error-box")(
      errors.map(e => div(text(e)))*
    )

  private def characterSummary(ch: Character): Html[Msg] =
    div(
      div(`class` := "flex-row", style := "margin-bottom: 0.5rem; gap: 0.5rem;")(
        span(`class` := "badge")(text(ch.dndClass.name)),
        span(`class` := "badge")(text(ch.species.name)),
        ch.species.subLabel.map(s => span(`class` := "badge")(text(s))).getOrElse(span()),
        span(`class` := "badge")(text(ch.background.name)),
        span(`class` := "badge badge--feat")(text(ch.originFeat.name))
      ),
      div(`class` := "stat-block")(
        statBox("HP", ch.maxHitPoints.toString, s"d${ch.dndClass.hitDie.sides}"),
        statBox("AC", ch.armorClass.toString, "unarmored"),
        statBox("Speed", s"${ch.speed}ft", ""),
        statBox("Initiative", AbilityScores.modifierString(10 + ch.initiative), ""),
        statBox("Prof. Bonus", s"+${ch.proficiencyBonus}", ""),
        statBox("Passive Perc.", ch.passivePerception.toString, "")
      ),
      ch.spellSaveDC match
        case Some(dc) =>
          div(`class` := "stat-block")(
            statBox("Spell Save DC", dc.toString, ""),
            statBox("Spell Attack", AbilityScores.modifierString(ch.spellAttackBonus.getOrElse(0)), ""),
            statBox("Cantrips", ch.dndClass.cantripsKnown.toString, ""),
            statBox("Spell Slots", ch.dndClass.level1SpellSlots.toString, "Level 1")
          )
        case None => div(),
      div(`class` := "section-title")(text("Ability Scores")),
      table(`class` := "ability-table")(
        thead(tr(th(text("Ability")), th(text("Score")), th(text("Mod")), th(text("Save")))),
        tbody(
          Ability.values.toList.map { ability =>
            val score = ch.finalScores.get(ability)
            val mod   = AbilityScores.modifierString(score)
            val save  = AbilityScores.modifierString(10 + ch.savingThrowBonus(ability) - 10)
            val profMark = if ch.isProficientInSave(ability) then " *" else ""
            tr(
              td(`class` := "ability-name")(text(ability.label)),
              td(`class` := "ability-score")(text(score.toString)),
              td(`class` := "ability-mod")(text(mod)),
              td(`class` := "ability-mod")(text(s"$save$profMark"))
            )
          }*
        )
      ),
      div(`class` := "section-title")(text("Skill Proficiencies")),
      div(`class` := "prof-list")(
        ch.allSkillProficiencies.toList.sortBy(_.label).map { skill =>
          div(`class` := "prof-item prof-item--proficient")(
            text(s"${skill.label} (${skill.ability.abbreviation}) ${AbilityScores.modifierString(ch.skillBonus(skill))}")
          )
        }*
      ),
      div(`class` := "section-title")(text("Class Features")),
      div(`class` := "feature-list")(
        ch.dndClass.level1Features.map { f =>
          div(`class` := "feature-item")(
            div(`class` := "feature-name")(text(f.name)),
            div(`class` := "feature-desc")(text(f.description))
          )
        }*
      ),
      div(`class` := "section-title")(text("Species Traits")),
      div(`class` := "feature-list")(
        ch.species.traits.map { t =>
          div(`class` := "feature-item")(
            div(`class` := "feature-name")(text(t))
          )
        }*
      ),
      div(`class` := "section-title")(text("Proficiencies")),
      div(style := "font-size: 0.85rem; color: var(--color-text-muted);")(
        div(text(s"Armor: ${if ch.dndClass.armorProficiencies.isEmpty then "None" else ch.dndClass.armorProficiencies.map(_.label).mkString(", ")}")),
        div(text(s"Weapons: ${ch.dndClass.weaponSummary}")),
        div(text(s"Tools: ${ch.background.toolProficiency}"))
      )
    )

  private def statBox(label: String, value: String, sub: String): Html[Msg] =
    div(`class` := "stat-box")(
      div(`class` := "stat-box-label")(text(label)),
      div(`class` := "stat-box-value")(text(value)),
      (if sub.nonEmpty then div(`class` := "stat-box-sub")(text(sub)) else div())
    )

final case class ReviewModel(
    dndClass: DndClass,
    species: Species,
    background: Background,
    baseScores: AbilityScores,
    backgroundBonus: BackgroundBonus,
    chosenSkills: Set[Skill],
    name: String,
    errors: List[String],
    saving: Boolean)

enum ReviewMsg:
  case SetName(n: String)
  case Save
  case Loaded(existing: List[StoredCharacter], newChar: StoredCharacter)
  case Saved
  case Error(msg: String)
  case Back
