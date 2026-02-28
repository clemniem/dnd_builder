package dndbuilder.screens

import cats.effect.IO
import dndbuilder.{NavigateNext, Screen, ScreenId, ScreenOutput, StorageKeys, StoredCharacter}
import dndbuilder.common.LocalStorageUtils
import dndbuilder.common.pdf.CharacterSheetPdf
import dndbuilder.dnd.*
import tyrian.Html.*
import tyrian.*

import scala.scalajs.js

object ReviewScreen extends Screen:
  type Model = ReviewModel
  type Msg   = ReviewMsg | NavigateNext

  val screenId: ScreenId = ScreenId.ReviewId

  def init(previous: Option[ScreenOutput]): (Model, Cmd[IO, Msg]) =
    val model = previous match
      case Some(ScreenOutput.SpellsChosen(s, c, b, sc, bn, sk, ar, sh, wp, ct, ps, sb, fs)) =>
        ReviewModel(c, s, b, sc, bn, sk, ar, sh, wp, ct, ps, sb, fs, "", Nil, false)
      case Some(ScreenOutput.ClassFeaturesChosen(s, c, b, sc, bn, sk, ar, sh, wp, fs)) =>
        ReviewModel(c, s, b, sc, bn, sk, ar, sh, wp, Nil, Nil, Nil, fs, "", Nil, false)
      case Some(ScreenOutput.EquipmentChosen(s, c, b, sc, bn, sk, ar, sh, wp)) =>
        ReviewModel(c, s, b, sc, bn, sk, ar, sh, wp, Nil, Nil, Nil, ClassFeatureSelections.empty, "", Nil, false)
      case _ =>
        ReviewModel(Barbarian, Human, Acolyte, AbilityScores.default,
          BackgroundBonus.ThreePlusOnes(Ability.Strength, Ability.Dexterity, Ability.Constitution),
          Set.empty[Skill], None, false, Nil, Nil, Nil, Nil, ClassFeatureSelections.empty, "", Nil, false)
    (model, Cmd.None)

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) =
    case ReviewMsg.SetName(n) =>
      (model.copy(name = n), Cmd.None)

    case ReviewMsg.Save =>
      val result = CharacterValidator.validate(
        model.name, model.species, model.dndClass, model.background,
        model.baseScores, model.backgroundBonus, model.chosenSkills,
        model.equippedArmor, model.equippedShield, model.equippedWeapons,
        model.chosenCantrips, model.preparedSpells, model.spellbookSpells,
        model.featureSelections, 1
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

    case ReviewMsg.ExportPdf =>
      CharacterSheetPdf.generate(buildCharacter(model))
      (model, Cmd.None)

    case ReviewMsg.Back =>
      if model.dndClass.isSpellcaster then
        val output = ScreenOutput.SpellsChosen(
          model.species, model.dndClass, model.background,
          model.baseScores, model.backgroundBonus, model.chosenSkills,
          model.equippedArmor, model.equippedShield, model.equippedWeapons,
          model.chosenCantrips, model.preparedSpells, model.spellbookSpells,
          model.featureSelections
        )
        (model, Cmd.Emit(NavigateNext(ScreenId.SpellsId, Some(output))))
      else
        val output = ScreenOutput.ClassFeaturesChosen(
          model.species, model.dndClass, model.background,
          model.baseScores, model.backgroundBonus, model.chosenSkills,
          model.equippedArmor, model.equippedShield, model.equippedWeapons,
          model.featureSelections
        )
        (model, Cmd.Emit(NavigateNext(ScreenId.ClassFeaturesId, Some(output))))

    case _: NavigateNext =>
      (model, Cmd.None)

  private def buildCharacter(model: Model): Character =
    Character(
      if model.name.trim.isEmpty then "Unnamed Hero" else model.name,
      model.species, model.dndClass, model.background,
      model.baseScores, model.backgroundBonus, model.chosenSkills,
      model.equippedArmor, model.equippedShield, model.equippedWeapons,
      model.chosenCantrips, model.preparedSpells, model.spellbookSpells,
      model.featureSelections,
      1
    )

  def view(model: Model): Html[Msg] =
    val character = buildCharacter(model)

    div(`class` := "screen-container")(
      StepIndicator(if model.dndClass.isSpellcaster then 9 else 8, model.dndClass.isSpellcaster),
      StepNav(
        if model.dndClass.isSpellcaster then "< Spells" else "< Equipment",
        ReviewMsg.Back, if model.saving then "Saving..." else "Save Character", ReviewMsg.Save, !model.saving),
      h1(`class` := "screen-title")(text("Review & Save")),
      div(`class` := "field-block")(
        label(`class` := "label-block")(text("Character Name")),
        input(`type` := "text", placeholder := "Enter name...", value := model.name,
          onInput(ReviewMsg.SetName.apply))
      ),
      div(style := "margin: 0.75rem 0;")(
        button(`class` := "btn btn--secondary", onClick(ReviewMsg.ExportPdf))(text("Export PDF"))
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
      spellsSummary(ch),
      div(`class` := "section-title")(text("Equipment")),
      div(style := "font-size: 0.85rem; color: var(--color-text-muted);")(
        div(text(s"Armor: ${ch.equippedArmor.fold("Unarmored")(_.name)}")),
        div(text(s"Shield: ${if ch.equippedShield then "+2 AC" else "No"}")),
        div(text(s"Weapons: ${if ch.equippedWeapons.isEmpty then "None" else ch.equippedWeapons.map(_.name).mkString(", ")}"))
      ),
      div(`class` := "section-title")(text("Proficiencies")),
      div(style := "font-size: 0.85rem; color: var(--color-text-muted);")(
        div(text(s"Armor: ${if ch.dndClass.armorProficiencies.isEmpty then "None" else ch.dndClass.armorProficiencies.map(_.label).mkString(", ")}")),
        div(text(s"Weapons: ${ch.dndClass.weaponSummary}")),
        div(text(s"Tools: ${ch.background.toolProficiency}"))
      )
    )

  private def spellsSummary(ch: Character): Html[Msg] =
    if !ch.isSpellcaster then div()
    else div(
      div(`class` := "section-title")(text("Spells")),
      (if ch.chosenCantrips.nonEmpty then
        div(style := "margin-bottom: 0.5rem;")(
          div(style := "font-weight: 500; margin-bottom: 0.25rem;")(text("Cantrips")),
          div(`class` := "prof-list")(
            ch.chosenCantrips.sortBy(_.name).map { s =>
              div(`class` := "prof-item prof-item--proficient")(
                text(s"${s.name} (${s.school.label})")
              )
            }*
          )
        )
      else div()),
      (if ch.spellbookSpells.nonEmpty then
        div(style := "margin-bottom: 0.5rem;")(
          div(style := "font-weight: 500; margin-bottom: 0.25rem;")(text("Spellbook")),
          div(`class` := "prof-list")(
            ch.spellbookSpells.sortBy(_.name).map { s =>
              val isPrepared = ch.preparedSpells.exists(_.name == s.name)
              div(`class` := (if isPrepared then "prof-item prof-item--proficient" else "prof-item"))(
                text(s"${s.name} (${s.school.label})${if isPrepared then " ★" else ""}")
              )
            }*
          )
        )
      else if ch.preparedSpells.nonEmpty then
        div(style := "margin-bottom: 0.5rem;")(
          div(style := "font-weight: 500; margin-bottom: 0.25rem;")(text("Prepared Spells")),
          div(`class` := "prof-list")(
            ch.preparedSpells.sortBy(_.name).map { s =>
              div(`class` := "prof-item prof-item--proficient")(
                text(s"${s.name} (${s.school.label})")
              )
            }*
          )
        )
      else div())
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
    equippedArmor: Option[Armor],
    equippedShield: Boolean,
    equippedWeapons: List[Weapon],
    chosenCantrips: List[Spell],
    preparedSpells: List[Spell],
    spellbookSpells: List[Spell],
    featureSelections: ClassFeatureSelections,
    name: String,
    errors: List[String],
    saving: Boolean)

enum ReviewMsg:
  case SetName(n: String)
  case Save
  case ExportPdf
  case Loaded(existing: List[StoredCharacter], newChar: StoredCharacter)
  case Saved
  case Error(msg: String)
  case Back
