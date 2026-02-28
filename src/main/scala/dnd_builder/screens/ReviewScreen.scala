package dndbuilder.screens

import cats.effect.IO
import dndbuilder.{NavigateNext, Screen, ScreenId, ScreenOutput, StorageKeys, StoredCharacter}
import dndbuilder.common.LocalStorageUtils
import dndbuilder.common.pdf.CharacterSheetPdf
import dndbuilder.dnd.*
import tyrian.Html.*
import tyrian.*

import scala.scalajs.js

object ReviewScreen extends Screen {
  type Model = ReviewModel
  type Msg   = ReviewMsg | NavigateNext

  val screenId: ScreenId = ScreenId.ReviewId

  def init(previous: Option[ScreenOutput]): (Model, Cmd[IO, Msg]) = {
    val draft = previous match {
      case Some(ScreenOutput.Draft(d)) => d
      case _ => CharacterDraft.empty
    }
    (ReviewModel(draft, "", Nil, false), Cmd.None)
  }

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = {
    case ReviewMsg.SetName(n) =>
      (model.copy(name = n), Cmd.None)

    case ReviewMsg.Save =>
      val d = model.draft
      val sp = d.species.getOrElse(Human: Species)
      val cls = d.dndClass.getOrElse(Barbarian: DndClass)
      val bg = d.background.getOrElse(Acolyte: Background)
      val scores = d.baseScores.getOrElse(AbilityScores.default)
      val bonus = d.backgroundBonus.getOrElse(BackgroundBonus.ThreePlusOnes(Ability.Strength, Ability.Dexterity, Ability.Constitution))
      val languages = sp.languages ++ d.chosenExtraLanguages
      val lvl = d.level.getOrElse(1)
      val result = CharacterValidator.validate(
        model.name, sp, cls, bg, scores, bonus, d.chosenSkills,
        d.equippedArmor, d.equippedShield, d.equippedWeapons,
        d.chosenCantrips, d.preparedSpells, d.spellbookSpells,
        d.featureSelections, languages, lvl, d.coins
      )
      result match {
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
      }

    case ReviewMsg.Loaded(existing, newChar) =>
      val updated = existing :+ newChar
      (model,
        LocalStorageUtils.saveList(StorageKeys.characters, updated)(
          _ => ReviewMsg.Saved(newChar),
          (msg, _) => ReviewMsg.Error(msg)
        ))

    case ReviewMsg.Saved(saved) =>
      (model, Cmd.Emit(NavigateNext(ScreenId.DetailId, Some(ScreenOutput.ViewCharacter(saved)))))

    case ReviewMsg.Error(msg) =>
      (model.copy(errors = List(msg), saving = false), Cmd.None)

    case ReviewMsg.ExportPdf =>
      org.scalajs.dom.console.log("[Export PDF] clicked, building character and calling generate")
      CharacterSheetPdf.generate(buildCharacter(model))
      (model, Cmd.None)

    case ReviewMsg.Back =>
      (model, Cmd.Emit(NavigateNext(ScreenId.LanguagesId, Some(ScreenOutput.Draft(model.draft)))))

    case _: NavigateNext =>
      (model, Cmd.None)
  }

  private def buildCharacter(model: Model): Character = {
    val d = model.draft
    val sp = d.species.getOrElse(Human: Species)
    val cls = d.dndClass.getOrElse(Barbarian: DndClass)
    val bg = d.background.getOrElse(Acolyte: Background)
    val scores = d.baseScores.getOrElse(AbilityScores.default)
    val bonus = d.backgroundBonus.getOrElse(BackgroundBonus.ThreePlusOnes(Ability.Strength, Ability.Dexterity, Ability.Constitution))
    val languages = sp.languages ++ d.chosenExtraLanguages
    val lvl = d.level.getOrElse(1)
    Character(
      if model.name.trim.isEmpty then "Unnamed Hero" else model.name,
      sp, List(ClassLevel(cls, lvl)), bg, scores, bonus, d.chosenSkills,
      d.equippedArmor, d.equippedShield, d.equippedWeapons,
      d.chosenCantrips, d.preparedSpells, d.spellbookSpells,
      d.featureSelections, languages,
      d.coins
    )
  }

  def view(model: Model): Html[Msg] = {
    val cls = model.draft.dndClass.getOrElse(Barbarian)
    val character = buildCharacter(model)

    div(`class` := "screen-container")(
      StepIndicator(if cls.isSpellcaster then 10 else 9, cls.isSpellcaster),
      StepNav(
        "< Languages",
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
  }

  private def errorsView(errors: List[String]): Html[Msg] =
    if errors.isEmpty then div()
    else div(`class` := "error-box")(
      errors.map(e => div(text(e)))*
    )

  private def characterSummary(ch: Character): Html[Msg] = {
    val prog = ch.spellProgression
    div(
      div(`class` := "flex-row", style := "margin-bottom: 0.5rem; gap: 0.5rem;")(
        span(`class` := "badge")(text(s"Level ${ch.characterLevel}")),
        span(`class` := "badge")(text(ch.classLabel)),
        span(`class` := "badge")(text(ch.species.displayName)),
        span(`class` := "badge")(text(ch.background.name)),
        span(`class` := "badge badge--feat")(text(ch.originFeat.name))
      ),
      div(`class` := "stat-block")(
        statBox("HP", ch.maxHitPoints.toString, s"d${ch.primaryClass.hitDie.sides}"),
        statBox("AC", ch.armorClass.toString, "unarmored"),
        statBox("Speed", s"${ch.speed}ft", ""),
        statBox("Initiative", ch.initiative.format, ""),
        statBox("Prof. Bonus", s"+${ch.proficiencyBonus}", ""),
        statBox("Passive Perc.", ch.passivePerception.toString, "")
      ),
      (ch.spellSaveDC, prog) match {
        case (Some(dc), Some(row)) =>
          val slotsStr = row.slots.zipWithIndex.collect { case (n, i) if n > 0 => s"Lv${i + 1}: $n" }.mkString(" ")
          div(`class` := "stat-block")(
            statBox("Spell Save DC", dc.toString, ""),
            statBox("Spell Attack", ch.spellAttackBonus.getOrElse(dndbuilder.dnd.DndTypes.Modifier.zero).format, ""),
            (if row.cantrips > 0 then statBox("Cantrips", row.cantrips.toString, "") else div()),
            statBox("Spell Slots", slotsStr, "")
          )
        case _ => div()
      },
      div(`class` := "section-title")(text("Ability Scores")),
      table(`class` := "ability-table")(
        thead(tr(th(text("Ability")), th(text("Score")), th(text("Mod")), th(text("Save")))),
        tbody(
          Ability.values.toList.map { ability =>
            val score = ch.finalScores.get(ability)
            val mod   = AbilityScores.modifierString(score)
            val save  = ch.savingThrowBonus(ability).format
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
            text(s"${skill.label} (${skill.ability.abbreviation}) ${ch.skillBonus(skill).format}")
          )
        }*
      ),
      div(`class` := "section-title")(text("Class Features")),
      div(`class` := "feature-list")(
        ClassProgression.featuresUpToLevel(ch.primaryClass, ch.primaryClassLevel).map { f =>
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
      div(`class` := "section-title")(text("Languages")),
      div(`class` := "prof-list")(
        ch.languages.toList.sortBy(_.label).map { lang =>
          div(`class` := "prof-item prof-item--proficient")(text(lang.label))
        }*
      ),
      div(`class` := "section-title")(text("Proficiencies")),
      div(style := "font-size: 0.85rem; color: var(--color-text-muted);")(
        div(text(s"Armor: ${if ch.primaryClass.armorProficiencies.isEmpty then "None" else ch.primaryClass.armorProficiencies.map(_.label).mkString(", ")}")),
        div(text(s"Weapons: ${ch.primaryClass.weaponSummary}")),
        div(text(s"Tools: ${ch.background.toolProficiency}"))
      )
    )
  }

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
}

final case class ReviewModel(
    draft: CharacterDraft,
    name: String,
    errors: List[String],
    saving: Boolean)

enum ReviewMsg {
  case SetName(n: String)
  case Save
  case ExportPdf
  case Loaded(existing: List[StoredCharacter], newChar: StoredCharacter)
  case Saved(saved: StoredCharacter)
  case Error(msg: String)
  case Back
}
