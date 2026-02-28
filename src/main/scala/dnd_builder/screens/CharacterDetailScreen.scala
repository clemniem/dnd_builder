package dndbuilder.screens

import cats.effect.IO
import dndbuilder.{NavigateNext, Screen, ScreenId, ScreenOutput, StoredCharacter}
import dndbuilder.common.pdf.CharacterSheetPdf
import dndbuilder.dnd.*
import tyrian.Html.*
import tyrian.*

object CharacterDetailScreen extends Screen {
  type Model = DetailModel
  type Msg   = DetailMsg | NavigateNext

  val screenId: ScreenId = ScreenId.DetailId

  def init(previous: Option[ScreenOutput]): (Model, Cmd[IO, Msg]) = {
    val sc = previous match {
      case Some(ScreenOutput.ViewCharacter(c)) => c
      case _ =>
        StoredCharacter("?", Character(
          "Unknown", Human, Barbarian, Acolyte,
          AbilityScores.default,
          BackgroundBonus.ThreePlusOnes(Ability.Intelligence, Ability.Wisdom, Ability.Charisma),
          Set.empty, None, false, Nil, Nil, Nil, Nil, ClassFeatureSelections.empty, Human.languages, 1
        ))
    }
    (DetailModel(sc), Cmd.None)
  }

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = {
    case DetailMsg.Back =>
      (model, Cmd.Emit(NavigateNext(ScreenId.GalleryId, None)))
    case DetailMsg.ExportPdf =>
      org.scalajs.dom.console.log("[Export PDF] Detail screen: clicked, calling generate")
      CharacterSheetPdf.generate(model.storedCharacter.character)
      (model, Cmd.None)
    case _: NavigateNext =>
      (model, Cmd.None)
  }

  def view(model: Model): Html[Msg] = {
    val ch = model.storedCharacter.character
    div(`class` := "screen-container")(
      div(`class` := "screen-header")(
        h1(`class` := "screen-title")(text(ch.name)),
        div(style := "display: flex; gap: 0.5rem;")(
          button(`class` := "btn btn--secondary", onClick(DetailMsg.ExportPdf))(text("Export PDF")),
          button(`class` := "btn-ghost", onClick(DetailMsg.Back))(text("< Back to Gallery"))
        )
      ),
      div(`class` := "flex-row", style := "margin-bottom: 1rem; gap: 0.5rem;")(
        span(`class` := "badge")(text(s"Level ${ch.level}")),
        span(`class` := "badge")(text(ch.species.name)),
        ch.species.subLabel.map(s => span(`class` := "badge")(text(s))).getOrElse(span()),
        span(`class` := "badge")(text(ch.dndClass.name)),
        span(`class` := "badge")(text(ch.background.name)),
        span(`class` := "badge badge--feat")(text(ch.originFeat.name))
      ),
      div(`class` := "stat-block")(
        statBox("Hit Points", ch.maxHitPoints.toString, s"d${ch.dndClass.hitDie.sides}"),
        statBox("Armor Class", ch.armorClass.toString, "unarmored"),
        statBox("Speed", s"${ch.speed}ft", ""),
        statBox("Initiative", AbilityScores.modifierString(10 + ch.initiative - 10), ""),
        statBox("Prof. Bonus", s"+${ch.proficiencyBonus}", ""),
        statBox("Passive Perc.", ch.passivePerception.toString, "")
      ),
      ch.spellSaveDC match {
        case Some(dc) =>
          div(`class` := "stat-block")(
            statBox("Spell Save DC", dc.toString, ""),
            statBox("Spell Attack", s"+${ch.spellAttackBonus.getOrElse(0)}", ""),
            statBox("Cantrips", ch.dndClass.cantripsKnown.toString, ""),
            statBox("Spell Slots", ch.dndClass.level1SpellSlots.toString, "Level 1")
          )
        case None => div()
      },
      div(`class` := "section-title")(text("Ability Scores")),
      table(`class` := "ability-table")(
        thead(tr(th(text("Ability")), th(text("Score")), th(text("Mod")), th(text("Save")))),
        tbody(
          Ability.values.toList.map { ability =>
            val score = ch.finalScores.get(ability)
            val mod   = AbilityScores.modifierString(score)
            val saveBonus = ch.savingThrowBonus(ability)
            val saveStr = AbilityScores.modifierString(10 + saveBonus - 10)
            val profMark = if ch.isProficientInSave(ability) then " *" else ""
            tr(
              td(`class` := "ability-name")(text(ability.label)),
              td(`class` := "ability-score")(text(score.toString)),
              td(`class` := "ability-mod")(text(mod)),
              td(`class` := "ability-mod")(text(s"$saveStr$profMark"))
            )
          }*
        )
      ),
      div(`class` := "section-title")(text("Skill Proficiencies")),
      div(`class` := "prof-list")(
        Skill.values.toList.map { skill =>
          val bonus = ch.skillBonus(skill)
          val bonusStr = AbilityScores.modifierString(bonus)
          val isProficient = ch.isSkillProficient(skill)
          div(`class` := (if isProficient then "prof-item prof-item--proficient" else "prof-item"))(
            text(s"${if isProficient then "* " else ""}${skill.label} (${skill.ability.abbreviation}) $bonusStr")
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
      div(`class` := "section-title")(text("Languages")),
      div(`class` := "prof-list", style := "margin-bottom: 1rem;")(
        ch.languages.toList.sortBy(_.label).map { lang =>
          div(`class` := "prof-item prof-item--proficient")(text(lang.label))
        }*
      ),
      div(`class` := "section-title")(text("Proficiencies")),
      div(style := "font-size: 0.85rem; color: var(--color-text-muted); margin-bottom: 1rem;")(
        div(text(s"Armor: ${if ch.dndClass.armorProficiencies.isEmpty then "None" else ch.dndClass.armorProficiencies.map(_.label).mkString(", ")}")),
        div(text(s"Weapons: ${ch.dndClass.weaponSummary}")),
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
                text(s"${s.name} (${s.school.label})${if isPrepared then " prepared" else ""}")
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

final case class DetailModel(storedCharacter: StoredCharacter)

enum DetailMsg {
  case Back
  case ExportPdf
}
