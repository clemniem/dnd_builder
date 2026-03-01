package dndbuilder.screens

import cats.effect.IO
import dndbuilder.{NavigateNext, Screen, ScreenId, ScreenOutput}
import dndbuilder.dnd.*
import tyrian.Html.*
import tyrian.*

object SkillsScreen extends Screen {
  type Model = SkillsModel
  type Msg   = SkillsMsg | NavigateNext

  val screenId: ScreenId = ScreenId.SkillsId

  def init(previous: Option[ScreenOutput]): (Model, Cmd[IO, Msg]) = {
    val draft = previous match {
      case Some(ScreenOutput.Draft(d)) => d
      case _ => CharacterDraft.empty
    }
    (SkillsModel(draft, draft.chosenSkills), Cmd.None)
  }

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = {
    case SkillsMsg.ToggleSkill(skill) =>
      val cls = model.draft.dndClass.getOrElse(Barbarian)
      val bg  = model.draft.background.getOrElse(Acolyte)
      val bgSkills = bg.skillProficiencySet
      if bgSkills.contains(skill) then (model, Cmd.None)
      else {
        val pool = cls.skillPool -- bgSkills
        if !pool.contains(skill) then (model, Cmd.None)
        else {
          val newChosen =
            if model.chosenSkills.contains(skill) then model.chosenSkills - skill
            else if model.chosenSkills.size < cls.numSkillChoices then model.chosenSkills + skill
            else model.chosenSkills
          (model.copy(chosenSkills = newChosen), Cmd.None)
        }
      }

    case SkillsMsg.Next =>
      val cls = model.draft.dndClass.getOrElse(Barbarian)
      if model.chosenSkills.size == cls.numSkillChoices then {
        val updated = model.draft.copy(chosenSkills = model.chosenSkills)
        (model, Cmd.Emit(NavigateNext(ScreenId.ClassFeaturesId, Some(ScreenOutput.Draft(updated)))))
      }
      else (model, Cmd.None)

    case SkillsMsg.Back =>
      val updated = model.draft.copy(chosenSkills = model.chosenSkills)
      (model, Cmd.Emit(NavigateNext(ScreenId.AbilitiesId, Some(ScreenOutput.Draft(updated)))))

    case _: NavigateNext =>
      (model, Cmd.None)
  }

  def view(model: Model): Html[Msg] = {
    val cls = model.draft.dndClass.getOrElse(Barbarian)
    val bg  = model.draft.background.getOrElse(Acolyte)
    val bgSkills = bg.skillProficiencySet
    val pool     = cls.skillPool -- bgSkills
    val remaining = cls.numSkillChoices - model.chosenSkills.size

    div(`class` := "screen-container")(
      StepIndicator(5, cls.isSpellcaster),
      StepNav(StepIndicator.backLabel(5, cls.isSpellcaster), SkillsMsg.Back, StepIndicator.nextLabel(5, cls.isSpellcaster), SkillsMsg.Next, remaining == 0),
      h1(`class` := "screen-title")(text("Choose Skills")),
      p(`class` := "screen-intro")(
        text(s"Select ${cls.numSkillChoices} skills from your class. Background skills are already granted.")
      ),
      div(`class` := "points-pool", style := "margin-bottom: 1rem;")(
        text("Remaining: "),
        span(`class` := "points-pool-value")(text(remaining.toString))
      ),
      div(`class` := "section-title")(text("Background Skills (granted)")),
      div(
        bgSkills.toList.sortBy(_.label).map { skill =>
          div(`class` := "skill-item skill-item--locked")(
            div(`class` := "skill-checkbox")(text("*")),
            span(`class` := "skill-label")(text(skill.label)),
            span(`class` := "skill-ability-tag")(text(skill.ability.abbreviation))
          )
        }*
      ),
      div(`class` := "section-title")(text("Class Skills (choose)")),
      div(
        Skill.byAbility.toList.sortBy(_._1.ordinal).flatMap { case (ability, skills) =>
          val classSkills = skills.filter(pool.contains)
          if classSkills.isEmpty then Nil
          else
            List(div(`class` := "skill-group")(
              div(`class` := "skill-group-title")(text(ability.label)),
              div(
                classSkills.map { skill =>
                  val isChosen = model.chosenSkills.contains(skill)
                  val clsName = if isChosen then "skill-item skill-item--selected" else "skill-item"
                  div(`class` := clsName, onClick(SkillsMsg.ToggleSkill(skill)))(
                    div(`class` := "skill-checkbox")(text(if isChosen then "*" else "")),
                    span(`class` := "skill-label")(text(skill.label)),
                    span(`class` := "skill-ability-tag")(text(ability.abbreviation))
                  )
                }*
              )
            ))
        }*
      )
    )
  }
}

final case class SkillsModel(
    draft: CharacterDraft,
    chosenSkills: Set[Skill])

enum SkillsMsg {
  case ToggleSkill(skill: Skill)
  case Next
  case Back
}
