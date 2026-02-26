package dndbuilder.screens

import cats.effect.IO
import dndbuilder.{NavigateNext, Screen, ScreenId, ScreenOutput}
import dndbuilder.dnd.*
import tyrian.Html.*
import tyrian.*

object SkillsScreen extends Screen:
  type Model = SkillsModel
  type Msg   = SkillsMsg | NavigateNext

  val screenId: ScreenId = ScreenId.SkillsId

  def init(previous: Option[ScreenOutput]): (Model, Cmd[IO, Msg]) =
    val (cls, sp, bg, scores, bonus) = previous match
      case Some(ScreenOutput.AbilitiesChosen(s, c, b, sc, bn)) => (c, s, b, sc, bn)
      case _ => (Barbarian, Human, Acolyte, AbilityScores.default, BackgroundBonus.ThreePlusOnes(Ability.Strength, Ability.Dexterity, Ability.Constitution))
    (SkillsModel(cls, sp, bg, scores, bonus, Set.empty), Cmd.None)

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) =
    case SkillsMsg.ToggleSkill(skill) =>
      val bgSkills = model.background.skillProficiencySet
      if bgSkills.contains(skill) then (model, Cmd.None)
      else
        val pool = model.dndClass.skillPool -- bgSkills
        if !pool.contains(skill) then (model, Cmd.None)
        else
          val newChosen =
            if model.chosenSkills.contains(skill) then model.chosenSkills - skill
            else if model.chosenSkills.size < model.dndClass.numSkillChoices then model.chosenSkills + skill
            else model.chosenSkills
          (model.copy(chosenSkills = newChosen), Cmd.None)

    case SkillsMsg.Next =>
      if model.chosenSkills.size == model.dndClass.numSkillChoices then
        val output = ScreenOutput.SkillsChosen(
          model.species, model.dndClass, model.background,
          model.baseScores, model.backgroundBonus, model.chosenSkills
        )
        (model, Cmd.Emit(NavigateNext(ScreenId.ReviewId, Some(output))))
      else (model, Cmd.None)

    case SkillsMsg.Back =>
      val output = ScreenOutput.BackgroundChosen(model.species, model.dndClass, model.background)
      (model, Cmd.Emit(NavigateNext(ScreenId.AbilitiesId, Some(output))))

    case _: NavigateNext =>
      (model, Cmd.None)

  def view(model: Model): Html[Msg] =
    val bgSkills = model.background.skillProficiencySet
    val pool     = model.dndClass.skillPool -- bgSkills
    val remaining = model.dndClass.numSkillChoices - model.chosenSkills.size

    div(`class` := "screen-container")(
      StepIndicator(5),
      StepNav("< Abilities", SkillsMsg.Back, "Next: Review >", SkillsMsg.Next, remaining == 0),
      h1(`class` := "screen-title")(text("Choose Skills")),
      p(`class` := "screen-intro")(
        text(s"Select ${model.dndClass.numSkillChoices} skills from your class. Background skills are already granted.")
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
                  val cls = if isChosen then "skill-item skill-item--selected" else "skill-item"
                  div(`class` := cls, onClick(SkillsMsg.ToggleSkill(skill)))(
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

final case class SkillsModel(
    dndClass: DndClass,
    species: Species,
    background: Background,
    baseScores: AbilityScores,
    backgroundBonus: BackgroundBonus,
    chosenSkills: Set[Skill])

enum SkillsMsg:
  case ToggleSkill(skill: Skill)
  case Next
  case Back
