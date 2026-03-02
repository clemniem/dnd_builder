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
      val cls = model.draft.resolvedClass
      val bg  = model.draft.resolvedBackground
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

    case SkillsMsg.ToggleGrantSkill(grantIndex, skill) =>
      val grants = model.draft.skillGrants
      if grantIndex < 0 || grantIndex >= grants.size then (model, Cmd.None)
      else {
        val g = grants(grantIndex)
        val newChosen =
          if g.chosen.contains(skill) then g.chosen - skill
          else if g.chosen.size < g.count then g.chosen + skill
          else g.chosen
        val newGrants = grants.updated(grantIndex, g.withChosen(newChosen))
        val updatedDraft = model.draft.withSkillGrants(newGrants)
        (model.copy(draft = updatedDraft), Cmd.None)
      }

    case SkillsMsg.Next =>
      val cls = model.draft.resolvedClass
      val classSkillsReady = model.chosenSkills.size == cls.numSkillChoices
      val grantSkillsReady = model.draft.skillGrants.forall(_.isFilled)
      if classSkillsReady && grantSkillsReady then {
        val updated = model.draft.copy(chosenSkills = model.chosenSkills)
        (model, Cmd.Emit(NavigateNext(ScreenId.FeaturesId, Some(ScreenOutput.Draft(updated)))))
      }
      else (model, Cmd.None)

    case SkillsMsg.Back =>
      val updated = model.draft.copy(chosenSkills = model.chosenSkills)
      (model, Cmd.Emit(NavigateNext(ScreenId.AbilitiesId, Some(ScreenOutput.Draft(updated)))))

    case _: NavigateNext =>
      (model, Cmd.None)
  }

  def view(model: Model): Html[Msg] = {
    val cls = model.draft.resolvedClass
    val bg  = model.draft.resolvedBackground
    val needsSpells = FeatureGrants.needsSpellScreen(model.draft)
    val bgSkills = bg.skillProficiencySet
    val pool     = cls.skillPool -- bgSkills
    val classRemaining = cls.numSkillChoices - model.chosenSkills.size
    val grantSkillsReady = model.draft.skillGrants.forall(_.isFilled)
    val canProceed = classRemaining == 0 && grantSkillsReady

    div(`class` := "screen-container")(
      StepIndicator(5, needsSpells),
      StepNav(StepIndicator.backLabel(5, needsSpells), SkillsMsg.Back, StepIndicator.nextLabel(5, needsSpells), SkillsMsg.Next, canProceed),
      h1(`class` := "screen-title")(text("Choose Skills")),
      p(`class` := "screen-intro")(
        text(s"Select ${cls.numSkillChoices} skills from your class. Background skills are already granted.")
      ),
      div(`class` := "points-pool", style := "margin-bottom: 1rem;")(
        text("Remaining: "),
        span(`class` := "points-pool-value")(text(classRemaining.toString))
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
      ),
      div(model.draft.skillGrants.zipWithIndex.map { case (grant, idx) =>
        grantSkillSection(model, grant, idx)
      }*)
    )
  }

  private def grantSkillSection(model: SkillsModel, grant: SkillGrant, grantIndex: Int): Html[Msg] = {
    val bg = model.draft.resolvedBackground
    val alreadyProficient = bg.skillProficiencySet ++ model.chosenSkills
    val otherGrantsChosen = model.draft.skillGrants.zipWithIndex.filter(_._2 != grantIndex).flatMap(_._1.chosen).toSet
    val pool = (grant.pool -- alreadyProficient -- otherGrantsChosen).toList.sortBy(_.label)
    val remaining = grant.count - grant.chosen.size
    div(style := "margin-top: 1.5rem;")(
      div(`class` := "section-title")(text(grant.sourceLabel)),
      div(`class` := "points-pool", style := "margin-bottom: 0.5rem;")(
        text(s"Choose $remaining of ${grant.count}: "),
        span(`class` := "points-pool-value")(text(s"${grant.chosen.size} / ${grant.count}"))
      ),
      div(
        Skill.byAbility.toList.sortBy(_._1.ordinal).flatMap { case (ability, skills) =>
          val available = skills.filter(s => pool.contains(s))
          if available.isEmpty then Nil
          else
            List(div(`class` := "skill-group")(
              div(`class` := "skill-group-title")(text(ability.label)),
              div(
                available.map { skill =>
                  val isChosen = grant.chosen.contains(skill)
                  val clsName = if isChosen then "skill-item skill-item--selected" else "skill-item"
                  div(`class` := clsName, onClick(SkillsMsg.ToggleGrantSkill(grantIndex, skill)))(
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
  case ToggleGrantSkill(grantIndex: Int, skill: Skill)
  case Next
  case Back
}
