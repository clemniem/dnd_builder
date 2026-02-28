package dndbuilder.screens

import cats.effect.IO
import dndbuilder.{NavigateNext, Screen, ScreenId, ScreenOutput}
import dndbuilder.dnd.*
import tyrian.Html.*
import tyrian.*

final case class ClassFeaturesModel(
    draft: CharacterDraft,
    featureSelections: ClassFeatureSelections
)

enum ClassFeaturesMsg {
  case Next
  case Back
  case NoOp
  case SetFightingStyle(style: Option[FightingStyle])
  case SetDivineOrder(order: Option[DivineOrder])
  case SetPrimalOrder(order: Option[PrimalOrder])
  case SetEldritchInvocation(inv: Option[EldritchInvocation])
  case ToggleExpertiseSkill(skill: Skill)
  case ToggleWeaponMastery(weapon: Weapon)
}

object ClassFeaturesScreen extends Screen {
  type Model = ClassFeaturesModel
  type Msg   = ClassFeaturesMsg | NavigateNext

  val screenId: ScreenId = ScreenId.ClassFeaturesId

  private def hasClassFeatureChoices(cls: DndClass): Boolean =
    cls match {
      case Fighter => true
      case Cleric => true
      case Druid => true
      case Warlock => true
      case Rogue => true
      case _ => cls.weaponMasteryCount > 0
    }

  private def canProceed(model: Model): Boolean = {
    val cls = model.draft.dndClass.getOrElse(Barbarian)
    val fs = model.featureSelections
    cls match {
      case Fighter =>
        fs.fightingStyle.isDefined && fs.weaponMasteries.size == 3
      case Cleric =>
        fs.divineOrder.isDefined
      case Druid =>
        fs.primalOrder.isDefined
      case Warlock =>
        fs.eldritchInvocation.isDefined
      case Rogue =>
        fs.expertiseSkills.size == 2 && fs.weaponMasteries.size == 2
      case _ =>
        fs.weaponMasteries.size == cls.weaponMasteryCount
    }
  }

  def init(previous: Option[ScreenOutput]): (Model, Cmd[IO, Msg]) = {
    val draft = previous match {
      case Some(ScreenOutput.Draft(d)) => d
      case _ => CharacterDraft.empty
    }
    val cls = draft.dndClass.getOrElse(Barbarian)
    val model = ClassFeaturesModel(draft, draft.featureSelections)
    if !hasClassFeatureChoices(cls) then {
      (model, Cmd.Emit(NavigateNext(ScreenId.EquipmentId, Some(ScreenOutput.Draft(draft)))))
    }
    else
      (model, Cmd.None)
  }

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = {
    case ClassFeaturesMsg.Next =>
      val updated = model.draft.copy(featureSelections = model.featureSelections)
      (model, Cmd.Emit(NavigateNext(ScreenId.EquipmentId, Some(ScreenOutput.Draft(updated)))))
    case ClassFeaturesMsg.Back =>
      val updated = model.draft.copy(featureSelections = model.featureSelections)
      (model, Cmd.Emit(NavigateNext(ScreenId.SkillsId, Some(ScreenOutput.Draft(updated)))))
    case ClassFeaturesMsg.SetFightingStyle(style) =>
      (model.copy(featureSelections = model.featureSelections.copy(fightingStyle = style)), Cmd.None)
    case ClassFeaturesMsg.SetDivineOrder(order) =>
      (model.copy(featureSelections = model.featureSelections.copy(divineOrder = order)), Cmd.None)
    case ClassFeaturesMsg.SetPrimalOrder(order) =>
      (model.copy(featureSelections = model.featureSelections.copy(primalOrder = order)), Cmd.None)
    case ClassFeaturesMsg.SetEldritchInvocation(inv) =>
      (model.copy(featureSelections = model.featureSelections.copy(eldritchInvocation = inv)), Cmd.None)
    case ClassFeaturesMsg.ToggleExpertiseSkill(skill) =>
      val fs = model.featureSelections
      val newExpertise =
        if fs.expertiseSkills.contains(skill) then fs.expertiseSkills - skill
        else if fs.expertiseSkills.size < 2 then fs.expertiseSkills + skill
        else fs.expertiseSkills
      (model.copy(featureSelections = fs.copy(expertiseSkills = newExpertise)), Cmd.None)
    case ClassFeaturesMsg.ToggleWeaponMastery(weapon) =>
      val cls = model.draft.dndClass.getOrElse(Barbarian)
      val fs = model.featureSelections
      val maxCount = cls.weaponMasteryCount
      val newList =
        if fs.weaponMasteries.contains(weapon) then fs.weaponMasteries.filter(_ != weapon)
        else if fs.weaponMasteries.size < maxCount then fs.weaponMasteries :+ weapon
        else fs.weaponMasteries
      (model.copy(featureSelections = fs.copy(weaponMasteries = newList)), Cmd.None)
    case ClassFeaturesMsg.NoOp =>
      (model, Cmd.None)
    case _: NavigateNext =>
      (model, Cmd.None)
  }

  def view(model: Model): Html[Msg] = {
    val cls = model.draft.dndClass.getOrElse(Barbarian)
    val nextEnabled = canProceed(model)
    div(`class` := "screen-container")(
      StepIndicator(6, cls.isSpellcaster),
      StepNav("< Skills", ClassFeaturesMsg.Back,
        "Next: Equipment >",
        ClassFeaturesMsg.Next, nextEnabled),
      h1(`class` := "screen-title")(text("Class Features")),
      p(`class` := "screen-intro")(text("Choose your class feature options.")),
      fightingStyleSection(model),
      divineOrderSection(model),
      primalOrderSection(model),
      eldritchInvocationSection(model),
      expertiseSection(model),
      weaponMasterySection(model)
    )
  }

  private def fightingStyleSection(model: Model): Html[Msg] = {
    val cls = model.draft.dndClass.getOrElse(Barbarian)
    if cls != Fighter then div()
    else
      div(style := "margin-bottom: 1.5rem;")(
        h2(`class` := "about-heading")(text("Fighting Style (choose 1)")),
        div(`class` := "card-grid")(
          FightingStyle.values.toList.map { style =>
            val selected = model.featureSelections.fightingStyle.contains(style)
            div(
              `class` := (if selected then "card card--selected" else "card"),
              onClick(ClassFeaturesMsg.SetFightingStyle(Some(style)))
            )(
              div(`class` := "card-title")(text(style.label)),
              div(`class` := "card-desc")(text(style.description))
            )
          }*
        )
      )
  }

  private def divineOrderSection(model: Model): Html[Msg] = {
    val cls = model.draft.dndClass.getOrElse(Barbarian)
    if cls != Cleric then div()
    else
      div(style := "margin-bottom: 1.5rem;")(
        h2(`class` := "about-heading")(text("Divine Order (choose 1)")),
        div(`class` := "card-grid")(
          DivineOrder.values.toList.map { order =>
            val selected = model.featureSelections.divineOrder.contains(order)
            div(
              `class` := (if selected then "card card--selected" else "card"),
              onClick(ClassFeaturesMsg.SetDivineOrder(Some(order)))
            )(
              div(`class` := "card-title")(text(order.label)),
              div(`class` := "card-desc")(text(order.description))
            )
          }*
        )
      )
  }

  private def primalOrderSection(model: Model): Html[Msg] = {
    val cls = model.draft.dndClass.getOrElse(Barbarian)
    if cls != Druid then div()
    else
      div(style := "margin-bottom: 1.5rem;")(
        h2(`class` := "about-heading")(text("Primal Order (choose 1)")),
        div(`class` := "card-grid")(
          PrimalOrder.values.toList.map { order =>
            val selected = model.featureSelections.primalOrder.contains(order)
            div(
              `class` := (if selected then "card card--selected" else "card"),
              onClick(ClassFeaturesMsg.SetPrimalOrder(Some(order)))
            )(
              div(`class` := "card-title")(text(order.label)),
              div(`class` := "card-desc")(text(order.description))
            )
          }*
        )
      )
  }

  private def eldritchInvocationSection(model: Model): Html[Msg] = {
    val cls = model.draft.dndClass.getOrElse(Barbarian)
    if cls != Warlock then div()
    else
      div(style := "margin-bottom: 1.5rem;")(
        h2(`class` := "about-heading")(text("Eldritch Invocation (choose 1)")),
        div(`class` := "card-grid")(
          EldritchInvocation.values.toList.map { inv =>
            val selected = model.featureSelections.eldritchInvocation.contains(inv)
            div(
              `class` := (if selected then "card card--selected" else "card"),
              onClick(ClassFeaturesMsg.SetEldritchInvocation(Some(inv)))
            )(
              div(`class` := "card-title")(text(inv.label)),
              div(`class` := "card-desc")(text(inv.description))
            )
          }*
        )
      )
  }

  private def expertiseSection(model: Model): Html[Msg] = {
    val cls = model.draft.dndClass.getOrElse(Barbarian)
    if cls != Rogue then div()
    else {
      val pool = cls.skillPool
      val chosen = model.featureSelections.expertiseSkills
      div(style := "margin-bottom: 1.5rem;")(
        h2(`class` := "about-heading")(text("Expertise (choose 2 skills)")),
        div(`class` := "points-pool", style := "margin-bottom: 0.5rem;")(
          text("Chosen: "),
          span(`class` := "points-pool-value")(text(s"${chosen.size} / 2"))
        ),
        div(
          pool.toList.sortBy(_.label).map { skill =>
            val isChosen = chosen.contains(skill)
            val clsName = if isChosen then "skill-item skill-item--selected" else "skill-item"
            div(`class` := clsName, onClick(ClassFeaturesMsg.ToggleExpertiseSkill(skill)))(
              div(`class` := "skill-checkbox")(text(if isChosen then "*" else "")),
              span(`class` := "skill-label")(text(skill.label)),
              span(`class` := "skill-ability-tag")(text(skill.ability.abbreviation))
            )
          }*
        )
      )
    }
  }

  private def weaponMasterySection(model: Model): Html[Msg] = {
    val cls = model.draft.dndClass.getOrElse(Barbarian)
    val count = cls.weaponMasteryCount
    if count <= 0 then div()
    else {
      val proficient = Weapon.all.filter(w => WeaponProficiency.isProficient(w, cls.weaponProficiencies))
      val chosen = model.featureSelections.weaponMasteries
      div(style := "margin-bottom: 1.5rem;")(
        h2(`class` := "about-heading")(text(s"Weapon Mastery (choose $count)")),
        div(`class` := "points-pool", style := "margin-bottom: 0.5rem;")(
          text("Chosen: "),
          span(`class` := "points-pool-value")(text(s"${chosen.size} / $count"))
        ),
        div(
          proficient.map { weapon =>
            val isChosen = chosen.contains(weapon)
            val clsName = if isChosen then "skill-item skill-item--selected" else "skill-item"
            div(`class` := clsName, onClick(ClassFeaturesMsg.ToggleWeaponMastery(weapon)))(
              div(`class` := "skill-checkbox")(text(if isChosen then "*" else "")),
              span(`class` := "skill-label")(text(s"${weapon.name} (${weapon.mastery})")),
              span(`class` := "skill-ability-tag")(text(weapon.damage))
            )
          }*
        )
      )
    }
  }
}
