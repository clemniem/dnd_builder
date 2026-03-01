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

  val screenId: ScreenId = ScreenId.FeaturesId

  private def level1GrantChoices(cls: DndClass): List[FeatureGrant] =
    ClassProgression.grantChoicesAtLevel(ClassProgression.atLevel(cls, 1))

  private def hasClassFeatureChoices(cls: DndClass): Boolean =
    level1GrantChoices(cls).nonEmpty

  private def canProceed(model: Model): Boolean = {
    val cls = model.draft.resolvedClass
    ClassProgression.satisfiesGrantChoices(level1GrantChoices(cls), model.featureSelections)
  }

  def init(previous: Option[ScreenOutput]): (Model, Cmd[IO, Msg]) = {
    val draft = previous match {
      case Some(ScreenOutput.Draft(d)) => d
      case _ => CharacterDraft.empty
    }
    val cls = draft.resolvedClass
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
      (model.copy(featureSelections = model.featureSelections.withChoices(style, model.featureSelections.divineOrder, model.featureSelections.primalOrder, model.featureSelections.eldritchInvocation, model.featureSelections.expertiseSkills, model.featureSelections.weaponMasteries, model.featureSelections.landType, model.featureSelections.hunterPrey)), Cmd.None)
    case ClassFeaturesMsg.SetDivineOrder(order) =>
      (model.copy(featureSelections = model.featureSelections.withChoices(model.featureSelections.fightingStyle, order, model.featureSelections.primalOrder, model.featureSelections.eldritchInvocation, model.featureSelections.expertiseSkills, model.featureSelections.weaponMasteries, model.featureSelections.landType, model.featureSelections.hunterPrey)), Cmd.None)
    case ClassFeaturesMsg.SetPrimalOrder(order) =>
      (model.copy(featureSelections = model.featureSelections.withChoices(model.featureSelections.fightingStyle, model.featureSelections.divineOrder, order, model.featureSelections.eldritchInvocation, model.featureSelections.expertiseSkills, model.featureSelections.weaponMasteries, model.featureSelections.landType, model.featureSelections.hunterPrey)), Cmd.None)
    case ClassFeaturesMsg.SetEldritchInvocation(inv) =>
      (model.copy(featureSelections = model.featureSelections.withChoices(model.featureSelections.fightingStyle, model.featureSelections.divineOrder, model.featureSelections.primalOrder, inv, model.featureSelections.expertiseSkills, model.featureSelections.weaponMasteries, model.featureSelections.landType, model.featureSelections.hunterPrey)), Cmd.None)
    case ClassFeaturesMsg.ToggleExpertiseSkill(skill) =>
      val cls = model.draft.resolvedClass
      val fs = model.featureSelections
      val maxCount = ClassProgression.expertiseCountFromGrants(level1GrantChoices(cls))
      val newExpertise =
        if fs.expertiseSkills.contains(skill) then fs.expertiseSkills - skill
        else if fs.expertiseSkills.size < maxCount then fs.expertiseSkills + skill
        else fs.expertiseSkills
      (model.copy(featureSelections = fs.withChoices(fs.fightingStyle, fs.divineOrder, fs.primalOrder, fs.eldritchInvocation, newExpertise, fs.weaponMasteries, fs.landType, fs.hunterPrey)), Cmd.None)
    case ClassFeaturesMsg.ToggleWeaponMastery(weapon) =>
      val cls = model.draft.resolvedClass
      val fs = model.featureSelections
      val maxCount = ClassProgression.weaponMasteryCountFromGrants(level1GrantChoices(cls))
      val newList =
        if fs.weaponMasteries.contains(weapon) then fs.weaponMasteries.filter(_ != weapon)
        else if fs.weaponMasteries.size < maxCount then fs.weaponMasteries :+ weapon
        else fs.weaponMasteries
      (model.copy(featureSelections = fs.withChoices(fs.fightingStyle, fs.divineOrder, fs.primalOrder, fs.eldritchInvocation, fs.expertiseSkills, newList, fs.landType, fs.hunterPrey)), Cmd.None)
    case ClassFeaturesMsg.NoOp =>
      (model, Cmd.None)
    case _: NavigateNext =>
      (model, Cmd.None)
  }

  def view(model: Model): Html[Msg] = {
    val cls = model.draft.resolvedClass
    val needsSpells = FeatureGrants.needsSpellScreen(cls, model.draft.background)
    val nextEnabled = canProceed(model)
    div(`class` := "screen-container")(
      StepIndicator(6, needsSpells),
      StepNav(StepIndicator.backLabel(6, needsSpells), ClassFeaturesMsg.Back,
        StepIndicator.nextLabel(6, needsSpells),
        ClassFeaturesMsg.Next, nextEnabled),
      h1(`class` := "screen-title")(text("Features")),
      p(`class` := "screen-intro")(text("Choose your class feature options. Background and race features are listed below.")),
      div(`class` := "section-title")(text("Class Features")),
      fightingStyleSection(model),
      divineOrderSection(model),
      primalOrderSection(model),
      eldritchInvocationSection(model),
      expertiseSection(model),
      weaponMasterySection(model),
      backgroundFeaturesSection(model),
      raceFeaturesSection(model)
    )
  }

  private def backgroundFeaturesSection(model: Model): Html[Msg] =
    model.draft.background match {
      case Some(bg) =>
        val spellGrants = model.draft.spellGrants
        val skillGrants = model.draft.skillGrants
        val spellResolved = spellGrants.forall(_.isFilled)
        val skillResolved = skillGrants.forall(_.isFilled)
        div(style := "margin-top: 1.5rem;")(
          div(`class` := "section-title")(text("Background Features")),
          div(`class` := "feature-list")(
            div(`class` := "feature-item")(
              div(`class` := "feature-name")(text("Origin Feat")),
              div(`class` := "feature-desc")(text(s"${bg.feat.name}: ${bg.feat.description}"))
            ),
            if spellGrants.nonEmpty then
              div(`class` := "feature-item")(
                div(`class` := "feature-name")(text("Spell choices")),
                div(`class` := "feature-desc")(
                  text(if spellResolved then "Chosen in Spells step." else "Will be chosen in Spells step.")
                )
              )
            else div(),
            if skillGrants.nonEmpty then
              div(`class` := "feature-item")(
                div(`class` := "feature-name")(text("Skill choices")),
                div(`class` := "feature-desc")(
                  text(if skillResolved then "Chosen in Skills step." else "Will be chosen in Skills step.")
                )
              )
            else div()
          )
        )
      case None => div()
    }

  private def raceFeaturesSection(model: Model): Html[Msg] =
    model.draft.species match {
      case Some(sp) =>
        div(style := "margin-top: 1.5rem;")(
          div(`class` := "section-title")(text("Race Features")),
          div(`class` := "feature-list")(
            div(`class` := "feature-item")(
              div(`class` := "feature-name")(text("Speed")),
              div(`class` := "feature-desc")(text(s"${sp.speed} ft"))
            ),
            sp.darkvision match {
              case Some(dv) =>
                div(`class` := "feature-item")(
                  div(`class` := "feature-name")(text("Darkvision")),
                  div(`class` := "feature-desc")(text(s"$dv ft"))
                )
              case None => div()
            },
            div(`class` := "feature-item")(
              div(`class` := "feature-name")(text("Traits")),
              div(`class` := "feature-desc")(text(sp.traits.mkString("; ")))
            )
          )
        )
      case None => div()
    }

  private def fightingStyleSection(model: Model): Html[Msg] = {
    val grants = level1GrantChoices(model.draft.resolvedClass)
    if !grants.exists { case FeatureGrant.FightingStyleChoice() => true; case _ => false } then div()
    else
      ChoiceWidgets.cardPicker(
        "Fighting Style (choose 1)",
        FightingStyle.values.toList.map(s => (s, s.label, Some(s.description))),
        model.featureSelections.fightingStyle,
        s => ClassFeaturesMsg.SetFightingStyle(Some(s))
      )
  }

  private def divineOrderSection(model: Model): Html[Msg] = {
    val grants = level1GrantChoices(model.draft.resolvedClass)
    if !grants.exists { case FeatureGrant.DivineOrderChoice() => true; case _ => false } then div()
    else
      ChoiceWidgets.cardPicker(
        "Divine Order (choose 1)",
        DivineOrder.values.toList.map(o => (o, o.label, Some(o.description))),
        model.featureSelections.divineOrder,
        o => ClassFeaturesMsg.SetDivineOrder(Some(o))
      )
  }

  private def primalOrderSection(model: Model): Html[Msg] = {
    val grants = level1GrantChoices(model.draft.resolvedClass)
    if !grants.exists { case FeatureGrant.PrimalOrderChoice() => true; case _ => false } then div()
    else
      ChoiceWidgets.cardPicker(
        "Primal Order (choose 1)",
        PrimalOrder.values.toList.map(o => (o, o.label, Some(o.description))),
        model.featureSelections.primalOrder,
        o => ClassFeaturesMsg.SetPrimalOrder(Some(o))
      )
  }

  private def eldritchInvocationSection(model: Model): Html[Msg] = {
    val grants = level1GrantChoices(model.draft.resolvedClass)
    if !grants.exists { case FeatureGrant.EldritchInvocationChoice() => true; case _ => false } then div()
    else
      ChoiceWidgets.cardPicker(
        "Eldritch Invocation (choose 1)",
        EldritchInvocation.values.toList.map(i => (i, i.label, Some(i.description))),
        model.featureSelections.eldritchInvocation,
        i => ClassFeaturesMsg.SetEldritchInvocation(Some(i))
      )
  }

  private def expertiseSection(model: Model): Html[Msg] = {
    val cls = model.draft.resolvedClass
    val expCount = ClassProgression.expertiseCountFromGrants(level1GrantChoices(cls))
    if expCount <= 0 then div()
    else
      ChoiceWidgets.skillPicker(
        s"Expertise (choose $expCount skill${if expCount > 1 then "s" else ""})",
        None,
        cls.skillPool.toList,
        model.featureSelections.expertiseSkills,
        expCount,
        ClassFeaturesMsg.ToggleExpertiseSkill.apply
      )
  }

  private def weaponMasterySection(model: Model): Html[Msg] = {
    val cls = model.draft.resolvedClass
    val count = ClassProgression.weaponMasteryCountFromGrants(level1GrantChoices(cls))
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
