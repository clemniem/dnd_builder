package dndbuilder.dnd

/** What a class gains at a given level; features resolved from registry at use site.
  * syntheticGrants: class-specific choice grants (e.g. ExtraSkillsChoice) that cannot live in the registry due to init order.
  */
final case class LevelGain(featureIds: List[String], syntheticGrants: List[FeatureGrant] = Nil)

object LevelGain {
  val empty: LevelGain = LevelGain(Nil, Nil)
}

object ClassProgression {

  def featuresFromGain(gain: LevelGain): List[Feature] =
    SRD_registry.getMany(gain.featureIds*)

  private val level1Entries: List[((String, Int), LevelGain)] = List(
    (DndClass.Barbarian.name, 1) -> LevelGain(DndClass.Barbarian.level1Features.map(_.id)),
    (DndClass.Bard.name, 1) -> LevelGain(DndClass.Bard.level1Features.map(_.id)),
    (DndClass.Cleric.name, 1) -> LevelGain(DndClass.Cleric.level1Features.map(_.id)),
    (DndClass.Druid.name, 1) -> LevelGain(DndClass.Druid.level1Features.map(_.id)),
    (DndClass.Fighter.name, 1) -> LevelGain(DndClass.Fighter.level1Features.map(_.id)),
    (DndClass.Monk.name, 1) -> LevelGain(DndClass.Monk.level1Features.map(_.id)),
    (DndClass.Paladin.name, 1) -> LevelGain(DndClass.Paladin.level1Features.map(_.id)),
    (DndClass.Ranger.name, 1) -> LevelGain(DndClass.Ranger.level1Features.map(_.id)),
    (DndClass.Rogue.name, 1) -> LevelGain(DndClass.Rogue.level1Features.map(_.id)),
    (DndClass.Sorcerer.name, 1) -> LevelGain(DndClass.Sorcerer.level1Features.map(_.id)),
    (DndClass.Warlock.name, 1) -> LevelGain(DndClass.Warlock.level1Features.map(_.id)),
    (DndClass.Wizard.name, 1) -> LevelGain(DndClass.Wizard.level1Features.map(_.id))
  )

  private val level2Entries: List[((String, Int), LevelGain)] = List(
    (DndClass.Barbarian.name, 2) -> LevelGain(List("danger-sense", "reckless-attack")),
    (DndClass.Bard.name, 2) -> LevelGain(List("expertise", "jack-of-all-trades")),
    (DndClass.Cleric.name, 2) -> LevelGain(List("channel-divinity-cleric")),
    (DndClass.Druid.name, 2) -> LevelGain(List("wild-shape", "wild-companion")),
    (DndClass.Fighter.name, 2) -> LevelGain(List("action-surge", "tactical-mind")),
    (DndClass.Monk.name, 2) -> LevelGain(List("monks-focus", "unarmored-movement")),
    (DndClass.Paladin.name, 2) -> LevelGain(List("fighting-style", "paladins-smite")),
    (DndClass.Ranger.name, 2) -> LevelGain(List("deft-explorer", "fighting-style")),
    (DndClass.Rogue.name, 2) -> LevelGain(List("cunning-action")),
    (DndClass.Sorcerer.name, 2) -> LevelGain(List("font-of-magic", "metamagic")),
    (DndClass.Warlock.name, 2) -> LevelGain(List("magical-cunning")),
    (DndClass.Wizard.name, 2) -> LevelGain(List("scholar"))
  )

  private val level3Entries: List[((String, Int), LevelGain)] = List(
    (DndClass.Barbarian.name, 3) -> LevelGain(List("primal-knowledge", "subclass"), List(FeatureGrant.ExtraSkillsChoice(1, DndClass.Barbarian.skillPool))),
    (DndClass.Bard.name, 3) -> LevelGain(List("subclass"), List(FeatureGrant.ExtraSkillsChoice(3, Skill.values.toSet))),
    (DndClass.Cleric.name, 3) -> LevelGain(List("subclass")),
    (DndClass.Druid.name, 3) -> LevelGain(List("subclass", "land-type-choice")),
    (DndClass.Fighter.name, 3) -> LevelGain(List("subclass")),
    (DndClass.Monk.name, 3) -> LevelGain(List("deflect-attacks", "subclass")),
    (DndClass.Paladin.name, 3) -> LevelGain(List("channel-divinity-paladin", "subclass")),
    (DndClass.Ranger.name, 3) -> LevelGain(List("subclass", "hunter-prey-choice")),
    (DndClass.Rogue.name, 3) -> LevelGain(List("steady-aim", "subclass")),
    (DndClass.Sorcerer.name, 3) -> LevelGain(List("subclass")),
    (DndClass.Warlock.name, 3) -> LevelGain(List("subclass")),
    (DndClass.Wizard.name, 3) -> LevelGain(List("subclass"))
  )

  private lazy val registry: Map[(String, Int), LevelGain] =
    (level1Entries ++ level2Entries ++ level3Entries).toMap

  def atLevel(dndClass: DndClass, level: Int): LevelGain =
    registry.getOrElse((dndClass.name, level), LevelGain.empty)

  def featuresUpToLevel(dndClass: DndClass, level: Int): List[Feature] =
    (1 to level).flatMap(l => featuresFromGain(atLevel(dndClass, l))).toList

  /** Features for character sheet / PDF: same as featuresUpToLevel but omits "Subclass - Choose your subclass at this level." for level 3+ (already chosen). */
  def featuresForDisplay(dndClass: DndClass, level: Int): List[Feature] = {
    val all = featuresUpToLevel(dndClass, level)
    if level >= 3 then all.filter(_.id != "subclass") else all
  }

  def maxSupportedLevel(dndClass: DndClass): Int =
    (1 to 20).reverse.find(l => registry.contains((dndClass.name, l))).getOrElse(1)

  // --- Grant-based choice helpers ---

  def expertiseCountFromGrants(grants: List[FeatureGrant]): Int =
    grants.collect { case FeatureGrant.ExpertiseChoice(n) => n }.sum

  def weaponMasteryCountFromGrants(grants: List[FeatureGrant]): Int =
    grants.collect { case FeatureGrant.WeaponMasteryChoice(n) => n }.sum

  /** Whether the given selections satisfy all choice-typed grants (e.g. for creation or level-up validation). */
  def satisfiesGrantChoices(grants: List[FeatureGrant], fs: ClassFeatureSelections): Boolean = {
    val needFightingStyle     = grants.exists { case FeatureGrant.FightingStyleChoice() => true; case _ => false }
    val needDivineOrder       = grants.exists { case FeatureGrant.DivineOrderChoice() => true; case _ => false }
    val needPrimalOrder       = grants.exists { case FeatureGrant.PrimalOrderChoice() => true; case _ => false }
    val needEldritchInvocation = grants.exists { case FeatureGrant.EldritchInvocationChoice() => true; case _ => false }
    val needLandType          = grants.exists { case FeatureGrant.LandTypeChoice() => true; case _ => false }
    val needHunterPrey        = grants.exists { case FeatureGrant.HunterPreyChoice() => true; case _ => false }
    val expertiseRequired     = expertiseCountFromGrants(grants)
    val weaponMasteryRequired = weaponMasteryCountFromGrants(grants)
    (!needFightingStyle || fs.fightingStyle.isDefined) &&
    (!needDivineOrder || fs.divineOrder.isDefined) &&
    (!needPrimalOrder || fs.primalOrder.isDefined) &&
    (!needEldritchInvocation || fs.eldritchInvocation.isDefined) &&
    (!needLandType || fs.landType.isDefined) &&
    (!needHunterPrey || fs.hunterPrey.isDefined) &&
    fs.expertiseSkills.size >= expertiseRequired &&
    fs.weaponMasteries.size >= weaponMasteryRequired
  }

  private def isChoiceGrant(g: FeatureGrant): Boolean = g match {
    case _: FeatureGrant.ExpertiseChoice       => true
    case _: FeatureGrant.WeaponMasteryChoice   => true
    case _: FeatureGrant.FightingStyleChoice   => true
    case _: FeatureGrant.DivineOrderChoice     => true
    case _: FeatureGrant.PrimalOrderChoice     => true
    case _: FeatureGrant.EldritchInvocationChoice => true
    case _: FeatureGrant.LandTypeChoice        => true
    case _: FeatureGrant.HunterPreyChoice      => true
    case _: FeatureGrant.ExtraSkillsChoice    => true
    case _: FeatureGrant.SubclassGate         => true
    case _                                    => false
  }

  /** Choice-typed grants at this level (from features' grants plus class-specific synthetic grants). */
  def grantChoicesAtLevel(gain: LevelGain): List[FeatureGrant] =
    featuresFromGain(gain).flatMap(_.grants).filter(isChoiceGrant) ++ gain.syntheticGrants

  /** Cumulative choice-typed grants from level 1 up to and including the given level. */
  def grantChoicesUpToLevel(dndClass: DndClass, level: Int): List[FeatureGrant] =
    (1 to level).flatMap(l => grantChoicesAtLevel(atLevel(dndClass, l))).toList
}
