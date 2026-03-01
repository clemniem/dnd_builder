package dndbuilder.dnd

final case class LevelGain(
    features: List[ClassFeature],
    choices: List[LevelChoice])

object LevelGain {
  val empty: LevelGain = LevelGain(Nil, Nil)
}

sealed trait LevelChoice
object LevelChoice {
  case object ASI extends LevelChoice
  case object ChooseSubclass extends LevelChoice
  case class ChooseExtraSkills(count: Int, pool: Set[Skill]) extends LevelChoice
  case object ChooseLandType extends LevelChoice
  case object ChooseHunterPrey extends LevelChoice
  case object ChooseFightingStyle extends LevelChoice
  case class ChooseExpertise(count: Int) extends LevelChoice
  case object ChooseDivineOrder extends LevelChoice
  case object ChoosePrimalOrder extends LevelChoice
  case object ChooseEldritchInvocation extends LevelChoice
  case class ChooseWeaponMastery(count: Int) extends LevelChoice
}

object ClassProgression {

  private val level1Entries: List[((String, Int), LevelGain)] = List(
    (DndClass.Barbarian.name, 1) -> LevelGain(DndClass.Barbarian.level1Features, List(LevelChoice.ChooseWeaponMastery(2))),
    (DndClass.Bard.name, 1) -> LevelGain(DndClass.Bard.level1Features, Nil),
    (DndClass.Cleric.name, 1) -> LevelGain(DndClass.Cleric.level1Features, List(LevelChoice.ChooseDivineOrder)),
    (DndClass.Druid.name, 1) -> LevelGain(DndClass.Druid.level1Features, List(LevelChoice.ChoosePrimalOrder)),
    (DndClass.Fighter.name, 1) -> LevelGain(DndClass.Fighter.level1Features, List(LevelChoice.ChooseFightingStyle, LevelChoice.ChooseWeaponMastery(3))),
    (DndClass.Monk.name, 1) -> LevelGain(DndClass.Monk.level1Features, Nil),
    (DndClass.Paladin.name, 1) -> LevelGain(DndClass.Paladin.level1Features, List(LevelChoice.ChooseWeaponMastery(2))),
    (DndClass.Ranger.name, 1) -> LevelGain(DndClass.Ranger.level1Features, List(LevelChoice.ChooseWeaponMastery(2))),
    (DndClass.Rogue.name, 1) -> LevelGain(DndClass.Rogue.level1Features, List(LevelChoice.ChooseExpertise(2), LevelChoice.ChooseWeaponMastery(2))),
    (DndClass.Sorcerer.name, 1) -> LevelGain(DndClass.Sorcerer.level1Features, Nil),
    (DndClass.Warlock.name, 1) -> LevelGain(DndClass.Warlock.level1Features, List(LevelChoice.ChooseEldritchInvocation)),
    (DndClass.Wizard.name, 1) -> LevelGain(DndClass.Wizard.level1Features, Nil)
  )

  private val level2Entries: List[((String, Int), LevelGain)] = List(
    (DndClass.Barbarian.name, 2) -> LevelGain(SRD_registry.getMany("danger-sense", "reckless-attack"), Nil),
    (DndClass.Bard.name, 2) -> LevelGain(SRD_registry.getMany("expertise", "jack-of-all-trades"), List(LevelChoice.ChooseExpertise(2))),
    (DndClass.Cleric.name, 2) -> LevelGain(SRD_registry.getMany("channel-divinity-cleric"), Nil),
    (DndClass.Druid.name, 2) -> LevelGain(SRD_registry.getMany("wild-shape", "wild-companion"), Nil),
    (DndClass.Fighter.name, 2) -> LevelGain(SRD_registry.getMany("action-surge", "tactical-mind"), Nil),
    (DndClass.Monk.name, 2) -> LevelGain(SRD_registry.getMany("monks-focus", "unarmored-movement"), Nil),
    (DndClass.Paladin.name, 2) -> LevelGain(SRD_registry.getMany("fighting-style", "paladins-smite"), List(LevelChoice.ChooseFightingStyle)),
    (DndClass.Ranger.name, 2) -> LevelGain(SRD_registry.getMany("deft-explorer", "fighting-style"), List(LevelChoice.ChooseExpertise(1), LevelChoice.ChooseFightingStyle)),
    (DndClass.Rogue.name, 2) -> LevelGain(SRD_registry.getMany("cunning-action"), Nil),
    (DndClass.Sorcerer.name, 2) -> LevelGain(SRD_registry.getMany("font-of-magic", "metamagic"), Nil),
    (DndClass.Warlock.name, 2) -> LevelGain(SRD_registry.getMany("magical-cunning"), Nil),
    (DndClass.Wizard.name, 2) -> LevelGain(SRD_registry.getMany("scholar"), List(LevelChoice.ChooseExpertise(1)))
  )

  private val level3Entries: List[((String, Int), LevelGain)] = List(
    (DndClass.Barbarian.name, 3) -> LevelGain(
      SRD_registry.getMany("primal-knowledge"),
      List(LevelChoice.ChooseSubclass, LevelChoice.ChooseExtraSkills(1, DndClass.Barbarian.skillPool))
    ),
    (DndClass.Bard.name, 3) -> LevelGain(Nil, List(LevelChoice.ChooseSubclass, LevelChoice.ChooseExtraSkills(3, Skill.values.toSet))),
    (DndClass.Cleric.name, 3) -> LevelGain(Nil, List(LevelChoice.ChooseSubclass)),
    (DndClass.Druid.name, 3) -> LevelGain(Nil, List(LevelChoice.ChooseSubclass, LevelChoice.ChooseLandType)),
    (DndClass.Fighter.name, 3) -> LevelGain(Nil, List(LevelChoice.ChooseSubclass)),
    (DndClass.Monk.name, 3) -> LevelGain(SRD_registry.getMany("deflect-attacks"), List(LevelChoice.ChooseSubclass)),
    (DndClass.Paladin.name, 3) -> LevelGain(SRD_registry.getMany("channel-divinity-paladin"), List(LevelChoice.ChooseSubclass)),
    (DndClass.Ranger.name, 3) -> LevelGain(Nil, List(LevelChoice.ChooseSubclass, LevelChoice.ChooseHunterPrey)),
    (DndClass.Rogue.name, 3) -> LevelGain(SRD_registry.getMany("steady-aim"), List(LevelChoice.ChooseSubclass)),
    (DndClass.Sorcerer.name, 3) -> LevelGain(Nil, List(LevelChoice.ChooseSubclass)),
    (DndClass.Warlock.name, 3) -> LevelGain(Nil, List(LevelChoice.ChooseSubclass)),
    (DndClass.Wizard.name, 3) -> LevelGain(Nil, List(LevelChoice.ChooseSubclass))
  )

  private lazy val registry: Map[(String, Int), LevelGain] =
    (level1Entries ++ level2Entries ++ level3Entries).toMap

  def atLevel(dndClass: DndClass, level: Int): LevelGain =
    registry.getOrElse((dndClass.name, level), LevelGain.empty)

  def featuresUpToLevel(dndClass: DndClass, level: Int): List[ClassFeature] =
    (1 to level).flatMap(l => atLevel(dndClass, l).features).toList

  def choicesUpToLevel(dndClass: DndClass, level: Int): List[LevelChoice] =
    (1 to level).flatMap(l => atLevel(dndClass, l).choices).toList

  def maxSupportedLevel(dndClass: DndClass): Int =
    (1 to 20).reverse.find(l => registry.contains((dndClass.name, l))).getOrElse(1)

  /** Whether the given selections satisfy all choices (e.g. for creation or level-up validation). */
  def satisfiesChoices(choices: List[LevelChoice], fs: ClassFeatureSelections): Boolean = {
    val needFightingStyle = choices.contains(LevelChoice.ChooseFightingStyle)
    val needDivineOrder = choices.contains(LevelChoice.ChooseDivineOrder)
    val needPrimalOrder = choices.contains(LevelChoice.ChoosePrimalOrder)
    val needEldritchInvocation = choices.contains(LevelChoice.ChooseEldritchInvocation)
    val needLandType = choices.contains(LevelChoice.ChooseLandType)
    val needHunterPrey = choices.contains(LevelChoice.ChooseHunterPrey)
    val expertiseRequired = choices.collect { case LevelChoice.ChooseExpertise(n) => n }.sum
    val weaponMasteryRequired = choices.collect { case LevelChoice.ChooseWeaponMastery(n) => n }.sum
    (!needFightingStyle || fs.fightingStyle.isDefined) &&
    (!needDivineOrder || fs.divineOrder.isDefined) &&
    (!needPrimalOrder || fs.primalOrder.isDefined) &&
    (!needEldritchInvocation || fs.eldritchInvocation.isDefined) &&
    (!needLandType || fs.landType.isDefined) &&
    (!needHunterPrey || fs.hunterPrey.isDefined) &&
    fs.expertiseSkills.size >= expertiseRequired &&
    fs.weaponMasteries.size >= weaponMasteryRequired
  }

  def expertiseCountFromChoices(choices: List[LevelChoice]): Int =
    choices.collect { case LevelChoice.ChooseExpertise(n) => n }.sum

  def weaponMasteryCountFromChoices(choices: List[LevelChoice]): Int =
    choices.collect { case LevelChoice.ChooseWeaponMastery(n) => n }.sum
}
