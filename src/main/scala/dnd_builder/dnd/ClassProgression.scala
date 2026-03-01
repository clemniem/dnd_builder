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
    (Barbarian.name, 1) -> LevelGain(Barbarian.level1Features, List(LevelChoice.ChooseWeaponMastery(2))),
    (Bard.name, 1) -> LevelGain(Bard.level1Features, Nil),
    (Cleric.name, 1) -> LevelGain(Cleric.level1Features, List(LevelChoice.ChooseDivineOrder)),
    (Druid.name, 1) -> LevelGain(Druid.level1Features, List(LevelChoice.ChoosePrimalOrder)),
    (Fighter.name, 1) -> LevelGain(Fighter.level1Features, List(LevelChoice.ChooseFightingStyle, LevelChoice.ChooseWeaponMastery(3))),
    (Monk.name, 1) -> LevelGain(Monk.level1Features, Nil),
    (Paladin.name, 1) -> LevelGain(Paladin.level1Features, List(LevelChoice.ChooseWeaponMastery(2))),
    (Ranger.name, 1) -> LevelGain(Ranger.level1Features, List(LevelChoice.ChooseWeaponMastery(2))),
    (Rogue.name, 1) -> LevelGain(Rogue.level1Features, List(LevelChoice.ChooseExpertise(2), LevelChoice.ChooseWeaponMastery(2))),
    (Sorcerer.name, 1) -> LevelGain(Sorcerer.level1Features, Nil),
    (Warlock.name, 1) -> LevelGain(Warlock.level1Features, List(LevelChoice.ChooseEldritchInvocation)),
    (Wizard.name, 1) -> LevelGain(Wizard.level1Features, Nil)
  )

  private val level2Entries: List[((String, Int), LevelGain)] = List(
    (Barbarian.name, 2) -> LevelGain(
      List(
        ClassFeature("Danger Sense", "Advantage on DEX saves you can see. Not Blinded, Deafened, or Incapacitated.", None),
        ClassFeature("Reckless Attack", "First attack on your turn: choose Advantage on STR attacks this turn; attacks against you also have Advantage.", None)
      ), Nil),
    (Bard.name, 2) -> LevelGain(
      List(
        ClassFeature("Expertise", "Choose 2 skill proficiencies to gain Expertise (double proficiency bonus).", None),
        ClassFeature("Jack of All Trades", "Add half your proficiency bonus (round down) to ability checks you are not proficient in.", None)
      ), List(LevelChoice.ChooseExpertise(2))),
    (Cleric.name, 2) -> LevelGain(
      List(
        ClassFeature("Channel Divinity", "2 uses per Short/Long Rest. Turn Undead: undead within 30ft must save or be turned for 1 min.", Some(2))
      ), Nil),
    (Druid.name, 2) -> LevelGain(
      List(
        ClassFeature("Wild Shape", "Bonus Action to magically assume a beast form. Uses = proficiency bonus per Long Rest.", None),
        ClassFeature("Wild Companion", "Expend a Wild Shape use to cast Find Familiar without material components.", None)
      ), Nil),
    (Fighter.name, 2) -> LevelGain(
      List(
        ClassFeature("Action Surge", "1 use per Short/Long Rest. On your turn, take one additional Action.", Some(1)),
        ClassFeature("Tactical Mind", "When you fail an ability check, expend a Second Wind use to add 1d10 to the roll.", None)
      ), Nil),
    (Monk.name, 2) -> LevelGain(
      List(
        ClassFeature("Monk's Focus", "2 Focus Points per Short/Long Rest. Spend points for Flurry of Blows (1), Patient Defense (1), or Step of the Wind (1).", Some(2)),
        ClassFeature("Unarmored Movement", "+10ft speed when not wearing armor or wielding a shield.", None)
      ), Nil),
    (Paladin.name, 2) -> LevelGain(
      List(
        ClassFeature("Fighting Style", "Choose a Fighting Style feat.", None),
        ClassFeature("Paladin's Smite", "When you hit with a melee weapon, expend a spell slot to deal 2d8 extra Radiant damage (+1d8 per slot level above 1st).", None)
      ), List(LevelChoice.ChooseFightingStyle)),
    (Ranger.name, 2) -> LevelGain(
      List(
        ClassFeature("Deft Explorer", "Choose one of your skill proficiencies. Your proficiency bonus is doubled for ability checks using that skill.", None),
        ClassFeature("Fighting Style", "Choose a Fighting Style feat.", None)
      ), List(LevelChoice.ChooseExpertise(1), LevelChoice.ChooseFightingStyle)),
    (Rogue.name, 2) -> LevelGain(
      List(
        ClassFeature("Cunning Action", "Bonus Action each turn: Dash, Disengage, or Hide.", None)
      ), Nil),
    (Sorcerer.name, 2) -> LevelGain(
      List(
        ClassFeature("Font of Magic", "2 Sorcery Points per Long Rest. Convert spell slots to points or points to slots.", Some(2)),
        ClassFeature("Metamagic", "Choose 2 Metamagic options. Spend Sorcery Points to twist spells.", None)
      ), Nil),
    (Warlock.name, 2) -> LevelGain(
      List(
        ClassFeature("Magical Cunning", "If all Pact Magic slots are expended, perform a 1-minute ritual to recover half (round up). Once per Long Rest.", Some(1))
      ), Nil),
    (Wizard.name, 2) -> LevelGain(
      List(
        ClassFeature("Scholar", "Choose one of your skill proficiencies. Your proficiency bonus is doubled for ability checks using that skill.", None)
      ), List(LevelChoice.ChooseExpertise(1)))
  )

  private val level3Entries: List[((String, Int), LevelGain)] = List(
    (Barbarian.name, 3) -> LevelGain(
      List(
        ClassFeature(
          "Primal Knowledge",
          "Gain proficiency in one more skill from the Barbarian skill list. While Rage is active, you can make Acrobatics, Intimidation, Perception, Stealth, or Survival checks using Strength.",
          None
        )
      ),
      List(LevelChoice.ChooseSubclass, LevelChoice.ChooseExtraSkills(1, Barbarian.skillPool))
    ),
    (Bard.name, 3) -> LevelGain(
      Nil,
      List(LevelChoice.ChooseSubclass, LevelChoice.ChooseExtraSkills(3, Skill.values.toSet))
    ),
    (Cleric.name, 3) -> LevelGain(Nil, List(LevelChoice.ChooseSubclass)),
    (Druid.name, 3) -> LevelGain(Nil, List(LevelChoice.ChooseSubclass, LevelChoice.ChooseLandType)),
    (Fighter.name, 3) -> LevelGain(Nil, List(LevelChoice.ChooseSubclass)),
    (Monk.name, 3) -> LevelGain(
      List(
        ClassFeature(
          "Deflect Attacks",
          "When an attack hits you and deals B/P/S damage, use a Reaction to reduce damage by 1d10 + DEX mod + Monk level. If reduced to 0, spend 1 Focus Point to redirect force at another creature.",
          None
        )
      ),
      List(LevelChoice.ChooseSubclass)
    ),
    (Paladin.name, 3) -> LevelGain(
      List(
        ClassFeature(
          "Channel Divinity",
          "2 uses per Short/Long Rest. Divine Sense: detect Celestials, Fiends, Undead within 60 ft for 10 min.",
          Some(2)
        )
      ),
      List(LevelChoice.ChooseSubclass)
    ),
    (Ranger.name, 3) -> LevelGain(Nil, List(LevelChoice.ChooseSubclass, LevelChoice.ChooseHunterPrey)),
    (Rogue.name, 3) -> LevelGain(
      List(
        ClassFeature(
          "Steady Aim",
          "Bonus Action: gain Advantage on your next attack roll this turn. You cannot have moved this turn; after use, Speed is 0 until end of turn.",
          None
        )
      ),
      List(LevelChoice.ChooseSubclass)
    ),
    (Sorcerer.name, 3) -> LevelGain(Nil, List(LevelChoice.ChooseSubclass)),
    (Warlock.name, 3) -> LevelGain(Nil, List(LevelChoice.ChooseSubclass)),
    (Wizard.name, 3) -> LevelGain(Nil, List(LevelChoice.ChooseSubclass))
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
