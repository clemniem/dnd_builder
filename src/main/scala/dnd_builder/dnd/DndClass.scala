package dndbuilder.dnd

import dndbuilder.dnd.DndTypes.Score

final case class ClassFeature(name: String, description: String, uses: Option[Int])

enum SpellCasterType {
  case NonCaster, FullCaster, HalfCaster, PactMagic
}

/** For full casters, which progression table (cantrips/prepared) to use. */
enum FullCasterVariant {
  case Bard, Cleric, Druid, Sorcerer, Wizard
}

final case class DndClass(
    name: String,
    hitDie: HitDie,
    primaryAbilities: List[Ability],
    savingThrows: Set[Ability],
    armorProficiencies: Set[ArmorType],
    weaponProficiencies: Set[WeaponProficiency],
    skillPool: Set[Skill],
    numSkillChoices: Int,
    spellcastingAbility: Option[Ability],
    spellCasterType: SpellCasterType,
    fullCasterVariant: Option[FullCasterVariant],
    level1Features: List[ClassFeature],
    description: String,
    recommendedScores: AbilityScores,
    weaponMasteryCount: Int,
    extraLanguageChoices: Int,
    unarmoredDefenseAbility: Option[Ability],
    usesSpellbook: Boolean,
    jackOfAllTradesAtLevel: Option[Int],
    grantedAttacks: List[AttackGrant]
) {
  def isSpellcaster: Boolean = spellCasterType != SpellCasterType.NonCaster

  def weaponSummary: String = {
    val hasSimple   = weaponProficiencies.contains(WeaponProficiency.AllSimple)
    val hasMartial  = weaponProficiencies.contains(WeaponProficiency.AllMartial)
    val martialIf   = weaponProficiencies.collect { case WeaponProficiency.MartialIf(p) => p }.headOption
    (hasSimple, hasMartial, martialIf) match {
      case (true, true, None) => "Simple & Martial weapons"
      case (true, false, None) => "Simple weapons"
      case (true, false, Some(ps)) if ps == Set(WeaponProperty.Light) => "Simple weapons, Martial (Light)"
      case (true, false, Some(_)) => "Simple weapons, Martial (Finesse/Light)"
      case _ => "Weapons"
    }
  }

  def level1HitPoints(conMod: Int): Int = hitDie.sides + conMod
}

object DndClass {

  /** Factory for classes where most optional fields are at their "normal" value.
    * Takes the ~10 fields every class must specify; sets the rest to defaults.
    * Callers use .copy() to override the few that differ. */
  def base(
      name: String,
      hitDie: HitDie,
      primaryAbilities: List[Ability],
      savingThrows: Set[Ability],
      armorProficiencies: Set[ArmorType],
      weaponProficiencies: Set[WeaponProficiency],
      skillPool: Set[Skill],
      level1Features: List[ClassFeature],
      description: String,
      recommendedScores: AbilityScores
  ): DndClass = DndClass(
    name = name,
    hitDie = hitDie,
    primaryAbilities = primaryAbilities,
    savingThrows = savingThrows,
    armorProficiencies = armorProficiencies,
    weaponProficiencies = weaponProficiencies,
    skillPool = skillPool,
    numSkillChoices = 2,
    spellcastingAbility = None,
    spellCasterType = SpellCasterType.NonCaster,
    fullCasterVariant = None,
    level1Features = level1Features,
    description = description,
    recommendedScores = recommendedScores,
    weaponMasteryCount = 0,
    extraLanguageChoices = 0,
    unarmoredDefenseAbility = None,
    usesSpellbook = false,
    jackOfAllTradesAtLevel = None,
    grantedAttacks = Nil
  )

  val Barbarian: DndClass = base(
    name = "Barbarian",
    hitDie = HitDie.D12,
    primaryAbilities = List(Ability.Strength),
    savingThrows = Set(Ability.Strength, Ability.Constitution),
    armorProficiencies = Set(ArmorType.Light, ArmorType.Medium, ArmorType.Shield),
    weaponProficiencies = Set(WeaponProficiency.AllSimple, WeaponProficiency.AllMartial),
    skillPool = Set(
      Skill.AnimalHandling, Skill.Athletics, Skill.Intimidation,
      Skill.Nature, Skill.Perception, Skill.Survival
    ),
    level1Features = SRD_registry.getMany("rage", "unarmored-defense-barbarian", "weapon-mastery-2"),
    description = "A fierce warrior fueled by primal rage",
    recommendedScores = AbilityScores(Score(15), Score(13), Score(14), Score(10), Score(12), Score(8))
  ).copy(
    weaponMasteryCount = 2,
    unarmoredDefenseAbility = Some(Ability.Constitution)
  )

  val Bard: DndClass = base(
    name = "Bard",
    hitDie = HitDie.D8,
    primaryAbilities = List(Ability.Charisma),
    savingThrows = Set(Ability.Dexterity, Ability.Charisma),
    armorProficiencies = Set(ArmorType.Light),
    weaponProficiencies = Set(WeaponProficiency.AllSimple),
    skillPool = Skill.values.toSet,
    level1Features = SRD_registry.getMany("bardic-inspiration", "spellcasting-bard"),
    description = "An inspiring magician whose music channels arcane power",
    recommendedScores = AbilityScores(Score(8), Score(14), Score(12), Score(13), Score(10), Score(15))
  ).copy(
    numSkillChoices = 3,
    spellcastingAbility = Some(Ability.Charisma),
    spellCasterType = SpellCasterType.FullCaster,
    fullCasterVariant = Some(FullCasterVariant.Bard),
    jackOfAllTradesAtLevel = Some(2),
    extraLanguageChoices = 1
  )

  val Cleric: DndClass = base(
    name = "Cleric",
    hitDie = HitDie.D8,
    primaryAbilities = List(Ability.Wisdom),
    savingThrows = Set(Ability.Wisdom, Ability.Charisma),
    armorProficiencies = Set(ArmorType.Light, ArmorType.Medium, ArmorType.Shield),
    weaponProficiencies = Set(WeaponProficiency.AllSimple),
    skillPool = Set(
      Skill.History, Skill.Insight, Skill.Medicine, Skill.Persuasion, Skill.Religion
    ),
    level1Features = SRD_registry.getMany("spellcasting-cleric", "divine-order"),
    description = "A divine champion who wields holy magic",
    recommendedScores = AbilityScores(Score(14), Score(8), Score(13), Score(10), Score(15), Score(12))
  ).copy(
    spellcastingAbility = Some(Ability.Wisdom),
    spellCasterType = SpellCasterType.FullCaster,
    fullCasterVariant = Some(FullCasterVariant.Cleric)
  )

  val Druid: DndClass = base(
    name = "Druid",
    hitDie = HitDie.D8,
    primaryAbilities = List(Ability.Wisdom),
    savingThrows = Set(Ability.Intelligence, Ability.Wisdom),
    armorProficiencies = Set(ArmorType.Light, ArmorType.Shield),
    weaponProficiencies = Set(WeaponProficiency.AllSimple),
    skillPool = Set(
      Skill.Arcana, Skill.AnimalHandling, Skill.Insight,
      Skill.Medicine, Skill.Nature, Skill.Perception,
      Skill.Religion, Skill.Survival
    ),
    level1Features = SRD_registry.getMany("spellcasting-druid", "druidic", "primal-order"),
    description = "A priest of nature who wields elemental and beast magic",
    recommendedScores = AbilityScores(Score(8), Score(12), Score(14), Score(13), Score(15), Score(10))
  ).copy(
    spellcastingAbility = Some(Ability.Wisdom),
    spellCasterType = SpellCasterType.FullCaster,
    fullCasterVariant = Some(FullCasterVariant.Druid)
  )

  val Fighter: DndClass = base(
    name = "Fighter",
    hitDie = HitDie.D10,
    primaryAbilities = List(Ability.Strength, Ability.Dexterity),
    savingThrows = Set(Ability.Strength, Ability.Constitution),
    armorProficiencies = Set(ArmorType.Light, ArmorType.Medium, ArmorType.Heavy, ArmorType.Shield),
    weaponProficiencies = Set(WeaponProficiency.AllSimple, WeaponProficiency.AllMartial),
    skillPool = Set(
      Skill.Acrobatics, Skill.AnimalHandling, Skill.Athletics,
      Skill.History, Skill.Insight, Skill.Intimidation,
      Skill.Perception, Skill.Survival
    ),
    level1Features = SRD_registry.getMany("fighting-style", "second-wind", "weapon-mastery-3"),
    description = "A master of martial combat with every weapon and armor",
    recommendedScores = AbilityScores(Score(15), Score(14), Score(13), Score(8), Score(10), Score(12))
  ).copy(weaponMasteryCount = 3)

  val Monk: DndClass = base(
    name = "Monk",
    hitDie = HitDie.D8,
    primaryAbilities = List(Ability.Dexterity, Ability.Wisdom),
    savingThrows = Set(Ability.Strength, Ability.Dexterity),
    armorProficiencies = Set.empty[ArmorType],
    weaponProficiencies = Set(WeaponProficiency.AllSimple, WeaponProficiency.MartialIf(Set(WeaponProperty.Light))),
    skillPool = Set(
      Skill.Acrobatics, Skill.Athletics, Skill.History,
      Skill.Insight, Skill.Religion, Skill.Stealth
    ),
    level1Features = SRD_registry.getMany("martial-arts", "unarmored-defense-monk"),
    description = "A martial artist harnessing body and mind as one weapon",
    recommendedScores = AbilityScores(Score(12), Score(15), Score(13), Score(10), Score(14), Score(8))
  ).copy(
    unarmoredDefenseAbility = Some(Ability.Wisdom),
    grantedAttacks = List(
      AttackGrant(
        "Unarmed Strike",
        AttackKind.Weapon,
        "1d6",
        "bludg.",
        AttackGrantDelivery.MeleeAttack(Ability.Dexterity),
        DiceScaling.MartialArts,
        false,
        "",
        "Monk"
      )
    )
  )

  val Paladin: DndClass = base(
    name = "Paladin",
    hitDie = HitDie.D10,
    primaryAbilities = List(Ability.Strength, Ability.Charisma),
    savingThrows = Set(Ability.Wisdom, Ability.Charisma),
    armorProficiencies = Set(ArmorType.Light, ArmorType.Medium, ArmorType.Heavy, ArmorType.Shield),
    weaponProficiencies = Set(WeaponProficiency.AllSimple, WeaponProficiency.AllMartial),
    skillPool = Set(
      Skill.Athletics, Skill.Insight, Skill.Intimidation,
      Skill.Medicine, Skill.Persuasion, Skill.Religion
    ),
    level1Features = SRD_registry.getMany("lay-on-hands", "spellcasting-paladin", "weapon-mastery-2"),
    description = "A holy warrior bound to a sacred oath",
    recommendedScores = AbilityScores(Score(15), Score(10), Score(13), Score(8), Score(12), Score(14))
  ).copy(
    spellcastingAbility = Some(Ability.Charisma),
    spellCasterType = SpellCasterType.HalfCaster,
    weaponMasteryCount = 2
  )

  val Ranger: DndClass = base(
    name = "Ranger",
    hitDie = HitDie.D10,
    primaryAbilities = List(Ability.Dexterity, Ability.Wisdom),
    savingThrows = Set(Ability.Strength, Ability.Dexterity),
    armorProficiencies = Set(ArmorType.Light, ArmorType.Medium, ArmorType.Shield),
    weaponProficiencies = Set(WeaponProficiency.AllSimple, WeaponProficiency.AllMartial),
    skillPool = Set(
      Skill.AnimalHandling, Skill.Athletics, Skill.Insight,
      Skill.Investigation, Skill.Nature, Skill.Perception,
      Skill.Stealth, Skill.Survival
    ),
    level1Features = SRD_registry.getMany("spellcasting-ranger", "favored-enemy", "weapon-mastery-2"),
    description = "A wandering warrior attuned to nature and the hunt",
    recommendedScores = AbilityScores(Score(12), Score(15), Score(13), Score(8), Score(14), Score(10))
  ).copy(
    numSkillChoices = 3,
    spellcastingAbility = Some(Ability.Wisdom),
    spellCasterType = SpellCasterType.HalfCaster,
    weaponMasteryCount = 2,
    extraLanguageChoices = 1
  )

  val Rogue: DndClass = base(
    name = "Rogue",
    hitDie = HitDie.D8,
    primaryAbilities = List(Ability.Dexterity),
    savingThrows = Set(Ability.Dexterity, Ability.Intelligence),
    armorProficiencies = Set(ArmorType.Light),
    weaponProficiencies = Set(WeaponProficiency.AllSimple, WeaponProficiency.MartialIf(Set(WeaponProperty.Finesse, WeaponProperty.Light))),
    skillPool = Set(
      Skill.Acrobatics, Skill.Athletics, Skill.Deception,
      Skill.Insight, Skill.Intimidation, Skill.Investigation,
      Skill.Perception, Skill.Performance, Skill.Persuasion,
      Skill.SleightOfHand, Skill.Stealth
    ),
    level1Features = SRD_registry.getMany("expertise", "sneak-attack", "thieves-cant", "weapon-mastery-2"),
    description = "A scoundrel who uses stealth and trickery to overcome obstacles",
    recommendedScores = AbilityScores(Score(12), Score(15), Score(13), Score(14), Score(10), Score(8))
  ).copy(
    numSkillChoices = 4,
    weaponMasteryCount = 2
  )

  val Sorcerer: DndClass = base(
    name = "Sorcerer",
    hitDie = HitDie.D6,
    primaryAbilities = List(Ability.Charisma),
    savingThrows = Set(Ability.Constitution, Ability.Charisma),
    armorProficiencies = Set.empty[ArmorType],
    weaponProficiencies = Set(WeaponProficiency.AllSimple),
    skillPool = Set(
      Skill.Arcana, Skill.Deception, Skill.Insight,
      Skill.Intimidation, Skill.Persuasion, Skill.Religion
    ),
    level1Features = SRD_registry.getMany("spellcasting-sorcerer", "innate-sorcery"),
    description = "A spellcaster who draws on inherent magic from birth or bloodline",
    recommendedScores = AbilityScores(Score(10), Score(13), Score(14), Score(8), Score(12), Score(15))
  ).copy(
    spellcastingAbility = Some(Ability.Charisma),
    spellCasterType = SpellCasterType.FullCaster,
    fullCasterVariant = Some(FullCasterVariant.Sorcerer)
  )

  val Warlock: DndClass = base(
    name = "Warlock",
    hitDie = HitDie.D8,
    primaryAbilities = List(Ability.Charisma),
    savingThrows = Set(Ability.Wisdom, Ability.Charisma),
    armorProficiencies = Set(ArmorType.Light),
    weaponProficiencies = Set(WeaponProficiency.AllSimple),
    skillPool = Set(
      Skill.Arcana, Skill.Deception, Skill.History,
      Skill.Intimidation, Skill.Investigation, Skill.Nature, Skill.Religion
    ),
    level1Features = SRD_registry.getMany("pact-magic", "eldritch-invocations"),
    description = "A wielder of magic granted by an otherworldly patron",
    recommendedScores = AbilityScores(Score(8), Score(14), Score(13), Score(12), Score(10), Score(15))
  ).copy(
    spellcastingAbility = Some(Ability.Charisma),
    spellCasterType = SpellCasterType.PactMagic
  )

  val Wizard: DndClass = base(
    name = "Wizard",
    hitDie = HitDie.D6,
    primaryAbilities = List(Ability.Intelligence),
    savingThrows = Set(Ability.Intelligence, Ability.Wisdom),
    armorProficiencies = Set.empty[ArmorType],
    weaponProficiencies = Set(WeaponProficiency.AllSimple),
    skillPool = Set(
      Skill.Arcana, Skill.History, Skill.Insight,
      Skill.Investigation, Skill.Medicine, Skill.Religion
    ),
    level1Features = SRD_registry.getMany("spellcasting-wizard", "ritual-adept", "arcane-recovery"),
    description = "A scholarly magic-user who commands arcane spells through study",
    recommendedScores = AbilityScores(Score(8), Score(12), Score(13), Score(15), Score(14), Score(10))
  ).copy(
    spellcastingAbility = Some(Ability.Intelligence),
    spellCasterType = SpellCasterType.FullCaster,
    fullCasterVariant = Some(FullCasterVariant.Wizard),
    usesSpellbook = true
  )

  val builtIn: List[DndClass] = List(
    Barbarian, Bard, Cleric, Druid, Fighter, Monk,
    Paladin, Ranger, Rogue, Sorcerer, Warlock, Wizard
  )

  /** Temporary alias; will be replaced by RuleSet.classes once threading is done. */
  val all: List[DndClass] = builtIn
}
