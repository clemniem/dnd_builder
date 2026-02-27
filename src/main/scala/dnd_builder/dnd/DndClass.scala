package dndbuilder.dnd

final case class ClassFeature(name: String, description: String)

sealed trait DndClass:
  def name: String
  def hitDie: HitDie
  def primaryAbilities: List[Ability]
  def savingThrows: (Ability, Ability)
  def armorProficiencies: Set[ArmorType]
  def weaponProficiencies: Set[WeaponProficiency]
  def skillPool: Set[Skill]
  def numSkillChoices: Int
  def spellcastingAbility: Option[Ability]
  def cantripsKnown: Int
  def level1SpellSlots: Int
  def numPreparedSpells: Int
  def spellbookSize: Int
  def level1Features: List[ClassFeature]
  def description: String
  def recommendedScores: AbilityScores

  def isSpellcaster: Boolean = spellcastingAbility.isDefined

  def weaponSummary: String =
    val hasSimple  = weaponProficiencies.contains(WeaponProficiency.AllSimple)
    val hasMartial  = weaponProficiencies.contains(WeaponProficiency.AllMartial)
    val martialIf   = weaponProficiencies.collect { case WeaponProficiency.MartialIf(p) => p }.headOption
    (hasSimple, hasMartial, martialIf) match
      case (true, true, None) => "Simple & Martial weapons"
      case (true, false, None) => "Simple weapons"
      case (true, false, Some(ps)) if ps == Set(WeaponProperty.Light) => "Simple weapons, Martial (Light)"
      case (true, false, Some(_)) => "Simple weapons, Martial (Finesse/Light)"
      case _ => "Weapons"

  def level1HitPoints(conMod: Int): Int = hitDie.sides + conMod

case object Barbarian extends DndClass:
  val name               = "Barbarian"
  val hitDie             = HitDie.D12
  val primaryAbilities   = List(Ability.Strength)
  val savingThrows       = (Ability.Strength, Ability.Constitution)
  val armorProficiencies  = Set(ArmorType.Light, ArmorType.Medium, ArmorType.Shield)
  val weaponProficiencies = Set(WeaponProficiency.AllSimple, WeaponProficiency.AllMartial)
  val skillPool          = Set(
    Skill.AnimalHandling, Skill.Athletics, Skill.Intimidation,
    Skill.Nature, Skill.Perception, Skill.Survival
  )
  val numSkillChoices      = 2
  val spellcastingAbility  = None
  val cantripsKnown        = 0
  val level1SpellSlots     = 0
  val numPreparedSpells    = 0
  val spellbookSize        = 0
  val description          = "A fierce warrior fueled by primal rage"
  val recommendedScores    = AbilityScores(15, 13, 14, 10, 12, 8)
  val level1Features       = List(
    ClassFeature("Rage", "2 uses. +2 damage, resistance to B/P/S, Advantage on STR checks. Bonus Action to activate."),
    ClassFeature("Unarmored Defense", "AC = 10 + DEX mod + CON mod when not wearing armor."),
    ClassFeature("Weapon Mastery", "Choose 2 weapons to gain Mastery properties.")
  )

case object Bard extends DndClass:
  val name               = "Bard"
  val hitDie             = HitDie.D8
  val primaryAbilities   = List(Ability.Charisma)
  val savingThrows       = (Ability.Dexterity, Ability.Charisma)
  val armorProficiencies  = Set(ArmorType.Light)
  val weaponProficiencies = Set(WeaponProficiency.AllSimple)
  val skillPool          = Skill.values.toSet
  val numSkillChoices      = 3
  val spellcastingAbility  = Some(Ability.Charisma)
  val cantripsKnown        = 2
  val level1SpellSlots     = 2
  val numPreparedSpells    = 4
  val spellbookSize        = 0
  val description          = "An inspiring magician whose music channels arcane power"
  val recommendedScores    = AbilityScores(8, 14, 12, 13, 10, 15)
  val level1Features       = List(
    ClassFeature("Bardic Inspiration", "d6 die. CHA mod uses per Long Rest. Bonus Action to grant to ally within 60ft."),
    ClassFeature("Spellcasting", "CHA-based. 2 cantrips, 4 prepared spells, 2 level-1 slots.")
  )

case object Cleric extends DndClass:
  val name               = "Cleric"
  val hitDie             = HitDie.D8
  val primaryAbilities   = List(Ability.Wisdom)
  val savingThrows       = (Ability.Wisdom, Ability.Charisma)
  val armorProficiencies  = Set(ArmorType.Light, ArmorType.Medium, ArmorType.Shield)
  val weaponProficiencies = Set(WeaponProficiency.AllSimple)
  val skillPool          = Set(
    Skill.History, Skill.Insight, Skill.Medicine, Skill.Persuasion, Skill.Religion
  )
  val numSkillChoices      = 2
  val spellcastingAbility  = Some(Ability.Wisdom)
  val cantripsKnown        = 3
  val level1SpellSlots     = 2
  val numPreparedSpells    = 4
  val spellbookSize        = 0
  val description          = "A divine champion who wields holy magic"
  val recommendedScores    = AbilityScores(14, 8, 13, 10, 15, 12)
  val level1Features       = List(
    ClassFeature("Spellcasting", "WIS-based. 3 cantrips, 4 prepared spells, 2 level-1 slots."),
    ClassFeature("Divine Order", "Choose Protector (Martial weapons + Heavy armor) or Thaumaturge (extra cantrip + INT bonus to Arcana/Religion).")
  )

case object Druid extends DndClass:
  val name               = "Druid"
  val hitDie             = HitDie.D8
  val primaryAbilities   = List(Ability.Wisdom)
  val savingThrows       = (Ability.Intelligence, Ability.Wisdom)
  val armorProficiencies  = Set(ArmorType.Light, ArmorType.Shield)
  val weaponProficiencies = Set(WeaponProficiency.AllSimple)
  val skillPool          = Set(
    Skill.Arcana, Skill.AnimalHandling, Skill.Insight,
    Skill.Medicine, Skill.Nature, Skill.Perception,
    Skill.Religion, Skill.Survival
  )
  val numSkillChoices      = 2
  val spellcastingAbility  = Some(Ability.Wisdom)
  val cantripsKnown        = 2
  val level1SpellSlots     = 2
  val numPreparedSpells    = 4
  val spellbookSize        = 0
  val description          = "A priest of nature who wields elemental and beast magic"
  val recommendedScores    = AbilityScores(8, 12, 14, 13, 15, 10)
  val level1Features       = List(
    ClassFeature("Spellcasting", "WIS-based. 2 cantrips, 4 prepared spells, 2 level-1 slots."),
    ClassFeature("Druidic", "Secret language. Speak with Animals always prepared."),
    ClassFeature("Primal Order", "Choose Magician (extra cantrip + INT bonus to Arcana/Nature) or Warden (Martial weapons + Medium armor).")
  )

case object Fighter extends DndClass:
  val name               = "Fighter"
  val hitDie             = HitDie.D10
  val primaryAbilities   = List(Ability.Strength, Ability.Dexterity)
  val savingThrows       = (Ability.Strength, Ability.Constitution)
  val armorProficiencies  = Set(ArmorType.Light, ArmorType.Medium, ArmorType.Heavy, ArmorType.Shield)
  val weaponProficiencies = Set(WeaponProficiency.AllSimple, WeaponProficiency.AllMartial)
  val skillPool          = Set(
    Skill.Acrobatics, Skill.AnimalHandling, Skill.Athletics,
    Skill.History, Skill.Insight, Skill.Intimidation,
    Skill.Perception, Skill.Survival
  )
  val numSkillChoices      = 2
  val spellcastingAbility  = None
  val cantripsKnown        = 0
  val level1SpellSlots     = 0
  val numPreparedSpells    = 0
  val spellbookSize        = 0
  val description          = "A master of martial combat with every weapon and armor"
  val recommendedScores    = AbilityScores(15, 14, 13, 8, 10, 12)
  val level1Features       = List(
    ClassFeature("Fighting Style", "Choose a Fighting Style feat."),
    ClassFeature("Second Wind", "2 uses. Bonus Action to heal 1d10 + Fighter level HP."),
    ClassFeature("Weapon Mastery", "Choose 3 weapons to gain Mastery properties.")
  )

case object Monk extends DndClass:
  val name               = "Monk"
  val hitDie             = HitDie.D8
  val primaryAbilities   = List(Ability.Dexterity, Ability.Wisdom)
  val savingThrows       = (Ability.Strength, Ability.Dexterity)
  val armorProficiencies  = Set.empty[ArmorType]
  val weaponProficiencies = Set(WeaponProficiency.AllSimple, WeaponProficiency.MartialIf(Set(WeaponProperty.Light)))
  val skillPool          = Set(
    Skill.Acrobatics, Skill.Athletics, Skill.History,
    Skill.Insight, Skill.Religion, Skill.Stealth
  )
  val numSkillChoices      = 2
  val spellcastingAbility  = None
  val cantripsKnown        = 0
  val level1SpellSlots     = 0
  val numPreparedSpells    = 0
  val spellbookSize        = 0
  val description          = "A martial artist harnessing body and mind as one weapon"
  val recommendedScores    = AbilityScores(12, 15, 13, 10, 14, 8)
  val level1Features       = List(
    ClassFeature("Martial Arts", "1d6 martial arts die. Bonus Action Unarmed Strike. Use DEX for monk weapon attacks."),
    ClassFeature("Unarmored Defense", "AC = 10 + DEX mod + WIS mod when not wearing armor or shield.")
  )

case object Paladin extends DndClass:
  val name               = "Paladin"
  val hitDie             = HitDie.D10
  val primaryAbilities   = List(Ability.Strength, Ability.Charisma)
  val savingThrows       = (Ability.Wisdom, Ability.Charisma)
  val armorProficiencies  = Set(ArmorType.Light, ArmorType.Medium, ArmorType.Heavy, ArmorType.Shield)
  val weaponProficiencies = Set(WeaponProficiency.AllSimple, WeaponProficiency.AllMartial)
  val skillPool          = Set(
    Skill.Athletics, Skill.Insight, Skill.Intimidation,
    Skill.Medicine, Skill.Persuasion, Skill.Religion
  )
  val numSkillChoices      = 2
  val spellcastingAbility  = Some(Ability.Charisma)
  val cantripsKnown        = 0
  val level1SpellSlots     = 2
  val numPreparedSpells    = 2
  val spellbookSize        = 0
  val description          = "A holy warrior bound to a sacred oath"
  val recommendedScores    = AbilityScores(15, 10, 13, 8, 12, 14)
  val level1Features       = List(
    ClassFeature("Lay On Hands", "Pool = 5 x Paladin level HP. Bonus Action to heal."),
    ClassFeature("Spellcasting", "CHA-based. 2 prepared spells, 2 level-1 slots."),
    ClassFeature("Weapon Mastery", "Choose 2 weapons to gain Mastery properties.")
  )

case object Ranger extends DndClass:
  val name               = "Ranger"
  val hitDie             = HitDie.D10
  val primaryAbilities   = List(Ability.Dexterity, Ability.Wisdom)
  val savingThrows       = (Ability.Strength, Ability.Dexterity)
  val armorProficiencies  = Set(ArmorType.Light, ArmorType.Medium, ArmorType.Shield)
  val weaponProficiencies = Set(WeaponProficiency.AllSimple, WeaponProficiency.AllMartial)
  val skillPool          = Set(
    Skill.AnimalHandling, Skill.Athletics, Skill.Insight,
    Skill.Investigation, Skill.Nature, Skill.Perception,
    Skill.Stealth, Skill.Survival
  )
  val numSkillChoices      = 3
  val spellcastingAbility  = Some(Ability.Wisdom)
  val cantripsKnown        = 0
  val level1SpellSlots     = 2
  val numPreparedSpells    = 2
  val spellbookSize        = 0
  val description          = "A wandering warrior attuned to nature and the hunt"
  val recommendedScores    = AbilityScores(12, 15, 13, 8, 14, 10)
  val level1Features       = List(
    ClassFeature("Spellcasting", "WIS-based. 2 prepared spells, 2 level-1 slots."),
    ClassFeature("Favored Enemy", "Hunter's Mark always prepared. 2 free casts per Long Rest."),
    ClassFeature("Weapon Mastery", "Choose 2 weapons to gain Mastery properties.")
  )

case object Rogue extends DndClass:
  val name               = "Rogue"
  val hitDie             = HitDie.D8
  val primaryAbilities   = List(Ability.Dexterity)
  val savingThrows       = (Ability.Dexterity, Ability.Intelligence)
  val armorProficiencies  = Set(ArmorType.Light)
  val weaponProficiencies = Set(WeaponProficiency.AllSimple, WeaponProficiency.MartialIf(Set(WeaponProperty.Finesse, WeaponProperty.Light)))
  val skillPool          = Set(
    Skill.Acrobatics, Skill.Athletics, Skill.Deception,
    Skill.Insight, Skill.Intimidation, Skill.Investigation,
    Skill.Perception, Skill.Performance, Skill.Persuasion,
    Skill.SleightOfHand, Skill.Stealth
  )
  val numSkillChoices      = 4
  val spellcastingAbility  = None
  val cantripsKnown        = 0
  val level1SpellSlots     = 0
  val numPreparedSpells    = 0
  val spellbookSize        = 0
  val description          = "A scoundrel who uses stealth and trickery to overcome obstacles"
  val recommendedScores    = AbilityScores(12, 15, 13, 14, 10, 8)
  val level1Features       = List(
    ClassFeature("Expertise", "Choose 2 skills to gain Expertise (double proficiency bonus)."),
    ClassFeature("Sneak Attack", "1d6 extra damage when you have Advantage or an ally adjacent to the target."),
    ClassFeature("Thieves' Cant", "Secret language of rogues."),
    ClassFeature("Weapon Mastery", "Choose 2 weapons to gain Mastery properties.")
  )

case object Sorcerer extends DndClass:
  val name               = "Sorcerer"
  val hitDie             = HitDie.D6
  val primaryAbilities   = List(Ability.Charisma)
  val savingThrows       = (Ability.Constitution, Ability.Charisma)
  val armorProficiencies  = Set.empty[ArmorType]
  val weaponProficiencies = Set(WeaponProficiency.AllSimple)
  val skillPool          = Set(
    Skill.Arcana, Skill.Deception, Skill.Insight,
    Skill.Intimidation, Skill.Persuasion, Skill.Religion
  )
  val numSkillChoices      = 2
  val spellcastingAbility  = Some(Ability.Charisma)
  val cantripsKnown        = 4
  val level1SpellSlots     = 2
  val numPreparedSpells    = 4
  val spellbookSize        = 0
  val description          = "A spellcaster who draws on inherent magic from birth or bloodline"
  val recommendedScores    = AbilityScores(10, 13, 14, 8, 12, 15)
  val level1Features       = List(
    ClassFeature("Spellcasting", "CHA-based. 4 cantrips, 4 prepared spells, 2 level-1 slots."),
    ClassFeature("Innate Sorcery", "2 uses. Bonus Action for 1 min: Advantage on spell attacks, spell save DC +1.")
  )

case object Warlock extends DndClass:
  val name               = "Warlock"
  val hitDie             = HitDie.D8
  val primaryAbilities   = List(Ability.Charisma)
  val savingThrows       = (Ability.Wisdom, Ability.Charisma)
  val armorProficiencies  = Set(ArmorType.Light)
  val weaponProficiencies = Set(WeaponProficiency.AllSimple)
  val skillPool          = Set(
    Skill.Arcana, Skill.Deception, Skill.History,
    Skill.Intimidation, Skill.Investigation, Skill.Nature, Skill.Religion
  )
  val numSkillChoices      = 2
  val spellcastingAbility  = Some(Ability.Charisma)
  val cantripsKnown        = 2
  val level1SpellSlots     = 1
  val numPreparedSpells    = 2
  val spellbookSize        = 0
  val description          = "A wielder of magic granted by an otherworldly patron"
  val recommendedScores    = AbilityScores(8, 14, 13, 12, 10, 15)
  val level1Features       = List(
    ClassFeature("Pact Magic", "CHA-based. 2 cantrips, 2 prepared spells, 1 level-1 slot. Slots recover on Short/Long Rest."),
    ClassFeature("Eldritch Invocations", "1 invocation. Choose from Pact of the Tome, Blade, or Chain.")
  )

case object Wizard extends DndClass:
  val name               = "Wizard"
  val hitDie             = HitDie.D6
  val primaryAbilities   = List(Ability.Intelligence)
  val savingThrows       = (Ability.Intelligence, Ability.Wisdom)
  val armorProficiencies  = Set.empty[ArmorType]
  val weaponProficiencies = Set(WeaponProficiency.AllSimple)
  val skillPool          = Set(
    Skill.Arcana, Skill.History, Skill.Insight,
    Skill.Investigation, Skill.Medicine, Skill.Religion
  )
  val numSkillChoices      = 2
  val spellcastingAbility  = Some(Ability.Intelligence)
  val cantripsKnown        = 3
  val level1SpellSlots     = 2
  val numPreparedSpells    = 4
  val spellbookSize        = 6
  val description          = "A scholarly magic-user who commands arcane spells through study"
  val recommendedScores    = AbilityScores(8, 12, 13, 15, 14, 10)
  val level1Features       = List(
    ClassFeature("Spellcasting", "INT-based. 3 cantrips, 4 prepared from spellbook, 2 level-1 slots. Spellbook starts with 6 level-1 spells."),
    ClassFeature("Ritual Adept", "Cast Ritual spells from spellbook without preparing them."),
    ClassFeature("Arcane Recovery", "Once per day on Short Rest, recover spell slots totaling up to half Wizard level (round up).")
  )

object DndClass:
  val all: List[DndClass] = List(
    Barbarian, Bard, Cleric, Druid, Fighter, Monk,
    Paladin, Ranger, Rogue, Sorcerer, Warlock, Wizard
  )
