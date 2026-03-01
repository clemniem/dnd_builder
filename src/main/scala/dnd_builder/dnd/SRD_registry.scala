package dndbuilder.dnd

/** Central registry of SRD class features by stable id. Used by DndClass and ClassProgression
  * so feature text lives in one place. Can later be swapped via RuleSet/config.
  */
object SRD_registry {

  private val all: Map[String, ClassFeature] = List(
    // --- Level 1 (classes) ---
    "rage" -> ClassFeature("Rage", "2 uses. +2 damage, resistance to B/P/S, Advantage on STR checks. Bonus Action to activate.", Some(2)),
    "unarmored-defense-barbarian" -> ClassFeature("Unarmored Defense", "AC = 10 + DEX mod + CON mod when not wearing armor.", None),
    "weapon-mastery-2" -> ClassFeature("Weapon Mastery", "Choose 2 weapons to gain Mastery properties.", None),
    "weapon-mastery-3" -> ClassFeature("Weapon Mastery", "Choose 3 weapons to gain Mastery properties.", None),
    "bardic-inspiration" -> ClassFeature("Bardic Inspiration", "d6 die. CHA mod uses per Long Rest. Bonus Action to grant to ally within 60ft.", None),
    "spellcasting-bard" -> ClassFeature("Spellcasting", "CHA-based. 2 cantrips, 4 prepared spells, 2 level-1 slots.", None),
    "spellcasting-cleric" -> ClassFeature("Spellcasting", "WIS-based. 3 cantrips, 4 prepared spells, 2 level-1 slots.", None),
    "divine-order" -> ClassFeature("Divine Order", "Choose Protector (Martial weapons + Heavy armor) or Thaumaturge (extra cantrip + INT bonus to Arcana/Religion).", None),
    "spellcasting-druid" -> ClassFeature("Spellcasting", "WIS-based. 2 cantrips, 4 prepared spells, 2 level-1 slots.", None),
    "druidic" -> ClassFeature("Druidic", "Secret language. Speak with Animals always prepared.", None),
    "primal-order" -> ClassFeature("Primal Order", "Choose Magician (extra cantrip + INT bonus to Arcana/Nature) or Warden (Martial weapons + Medium armor).", None),
    "fighting-style" -> ClassFeature("Fighting Style", "Choose a Fighting Style feat.", None),
    "second-wind" -> ClassFeature("Second Wind", "2 uses. Bonus Action to heal 1d10 + Fighter level HP.", Some(2)),
    "martial-arts" -> ClassFeature("Martial Arts", "1d6 martial arts die. Bonus Action Unarmed Strike. Use DEX for monk weapon attacks.", None),
    "unarmored-defense-monk" -> ClassFeature("Unarmored Defense", "AC = 10 + DEX mod + WIS mod when not wearing armor or shield.", None),
    "lay-on-hands" -> ClassFeature("Lay On Hands", "Pool = 5 x Paladin level HP. Bonus Action to heal.", None),
    "spellcasting-paladin" -> ClassFeature("Spellcasting", "CHA-based. 2 prepared spells, 2 level-1 slots.", None),
    "spellcasting-ranger" -> ClassFeature("Spellcasting", "WIS-based. 2 prepared spells, 2 level-1 slots.", None),
    "favored-enemy" -> ClassFeature("Favored Enemy", "Hunter's Mark always prepared. 2 free casts per Long Rest.", Some(2)),
    "expertise" -> ClassFeature("Expertise", "Choose 2 skills to gain Expertise (double proficiency bonus).", None),
    "sneak-attack" -> ClassFeature("Sneak Attack", "1d6 extra damage when you have Advantage or an ally adjacent to the target.", None),
    "thieves-cant" -> ClassFeature("Thieves' Cant", "Secret language of rogues.", None),
    "spellcasting-sorcerer" -> ClassFeature("Spellcasting", "CHA-based. 4 cantrips, 4 prepared spells, 2 level-1 slots.", None),
    "innate-sorcery" -> ClassFeature("Innate Sorcery", "2 uses. Bonus Action for 1 min: Advantage on spell attacks, spell save DC +1.", Some(2)),
    "pact-magic" -> ClassFeature("Pact Magic", "CHA-based. 2 cantrips, 2 prepared spells, 1 level-1 slot. Slots recover on Short/Long Rest.", None),
    "eldritch-invocations" -> ClassFeature("Eldritch Invocations", "1 invocation. Choose from Pact of the Tome, Blade, or Chain.", None),
    "spellcasting-wizard" -> ClassFeature("Spellcasting", "INT-based. 3 cantrips, 4 prepared from spellbook, 2 level-1 slots. Spellbook starts with 6 level-1 spells.", None),
    "ritual-adept" -> ClassFeature("Ritual Adept", "Cast Ritual spells from spellbook without preparing them.", None),
    "arcane-recovery" -> ClassFeature("Arcane Recovery", "Once per day on Short Rest, recover spell slots totaling up to half Wizard level (round up).", Some(1)),
    // --- Level 2 (ClassProgression) ---
    "danger-sense" -> ClassFeature("Danger Sense", "Advantage on DEX saves you can see. Not Blinded, Deafened, or Incapacitated.", None),
    "reckless-attack" -> ClassFeature("Reckless Attack", "First attack on your turn: choose Advantage on STR attacks this turn; attacks against you also have Advantage.", None),
    "jack-of-all-trades" -> ClassFeature("Jack of All Trades", "Add half your proficiency bonus (round down) to ability checks you are not proficient in.", None),
    "channel-divinity-cleric" -> ClassFeature("Channel Divinity", "2 uses per Short/Long Rest. Turn Undead: undead within 30ft must save or be turned for 1 min.", Some(2)),
    "wild-shape" -> ClassFeature("Wild Shape", "Bonus Action to magically assume a beast form. Uses = proficiency bonus per Long Rest.", None),
    "wild-companion" -> ClassFeature("Wild Companion", "Expend a Wild Shape use to cast Find Familiar without material components.", None),
    "action-surge" -> ClassFeature("Action Surge", "1 use per Short/Long Rest. On your turn, take one additional Action.", Some(1)),
    "tactical-mind" -> ClassFeature("Tactical Mind", "When you fail an ability check, expend a Second Wind use to add 1d10 to the roll.", None),
    "monks-focus" -> ClassFeature("Monk's Focus", "2 Focus Points per Short/Long Rest. Spend points for Flurry of Blows (1), Patient Defense (1), or Step of the Wind (1).", Some(2)),
    "unarmored-movement" -> ClassFeature("Unarmored Movement", "+10ft speed when not wearing armor or wielding a shield.", None),
    "paladins-smite" -> ClassFeature("Paladin's Smite", "When you hit with a melee weapon, expend a spell slot to deal 2d8 extra Radiant damage (+1d8 per slot level above 1st).", None),
    "deft-explorer" -> ClassFeature("Deft Explorer", "Choose one of your skill proficiencies. Your proficiency bonus is doubled for ability checks using that skill.", None),
    "cunning-action" -> ClassFeature("Cunning Action", "Bonus Action each turn: Dash, Disengage, or Hide.", None),
    "font-of-magic" -> ClassFeature("Font of Magic", "2 Sorcery Points per Long Rest. Convert spell slots to points or points to slots.", Some(2)),
    "metamagic" -> ClassFeature("Metamagic", "Choose 2 Metamagic options. Spend Sorcery Points to twist spells.", None),
    "magical-cunning" -> ClassFeature("Magical Cunning", "If all Pact Magic slots are expended, perform a 1-minute ritual to recover half (round up). Once per Long Rest.", Some(1)),
    "scholar" -> ClassFeature("Scholar", "Choose one of your skill proficiencies. Your proficiency bonus is doubled for ability checks using that skill.", None),
    // --- Level 3 (ClassProgression) ---
    "primal-knowledge" -> ClassFeature(
      "Primal Knowledge",
      "Gain proficiency in one more skill from the Barbarian skill list. While Rage is active, you can make Acrobatics, Intimidation, Perception, Stealth, or Survival checks using Strength.",
      None
    ),
    "deflect-attacks" -> ClassFeature(
      "Deflect Attacks",
      "When an attack hits you and deals B/P/S damage, use a Reaction to reduce damage by 1d10 + DEX mod + Monk level. If reduced to 0, spend 1 Focus Point to redirect force at another creature.",
      None
    ),
    "channel-divinity-paladin" -> ClassFeature(
      "Channel Divinity",
      "2 uses per Short/Long Rest. Divine Sense: detect Celestials, Fiends, Undead within 60 ft for 10 min.",
      Some(2)
    ),
    "steady-aim" -> ClassFeature(
      "Steady Aim",
      "Bonus Action: gain Advantage on your next attack roll this turn. You cannot have moved this turn; after use, Speed is 0 until end of turn.",
      None
    )
  ).toMap

  def get(id: String): Option[ClassFeature] = all.get(id)

  /** Resolve ids to features in order; missing ids are skipped (caller must pass valid ids). */
  def getMany(ids: String*): List[ClassFeature] = ids.flatMap(get).toList
}
