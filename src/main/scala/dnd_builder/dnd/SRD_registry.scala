package dndbuilder.dnd

/** Central registry of SRD class features by stable id. Used by DndClass and ClassProgression
  * so feature text lives in one place. Can later be swapped via RuleSet/config.
  */
object SRD_registry {

  private def feat(id: String, name: String, description: String, uses: Option[Int], informative: Boolean): (String, Feature) =
    id -> Feature(id, name, description, uses, Nil, informative)

  private def featWith(id: String, name: String, description: String, uses: Option[Int], grants: List[FeatureGrant], informative: Boolean): (String, Feature) =
    id -> Feature(id, name, description, uses, grants, informative)

  private val monkUnarmedStrike: AttackGrant = AttackGrant(
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

  private val all: Map[String, Feature] = List(
    // --- Level 1 (classes) ---
    feat("rage", "Rage", "2 uses. +2 damage, resistance to B/P/S, Advantage on STR checks. Bonus Action to activate.", Some(2), false),
    featWith("unarmored-defense-barbarian", "Unarmored Defense", "AC = 10 + DEX mod + CON mod when not wearing armor.", None, List(FeatureGrant.ACFormula(List(Ability.Constitution))), true),
    featWith("weapon-mastery-2", "Weapon Mastery", "Choose 2 weapons to gain Mastery properties.", None, List(FeatureGrant.WeaponMasteryChoice(2)), false),
    featWith("weapon-mastery-3", "Weapon Mastery", "Choose 3 weapons to gain Mastery properties.", None, List(FeatureGrant.WeaponMasteryChoice(3)), false),
    feat("bardic-inspiration", "Bardic Inspiration", "d6 die. CHA mod uses per Long Rest. Bonus Action to grant to ally within 60ft.", None, false),
    feat("spellcasting-bard", "Spellcasting", "CHA-based. 2 cantrips, 4 prepared spells, 2 level-1 slots.", None, true),
    feat("spellcasting-cleric", "Spellcasting", "WIS-based. 3 cantrips, 4 prepared spells, 2 level-1 slots.", None, true),
    featWith("divine-order", "Divine Order", "Choose Protector (Martial weapons + Heavy armor) or Thaumaturge (extra cantrip + INT bonus to Arcana/Religion).", None, List(FeatureGrant.DivineOrderChoice()), false),
    feat("spellcasting-druid", "Spellcasting", "WIS-based. 2 cantrips, 4 prepared spells, 2 level-1 slots.", None, true),
    feat("druidic", "Druidic", "Secret language. Speak with Animals always prepared.", None, true),
    featWith("primal-order", "Primal Order", "Choose Magician (extra cantrip + INT bonus to Arcana/Nature) or Warden (Martial weapons + Medium armor).", None, List(FeatureGrant.PrimalOrderChoice()), false),
    featWith("fighting-style", "Fighting Style", "Choose a Fighting Style feat.", None, List(FeatureGrant.FightingStyleChoice()), false),
    feat("second-wind", "Second Wind", "2 uses. Bonus Action to heal 1d10 + Fighter level HP.", Some(2), false),
    featWith("martial-arts", "Martial Arts", "1d6 martial arts die. Bonus Action Unarmed Strike. Use DEX for monk weapon attacks.", None, List(FeatureGrant.Attack(monkUnarmedStrike)), false),
    featWith("unarmored-defense-monk", "Unarmored Defense", "AC = 10 + DEX mod + WIS mod when not wearing armor or shield.", None, List(FeatureGrant.ACFormula(List(Ability.Wisdom))), true),
    feat("lay-on-hands", "Lay On Hands", "Pool = 5 x Paladin level HP. Bonus Action to heal.", None, false),
    feat("spellcasting-paladin", "Spellcasting", "CHA-based. 2 prepared spells, 2 level-1 slots.", None, true),
    feat("spellcasting-ranger", "Spellcasting", "WIS-based. 2 prepared spells, 2 level-1 slots.", None, true),
    feat("favored-enemy", "Favored Enemy", "Hunter's Mark always prepared. 2 free casts per Long Rest.", Some(2), false),
    featWith("expertise", "Expertise", "Choose 2 skills to gain Expertise (double proficiency bonus).", None, List(FeatureGrant.ExpertiseChoice(2)), false),
    feat("sneak-attack", "Sneak Attack", "1d6 extra damage when you have Advantage or an ally adjacent to the target.", None, false),
    feat("thieves-cant", "Thieves' Cant", "Secret language of rogues.", None, true),
    feat("spellcasting-sorcerer", "Spellcasting", "CHA-based. 4 cantrips, 4 prepared spells, 2 level-1 slots.", None, true),
    feat("innate-sorcery", "Innate Sorcery", "2 uses. Bonus Action for 1 min: Advantage on spell attacks, spell save DC +1.", Some(2), false),
    feat("pact-magic", "Pact Magic", "CHA-based. 2 cantrips, 2 prepared spells, 1 level-1 slot. Slots recover on Short/Long Rest.", None, true),
    featWith("eldritch-invocations", "Eldritch Invocations", "1 invocation. Choose from Pact of the Tome, Blade, or Chain.", None, List(FeatureGrant.EldritchInvocationChoice()), false),
    feat("spellcasting-wizard", "Spellcasting", "INT-based. 3 cantrips, 4 prepared from spellbook, 2 level-1 slots. Spellbook starts with 6 level-1 spells.", None, true),
    feat("ritual-adept", "Ritual Adept", "Cast Ritual spells from spellbook without preparing them.", None, true),
    feat("arcane-recovery", "Arcane Recovery", "Once per day on Short Rest, recover spell slots totaling up to half Wizard level (round up).", Some(1), false),
    // --- Level 2 (ClassProgression) ---
    feat("danger-sense", "Danger Sense", "Advantage on DEX saves you can see. Not Blinded, Deafened, or Incapacitated.", None, false),
    feat("reckless-attack", "Reckless Attack", "First attack on your turn: choose Advantage on STR attacks this turn; attacks against you also have Advantage.", None, false),
    featWith("jack-of-all-trades", "Jack of All Trades", "Add half your proficiency bonus (round down) to ability checks you are not proficient in.", None, List(FeatureGrant.HalfProfUnproficientChecks()), true),
    feat("channel-divinity-cleric", "Channel Divinity", "2 uses per Short/Long Rest. Turn Undead: undead within 30ft must save or be turned for 1 min.", Some(2), false),
    feat("wild-shape", "Wild Shape", "Bonus Action to magically assume a beast form. Uses = proficiency bonus per Long Rest.", None, false),
    feat("wild-companion", "Wild Companion", "Expend a Wild Shape use to cast Find Familiar without material components.", None, false),
    feat("action-surge", "Action Surge", "1 use per Short/Long Rest. On your turn, take one additional Action.", Some(1), false),
    feat("tactical-mind", "Tactical Mind", "When you fail an ability check, expend a Second Wind use to add 1d10 to the roll.", None, false),
    feat("monks-focus", "Monk's Focus", "2 Focus Points per Short/Long Rest. Spend points for Flurry of Blows (1), Patient Defense (1), or Step of the Wind (1).", Some(2), false),
    feat("unarmored-movement", "Unarmored Movement", "+10ft speed when not wearing armor or wielding a shield.", None, false),
    feat("paladins-smite", "Paladin's Smite", "When you hit with a melee weapon, expend a spell slot to deal 2d8 extra Radiant damage (+1d8 per slot level above 1st).", None, false),
    featWith("deft-explorer", "Deft Explorer", "Choose one of your skill proficiencies. Your proficiency bonus is doubled for ability checks using that skill.", None, List(FeatureGrant.ExpertiseChoice(1)), false),
    feat("cunning-action", "Cunning Action", "Bonus Action each turn: Dash, Disengage, or Hide.", None, false),
    feat("font-of-magic", "Font of Magic", "2 Sorcery Points per Long Rest. Convert spell slots to points or points to slots.", Some(2), false),
    feat("metamagic", "Metamagic", "Choose 2 Metamagic options. Spend Sorcery Points to twist spells.", None, false),
    feat("magical-cunning", "Magical Cunning", "If all Pact Magic slots are expended, perform a 1-minute ritual to recover half (round up). Once per Long Rest.", Some(1), false),
    featWith("scholar", "Scholar", "Choose one of your skill proficiencies. Your proficiency bonus is doubled for ability checks using that skill.", None, List(FeatureGrant.ExpertiseChoice(1)), false),
    // --- Level 3 (ClassProgression) ---
    ("primal-knowledge", Feature(
      "primal-knowledge",
      "Primal Knowledge",
      "Gain proficiency in one more skill from the Barbarian skill list. While Rage is active, you can make Acrobatics, Intimidation, Perception, Stealth, or Survival checks using Strength.",
      None,
      Nil,
      false
    )),
    featWith("subclass", "Subclass", "Choose your subclass at this level.", None, List(FeatureGrant.SubclassGate()), false),
    featWith("land-type-choice", "Circle of the Land", "Choose your Land type.", None, List(FeatureGrant.LandTypeChoice()), false),
    featWith("hunter-prey-choice", "Hunter's Prey", "Choose your Hunter's Prey option.", None, List(FeatureGrant.HunterPreyChoice()), false),
    ("deflect-attacks", Feature(
      "deflect-attacks",
      "Deflect Attacks",
      "When an attack hits you and deals B/P/S damage, use a Reaction to reduce damage by 1d10 + DEX mod + Monk level. If reduced to 0, spend 1 Focus Point to redirect force at another creature.",
      None,
      Nil,
      false
    )),
    ("channel-divinity-paladin", Feature(
      "channel-divinity-paladin",
      "Channel Divinity",
      "2 uses per Short/Long Rest. Divine Sense: detect Celestials, Fiends, Undead within 60 ft for 10 min.",
      Some(2),
      Nil,
      false
    )),
    ("steady-aim", Feature(
      "steady-aim",
      "Steady Aim",
      "Bonus Action: gain Advantage on your next attack roll this turn. You cannot have moved this turn; after use, Speed is 0 until end of turn.",
      None,
      Nil,
      false
    ))
  ).toMap

  def get(id: String): Option[Feature] = all.get(id)

  /** Resolve ids to features in order; missing ids are skipped (caller must pass valid ids). */
  def getMany(ids: String*): List[Feature] = ids.flatMap(get).toList
}
