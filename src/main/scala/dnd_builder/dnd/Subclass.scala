package dndbuilder.dnd

/** SRD 2024: one subclass per class. Level 3 features only for Phase 1. */
sealed trait Subclass {
  def name: String
  def dndClass: DndClass
  def description: String
  def features: Map[Int, List[ClassFeature]]
  def alwaysPreparedByLevel: Map[Int, List[Spell]]
}

object Subclass {

  private def spells(names: String*): List[Spell] =
    names.flatMap(n => Spell.byName.get(n)).toList

  private def level3Only(fs: List[ClassFeature]): Map[Int, List[ClassFeature]] =
    Map(3 -> fs)

  private def alwaysPrepared(level: Int, names: String*): Map[Int, List[Spell]] =
    Map(level -> spells(names*))

  case object PathOfTheBerserker extends Subclass {
    val name         = "Path of the Berserker"
    val dndClass     = DndClass.Barbarian
    val description = "Channel Rage into violent fury. Thrill in the chaos of battle."
    val features    = level3Only(
      List(
        ClassFeature(
          "Frenzy",
          "If you use Reckless Attack while Rage is active, deal extra damage to the first target you hit on your turn with a Strength-based attack. Roll a number of d6s equal to your Rage Damage bonus.",
          None
        )
      )
    )
    val alwaysPreparedByLevel = Map.empty[Int, List[Spell]]
  }

  case object CollegeOfLore extends Subclass {
    val name         = "College of Lore"
    val dndClass     = DndClass.Bard
    val description = "Plumb the depths of magical knowledge. Collect spells and secrets from diverse sources."
    val features    = level3Only(
      List(
        ClassFeature(
          "Bonus Proficiencies",
          "You gain proficiency with three skills of your choice.",
          None
        ),
        ClassFeature(
          "Cutting Words",
          "When a creature you can see within 60 feet makes a damage roll or succeeds on an ability check or attack roll, you can use a Reaction to expend Bardic Inspiration; roll the die and subtract the result from that roll.",
          None
        )
      )
    )
    val alwaysPreparedByLevel = Map.empty[Int, List[Spell]]
  }

  case object LifeDomain extends Subclass {
    val name         = "Life Domain"
    val dndClass     = DndClass.Cleric
    val description = "Soothe the hurts of the world. Masters of healing using positive energy."
    val features    = level3Only(
      List(
        ClassFeature(
          "Disciple of Life",
          "When a spell you cast with a spell slot restores Hit Points, the creature regains additional Hit Points equal to 2 + the spell's level.",
          None
        ),
        ClassFeature(
          "Life Domain Spells",
          "You always have Aid, Bless, Cure Wounds, and Lesser Restoration prepared.",
          None
        ),
        ClassFeature(
          "Preserve Life",
          "As a Magic action, expend Channel Divinity to restore Hit Points equal to 5 x Cleric level among Bloodied creatures within 30 feet (no creature above half HP).",
          None
        )
      )
    )
    val alwaysPreparedByLevel = alwaysPrepared(3, "Aid", "Bless", "Cure Wounds", "Lesser Restoration")
  }

  case object CircleOfTheLand extends Subclass {
    val name         = "Circle of the Land"
    val dndClass     = DndClass.Druid
    val description = "Mystics and sages who safeguard ancient knowledge. Connection to the natural world."
    val features    = level3Only(
      List(
        ClassFeature(
          "Circle of the Land Spells",
          "Whenever you finish a Long Rest, choose one land type (arid, polar, temperate, tropical). You have the listed circle spells prepared for your level.",
          None
        ),
        ClassFeature(
          "Land's Aid",
          "As a Magic action, expend Wild Shape; choose a point within 60 feet. Each creature of your choice in a 10-foot radius must save or take 2d6 Necrotic (half on save). One creature of your choice in the area regains 2d6 HP.",
          None
        )
      )
    )
    val alwaysPreparedByLevel = Map.empty[Int, List[Spell]]
  }

  case object Champion extends Subclass {
    val name         = "Champion"
    val dndClass     = DndClass.Fighter
    val description = "Physical excellence in combat. Devastating blows, withstand peril, garner glory."
    val features    = level3Only(
      List(
        ClassFeature(
          "Improved Critical",
          "Your attack rolls with weapons and Unarmed Strikes score a Critical Hit on a roll of 19 or 20.",
          None
        ),
        ClassFeature(
          "Remarkable Athlete",
          "Advantage on Initiative and Strength (Athletics) checks. When you score a Critical Hit, you can move up to half your Speed without provoking Opportunity Attacks.",
          None
        )
      )
    )
    val alwaysPreparedByLevel = Map.empty[Int, List[Spell]]
  }

  case object WarriorOfTheOpenHand extends Subclass {
    val name         = "Warrior of the Open Hand"
    val dndClass     = DndClass.Monk
    val description = "Master unarmed combat techniques. Push, trip, and manipulate energy to protect yourself."
    val features    = level3Only(
      List(
        ClassFeature(
          "Open Hand Technique",
          "When you hit with Flurry of Blows, you can impose one effect: Addle (no Opportunity Attacks until next turn), Push (STR save or pushed 15 ft), or Topple (DEX save or Prone).",
          None
        )
      )
    )
    val alwaysPreparedByLevel = Map.empty[Int, List[Spell]]
  }

  case object OathOfDevotion extends Subclass {
    val name         = "Oath of Devotion"
    val dndClass     = DndClass.Paladin
    val description = "Uphold the ideals of justice and order. Knight in shining armor."
    val features    = level3Only(
      List(
        ClassFeature(
          "Oath of Devotion Spells",
          "You always have Protection from Evil and Good and Shield of Faith prepared.",
          None
        ),
        ClassFeature(
          "Sacred Weapon",
          "When you take the Attack action, expend Channel Divinity to imbue a melee weapon. For 10 min: add CHA to attack rolls, deal Radiant or normal damage, weapon emits bright light 20 ft.",
          None
        )
      )
    )
    val alwaysPreparedByLevel = alwaysPrepared(3, "Protection from Evil and Good", "Shield of Faith")
  }

  case object Hunter extends Subclass {
    val name         = "Hunter"
    val dndClass     = DndClass.Ranger
    val description = "Stalk prey in the wilds. Protect nature and people from destruction."
    val features    = level3Only(
      List(
        ClassFeature(
          "Hunter's Lore",
          "While a creature is marked by Hunter's Mark, you know whether it has Immunities, Resistances, or Vulnerabilities, and what they are.",
          None
        ),
        ClassFeature(
          "Hunter's Prey",
          "Choose Colossus Slayer (extra 1d8 damage once per turn when target is missing HP) or Horde Breaker (extra attack against different creature within 5 ft). You can swap on Short or Long Rest.",
          None
        )
      )
    )
    val alwaysPreparedByLevel = Map.empty[Int, List[Spell]]
  }

  case object Thief extends Subclass {
    val name         = "Thief"
    val dndClass     = DndClass.Rogue
    val description = "Burglar, treasure hunter, explorer. Agility, stealth, and magic item use."
    val features    = level3Only(
      List(
        ClassFeature(
          "Fast Hands",
          "As a Bonus Action: make a Sleight of Hand check to pick a lock, disarm a trap, or pick a pocket; or take the Utilize action or use a magic item that requires the Magic action.",
          None
        ),
        ClassFeature(
          "Second-Story Work",
          "Climb Speed equal to your Speed. You can use Dexterity instead of Strength to determine jump distance.",
          None
        )
      )
    )
    val alwaysPreparedByLevel = Map.empty[Int, List[Spell]]
  }

  case object DraconicSorcery extends Subclass {
    val name         = "Draconic Sorcery"
    val dndClass     = DndClass.Sorcerer
    val description = "Innate magic from the gift of a dragon. Draconic resilience and elemental affinity."
    val features    = level3Only(
      List(
        ClassFeature(
          "Draconic Resilience",
          "HP max increases by 3, and by 1 per Sorcerer level. While not wearing armor, base AC = 10 + DEX + CHA.",
          None
        ),
        ClassFeature(
          "Draconic Spells",
          "You always have Alter Self, Chromatic Orb, Command, and Dragon's Breath prepared.",
          None
        )
      )
    )
    val alwaysPreparedByLevel = alwaysPrepared(3, "Alter Self", "Chromatic Orb", "Command", "Dragon's Breath")
  }

  case object FiendPatron extends Subclass {
    val name         = "The Fiend"
    val dndClass     = DndClass.Warlock
    val description = "Your pact draws on the Lower Planes. Demons, archdevils, or other mighty fiends."
    val features    = level3Only(
      List(
        ClassFeature(
          "Dark One's Blessing",
          "When you reduce an enemy to 0 HP (or an ally within 10 ft does), gain Temporary HP = CHA mod + Warlock level.",
          None
        ),
        ClassFeature(
          "Fiend Spells",
          "You always have Burning Hands, Command, Scorching Ray, and Suggestion prepared.",
          None
        )
      )
    )
    val alwaysPreparedByLevel = alwaysPrepared(3, "Burning Hands", "Command", "Scorching Ray", "Suggestion")
  }

  case object SchoolOfEvocation extends Subclass {
    val name         = "School of Evocation"
    val dndClass     = DndClass.Wizard
    val description = "Magic that creates powerful elemental effects: cold, flame, thunder, lightning, acid."
    val features    = level3Only(
      List(
        ClassFeature(
          "Evocation Savant",
          "Choose two Wizard Evocation spells of level 2 or lower and add them to your spellbook for free. When you gain a new spell slot level, add one Evocation spell of that level to your spellbook for free.",
          None
        ),
        ClassFeature(
          "Potent Cantrip",
          "When you cast a cantrip at a creature and miss or the target saves, the target still takes half the cantrip's damage (if any) but no other effect.",
          None
        )
      )
    )
    val alwaysPreparedByLevel = Map.empty[Int, List[Spell]]
  }

  val all: List[Subclass] = List(
    PathOfTheBerserker,
    CollegeOfLore,
    LifeDomain,
    CircleOfTheLand,
    Champion,
    WarriorOfTheOpenHand,
    OathOfDevotion,
    Hunter,
    Thief,
    DraconicSorcery,
    FiendPatron,
    SchoolOfEvocation
  )

  def forClass(cls: DndClass): Option[Subclass] =
    all.find(_.dndClass == cls)

  def byName(name: String): Option[Subclass] =
    all.find(_.name == name)
}
