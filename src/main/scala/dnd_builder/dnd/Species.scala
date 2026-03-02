package dndbuilder.dnd

/** A species trait with an optional short description for display (e.g. in PDF or app). */
final case class SpeciesTrait(name: String, description: Option[String])

object SpeciesTrait {
  def apply(name: String, description: String): SpeciesTrait = SpeciesTrait(name, Some(description))
  def nameOnly(name: String): SpeciesTrait = SpeciesTrait(name, None)
}

enum ElvenLineage(val label: String) {
  case High extends ElvenLineage("High Elf")
  case Wood extends ElvenLineage("Wood Elf")
  case Drow extends ElvenLineage("Drow")
}

enum GnomishLineage(val label: String) {
  case Forest extends GnomishLineage("Forest Gnome")
  case Rock   extends GnomishLineage("Rock Gnome")
}

enum GiantAncestry(val label: String) {
  case Cloud extends GiantAncestry("Cloud Giant")
  case Fire  extends GiantAncestry("Fire Giant")
  case Frost extends GiantAncestry("Frost Giant")
  case Hill  extends GiantAncestry("Hill Giant")
  case Stone extends GiantAncestry("Stone Giant")
  case Storm extends GiantAncestry("Storm Giant")
}

enum FiendishLegacy(val label: String) {
  case Abyssal  extends FiendishLegacy("Abyssal")
  case Chthonic extends FiendishLegacy("Chthonic")
  case Infernal extends FiendishLegacy("Infernal")
}

enum DragonAncestry(val label: String, val damageType: String) {
  case Black  extends DragonAncestry("Black", "Acid")
  case Blue   extends DragonAncestry("Blue", "Lightning")
  case Brass  extends DragonAncestry("Brass", "Fire")
  case Bronze extends DragonAncestry("Bronze", "Lightning")
  case Copper extends DragonAncestry("Copper", "Acid")
  case Gold   extends DragonAncestry("Gold", "Fire")
  case Green  extends DragonAncestry("Green", "Poison")
  case Red    extends DragonAncestry("Red", "Fire")
  case Silver extends DragonAncestry("Silver", "Cold")
  case White  extends DragonAncestry("White", "Cold")
}

sealed trait Species {
  def name: String
  def size: Size
  def speed: Int
  def darkvision: Option[Int]
  def hpBonusPerLevel: Int
  def traits: List[SpeciesTrait]
  def subLabel: Option[String]
  /** Species name including subrace when present (e.g. "Elf (High Elf)"). Use this for display; Subclass is for class archetype at level 3+. */
  def displayName: String = subLabel.fold(name)(sub => s"$name ($sub)")
  /** Languages granted by this species (everyone has Common; race adds others). */
  def languages: Set[Language]
  /** Attack grants from this species (e.g. Breath Weapon). */
  def grantedAttacks: List[AttackGrant] = Nil
}

case object Dragonborn extends Species {
  val name             = "Dragonborn"
  val size             = Size.Medium
  val speed            = 30
  val darkvision       = Some(60)
  val hpBonusPerLevel  = 0
  val traits           = List(
    SpeciesTrait("Draconic Ancestry", "Your ancestry determines damage type for breath and resistance."),
    SpeciesTrait("Breath Weapon", "Exhale destructive energy in a 15ft cone or 30ft line; save for half."),
    SpeciesTrait("Damage Resistance", "You have resistance to your ancestry's damage type."),
    SpeciesTrait("Darkvision 60ft", "You see in dim light as bright light and in darkness as dim, out to 60ft.")
  )
  val subLabel         = None
  val languages        = Set(Language.Common, Language.Draconic)
}

final case class DragonbornOf(ancestry: DragonAncestry) extends Species {
  val name             = "Dragonborn"
  val size             = Size.Medium
  val speed            = 30
  val darkvision       = Some(60)
  val hpBonusPerLevel  = 0
  val traits           = List(
    SpeciesTrait(s"Draconic Ancestry (${ancestry.label})", "Your ancestry determines damage type for breath and resistance."),
    SpeciesTrait(s"Breath Weapon (${ancestry.damageType})", "Exhale destructive energy in a 15ft cone or 30ft line; save for half."),
    SpeciesTrait(s"${ancestry.damageType} Resistance", "You have resistance to your ancestry's damage type."),
    SpeciesTrait("Darkvision 60ft", "You see in dim light as bright light and in darkness as dim, out to 60ft.")
  )
  val subLabel         = Some(ancestry.label)
  val languages        = Set(Language.Common, Language.Draconic)
  override val grantedAttacks: List[AttackGrant] = List(
    AttackGrant(
      "Breath Weapon",
      AttackKind.Spell,
      "1d10",
      ancestry.damageType,
      AttackGrantDelivery.SaveDC(Ability.Dexterity, Ability.Constitution),
      DiceScaling.Cantrip,
      true,
      "15ft/30ft",
      s"Dragonborn (${ancestry.label})"
    )
  )
}

case object Dwarf extends Species {
  val name             = "Dwarf"
  val size             = Size.Medium
  val speed            = 30
  val darkvision       = Some(120)
  val hpBonusPerLevel  = 1
  val traits           = List(
    SpeciesTrait("Darkvision 120ft", "You see in dim light as bright light and in darkness as dim, out to 120ft."),
    SpeciesTrait("Dwarven Resilience", "Advantage on saves against poison; resistance to poison damage."),
    SpeciesTrait("Dwarven Toughness (+1 HP/level)", "Your hit point maximum increases by 1 per level."),
    SpeciesTrait("Stonecunning", "Bonus to History checks related to stonework; tremorsense while touching stone.")
  )
  val subLabel         = None
  val languages        = Set(Language.Common, Language.Dwarvish)
}

final case class Elf(lineage: ElvenLineage) extends Species {
  val name             = "Elf"
  val size             = Size.Medium
  val speed            = if lineage == ElvenLineage.Wood then 35 else 30
  val darkvision       = Some(60)
  val hpBonusPerLevel  = 0
  val traits           = List(
    SpeciesTrait("Darkvision 60ft", "You see in dim light as bright light and in darkness as dim, out to 60ft."),
    SpeciesTrait(s"Elven Lineage (${lineage.label})", "Grants lineage-specific spells or abilities."),
    SpeciesTrait("Fey Ancestry", "Advantage on saves against being charmed; magic cannot put you to sleep."),
    SpeciesTrait("Keen Senses", "Proficiency in the Perception skill."),
    SpeciesTrait("Trance", "You meditate 4 hours instead of sleeping; remain semiconscious.")
  )
  val subLabel         = Some(lineage.label)
  val languages        = Set(Language.Common, Language.Elvish)
}

final case class Gnome(lineage: GnomishLineage) extends Species {
  val name             = "Gnome"
  val size             = Size.Small
  val speed            = 30
  val darkvision       = Some(60)
  val hpBonusPerLevel  = 0
  val traits           = List(
    SpeciesTrait("Darkvision 60ft", "You see in dim light as bright light and in darkness as dim, out to 60ft."),
    SpeciesTrait("Gnomish Cunning", "Advantage on Int, Wis, and Cha saves against magic."),
    SpeciesTrait(s"Gnomish Lineage (${lineage.label})", "Grants lineage-specific spells or abilities.")
  )
  val subLabel         = Some(lineage.label)
  val languages        = Set(Language.Common, Language.Gnomish)
}

final case class Goliath(ancestry: GiantAncestry) extends Species {
  val name             = "Goliath"
  val size             = Size.Medium
  val speed            = 35
  val darkvision       = None
  val hpBonusPerLevel  = 0
  val traits           = List(
    SpeciesTrait(s"Giant Ancestry (${ancestry.label})", "Grants ancestry-specific spells or abilities at higher levels."),
    SpeciesTrait("Large Form (level 5)", "At 5th level you can become Large for 10 minutes; rest to reuse."),
    SpeciesTrait("Powerful Build", "Count as one size larger for carrying capacity and push/drag/lift.")
  )
  val subLabel         = Some(ancestry.label)
  val languages        = Set(Language.Common, Language.Giant)
}

case object Halfling extends Species {
  val name             = "Halfling"
  val size             = Size.Small
  val speed            = 30
  val darkvision       = None
  val hpBonusPerLevel  = 0
  val traits           = List(
    SpeciesTrait("Brave", "Advantage on saves against being frightened."),
    SpeciesTrait("Halfling Nimbleness", "You can move through the space of any Medium or larger creature."),
    SpeciesTrait("Luck", "When you roll a 1 on a d20 for an attack, check, or save, you can reroll once per turn."),
    SpeciesTrait("Naturally Stealthy", "You can attempt to hide when obscured only by a Medium or larger creature.")
  )
  val subLabel         = None
  val languages        = Set(Language.Common, Language.Halfling)
}

case object Human extends Species {
  val name             = "Human"
  val size             = Size.Medium
  val speed            = 30
  val darkvision       = None
  val hpBonusPerLevel  = 0
  val traits           = List(
    SpeciesTrait("Resourceful", "Gain Inspiration whenever you finish a Long Rest."),
    SpeciesTrait("Skillful", "Gain one extra skill proficiency and one extra feat."),
    SpeciesTrait("Versatile", "Increase one ability score by 2 and another by 1, or three scores by 1.")
  )
  val subLabel         = None
  val languages        = Set(Language.Common)
}

case object Orc extends Species {
  val name             = "Orc"
  val size             = Size.Medium
  val speed            = 30
  val darkvision       = Some(120)
  val hpBonusPerLevel  = 0
  val traits           = List(
    SpeciesTrait("Adrenaline Rush", "Bonus action to Dash or make a weapon attack; gain temp HP; rest to reuse."),
    SpeciesTrait("Darkvision 120ft", "You see in dim light as bright light and in darkness as dim, out to 120ft."),
    SpeciesTrait("Relentless Endurance", "When reduced to 0 HP, drop to 1 instead once per long rest.")
  )
  val subLabel         = None
  val languages        = Set(Language.Common, Language.Orc)
}

final case class Tiefling(legacy: FiendishLegacy) extends Species {
  val name             = "Tiefling"
  val size             = Size.Medium
  val speed            = 30
  val darkvision       = Some(60)
  val hpBonusPerLevel  = 0
  val traits           = List(
    SpeciesTrait("Darkvision 60ft", "You see in dim light as bright light and in darkness as dim, out to 60ft."),
    SpeciesTrait(s"Fiendish Legacy (${legacy.label})", "Grants a cantrip and damage resistance; at 3rd and 5th level, additional spells from your legacy."),
    SpeciesTrait("Otherworldly Presence", "You know the Thaumaturgy cantrip. Advantage on saves against being frightened.")
  )
  val subLabel         = Some(legacy.label)
  val languages        = Set(Language.Common, Language.Infernal)
}

object Species {
  sealed trait SpeciesTemplate {
    def name: String
    def needsSubchoice: Boolean
    def defaultInstance: Species
  }

  case object DragonbornTemplate extends SpeciesTemplate {
    val name = "Dragonborn"
    val needsSubchoice = true
    val defaultInstance = DragonbornOf(DragonAncestry.Red)
  }

  case object DwarfTemplate extends SpeciesTemplate {
    val name = "Dwarf"
    val needsSubchoice = false
    val defaultInstance: Species = Dwarf
  }

  case object ElfTemplate extends SpeciesTemplate {
    val name = "Elf"
    val needsSubchoice = true
    val defaultInstance = Elf(ElvenLineage.High)
  }

  case object GnomeTemplate extends SpeciesTemplate {
    val name = "Gnome"
    val needsSubchoice = true
    val defaultInstance = Gnome(GnomishLineage.Forest)
  }

  case object GoliathTemplate extends SpeciesTemplate {
    val name = "Goliath"
    val needsSubchoice = true
    val defaultInstance = Goliath(GiantAncestry.Stone)
  }

  case object HalflingTemplate extends SpeciesTemplate {
    val name = "Halfling"
    val needsSubchoice = false
    val defaultInstance: Species = Halfling
  }

  case object HumanTemplate extends SpeciesTemplate {
    val name = "Human"
    val needsSubchoice = false
    val defaultInstance: Species = Human
  }

  case object OrcTemplate extends SpeciesTemplate {
    val name = "Orc"
    val needsSubchoice = false
    val defaultInstance: Species = Orc
  }

  case object TieflingTemplate extends SpeciesTemplate {
    val name = "Tiefling"
    val needsSubchoice = true
    val defaultInstance = Tiefling(FiendishLegacy.Infernal)
  }

  val allTemplates: List[SpeciesTemplate] = List(
    DragonbornTemplate, DwarfTemplate, ElfTemplate, GnomeTemplate,
    GoliathTemplate, HalflingTemplate, HumanTemplate, OrcTemplate, TieflingTemplate
  )
}
