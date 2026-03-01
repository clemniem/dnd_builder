package dndbuilder.dnd

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
  def traits: List[String]
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
  val traits           = List("Draconic Ancestry", "Breath Weapon", "Damage Resistance", "Darkvision 60ft")
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
    s"Draconic Ancestry (${ancestry.label})",
    s"Breath Weapon (${ancestry.damageType})",
    s"${ancestry.damageType} Resistance",
    "Darkvision 60ft"
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
      true,
      false,
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
  val traits           = List("Darkvision 120ft", "Dwarven Resilience", "Dwarven Toughness (+1 HP/level)", "Stonecunning")
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
    "Darkvision 60ft",
    s"Elven Lineage (${lineage.label})",
    "Fey Ancestry",
    "Keen Senses",
    "Trance"
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
    "Darkvision 60ft",
    "Gnomish Cunning",
    s"Gnomish Lineage (${lineage.label})"
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
    s"Giant Ancestry (${ancestry.label})",
    "Large Form (level 5)",
    "Powerful Build"
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
  val traits           = List("Brave", "Halfling Nimbleness", "Luck", "Naturally Stealthy")
  val subLabel         = None
  val languages        = Set(Language.Common, Language.Halfling)
}

case object Human extends Species {
  val name             = "Human"
  val size             = Size.Medium
  val speed            = 30
  val darkvision       = None
  val hpBonusPerLevel  = 0
  val traits           = List("Resourceful", "Skillful", "Versatile")
  val subLabel         = None
  val languages        = Set(Language.Common)
}

case object Orc extends Species {
  val name             = "Orc"
  val size             = Size.Medium
  val speed            = 30
  val darkvision       = Some(120)
  val hpBonusPerLevel  = 0
  val traits           = List("Adrenaline Rush", "Darkvision 120ft", "Relentless Endurance")
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
    "Darkvision 60ft",
    s"Fiendish Legacy (${legacy.label})",
    "Otherworldly Presence"
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
