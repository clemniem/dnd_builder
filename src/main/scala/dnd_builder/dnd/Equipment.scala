package dndbuilder.dnd

enum WeaponCategory {
  case Simple, Martial
}

enum WeaponRange {
  case Melee, Ranged
}

enum DamageType(val label: String, val pdfAbbrev: String) {
  case Slashing    extends DamageType("slashing", "slash.")
  case Piercing    extends DamageType("piercing", "pierc.")
  case Bludgeoning extends DamageType("bludgeoning", "bludg.")
}

enum WeaponProperty {
  case Ammunition, Finesse, Heavy, Light, Loading, Reach, Thrown, TwoHanded, Versatile
}

enum MasteryProperty {
  case Cleave, Graze, Nick, Push, Sap, Slow, Topple, Vex
}

final case class Weapon(
    name: String,
    category: WeaponCategory,
    range: WeaponRange,
    damageDice: String,
    damageType: DamageType,
    properties: Set[WeaponProperty],
    mastery: MasteryProperty,
    stars: Int
) {
  def damage: String = s"$damageDice ${damageType.label}"
}

object Weapon {
  import WeaponCategory.*
  import WeaponRange.*
  import WeaponProperty.*
  import MasteryProperty.*

  import DamageType.*

  // Star baseline: d4=1, d6=1 (no effect) / 2 (with effect), d8=2 / 3 (+effect), d10=3, d12 or 2d6=4
  val all: List[Weapon] = List(
    // Simple Melee
    Weapon("Club", Simple, Melee, "1d4", Bludgeoning, Set(Light), Slow, 1),
    Weapon("Dagger", Simple, Melee, "1d4", Piercing, Set(Finesse, Light, Thrown), Nick, 1),
    Weapon("Greatclub", Simple, Melee, "1d8", Bludgeoning, Set(TwoHanded), Push, 2),
    Weapon("Handaxe", Simple, Melee, "1d6", Slashing, Set(Light, Thrown), Vex, 2),
    Weapon("Javelin", Simple, Melee, "1d6", Piercing, Set(Thrown), Slow, 2),
    Weapon("Light Hammer", Simple, Melee, "1d4", Bludgeoning, Set(Light, Thrown), Nick, 1),
    Weapon("Mace", Simple, Melee, "1d6", Bludgeoning, Set.empty, Sap, 1),
    Weapon("Quarterstaff", Simple, Melee, "1d6", Bludgeoning, Set(Versatile), Topple, 2),
    Weapon("Sickle", Simple, Melee, "1d4", Slashing, Set(Light), Nick, 1),
    Weapon("Spear", Simple, Melee, "1d6", Piercing, Set(Thrown, Versatile), Sap, 2),
    // Simple Ranged
    Weapon("Dart", Simple, Ranged, "1d4", Piercing, Set(Finesse, Thrown), Vex, 1),
    Weapon("Light Crossbow", Simple, Ranged, "1d8", Piercing, Set(Ammunition, Loading, TwoHanded), Slow, 2),
    Weapon("Shortbow", Simple, Ranged, "1d6", Piercing, Set(Ammunition, TwoHanded), Vex, 2),
    Weapon("Sling", Simple, Ranged, "1d4", Bludgeoning, Set(Ammunition), Slow, 1),
    // Martial Melee
    Weapon("Battleaxe", Martial, Melee, "1d8", Slashing, Set(Versatile), Topple, 3),
    Weapon("Flail", Martial, Melee, "1d8", Bludgeoning, Set.empty, Sap, 2),
    Weapon("Glaive", Martial, Melee, "1d10", Slashing, Set(Heavy, Reach, TwoHanded), Graze, 3),
    Weapon("Greataxe", Martial, Melee, "1d12", Slashing, Set(Heavy, TwoHanded), Cleave, 4),
    Weapon("Greatsword", Martial, Melee, "2d6", Slashing, Set(Heavy, TwoHanded), Graze, 4),
    Weapon("Halberd", Martial, Melee, "1d10", Slashing, Set(Heavy, Reach, TwoHanded), Cleave, 3),
    Weapon("Lance", Martial, Melee, "1d10", Piercing, Set(Heavy, Reach, TwoHanded), Topple, 3),
    Weapon("Longsword", Martial, Melee, "1d8", Slashing, Set(Versatile), Sap, 3),
    Weapon("Maul", Martial, Melee, "2d6", Bludgeoning, Set(Heavy, TwoHanded), Topple, 4),
    Weapon("Morningstar", Martial, Melee, "1d8", Piercing, Set.empty, Sap, 2),
    Weapon("Pike", Martial, Melee, "1d10", Piercing, Set(Heavy, Reach, TwoHanded), Push, 3),
    Weapon("Rapier", Martial, Melee, "1d8", Piercing, Set(Finesse), Vex, 3),
    Weapon("Scimitar", Martial, Melee, "1d6", Slashing, Set(Finesse, Light), Nick, 2),
    Weapon("Shortsword", Martial, Melee, "1d6", Piercing, Set(Finesse, Light), Vex, 2),
    Weapon("Trident", Martial, Melee, "1d8", Piercing, Set(Thrown, Versatile), Topple, 3),
    Weapon("Warhammer", Martial, Melee, "1d8", Bludgeoning, Set(Versatile), Push, 3),
    Weapon("War Pick", Martial, Melee, "1d8", Piercing, Set(Versatile), Sap, 3),
    Weapon("Whip", Martial, Melee, "1d4", Slashing, Set(Finesse, Reach), Slow, 1),
    // Martial Ranged
    Weapon("Blowgun", Martial, Ranged, "1", Piercing, Set(Ammunition, Loading), Vex, 1),
    Weapon("Hand Crossbow", Martial, Ranged, "1d6", Piercing, Set(Ammunition, Light, Loading), Vex, 2),
    Weapon("Heavy Crossbow", Martial, Ranged, "1d10", Piercing, Set(Ammunition, Heavy, Loading, TwoHanded), Push, 3),
    Weapon("Longbow", Martial, Ranged, "1d8", Piercing, Set(Ammunition, Heavy, TwoHanded), Slow, 3),
    Weapon("Musket", Martial, Ranged, "1d12", Piercing, Set(Ammunition, Loading, TwoHanded), Slow, 4),
    Weapon("Pistol", Martial, Ranged, "1d10", Piercing, Set(Ammunition, Loading), Vex, 3)
  ).sortBy(_.stars)

  def byName(name: String): Option[Weapon] =
    all.find(_.name.equalsIgnoreCase(name))
}

sealed trait WeaponProficiency
object WeaponProficiency {
  case object AllSimple  extends WeaponProficiency
  case object AllMartial extends WeaponProficiency
  final case class MartialIf(requiredProperties: Set[WeaponProperty]) extends WeaponProficiency

  def isProficient(weapon: Weapon, profs: Set[WeaponProficiency]): Boolean = {
    import WeaponCategory.*
    weapon.category match {
      case Simple =>
        profs.contains(AllSimple)
      case Martial =>
        profs.contains(AllMartial) || profs.collect { case MartialIf(props) => props }.exists { required =>
          required.exists(weapon.properties.contains)
        }
    }
  }
} // end WeaponProficiency

final case class Armor(
    name: String,
    armorType: ArmorType,
    baseAC: Int,
    addDex: Boolean,
    maxDexBonus: Option[Int],
    strengthReq: Option[Int],
    stealthDisadvantage: Boolean,
    stars: Int
)

object Armor {
  import ArmorType.*
  val all: List[Armor] = List(
    Armor("Padded Armor", Light, 11, addDex = true, None, None, stealthDisadvantage = true, 1),
    Armor("Leather Armor", Light, 11, addDex = true, None, None, stealthDisadvantage = false, 1),
    Armor("Studded Leather Armor", Light, 12, addDex = true, None, None, stealthDisadvantage = false, 2),
    Armor("Hide Armor", Medium, 12, addDex = true, Some(2), None, stealthDisadvantage = false, 1),
    Armor("Chain Shirt", Medium, 13, addDex = true, Some(2), None, stealthDisadvantage = false, 2),
    Armor("Scale Mail", Medium, 14, addDex = true, Some(2), None, stealthDisadvantage = true, 2),
    Armor("Breastplate", Medium, 14, addDex = true, Some(2), None, stealthDisadvantage = false, 4),
    Armor("Half Plate Armor", Medium, 15, addDex = true, Some(2), None, stealthDisadvantage = true, 5),
    Armor("Ring Mail", Heavy, 14, addDex = false, None, None, stealthDisadvantage = true, 1),
    Armor("Chain Mail", Heavy, 16, addDex = false, None, Some(13), stealthDisadvantage = true, 2),
    Armor("Splint Armor", Heavy, 17, addDex = false, None, Some(15), stealthDisadvantage = true, 3),
    Armor("Plate Armor", Heavy, 18, addDex = false, None, Some(15), stealthDisadvantage = true, 5)
  ).sortBy(_.stars)

  def byName(name: String): Option[Armor] =
    all.find(_.name.equalsIgnoreCase(name))

  def shieldACBonus: Int = 2
  def shieldStars: Int = 1
}
