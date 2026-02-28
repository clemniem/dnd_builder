package dndbuilder.dnd

enum FightingStyle(val label: String, val description: String):
  case Archery             extends FightingStyle("Archery", "+2 ranged attack rolls")
  case BlindFighting       extends FightingStyle("Blind Fighting", "10ft blindsight")
  case Defense             extends FightingStyle("Defense", "+1 AC while wearing armor")
  case Dueling             extends FightingStyle("Dueling", "+2 damage with one-handed melee, no other weapon")
  case GreatWeaponFighting extends FightingStyle("Great Weapon Fighting", "treat 1s and 2s as 3 on two-handed damage dice")
  case Interception        extends FightingStyle("Interception", "reaction: reduce ally damage by 1d10 + prof bonus")
  case Protection          extends FightingStyle("Protection", "reaction + shield: impose disadvantage on attacks vs nearby ally")
  case ThrownWeaponFighting extends FightingStyle("Thrown Weapon Fighting", "+2 damage on ranged attacks with thrown weapons")
  case TwoWeaponFighting   extends FightingStyle("Two-Weapon Fighting", "add ability mod to offhand attack damage")
  case UnarmedFighting     extends FightingStyle("Unarmed Fighting", "1d6/1d8 unarmed strikes, 1d4 to grappled")
  case BlessedWarrior      extends FightingStyle("Blessed Warrior", "2 Cleric cantrips (Paladin only)")
  case DruidicWarrior      extends FightingStyle("Druidic Warrior", "2 Druid cantrips (Ranger only)")

enum DivineOrder(val label: String, val description: String):
  case Protector   extends DivineOrder("Protector", "Heavy armor, martial weapons")
  case Thaumaturge extends DivineOrder("Thaumaturge", "Extra cantrip, spellcasting focus")

enum PrimalOrder(val label: String, val description: String):
  case Magician extends PrimalOrder("Magician", "Druidcraft, extra cantrip")
  case Warden   extends PrimalOrder("Warden", "Medium armor, shield, martial weapons")

enum EldritchInvocation(val label: String, val description: String):
  case ArmorOfShadows   extends EldritchInvocation("Armor of Shadows", "Mage Armor at will")
  case EldritchMind     extends EldritchInvocation("Eldritch Mind", "Advantage on concentration")
  case MaskOfManyFaces  extends EldritchInvocation("Mask of Many Faces", "Disguise Self at will")
  case PactOfTheBlade   extends EldritchInvocation("Pact of the Blade", "Conjure a pact weapon")
  case PactOfTheChain   extends EldritchInvocation("Pact of the Chain", "Find Familiar with special forms")
  case PactOfTheTome   extends EldritchInvocation("Pact of the Tome", "Book of Shadows with 3 cantrips")

final case class ClassFeatureSelections(
    fightingStyle: Option[FightingStyle],
    divineOrder: Option[DivineOrder],
    primalOrder: Option[PrimalOrder],
    eldritchInvocation: Option[EldritchInvocation],
    expertiseSkills: Set[Skill],
    weaponMasteries: List[Weapon]
)

object ClassFeatureSelections:
  val empty: ClassFeatureSelections =
    ClassFeatureSelections(None, None, None, None, Set.empty, Nil)
