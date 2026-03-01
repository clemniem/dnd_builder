package dndbuilder.dnd

enum FightingStyle(val label: String, val description: String) {
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
}

enum DivineOrder(val label: String, val description: String) {
  case Protector   extends DivineOrder("Protector", "Heavy armor, martial weapons")
  case Thaumaturge extends DivineOrder("Thaumaturge", "Extra cantrip, spellcasting focus")
}

enum PrimalOrder(val label: String, val description: String) {
  case Magician extends PrimalOrder("Magician", "Druidcraft, extra cantrip")
  case Warden   extends PrimalOrder("Warden", "Medium armor, shield, martial weapons")
}

enum EldritchInvocation(val label: String, val description: String) {
  case ArmorOfShadows   extends EldritchInvocation("Armor of Shadows", "Mage Armor at will")
  case EldritchMind     extends EldritchInvocation("Eldritch Mind", "Advantage on concentration")
  case MaskOfManyFaces  extends EldritchInvocation("Mask of Many Faces", "Disguise Self at will")
  case PactOfTheBlade   extends EldritchInvocation("Pact of the Blade", "Conjure a pact weapon")
  case PactOfTheChain   extends EldritchInvocation("Pact of the Chain", "Find Familiar with special forms")
  case PactOfTheTome   extends EldritchInvocation("Pact of the Tome", "Book of Shadows with 3 cantrips")
}

enum LandType(val label: String) {
  case Arid      extends LandType("Arid")
  case Polar     extends LandType("Polar")
  case Temperate extends LandType("Temperate")
  case Tropical  extends LandType("Tropical")
}

enum HunterPreyChoice(val label: String, val description: String) {
  case ColossusSlayer
      extends HunterPreyChoice(
        "Colossus Slayer",
        "Extra 1d8 damage when target is missing HP (once per turn)"
      )
  case HordeBreaker
      extends HunterPreyChoice(
        "Horde Breaker",
        "Extra attack against different creature within 5 ft"
      )
}

/** Extensible sum type: add new variants without changing ClassFeatureSelections shape. */
sealed trait FeatureSelection
object FeatureSelection {
  case class FightingStyleChoice(value: FightingStyle)       extends FeatureSelection
  case class DivineOrderChoice(value: DivineOrder)             extends FeatureSelection
  case class PrimalOrderChoice(value: PrimalOrder)             extends FeatureSelection
  case class EldritchInvocationChoice(value: EldritchInvocation) extends FeatureSelection
  case class ExpertiseChoice(skills: Set[Skill])               extends FeatureSelection
  case class WeaponMasteryChoice(weapons: List[Weapon])       extends FeatureSelection
  case class LandTypeChoice(value: LandType)                  extends FeatureSelection
  case class HunterPreyChoiceSelection(value: HunterPreyChoice) extends FeatureSelection
}

/** Stores class feature choices as a list; new choice types add a FeatureSelection variant only. */
final case class ClassFeatureSelections(selections: List[FeatureSelection]) {

  def fightingStyle: Option[FightingStyle] =
    selections.collectFirst { case FeatureSelection.FightingStyleChoice(s) => s }

  def divineOrder: Option[DivineOrder] =
    selections.collectFirst { case FeatureSelection.DivineOrderChoice(o) => o }

  def primalOrder: Option[PrimalOrder] =
    selections.collectFirst { case FeatureSelection.PrimalOrderChoice(o) => o }

  def eldritchInvocation: Option[EldritchInvocation] =
    selections.collectFirst { case FeatureSelection.EldritchInvocationChoice(i) => i }

  def expertiseSkills: Set[Skill] =
    selections.collect { case FeatureSelection.ExpertiseChoice(skills) => skills }.flatten.toSet

  def weaponMasteries: List[Weapon] =
    selections.collect { case FeatureSelection.WeaponMasteryChoice(weapons) => weapons }.flatten

  def landType: Option[LandType] =
    selections.collectFirst { case FeatureSelection.LandTypeChoice(lt) => lt }

  def hunterPrey: Option[HunterPreyChoice] =
    selections.collectFirst { case FeatureSelection.HunterPreyChoiceSelection(hp) => hp }

  /** Update choices; pass all parameters (use this.foo for unchanged). */
  def withChoices(
      fightingStyle: Option[FightingStyle],
      divineOrder: Option[DivineOrder],
      primalOrder: Option[PrimalOrder],
      eldritchInvocation: Option[EldritchInvocation],
      expertiseSkills: Set[Skill],
      weaponMasteries: List[Weapon],
      landType: Option[LandType],
      hunterPrey: Option[HunterPreyChoice]
  ): ClassFeatureSelections = {
    val without =
      selections.filterNot {
        case FeatureSelection.FightingStyleChoice(_)       => true
        case FeatureSelection.DivineOrderChoice(_)         => true
        case FeatureSelection.PrimalOrderChoice(_)        => true
        case FeatureSelection.EldritchInvocationChoice(_) => true
        case FeatureSelection.ExpertiseChoice(_)          => true
        case FeatureSelection.WeaponMasteryChoice(_)      => true
        case FeatureSelection.LandTypeChoice(_)            => true
        case FeatureSelection.HunterPreyChoiceSelection(_) => true
      }
    val added = List(
      fightingStyle.map(FeatureSelection.FightingStyleChoice.apply),
      divineOrder.map(FeatureSelection.DivineOrderChoice.apply),
      primalOrder.map(FeatureSelection.PrimalOrderChoice.apply),
      eldritchInvocation.map(FeatureSelection.EldritchInvocationChoice.apply),
      Some(FeatureSelection.ExpertiseChoice(expertiseSkills)),
      Some(FeatureSelection.WeaponMasteryChoice(weaponMasteries)),
      landType.map(FeatureSelection.LandTypeChoice.apply),
      hunterPrey.map(FeatureSelection.HunterPreyChoiceSelection.apply)
    ).flatten
    ClassFeatureSelections(without ++ added)
  }
}

object ClassFeatureSelections {
  val empty: ClassFeatureSelections = ClassFeatureSelections(Nil)
}
