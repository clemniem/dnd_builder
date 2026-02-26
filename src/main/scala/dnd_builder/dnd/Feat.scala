package dndbuilder.dnd

sealed trait Feat:
  def name: String
  def description: String
  def category: String

sealed trait OriginFeat extends Feat:
  val category = "Origin"

case object Alert extends OriginFeat:
  val name        = "Alert"
  val description = "Add Proficiency Bonus to Initiative. You can swap Initiative with a willing ally within 5ft."

final case class MagicInitiate(spellList: SpellList) extends OriginFeat:
  val name        = s"Magic Initiate (${spellList.label})"
  val description = s"2 cantrips + 1 level-1 spell from the ${spellList.label} spell list. WIS/INT/CHA as spellcasting ability."

case object SavageAttacker extends OriginFeat:
  val name        = "Savage Attacker"
  val description = "Once per turn, roll weapon damage dice twice and use either result."

case object Skilled extends OriginFeat:
  val name        = "Skilled"
  val description = "Gain proficiency in 3 skills or tools of your choice."

sealed trait GeneralFeat extends Feat:
  val category = "General"
  def minLevel: Int
  def meetsPrerequisites(scores: AbilityScores, level: Int): Boolean

case object AbilityScoreImprovement extends GeneralFeat:
  val name        = "Ability Score Improvement"
  val description = "+2 to one ability score or +1 to two ability scores. No score can exceed 20."
  val minLevel    = 4
  def meetsPrerequisites(scores: AbilityScores, level: Int): Boolean =
    level >= minLevel

final case class Grappler(abilityIncrease: Ability) extends GeneralFeat:
  val name        = "Grappler"
  val description = "+1 STR or DEX. Punch and grapple simultaneously. Advantage on attacks vs grappled creatures."
  val minLevel    = 4
  def meetsPrerequisites(scores: AbilityScores, level: Int): Boolean =
    level >= minLevel &&
    (scores.get(Ability.Strength) >= 13 || scores.get(Ability.Dexterity) >= 13)

sealed trait FightingStyleFeat extends Feat:
  val category = "Fighting Style"

case object Archery extends FightingStyleFeat:
  val name        = "Archery"
  val description = "+2 bonus to attack rolls with Ranged weapons."

case object Defense extends FightingStyleFeat:
  val name        = "Defense"
  val description = "+1 AC while wearing armor."

case object GreatWeaponFighting extends FightingStyleFeat:
  val name        = "Great Weapon Fighting"
  val description = "When rolling 1 or 2 on damage dice with two-handed/versatile weapons, treat as 3."

case object TwoWeaponFighting extends FightingStyleFeat:
  val name        = "Two-Weapon Fighting"
  val description = "Add your ability modifier to the damage of the second attack with Two-Weapon Fighting."
