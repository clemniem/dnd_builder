package dndbuilder.dnd

/** Grant entries: each describes what can be picked (count, pool/spell list, source) and carries its resolution (chosen). */
final case class SpellGrant(
    count: Int,
    spellLevel: Int,
    spellListLabel: String,
    sourceLabel: String,
    chosen: List[Spell]
) {
  def isFilled: Boolean = chosen.size >= count
  def withChosen(newChosen: List[Spell]): SpellGrant = copy(chosen = newChosen)
}

final case class SkillGrant(
    count: Int,
    pool: Set[Skill],
    sourceLabel: String,
    chosen: Set[Skill]
) {
  def isFilled: Boolean = chosen.size >= count
  def withChosen(newChosen: Set[Skill]): SkillGrant = copy(chosen = newChosen)
}

case class Grants(
    spellGrants: List[SpellGrant],
    skillGrants: List[SkillGrant],
    attackGrants: List[AttackGrant]
) {
  def ++(other: Grants): Grants = Grants(
    spellGrants ++ other.spellGrants,
    skillGrants ++ other.skillGrants,
    attackGrants ++ other.attackGrants
  )
}
object Grants {
  val empty: Grants = Grants(Nil, Nil, Nil)
}

/** Unified feature-grant collection: build draft grants from class, species, and origin-feat grants. */
object FeatureGrants {

  /** Origin feat effects as FeatureGrant (replaces reading grantsInitiativeBonus / skillGrant / spellGrantSpecs). */
  def grantsFromOriginFeat(feat: OriginFeat): List[FeatureGrant] = feat match {
    case Alert => List(FeatureGrant.InitiativeBonus())
    case Skilled => List(FeatureGrant.SkillChoice(3, Skill.values.toSet))
    case mi: MagicInitiate =>
      mi.spellGrantSpecs.map { case (count, level, list) => FeatureGrant.SpellChoice(count, level, list.label) }
    case _ => Nil
  }

  def grantsFromSpecies(sp: Species): List[FeatureGrant] =
    sp.grantedAttacks.map(FeatureGrant.Attack.apply)

  def grantsFromClass(cls: DndClass, level: Int): List[FeatureGrant] =
    ClassProgression.featuresUpToLevel(cls, level).flatMap(_.grants)

  private def buildGrantsFromFeatureGrants(grants: List[FeatureGrant], sourceLabel: String): Grants = Grants(
    spellGrants = grants.collect { case FeatureGrant.SpellChoice(c, l, list) => SpellGrant(c, l, list, sourceLabel, Nil) },
    skillGrants = grants.collect { case FeatureGrant.SkillChoice(c, pool) => SkillGrant(c, pool, sourceLabel, Set.empty) },
    attackGrants = grants.collect { case FeatureGrant.Attack(a) => a }
  )

  /** Grants from species only (e.g. for PDF test character). */
  def grantsForSpecies(sp: Species): Grants =
    buildGrantsFromFeatureGrants(grantsFromSpecies(sp), sp.name)

  /** Single entry point: all spell/skill/attack grants for the draft from class + species + background feat. */
  def allGrantsForDraft(draft: CharacterDraft): Grants = {
    val level = draft.level.getOrElse(1)
    val classG = buildGrantsFromFeatureGrants(grantsFromClass(draft.resolvedClass, level), draft.resolvedClass.name)
    val speciesG = buildGrantsFromFeatureGrants(grantsFromSpecies(draft.resolvedSpecies), draft.resolvedSpecies.name)
    val bgG = draft.background match {
      case Some(bg) => buildGrantsFromFeatureGrants(grantsFromOriginFeat(bg.feat), s"${bg.name}: ${bg.feat.name}")
      case None => Grants.empty
    }
    classG ++ speciesG ++ bgG
  }

  /** Spell grants from Blessed Warrior (2 Cleric cantrips) or Druidic Warrior (2 Druid cantrips). */
  def spellGrantsForFightingStyle(fs: Option[FightingStyle]): List[SpellGrant] = fs match {
    case Some(FightingStyle.BlessedWarrior) => List(SpellGrant(2, 0, "Cleric", "Blessed Warrior", Nil))
    case Some(FightingStyle.DruidicWarrior) => List(SpellGrant(2, 0, "Druid", "Druidic Warrior", Nil))
    case _ => Nil
  }

  /** Whether the creation flow should show the Spells screen (class spells, background feat spells, or fighting-style cantrips). */
  def needsSpellScreen(cls: DndClass, bg: Option[Background]): Boolean =
    cls.isSpellcaster || bg.exists(b => grantsFromOriginFeat(b.feat).exists { case _: FeatureGrant.SpellChoice => true; case _ => false })

  /** Same as above but also true when draft has Blessed Warrior or Druidic Warrior (extra cantrips chosen on Spells step). */
  def needsSpellScreen(draft: CharacterDraft): Boolean =
    needsSpellScreen(draft.resolvedClass, draft.background) ||
      draft.featureSelections.fightingStyle.exists(fs =>
        fs == FightingStyle.BlessedWarrior || fs == FightingStyle.DruidicWarrior
      )
}
