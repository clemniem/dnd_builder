package dndbuilder.dnd

/** Accumulates choices across all creation-flow screens. Add new fields here (not new ScreenOutput variants) when extending the creation flow. */
case class CharacterDraft(
    ruleSet: RuleSet,
    species: Option[Species],
    dndClass: Option[DndClass],
    level: Option[Int],
    background: Option[Background],
    baseScores: Option[AbilityScores],
    backgroundBonus: Option[BackgroundBonus],
    chosenSkills: Set[Skill],
    equippedArmor: Option[Armor],
    equippedShield: Boolean,
    equippedWeapons: List[Weapon],
    chosenCantrips: List[Spell],
    preparedSpells: List[Spell],
    spellbookSpells: List[Spell],
    featureSelections: ClassFeatureSelections,
    subclass: Option[Subclass],
    chosenExtraLanguages: Set[Language],
    coins: Coins,
    grants: List[Grant]
) {
  def resolvedClass: DndClass = dndClass.getOrElse(ruleSet.classes.head)
  def resolvedSpecies: Species = species.getOrElse(Human)
  def resolvedBackground: Background = background.getOrElse(Background.all.head)

  def spellGrants: List[SpellGrant] = grants.collect { case g: SpellGrant => g }
  def skillGrants: List[SkillGrant] = grants.collect { case g: SkillGrant => g }
  def attackGrants: List[AttackGrant] = grants.collect { case g: AttackGrant => g }

  def withSpellGrants(sg: List[SpellGrant]): CharacterDraft =
    copy(grants = grants.filter { case _: SpellGrant => false; case _ => true } ++ sg)
  def withSkillGrants(sg: List[SkillGrant]): CharacterDraft =
    copy(grants = grants.filter { case _: SkillGrant => false; case _ => true } ++ sg)
}

object CharacterDraft {
  val empty: CharacterDraft = CharacterDraft(
    RuleSet.default,
    None, None, Some(1), None, None, None, Set.empty,
    None, false, Nil, Nil, Nil, Nil,
    ClassFeatureSelections.empty, None, Set.empty, Coins.empty,
    Nil
  )
}
