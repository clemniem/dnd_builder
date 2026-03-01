package dndbuilder.dnd

/** Accumulates choices across all creation-flow screens. Add new fields here (not new ScreenOutput variants) when extending the creation flow. */
case class CharacterDraft(
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
    spellGrants: List[SpellGrant],
    skillGrants: List[SkillGrant]
)

object CharacterDraft {
  val empty: CharacterDraft = CharacterDraft(
    None, None, Some(1), None, None, None, Set.empty,
    None, false, Nil, Nil, Nil, Nil,
    ClassFeatureSelections.empty, None, Set.empty, Coins.empty,
    Nil, Nil
  )
}
