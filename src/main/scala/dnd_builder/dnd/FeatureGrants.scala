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

object FeatureGrants {

  def fromBackground(bg: Background): (List[SpellGrant], List[SkillGrant]) =
    bg.feat match {
      case MagicInitiate(sl) =>
        val sourceLabel = s"${bg.name}: ${bg.feat.name}"
        val spellGrants = List(
          SpellGrant(2, 0, sl.label, sourceLabel, Nil),
          SpellGrant(1, 1, sl.label, sourceLabel, Nil)
        )
        (spellGrants, Nil)
      case Skilled =>
        val skillGrants = List(
          SkillGrant(3, Skill.values.toSet, s"${bg.name}: Skilled", Set.empty)
        )
        (Nil, skillGrants)
      case _ =>
        (Nil, Nil)
    }

  def fromSpecies(sp: Species): (List[SpellGrant], List[SkillGrant]) = {
    val _ = sp
    (Nil, Nil)
  }

  def needsSpellScreen(cls: DndClass, bg: Option[Background]): Boolean =
    cls.isSpellcaster || bg.exists(b => fromBackground(b)._1.nonEmpty)
}
