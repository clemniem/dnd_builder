package dndbuilder.dnd

/** Aggregate of everything that changes at a given class level (features, choice grants, spells, subclass). */
final case class LevelSummary(
    features: List[Feature],
    grantChoices: List[FeatureGrant],
    spellProgression: Option[SpellSlotRow],
    subclassEligible: Option[Subclass]
)

object LevelSummary {
  /** One-stop lookup for what a character gains at a given class level. */
  def forClassAtLevel(cls: DndClass, level: Int): LevelSummary = {
    val gain = ClassProgression.atLevel(cls, level)
    val sub = if level >= 3 then Subclass.forClass(cls) else None
    val subclassFeatures = sub.toList.flatMap(_.features.getOrElse(level, Nil))
    LevelSummary(
      features = ClassProgression.featuresFromGain(gain) ++ subclassFeatures,
      grantChoices = ClassProgression.grantChoicesAtLevel(gain),
      spellProgression = SpellProgression.forClass(cls, level),
      subclassEligible = sub
    )
  }
}
