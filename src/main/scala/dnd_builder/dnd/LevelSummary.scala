package dndbuilder.dnd

/** Aggregate of everything that changes at a given class level (features, choices, spells, subclass). */
final case class LevelSummary(
    features: List[ClassFeature],
    choices: List[LevelChoice],
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
      features = gain.features ++ subclassFeatures,
      choices = gain.choices,
      spellProgression = SpellProgression.forClass(cls, level),
      subclassEligible = sub
    )
  }
}
