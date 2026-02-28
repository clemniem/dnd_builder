package dndbuilder.dnd

/** Standard D&D 5e languages. Everyone has Common; species and some classes grant others. */
enum Language(val label: String) {
  case Common    extends Language("Common")
  case Dwarvish  extends Language("Dwarvish")
  case Elvish    extends Language("Elvish")
  case Giant     extends Language("Giant")
  case Gnomish   extends Language("Gnomish")
  case Halfling  extends Language("Halfling")
  case Orc       extends Language("Orc")
  case Draconic  extends Language("Draconic")
  case Infernal extends Language("Infernal")
  case Abyssal   extends Language("Abyssal")
  case Celestial extends Language("Celestial")
  case Primordial extends Language("Primordial")
}

object Language {
  val common: Language = Common
  /** All languages that can be chosen as "extra" (e.g. from class). Excludes Common. */
  val choicePool: List[Language] =
    List(Dwarvish, Elvish, Giant, Gnomish, Halfling, Orc, Draconic, Infernal, Abyssal, Celestial, Primordial)
}
