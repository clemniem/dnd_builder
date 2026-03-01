package dndbuilder.dnd

final case class RuleSet(
    id: String,
    name: String,
    classes: List[DndClass]
)

object RuleSet {
  val default: RuleSet = RuleSet("srd", "Standard (SRD)", DndClass.builtIn)
}
