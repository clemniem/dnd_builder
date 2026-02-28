package dndbuilder.dnd

enum Ability(val label: String, val abbreviation: String) {
  case Strength     extends Ability("Strength", "STR")
  case Dexterity    extends Ability("Dexterity", "DEX")
  case Constitution extends Ability("Constitution", "CON")
  case Intelligence extends Ability("Intelligence", "INT")
  case Wisdom       extends Ability("Wisdom", "WIS")
  case Charisma     extends Ability("Charisma", "CHA")
}

enum Skill(val ability: Ability, val label: String) {
  case Athletics     extends Skill(Ability.Strength, "Athletics")
  case Acrobatics    extends Skill(Ability.Dexterity, "Acrobatics")
  case SleightOfHand extends Skill(Ability.Dexterity, "Sleight of Hand")
  case Stealth       extends Skill(Ability.Dexterity, "Stealth")
  case Arcana        extends Skill(Ability.Intelligence, "Arcana")
  case History       extends Skill(Ability.Intelligence, "History")
  case Investigation extends Skill(Ability.Intelligence, "Investigation")
  case Nature        extends Skill(Ability.Intelligence, "Nature")
  case Religion      extends Skill(Ability.Intelligence, "Religion")
  case AnimalHandling extends Skill(Ability.Wisdom, "Animal Handling")
  case Insight       extends Skill(Ability.Wisdom, "Insight")
  case Medicine      extends Skill(Ability.Wisdom, "Medicine")
  case Perception    extends Skill(Ability.Wisdom, "Perception")
  case Survival      extends Skill(Ability.Wisdom, "Survival")
  case Deception     extends Skill(Ability.Charisma, "Deception")
  case Intimidation  extends Skill(Ability.Charisma, "Intimidation")
  case Performance   extends Skill(Ability.Charisma, "Performance")
  case Persuasion    extends Skill(Ability.Charisma, "Persuasion")
}

object Skill {
  val byAbility: Map[Ability, List[Skill]] =
    Skill.values.toList.groupBy(_.ability)
}

enum HitDie(val sides: Int, val avgGain: Int) {
  case D6  extends HitDie(6, 4)
  case D8  extends HitDie(8, 5)
  case D10 extends HitDie(10, 6)
  case D12 extends HitDie(12, 7)
}

enum Size(val label: String) {
  case Small  extends Size("Small")
  case Medium extends Size("Medium")
}

enum ArmorType(val label: String) {
  case Light  extends ArmorType("Light Armor")
  case Medium extends ArmorType("Medium Armor")
  case Heavy  extends ArmorType("Heavy Armor")
  case Shield extends ArmorType("Shield")
}

enum SpellList(val label: String) {
  case Cleric extends SpellList("Cleric")
  case Druid  extends SpellList("Druid")
  case Wizard extends SpellList("Wizard")
}

enum ProficiencyLevel {
  case None, Proficient, Expertise
}
