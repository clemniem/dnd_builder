package dndbuilder.dnd

sealed trait Background {
  def name: String
  def abilityOptions: (Ability, Ability, Ability)
  def skillProficiencies: (Skill, Skill)
  def toolProficiency: String
  def feat: OriginFeat
  def startingGold: Int
  def description: String

  def abilityOptionsList: List[Ability] = {
    val (a, b, c) = abilityOptions
    List(a, b, c)
  }

  def skillProficiencySet: Set[Skill] = {
    val (s1, s2) = skillProficiencies
    Set(s1, s2)
  }
}

case object Acolyte extends Background {
  val name               = "Acolyte"
  val abilityOptions     = (Ability.Intelligence, Ability.Wisdom, Ability.Charisma)
  val skillProficiencies = (Skill.Insight, Skill.Religion)
  val toolProficiency    = "Calligrapher's Supplies"
  val feat               = MagicInitiate(SpellList.Cleric)
  val startingGold       = 8
  val description        = "You devoted yourself to service in a temple."
}

case object Criminal extends Background {
  val name               = "Criminal"
  val abilityOptions     = (Ability.Dexterity, Ability.Constitution, Ability.Intelligence)
  val skillProficiencies = (Skill.SleightOfHand, Skill.Stealth)
  val toolProficiency    = "Thieves' Tools"
  val feat               = Alert
  val startingGold       = 15
  val description        = "You learned to bend the rules and break the law."
}

case object Sage extends Background {
  val name               = "Sage"
  val abilityOptions     = (Ability.Constitution, Ability.Intelligence, Ability.Wisdom)
  val skillProficiencies = (Skill.Arcana, Skill.History)
  val toolProficiency    = "Calligrapher's Supplies"
  val feat               = MagicInitiate(SpellList.Wizard)
  val startingGold       = 8
  val description        = "You spent years learning the lore of the multiverse."
}

case object Soldier extends Background {
  val name               = "Soldier"
  val abilityOptions     = (Ability.Strength, Ability.Dexterity, Ability.Constitution)
  val skillProficiencies = (Skill.Athletics, Skill.Intimidation)
  val toolProficiency    = "Gaming Set"
  val feat               = SavageAttacker
  val startingGold       = 14
  val description        = "You began training for war as soon as you could hold a weapon."
}

object Background {
  val all: List[Background] = List(Acolyte, Criminal, Sage, Soldier)
}
