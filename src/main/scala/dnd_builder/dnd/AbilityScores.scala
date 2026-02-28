package dndbuilder.dnd

final case class AbilityScores(
    strength: Int,
    dexterity: Int,
    constitution: Int,
    intelligence: Int,
    wisdom: Int,
    charisma: Int) {

  def get(ability: Ability): Int = ability match {
    case Ability.Strength     => strength
    case Ability.Dexterity    => dexterity
    case Ability.Constitution => constitution
    case Ability.Intelligence => intelligence
    case Ability.Wisdom       => wisdom
    case Ability.Charisma     => charisma
  }

  def set(ability: Ability, value: Int): AbilityScores = ability match {
    case Ability.Strength     => copy(strength = value)
    case Ability.Dexterity    => copy(dexterity = value)
    case Ability.Constitution => copy(constitution = value)
    case Ability.Intelligence => copy(intelligence = value)
    case Ability.Wisdom       => copy(wisdom = value)
    case Ability.Charisma     => copy(charisma = value)
  }

  def adjust(ability: Ability, delta: Int): AbilityScores =
    set(ability, get(ability) + delta)

  def modifier(ability: Ability): Int =
    AbilityScores.modifier(get(ability))

  def toList: List[(Ability, Int)] =
    Ability.values.toList.map(a => (a, get(a)))

  def totalPointBuyCost: Either[String, Int] = {
    val costs = toList.map { case (a, v) =>
      AbilityScores.pointBuyCost.get(v) match {
        case Some(c) => Right(c)
        case None    => Left(s"${a.label} score $v is outside point-buy range (8-15)")
      }
    }
    val errors = costs.collect { case Left(e) => e }
    if errors.nonEmpty then Left(errors.mkString("; "))
    else Right(costs.collect { case Right(c) => c }.sum)
  }
}

object AbilityScores {
  val default: AbilityScores = AbilityScores(8, 8, 8, 8, 8, 8)

  val standardArray: List[Int] = List(15, 14, 13, 12, 10, 8)

  val pointBuyTotal: Int = 27

  val pointBuyCost: Map[Int, Int] = Map(
    8 -> 0, 9 -> 1, 10 -> 2, 11 -> 3,
    12 -> 4, 13 -> 5, 14 -> 7, 15 -> 9
  )

  def modifier(score: Int): Int =
    Math.floorDiv(score - 10, 2)

  def modifierString(score: Int): String = {
    val m = modifier(score)
    if m >= 0 then s"+$m" else s"$m"
  }

  def applyBonus(base: AbilityScores, bonus: BackgroundBonus): AbilityScores =
    bonus.increases.foldLeft(base) { case (scores, (ability, amount)) =>
      scores.adjust(ability, amount)
    }
}

sealed trait BackgroundBonus {
  def increases: List[(Ability, Int)]
  def totalPoints: Int = increases.map(_._2).sum
}

object BackgroundBonus {
  final case class TwoPlusOne(plus2: Ability, plus1: Ability) extends BackgroundBonus {
    def increases: List[(Ability, Int)] = List((plus2, 2), (plus1, 1))
  }

  final case class ThreePlusOnes(a1: Ability, a2: Ability, a3: Ability) extends BackgroundBonus {
    def increases: List[(Ability, Int)] = List((a1, 1), (a2, 1), (a3, 1))
  }
}
