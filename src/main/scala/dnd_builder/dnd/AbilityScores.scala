package dndbuilder.dnd

import dndbuilder.dnd.DndTypes.{Modifier, Score}

final case class AbilityScores(
    strength: Score,
    dexterity: Score,
    constitution: Score,
    intelligence: Score,
    wisdom: Score,
    charisma: Score) {

  def get(ability: Ability): Score = ability match {
    case Ability.Strength     => strength
    case Ability.Dexterity    => dexterity
    case Ability.Constitution => constitution
    case Ability.Intelligence => intelligence
    case Ability.Wisdom       => wisdom
    case Ability.Charisma     => charisma
  }

  def set(ability: Ability, value: Score): AbilityScores = ability match {
    case Ability.Strength     => copy(strength = value)
    case Ability.Dexterity    => copy(dexterity = value)
    case Ability.Constitution => copy(constitution = value)
    case Ability.Intelligence => copy(intelligence = value)
    case Ability.Wisdom       => copy(wisdom = value)
    case Ability.Charisma     => copy(charisma = value)
  }

  def adjust(ability: Ability, delta: Int): AbilityScores =
    set(ability, get(ability) + delta)

  def modifier(ability: Ability): Modifier =
    AbilityScores.modifier(get(ability))

  def toList: List[(Ability, Score)] =
    Ability.values.toList.map(a => (a, get(a)))

  def totalPointBuyCost: Either[String, Int] = {
    val costs = toList.map { case (a, v) =>
      AbilityScores.pointBuyCost.get(v) match {
        case Some(c) => Right(c)
        case None    => Left(s"${a.label} score ${v.value} is outside point-buy range (8-15)")
      }
    }
    val errors = costs.collect { case Left(e) => e }
    if errors.nonEmpty then Left(errors.mkString("; "))
    else Right(costs.collect { case Right(c) => c }.sum)
  }
}

object AbilityScores {
  val default: AbilityScores =
    AbilityScores(Score(8), Score(8), Score(8), Score(8), Score(8), Score(8))

  val standardArray: List[Score] =
    List(Score(15), Score(14), Score(13), Score(12), Score(10), Score(8))

  val pointBuyTotal: Int = 27

  val pointBuyCost: Map[Score, Int] = Map(
    Score(8)  -> 0,
    Score(9)  -> 1,
    Score(10) -> 2,
    Score(11) -> 3,
    Score(12) -> 4,
    Score(13) -> 5,
    Score(14) -> 7,
    Score(15) -> 9
  )

  def modifier(score: Score): Modifier =
    Modifier(Math.floorDiv(score.value - 10, 2))

  def modifierString(score: Score): String =
    modifier(score).format

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
