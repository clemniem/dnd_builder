package dndbuilder.dnd

enum ValidationError(val message: String):
  case EmptyName extends ValidationError("Character name cannot be empty.")
  case ScoreOutOfRange(ability: Ability, score: Int)
    extends ValidationError(s"${ability.label} score $score is out of valid range (1-30).")
  case PointBuyOverBudget(spent: Int)
    extends ValidationError(s"Point buy total is $spent, but maximum is ${AbilityScores.pointBuyTotal}.")
  case PointBuyScoreOutOfRange(ability: Ability, score: Int)
    extends ValidationError(s"${ability.label} base score $score is outside point-buy range (8-15).")
  case InvalidSkillChoice(skill: Skill)
    extends ValidationError(s"${skill.label} is not in the class skill pool.")
  case SkillAlreadyFromBackground(skill: Skill)
    extends ValidationError(s"${skill.label} is already granted by your background.")
  case WrongSkillCount(chosen: Int, expected: Int)
    extends ValidationError(s"Expected $expected skill choices, but got $chosen.")
  case BonusAbilityNotAllowed(ability: Ability)
    extends ValidationError(s"${ability.label} is not one of the background's three ability options.")
  case BonusTotalWrong(total: Int)
    extends ValidationError(s"Background bonus must total +3, but totals +$total.")

object CharacterValidator:

  def validateName(name: String): List[ValidationError] =
    if name.trim.isEmpty then List(ValidationError.EmptyName) else Nil

  def validateFinalScores(scores: AbilityScores): List[ValidationError] =
    scores.toList.flatMap { case (ability, value) =>
      if value < 1 || value > 30 then List(ValidationError.ScoreOutOfRange(ability, value))
      else Nil
    }

  def validatePointBuy(scores: AbilityScores): List[ValidationError] =
    val rangeErrors = scores.toList.flatMap { case (ability, value) =>
      if value < 8 || value > 15 then List(ValidationError.PointBuyScoreOutOfRange(ability, value))
      else Nil
    }
    if rangeErrors.nonEmpty then rangeErrors
    else
      scores.totalPointBuyCost match
        case Left(_) => Nil
        case Right(total) =>
          if total > AbilityScores.pointBuyTotal then
            List(ValidationError.PointBuyOverBudget(total))
          else Nil

  def validateBackgroundBonus(bonus: BackgroundBonus, background: Background): List[ValidationError] =
    val allowed = background.abilityOptionsList.toSet
    val abilityErrors = bonus.increases.flatMap { case (ability, _) =>
      if !allowed.contains(ability) then List(ValidationError.BonusAbilityNotAllowed(ability))
      else Nil
    }
    val totalError =
      if bonus.totalPoints != 3 then List(ValidationError.BonusTotalWrong(bonus.totalPoints))
      else Nil
    abilityErrors ++ totalError

  def validateSkillSelection(
      chosen: Set[Skill],
      dndClass: DndClass,
      background: Background
  ): List[ValidationError] =
    val bgSkills = background.skillProficiencySet
    val pool     = dndClass.skillPool -- bgSkills
    val expected = dndClass.numSkillChoices

    val bgOverlap = chosen.toList.flatMap { s =>
      if bgSkills.contains(s) then List(ValidationError.SkillAlreadyFromBackground(s))
      else Nil
    }
    val notInPool = chosen.toList.flatMap { s =>
      if !pool.contains(s) && !bgSkills.contains(s) then List(ValidationError.InvalidSkillChoice(s))
      else Nil
    }
    val countError =
      if chosen.size != expected then List(ValidationError.WrongSkillCount(chosen.size, expected))
      else Nil

    bgOverlap ++ notInPool ++ countError

  def validate(
      name: String,
      species: Species,
      dndClass: DndClass,
      background: Background,
      baseScores: AbilityScores,
      bonus: BackgroundBonus,
      chosenSkills: Set[Skill],
      level: Int
  ): Either[List[ValidationError], Character] =
    val finalScores = AbilityScores.applyBonus(baseScores, bonus)
    val errors =
      validateName(name) ++
      validateBackgroundBonus(bonus, background) ++
      validateFinalScores(finalScores) ++
      validateSkillSelection(chosenSkills, dndClass, background)

    if errors.nonEmpty then Left(errors)
    else Right(Character(name, species, dndClass, background, baseScores, bonus, chosenSkills, level))
