package dndbuilder.dnd

enum ValidationError(val message: String) {
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
  case WrongCantripCount(chosen: Int, expected: Int)
    extends ValidationError(s"Expected $expected cantrips, but got $chosen.")
  case CantripNotOnClassList(spellName: String)
    extends ValidationError(s"$spellName is not on the class cantrip list.")
  case WrongPreparedCount(chosen: Int, expected: Int)
    extends ValidationError(s"Expected $expected prepared spells, but got $chosen.")
  case PreparedNotOnClassList(spellName: String)
    extends ValidationError(s"$spellName is not on the class spell list.")
  case PreparedNotInSpellbook(spellName: String)
    extends ValidationError(s"$spellName is not in the spellbook.")
  case WrongSpellbookCount(chosen: Int, expected: Int)
    extends ValidationError(s"Expected $expected spellbook spells, but got $chosen.")
  case DuplicateSpell(spellName: String)
    extends ValidationError(s"$spellName is selected more than once.")
  case SubclassWrongClass(subclassName: String, className: String)
    extends ValidationError(s"Subclass $subclassName does not belong to class $className.")
  case LevelOutOfRange(level: Int)
    extends ValidationError(s"Level $level is out of valid range (1-20).")
  case FeatureChoicesNotSatisfied
    extends ValidationError("Required class feature choices (e.g. fighting style, divine order) are missing.")
  case SpellGrantsNotFilled
    extends ValidationError("Some spell grants from background or origin are not filled.")
  case SkillGrantsNotFilled
    extends ValidationError("Some skill grants from background or origin are not filled.")
}

object CharacterValidator {

  def validateName(name: String): List[ValidationError] =
    if name.trim.isEmpty then List(ValidationError.EmptyName) else Nil

  def validateFinalScores(scores: AbilityScores): List[ValidationError] =
    scores.toList.flatMap { case (ability, value) =>
      val v = value.value
      if v < 1 || v > 30 then List(ValidationError.ScoreOutOfRange(ability, v))
      else Nil
    }

  def validatePointBuy(scores: AbilityScores): List[ValidationError] = {
    val rangeErrors = scores.toList.flatMap { case (ability, value) =>
      val v = value.value
      if v < 8 || v > 15 then List(ValidationError.PointBuyScoreOutOfRange(ability, v))
      else Nil
    }
    if rangeErrors.nonEmpty then rangeErrors
    else
      scores.totalPointBuyCost match {
        case Left(_) => Nil
        case Right(total) =>
          if total > AbilityScores.pointBuyTotal then
            List(ValidationError.PointBuyOverBudget(total))
          else Nil
      }
  }

  def validateBackgroundBonus(bonus: BackgroundBonus, background: Background): List[ValidationError] = {
    val allowed = background.abilityOptionsList.toSet
    val abilityErrors = bonus.increases.flatMap { case (ability, _) =>
      if !allowed.contains(ability) then List(ValidationError.BonusAbilityNotAllowed(ability))
      else Nil
    }
    val totalError =
      if bonus.totalPoints != 3 then List(ValidationError.BonusTotalWrong(bonus.totalPoints))
      else Nil
    abilityErrors ++ totalError
  }

  def validateSkillSelection(
      chosen: Set[Skill],
      dndClass: DndClass,
      background: Background
  ): List[ValidationError] = {
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
  }

  def validateSpells(
      dndClass: DndClass,
      level: Int,
      chosenCantrips: List[Spell],
      preparedSpells: List[Spell],
      spellbookSpells: List[Spell]
  ): List[ValidationError] =
    if !dndClass.isSpellcaster then Nil
    else {
      val prog = SpellProgression.forClass(dndClass, level)
      val expectedCantrips = prog.map(_.cantrips).getOrElse(0)
      val expectedPrepared = prog.map(_.preparedSpells).getOrElse(0)
      val maxSpellLevel = SpellProgression.maxSpellLevelForSlots(dndClass, level)
      val isWizard = dndClass.usesSpellbook
      val expectedSpellbook = if isWizard then SpellProgression.wizardSpellbookSize(level) else 0

      val cantripErrors =
        if expectedCantrips > 0 then {
          val countErr =
            if chosenCantrips.size != expectedCantrips then
              List(ValidationError.WrongCantripCount(chosenCantrips.size, expectedCantrips))
            else Nil
          val classErr = chosenCantrips.flatMap { s =>
            if !s.availableToClass(dndClass) || s.level != 0 then
              List(ValidationError.CantripNotOnClassList(s.name))
            else Nil
          }
          val dupErr = chosenCantrips.groupBy(_.name).toList.flatMap { case (nm, spells) =>
            if spells.size > 1 then List(ValidationError.DuplicateSpell(nm)) else Nil
          }
          countErr ++ classErr ++ dupErr
        }
        else Nil

      val spellbookErrors =
        if expectedSpellbook > 0 then {
          val countErr =
            if spellbookSpells.size != expectedSpellbook then
              List(ValidationError.WrongSpellbookCount(spellbookSpells.size, expectedSpellbook))
            else Nil
          val classErr = spellbookSpells.flatMap { s =>
            if !s.availableToClass(dndClass) || s.level < 1 || s.level > maxSpellLevel then
              List(ValidationError.PreparedNotOnClassList(s.name))
            else Nil
          }
          countErr ++ classErr
        }
        else Nil

      val preparedErrors =
        if expectedPrepared > 0 then {
          val countErr =
            if preparedSpells.size != expectedPrepared then
              List(ValidationError.WrongPreparedCount(preparedSpells.size, expectedPrepared))
            else Nil
          val sourceErr =
            if isWizard then
              preparedSpells.flatMap { s =>
                if !spellbookSpells.exists(_.name == s.name) then
                  List(ValidationError.PreparedNotInSpellbook(s.name))
                else Nil
              }
            else
              preparedSpells.flatMap { s =>
                if !s.availableToClass(dndClass) || s.level < 1 || s.level > maxSpellLevel then
                  List(ValidationError.PreparedNotOnClassList(s.name))
                else Nil
              }
          val dupErr = preparedSpells.groupBy(_.name).toList.flatMap { case (nm, spells) =>
            if spells.size > 1 then List(ValidationError.DuplicateSpell(nm)) else Nil
          }
          countErr ++ sourceErr ++ dupErr
        }
        else Nil

      cantripErrors ++ spellbookErrors ++ preparedErrors
    }

  def validateSubclass(subclass: Option[Subclass], primaryClass: DndClass): List[ValidationError] =
    subclass.toList.flatMap { sub =>
      if sub.dndClass != primaryClass then List(ValidationError.SubclassWrongClass(sub.name, primaryClass.name))
      else Nil
    }

  def validateLevel(level: Int): List[ValidationError] =
    if level < 1 || level > 20 then List(ValidationError.LevelOutOfRange(level))
    else Nil

  def validateFeatureChoices(
      dndClass: DndClass,
      level: Int,
      featureSelections: ClassFeatureSelections
  ): List[ValidationError] = {
    val grants = ClassProgression.grantChoicesUpToLevel(dndClass, level)
    if ClassProgression.satisfiesGrantChoices(grants, featureSelections) then Nil
    else List(ValidationError.FeatureChoicesNotSatisfied)
  }

  def validateSpellGrants(spellGrants: List[SpellGrant]): List[ValidationError] =
    if spellGrants.forall(_.isFilled) then Nil
    else List(ValidationError.SpellGrantsNotFilled)

  def validateSkillGrants(skillGrants: List[SkillGrant]): List[ValidationError] =
    if skillGrants.forall(_.isFilled) then Nil
    else List(ValidationError.SkillGrantsNotFilled)

  def validate(
      name: String,
      species: Species,
      dndClass: DndClass,
      background: Background,
      baseScores: AbilityScores,
      bonus: BackgroundBonus,
      chosenSkills: Set[Skill],
      equippedArmor: Option[Armor],
      equippedShield: Boolean,
      equippedWeapons: List[Weapon],
      chosenCantrips: List[Spell],
      preparedSpells: List[Spell],
      spellbookSpells: List[Spell],
      featureSelections: ClassFeatureSelections,
      subclass: Option[Subclass],
      languages: Set[Language],
      level: Int,
      coins: Coins,
      grants: List[Grant]
  ): Either[List[ValidationError], Character] = {
    val spellGrants = grants.collect { case g: SpellGrant => g }
    val skillGrants = grants.collect { case g: SkillGrant => g }
    val finalScores = AbilityScores.applyBonus(baseScores, bonus)
    val errors =
      validateName(name) ++
      validateBackgroundBonus(bonus, background) ++
      validateFinalScores(finalScores) ++
      validateSkillSelection(chosenSkills, dndClass, background) ++
      validateSpells(dndClass, level, chosenCantrips, preparedSpells, spellbookSpells) ++
      validateSubclass(subclass, dndClass) ++
      validateLevel(level) ++
      validateFeatureChoices(dndClass, level, featureSelections) ++
      validateSpellGrants(spellGrants) ++
      validateSkillGrants(skillGrants)

    val classLevels = List(ClassLevel(dndClass, level))
    if errors.nonEmpty then Left(errors)
    else Right(Character(name, species, classLevels, background, baseScores, bonus, chosenSkills,
      equippedArmor, equippedShield, equippedWeapons, chosenCantrips, preparedSpells, spellbookSpells,
      featureSelections, subclass, languages, coins, grants))
  }
}
