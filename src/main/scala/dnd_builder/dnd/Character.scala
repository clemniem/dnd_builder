package dndbuilder.dnd

final case class Character(
    name: String,
    species: Species,
    dndClass: DndClass,
    background: Background,
    baseScores: AbilityScores,
    backgroundBonus: BackgroundBonus,
    chosenSkills: Set[Skill],
    level: Int):

  def finalScores: AbilityScores =
    AbilityScores.applyBonus(baseScores, backgroundBonus)

  def modifier(ability: Ability): Int =
    finalScores.modifier(ability)

  def proficiencyBonus: Int = level match
    case l if l <= 4  => 2
    case l if l <= 8  => 3
    case l if l <= 12 => 4
    case l if l <= 16 => 5
    case _            => 6

  def maxHitPoints: Int =
    val conMod  = modifier(Ability.Constitution)
    val base    = dndClass.hitDie.sides + conMod
    val perLevel = if level > 1 then (level - 1) * (dndClass.hitDie.avgGain + conMod) else 0
    val speciesBonus = species.hpBonusPerLevel * level
    base + perLevel + speciesBonus

  def initiative: Int =
    val dexMod = modifier(Ability.Dexterity)
    val alertBonus = if background.feat == Alert then proficiencyBonus else 0
    dexMod + alertBonus

  def savingThrowBonus(ability: Ability): Int =
    val mod = modifier(ability)
    val (st1, st2) = dndClass.savingThrows
    if ability == st1 || ability == st2 then mod + proficiencyBonus
    else mod

  def isProficientInSave(ability: Ability): Boolean =
    val (st1, st2) = dndClass.savingThrows
    ability == st1 || ability == st2

  def allSkillProficiencies: Set[Skill] =
    background.skillProficiencySet ++ chosenSkills

  def skillBonus(skill: Skill): Int =
    val abilityMod = modifier(skill.ability)
    if allSkillProficiencies.contains(skill) then abilityMod + proficiencyBonus
    else abilityMod

  def isSkillProficient(skill: Skill): Boolean =
    allSkillProficiencies.contains(skill)

  def passivePerception: Int =
    10 + skillBonus(Skill.Perception)

  def armorClass: Int =
    val dexMod = modifier(Ability.Dexterity)
    dndClass match
      case Barbarian =>
        10 + dexMod + modifier(Ability.Constitution)
      case Monk =>
        10 + dexMod + modifier(Ability.Wisdom)
      case _ =>
        10 + dexMod

  def spellSaveDC: Option[Int] =
    dndClass.spellcastingAbility.map { ability =>
      8 + proficiencyBonus + modifier(ability)
    }

  def spellAttackBonus: Option[Int] =
    dndClass.spellcastingAbility.map { ability =>
      proficiencyBonus + modifier(ability)
    }

  def speed: Int = species.speed

  def originFeat: OriginFeat = background.feat
