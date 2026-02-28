package dndbuilder.dnd

import dndbuilder.dnd.DndTypes.Modifier

final case class Character(
    name: String,
    species: Species,
    classLevels: List[ClassLevel],
    background: Background,
    baseScores: AbilityScores,
    backgroundBonus: BackgroundBonus,
    chosenSkills: Set[Skill],
    equippedArmor: Option[Armor],
    equippedShield: Boolean,
    equippedWeapons: List[Weapon],
    chosenCantrips: List[Spell],
    preparedSpells: List[Spell],
    spellbookSpells: List[Spell],
    featureSelections: ClassFeatureSelections,
    languages: Set[Language]) {

  def primaryClass: DndClass = classLevels.head.dndClass

  def primaryClassLevel: Int = classLevels.head.classLevel

  def characterLevel: Int = classLevels.map(_.classLevel).sum

  def finalScores: AbilityScores =
    AbilityScores.applyBonus(baseScores, backgroundBonus)

  def modifier(ability: Ability): Modifier =
    finalScores.modifier(ability)

  def proficiencyBonus: Modifier = characterLevel match {
    case l if l <= 4  => Modifier(2)
    case l if l <= 8  => Modifier(3)
    case l if l <= 12 => Modifier(4)
    case l if l <= 16 => Modifier(5)
    case _            => Modifier(6)
  }

  def maxHitPoints: Int = {
    val conMod = modifier(Ability.Constitution)
    val primary = classLevels.head
    val base = primary.dndClass.hitDie.sides + conMod.toInt
    val subsequent = classLevels.zipWithIndex.map { case (cl, idx) =>
      val levelsToCount = if idx == 0 then cl.classLevel - 1 else cl.classLevel
      levelsToCount * (cl.dndClass.hitDie.avgGain + conMod.toInt)
    }.sum
    val speciesBonus = species.hpBonusPerLevel * characterLevel
    math.max(1, base + subsequent + speciesBonus)
  }

  def initiative: Modifier = {
    val dexMod = modifier(Ability.Dexterity)
    val alertBonus = if background.feat == Alert then proficiencyBonus else Modifier.zero
    dexMod + alertBonus
  }

  def savingThrowBonus(ability: Ability): Modifier = {
    val mod = modifier(ability)
    val (st1, st2) = primaryClass.savingThrows
    if ability == st1 || ability == st2 then mod + proficiencyBonus
    else mod
  }

  def isProficientInSave(ability: Ability): Boolean = {
    val (st1, st2) = primaryClass.savingThrows
    ability == st1 || ability == st2
  }

  def allSkillProficiencies: Set[Skill] =
    background.skillProficiencySet ++ chosenSkills

  def skillBonus(skill: Skill): Modifier = {
    val abilityMod = modifier(skill.ability)
    if allSkillProficiencies.contains(skill) then abilityMod + proficiencyBonus
    else abilityMod
  }

  def isSkillProficient(skill: Skill): Boolean =
    allSkillProficiencies.contains(skill)

  def passivePerception: Int =
    10 + skillBonus(Skill.Perception).toInt

  def armorClass: Int = {
    val dexMod = modifier(Ability.Dexterity)
    val shieldBonus = if equippedShield then Armor.shieldACBonus else 0
    equippedArmor match {
      case None =>
        val unarmored = primaryClass match {
          case Barbarian => 10 + dexMod.toInt + modifier(Ability.Constitution).toInt
          case Monk      => 10 + dexMod.toInt + modifier(Ability.Wisdom).toInt
          case _         => 10 + dexMod.toInt
        }
        unarmored + shieldBonus
      case Some(armor) =>
        import ArmorType.*
        val base = armor.armorType match {
          case Light =>
            armor.baseAC + dexMod.toInt
          case Medium =>
            val capped = armor.maxDexBonus.fold(dexMod.toInt)(cap => math.min(dexMod.toInt, cap))
            armor.baseAC + capped
          case Heavy | Shield =>
            armor.baseAC
        }
        base + shieldBonus
    }
  }

  def spellSaveDC: Option[Int] =
    primaryClass.spellcastingAbility.map { ability =>
      8 + proficiencyBonus.toInt + modifier(ability).toInt
    }

  def spellAttackBonus: Option[Modifier] =
    primaryClass.spellcastingAbility.map { ability =>
      proficiencyBonus + modifier(ability)
    }

  def speed: Int = species.speed

  def originFeat: OriginFeat = background.feat

  def weaponAbilityMod(weapon: Weapon): Modifier = {
    val strMod = modifier(Ability.Strength)
    val dexMod = modifier(Ability.Dexterity)
    if weapon.properties.contains(WeaponProperty.Finesse) then Modifier(math.max(strMod.toInt, dexMod.toInt))
    else if weapon.range == WeaponRange.Ranged then dexMod
    else strMod
  }

  def weaponAttackBonus(weapon: Weapon): Modifier =
    proficiencyBonus + weaponAbilityMod(weapon)

  def weaponDamageBonus(weapon: Weapon): Modifier =
    weaponAbilityMod(weapon)

  def weaponDamageString(weapon: Weapon): String = {
    val dmgMod = weaponDamageBonus(weapon).toInt
    val parts   = weapon.damage.split(" ", 2)
    val dice    = parts.headOption.getOrElse("")
    val dmgType = if parts.length > 1 then parts(1) else ""
    val modStr  = if dmgMod >= 0 then s" + $dmgMod" else s" - ${math.abs(dmgMod)}"
    val typeStr = if dmgType.nonEmpty then s" $dmgType" else ""
    s"$dice$modStr$typeStr"
  }

  def weaponPropertiesSummary(weapon: Weapon): String =
    weapon.properties.toList.sortBy(_.ordinal).map(_.toString).mkString(", ")

  def spellProgression: Option[SpellSlotRow] =
    SpellProgression.forClass(primaryClass, primaryClassLevel)

  def isSpellcaster: Boolean =
    primaryClass.spellcastingAbility.isDefined

  def equipmentSummary: String = {
    val armorStr = equippedArmor.fold("Unarmored")(_.name)
    val shieldStr = if equippedShield then ", Shield" else ""
    val weaponStr = if equippedWeapons.isEmpty then "" else ", " + equippedWeapons.map(_.name).mkString(", ")
    armorStr + shieldStr + weaponStr
  }

  def hitDiceString: String =
    classLevels.map(cl => s"${cl.classLevel}d${cl.dndClass.hitDie.sides}").mkString(" + ")

  def classLabel: String =
    if classLevels.size == 1 then primaryClass.name
    else classLevels.map(cl => s"${cl.dndClass.name} ${cl.classLevel}").mkString(" / ")
}
