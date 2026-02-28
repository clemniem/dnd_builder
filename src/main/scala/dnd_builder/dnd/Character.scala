package dndbuilder.dnd

final case class Character(
    name: String,
    species: Species,
    dndClass: DndClass,
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
    languages: Set[Language],
    level: Int) {

  def finalScores: AbilityScores =
    AbilityScores.applyBonus(baseScores, backgroundBonus)

  def modifier(ability: Ability): Int =
    finalScores.modifier(ability)

  def proficiencyBonus: Int = level match {
    case l if l <= 4  => 2
    case l if l <= 8  => 3
    case l if l <= 12 => 4
    case l if l <= 16 => 5
    case _            => 6
  }

  def maxHitPoints: Int = {
    val conMod  = modifier(Ability.Constitution)
    val base    = dndClass.hitDie.sides + conMod
    val perLevel = if level > 1 then (level - 1) * (dndClass.hitDie.avgGain + conMod) else 0
    val speciesBonus = species.hpBonusPerLevel * level
    base + perLevel + speciesBonus
  }

  def initiative: Int = {
    val dexMod = modifier(Ability.Dexterity)
    val alertBonus = if background.feat == Alert then proficiencyBonus else 0
    dexMod + alertBonus
  }

  def savingThrowBonus(ability: Ability): Int = {
    val mod = modifier(ability)
    val (st1, st2) = dndClass.savingThrows
    if ability == st1 || ability == st2 then mod + proficiencyBonus
    else mod
  }

  def isProficientInSave(ability: Ability): Boolean = {
    val (st1, st2) = dndClass.savingThrows
    ability == st1 || ability == st2
  }

  def allSkillProficiencies: Set[Skill] =
    background.skillProficiencySet ++ chosenSkills

  def skillBonus(skill: Skill): Int = {
    val abilityMod = modifier(skill.ability)
    if allSkillProficiencies.contains(skill) then abilityMod + proficiencyBonus
    else abilityMod
  }

  def isSkillProficient(skill: Skill): Boolean =
    allSkillProficiencies.contains(skill)

  def passivePerception: Int =
    10 + skillBonus(Skill.Perception)

  def armorClass: Int = {
    val dexMod = modifier(Ability.Dexterity)
    val shieldBonus = if equippedShield then Armor.shieldACBonus else 0
    equippedArmor match {
      case None =>
        val unarmored = dndClass match {
          case Barbarian => 10 + dexMod + modifier(Ability.Constitution)
          case Monk      => 10 + dexMod + modifier(Ability.Wisdom)
          case _        => 10 + dexMod
        }
        unarmored + shieldBonus
      case Some(armor) =>
        import ArmorType.*
        val base = armor.armorType match {
          case Light =>
            armor.baseAC + dexMod
          case Medium =>
            val capped = armor.maxDexBonus.fold(dexMod)(cap => math.min(dexMod, cap))
            armor.baseAC + capped
          case Heavy | Shield =>
            armor.baseAC
        }
        base + shieldBonus
    }
  }

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

  def weaponAbilityMod(weapon: Weapon): Int = {
    val strMod = modifier(Ability.Strength)
    val dexMod = modifier(Ability.Dexterity)
    if weapon.properties.contains(WeaponProperty.Finesse) then math.max(strMod, dexMod)
    else if weapon.range == WeaponRange.Ranged then dexMod
    else strMod
  }

  def weaponAttackBonus(weapon: Weapon): Int =
    proficiencyBonus + weaponAbilityMod(weapon)

  def weaponDamageBonus(weapon: Weapon): Int =
    weaponAbilityMod(weapon)

  def weaponDamageString(weapon: Weapon): String = {
    val dmgMod = weaponDamageBonus(weapon)
    val parts  = weapon.damage.split(" ", 2)
    val dice   = parts.headOption.getOrElse("")
    val dmgType = if parts.length > 1 then parts(1) else ""
    val modStr = if dmgMod >= 0 then s" + $dmgMod" else s" - ${math.abs(dmgMod)}"
    val typeStr = if dmgType.nonEmpty then s" $dmgType" else ""
    s"$dice$modStr$typeStr"
  }

  def weaponPropertiesSummary(weapon: Weapon): String =
    weapon.properties.toList.sortBy(_.ordinal).map(_.toString).mkString(", ")

  def spellProgression: Option[SpellSlotRow] =
    SpellProgression.forClass(dndClass, level)

  def isSpellcaster: Boolean =
    dndClass.spellcastingAbility.isDefined

  def equipmentSummary: String = {
    val armorStr = equippedArmor.fold("Unarmored")(_.name)
    val shieldStr = if equippedShield then ", Shield" else ""
    val weaponStr = if equippedWeapons.isEmpty then "" else ", " + equippedWeapons.map(_.name).mkString(", ")
    armorStr + shieldStr + weaponStr
  }
}
