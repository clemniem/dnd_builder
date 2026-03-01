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
    subclass: Option[Subclass],
    languages: Set[Language],
    coins: Coins,
    spellGrants: List[SpellGrant],
    skillGrants: List[SkillGrant],
    attackGrants: List[AttackGrant]) {

  require(classLevels.nonEmpty, "classLevels cannot be empty")

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
    val alertBonus = if background.feat.grantsInitiativeBonus then proficiencyBonus else Modifier.zero
    val jackBonus = if hasJackOfAllTrades && !background.feat.grantsInitiativeBonus then Modifier(proficiencyBonus.toInt / 2) else Modifier.zero
    dexMod + alertBonus + jackBonus
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

  def skillProficiencyLevel(skill: Skill): SkillProficiency =
    if featureSelections.expertiseSkills.contains(skill) then SkillProficiency.Expert
    else if allSkillProficiencies.contains(skill) then SkillProficiency.Proficient
    else SkillProficiency.None

  /** True if character has Jack of All Trades (half proficiency on non-proficient checks). */
  def hasJackOfAllTrades: Boolean =
    primaryClass.jackOfAllTradesAtLevel.exists(_ <= primaryClassLevel)

  def skillBonus(skill: Skill): Modifier = {
    val abilityMod = modifier(skill.ability)
    skillProficiencyLevel(skill) match {
      case SkillProficiency.Expert     => abilityMod + proficiencyBonus + proficiencyBonus
      case SkillProficiency.Proficient => abilityMod + proficiencyBonus
      case SkillProficiency.None =>
        if hasJackOfAllTrades then abilityMod + Modifier(proficiencyBonus.toInt / 2)
        else abilityMod
    }
  }

  def isSkillProficient(skill: Skill): Boolean =
    skillProficiencyLevel(skill) != SkillProficiency.None

  def passivePerception: Int =
    10 + skillBonus(Skill.Perception).toInt

  def armorClass: Int = {
    val dexMod = modifier(Ability.Dexterity)
    val shieldBonus = if equippedShield then Armor.shieldACBonus else 0
    equippedArmor match {
      case None =>
        val unarmoredBonus = primaryClass.unarmoredDefenseAbility.fold(0)(a => modifier(a).toInt)
        val unarmored = 10 + dexMod.toInt + unarmoredBonus
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
    effectiveSpellcastingAbility.map { ability =>
      8 + proficiencyBonus.toInt + modifier(ability).toInt
    }

  def spellAttackBonus: Option[Modifier] =
    effectiveSpellcastingAbility.map { ability =>
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
    val modStr = if dmgMod >= 0 then s" + $dmgMod" else s" - ${math.abs(dmgMod)}"
    s"${weapon.damageDice}$modStr ${weapon.damageType.pdfAbbrev}"
  }

  def weaponPropertiesSummary(weapon: Weapon): String =
    weapon.properties.toList.sortBy(_.ordinal).map(_.toString).mkString(", ")

  def spellProgression: Option[SpellSlotRow] =
    SpellProgression.forClass(primaryClass, primaryClassLevel)

  def isSpellcaster: Boolean =
    effectiveSpellcastingAbility.isDefined

  /** Spellcasting ability for class spells, or inferred from first spell grant (e.g. Magic Initiate) when class has none. */
  def effectiveSpellcastingAbility: Option[Ability] =
    primaryClass.spellcastingAbility.orElse(
      spellGrants.headOption.flatMap { g =>
        SpellList.values.find(_.label == g.spellListLabel).map(_.ability)
      }
    )

  def equipmentSummary: String = {
    val armorStr = equippedArmor.fold("Unarmored")(_.name)
    val shieldStr = if equippedShield then ", Shield" else ""
    val weaponStr = if equippedWeapons.isEmpty then "" else ", " + equippedWeapons.map(_.name).mkString(", ")
    armorStr + shieldStr + weaponStr
  }

  def hitDiceString: String =
    classLevels.map(cl => s"${cl.classLevel}d${cl.dndClass.hitDie.sides}").mkString(" + ")

  def classLabel: String =
    if classLevels.size == 1 then {
      val base = s"${primaryClass.name} $primaryClassLevel"
      subclass.fold(base)(s => s"$base (${s.name})")
    } else classLevels.map(cl => s"${cl.dndClass.name} ${cl.classLevel}").mkString(" / ")

  private def scaledDice(baseDice: String): String = {
    val idx = baseDice.indexOf('d')
    require(
      idx > 0 &&
        baseDice.substring(0, idx).forall(_.isDigit) &&
        baseDice.length > idx + 1 &&
        baseDice.drop(idx + 1).forall(_.isDigit),
      s"Invalid dice format (expected NdM, e.g. 1d6): $baseDice"
    )
    val numDice = characterLevel match {
      case l if l >= 17 => 4
      case l if l >= 11 => 3
      case l if l >= 5  => 2
      case _            => 1
    }
    s"$numDice${baseDice.dropWhile(_.isDigit)}"
  }

  /** Martial Arts die by character level: 1d6 (1-4), 1d8 (5-10), 1d10 (11-16), 1d12 (17-20). */
  private def martialArtsDiceForLevel(level: Int): String = level match {
    case l if l >= 17 => "1d12"
    case l if l >= 11 => "1d10"
    case l if l >= 5  => "1d8"
    case _            => "1d6"
  }

  private def resolveGrants(grants: List[AttackGrant]): List[Attack] =
    grants.map { grant =>
      val dice = grant.diceScaling match {
        case DiceScaling.MartialArts => martialArtsDiceForLevel(characterLevel)
        case DiceScaling.Cantrip     => scaledDice(grant.baseDamageDice)
        case DiceScaling.None        => grant.baseDamageDice
      }
      val delivery = grant.delivery match {
        case AttackGrantDelivery.MeleeAttack(ability) =>
          AttackDelivery.AttackRoll(proficiencyBonus + modifier(ability))
        case AttackGrantDelivery.RangedAttack(ability) =>
          AttackDelivery.AttackRoll(proficiencyBonus + modifier(ability))
        case AttackGrantDelivery.SaveDC(save, dcAbility) =>
          AttackDelivery.SaveDC(
            8 + proficiencyBonus.toInt + modifier(dcAbility).toInt,
            save
          )
      }
      val usesStr =
        if grant.usesPerLR then List.fill(proficiencyBonus.toInt)("o").mkString(" ")
        else ""
      val notes = List(grant.range, usesStr).filter(_.nonEmpty).mkString(", ")
      Attack(
        grant.name,
        grant.kind,
        delivery,
        s"$dice ${grant.damageType}",
        notes
      )
    }

  def allAttacks: List[Attack] = {
    val weaponAttacks = equippedWeapons.map { weapon =>
      Attack(
        weapon.name,
        AttackKind.Weapon,
        AttackDelivery.AttackRoll(weaponAttackBonus(weapon)),
        weaponDamageString(weapon),
        weaponPropertiesSummary(weapon)
      )
    }

    val grantCantrips = spellGrants.flatMap(g =>
      if g.spellLevel == 0 then g.chosen else Nil
    )
    val spellAttacks = (chosenCantrips ++ grantCantrips)
      .filter(_.damageDice.isDefined)
      .map { spell =>
        val delivery = spell.spellDelivery match {
          case Some(SpellDelivery.RangedAttack | SpellDelivery.MeleeAttack) =>
            AttackDelivery.AttackRoll(
              spellAttackBonus.getOrElse(Modifier.zero)
            )
          case Some(SpellDelivery.Save(ability)) =>
            AttackDelivery.SaveDC(spellSaveDC.getOrElse(0), ability)
          case scala.None =>
            AttackDelivery.AttackRoll(Modifier.zero)
        }
        val dice = scaledDice(spell.damageDice.get)
        Attack(
          spell.name,
          AttackKind.Spell,
          delivery,
          s"$dice ${spell.damageType.getOrElse("")}",
          ""
        )
      }

    val granted = resolveGrants(attackGrants)

    val hasUnarmed = granted.exists(_.name == "Unarmed Strike")
    val fightingStyleAttack =
      if !hasUnarmed && featureSelections.fightingStyle
        .exists(_.label == "Unarmed Fighting")
      then {
        val bonus = proficiencyBonus + modifier(Ability.Strength)
        List(
          Attack(
            "Unarmed Strike",
            AttackKind.Weapon,
            AttackDelivery.AttackRoll(bonus),
            "1d6 bludg.",
            "1d8 two-hand, 1d4 grapple"
          )
        )
      }
      else Nil

    weaponAttacks ++ spellAttacks ++ granted ++ fightingStyleAttack
  }
}
