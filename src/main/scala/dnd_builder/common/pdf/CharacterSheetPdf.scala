package dndbuilder.common.pdf

import dndbuilder.dnd.*

import scala.scalajs.js

object CharacterSheetPdf:

  private val pdfUrl =
    "https://raw.githubusercontent.com/birddie721/5e2024Builder/main/Character-Sheet.pdf"

  def generate(ch: Character): Unit =
    PdfLib.loadFromUrl(pdfUrl) { doc =>
      val form = PdfLib.getForm(doc)
      fillHeader(form, ch)
      fillCombatStats(form, ch)
      fillAbilityScores(form, ch)
      fillSavingThrows(form, ch)
      fillSkills(form, ch)
      fillArmorProficiencies(form, ch)
      fillWeapons(form, ch)
      fillSpellcasting(form, ch)
      fillCurrency(form, ch)
      fillProficienciesAndFeatures(form, ch)
      val safeName = ch.name.replaceAll("[^a-zA-Z0-9_\\- ]", "").trim
      val filename = if safeName.isEmpty then "character-sheet.pdf" else s"$safeName.pdf"
      PdfLib.saveAndOpen(doc, filename)
    } { error =>
      org.scalajs.dom.console.error(s"PDF generation failed: $error")
    }

  private def setFieldSized(form: js.Dynamic, name: String, value: String, pt: Int): Unit =
    val field = PdfLib.getTextField(form, name)
    PdfLib.setFontSize(field, pt)
    PdfLib.setText(field, value)

  private def checkBox(form: js.Dynamic, name: String): Unit =
    PdfLib.check(PdfLib.getCheckBox(form, name))

  private def modStr(v: Int): String =
    if v >= 0 then s"+$v" else v.toString

  private def fillHeader(form: js.Dynamic, ch: Character): Unit =
    setFieldSized(form, "Name", ch.name, 14)
    setFieldSized(form, "Class", ch.dndClass.name, 10)
    setFieldSized(form, "Species", ch.species.name, 10)
    setFieldSized(form, "Background", ch.background.name, 10)
    setFieldSized(form, "Level", ch.level.toString, 12)
    setFieldSized(form, "XP Points", "0", 10)
    ch.species.subLabel.foreach { sub =>
      setFieldSized(form, "Subclass", sub, 10)
    }

  private def fillCombatStats(form: js.Dynamic, ch: Character): Unit =
    setFieldSized(form, "Armor Class", ch.armorClass.toString, 12)
    setFieldSized(form, "Max HP", ch.maxHitPoints.toString, 12)
    setFieldSized(form, "PROF BONUS", modStr(ch.proficiencyBonus), 12)
    setFieldSized(form, "PASSIVE PERCEPTION", ch.passivePerception.toString, 10)
    setFieldSized(form, "init", modStr(ch.initiative), 12)
    setFieldSized(form, "SPEED", s"${ch.speed}ft", 10)
    setFieldSized(form, "SIZE", ch.species.size.label, 10)

  private def fillAbilityScores(form: js.Dynamic, ch: Character): Unit =
    val scores = ch.finalScores
    val abilities = List(
      ("STR", Ability.Strength),
      ("DEX", Ability.Dexterity),
      ("CON", Ability.Constitution),
      ("INT", Ability.Intelligence),
      ("WIS", Ability.Wisdom),
      ("CHA", Ability.Charisma)
    )
    abilities.foreach { case (abbr, ability) =>
      val score = scores.get(ability)
      val mod   = AbilityScores.modifier(score)
      setFieldSized(form, s"$abbr SCORE", score.toString, 14)
      setFieldSized(form, s"$abbr MOD", modStr(mod), 10)
    }

  private val saveCheckboxes: Map[Ability, String] = Map(
    Ability.Strength     -> "Check Box18",
    Ability.Dexterity    -> "Check Box11",
    Ability.Constitution -> "Check Box7",
    Ability.Intelligence -> "Check Box25",
    Ability.Wisdom       -> "Check Box17",
    Ability.Charisma     -> "Check Box6"
  )

  private val saveFields: Map[Ability, String] = Map(
    Ability.Strength     -> "STR SAVE",
    Ability.Dexterity    -> "DEX SAVE",
    Ability.Constitution -> "CON SAVE",
    Ability.Intelligence -> "INT SAVE",
    Ability.Wisdom       -> "Text Field71",
    Ability.Charisma     -> "CHA SAVE"
  )

  private def fillSavingThrows(form: js.Dynamic, ch: Character): Unit =
    Ability.values.foreach { ability =>
      val bonus = ch.savingThrowBonus(ability)
      saveFields.get(ability).foreach(f => setFieldSized(form, f, modStr(bonus), 10))
      if ch.isProficientInSave(ability) then
        saveCheckboxes.get(ability).foreach(c => checkBox(form, c))
    }

  private val skillCheckboxes: Map[Skill, String] = Map(
    Skill.Athletics      -> "Check Box19",
    Skill.Acrobatics     -> "Check Box8",
    Skill.SleightOfHand  -> "Check Box9",
    Skill.Stealth        -> "Check Box10",
    Skill.Arcana         -> "Check Box24",
    Skill.History        -> "Check Box20",
    Skill.Investigation  -> "Check Box21",
    Skill.Nature         -> "Check Box22",
    Skill.Religion       -> "Check Box23",
    Skill.AnimalHandling -> "Check Box15",
    Skill.Insight        -> "Check Box13",
    Skill.Medicine       -> "Check Box12",
    Skill.Perception     -> "Check Box14",
    Skill.Survival       -> "Check Box16",
    Skill.Deception      -> "Check Box5",
    Skill.Intimidation   -> "Check Box4",
    Skill.Performance    -> "Check Box3",
    Skill.Persuasion     -> "Check Box2"
  )

  private val skillFields: Map[Skill, String] = Map(
    Skill.Athletics      -> "ATHLETICS",
    Skill.Acrobatics     -> "ACROBATICS",
    Skill.SleightOfHand  -> "SLEIGHT OF HAND",
    Skill.Stealth        -> "STEALTH",
    Skill.Arcana         -> "ARCANA",
    Skill.History        -> "HISTORY",
    Skill.Investigation  -> "INVESTIGATION",
    Skill.Nature         -> "NATURE",
    Skill.Religion       -> "RELIGION",
    Skill.AnimalHandling -> "ANIMAL HANDLING",
    Skill.Insight        -> "INSIGHT",
    Skill.Medicine       -> "MEDICINE",
    Skill.Perception     -> "PERCEPTION",
    Skill.Survival       -> "SURVIVAL",
    Skill.Deception      -> "DECEPTION",
    Skill.Intimidation   -> "INTIMIDATE",
    Skill.Performance    -> "PERFORMANCE",
    Skill.Persuasion     -> "PERSUASION"
  )

  private def fillSkills(form: js.Dynamic, ch: Character): Unit =
    Skill.values.foreach { skill =>
      val bonus = ch.skillBonus(skill)
      skillFields.get(skill).foreach(f => setFieldSized(form, f, modStr(bonus), 10))
      if ch.isSkillProficient(skill) then
        skillCheckboxes.get(skill).foreach(c => checkBox(form, c))
    }

  private def fillArmorProficiencies(form: js.Dynamic, ch: Character): Unit =
    val profs = ch.dndClass.armorProficiencies
    if profs.contains(ArmorType.Light) then checkBox(form, "Check Box33")
    if profs.contains(ArmorType.Medium) then checkBox(form, "Check Box34")
    if profs.contains(ArmorType.Heavy) then checkBox(form, "Check Box35")
    if profs.contains(ArmorType.Shield) then checkBox(form, "Check Box36")
    if ch.equippedShield then checkBox(form, "shield chk")

  private def fillWeapons(form: js.Dynamic, ch: Character): Unit =
    ch.equippedWeapons.zipWithIndex.foreach { case (weapon, idx) =>
      val n = idx + 1
      if n <= 6 then
        setFieldSized(form, s"NAME - WEAPON $n", weapon.name, 8)
        setFieldSized(form, s"BONUS/DC - WEAPON $n", modStr(ch.weaponAttackBonus(weapon)), 8)
        setFieldSized(form, s"DAMAGE/TYPE - WEAPON $n", ch.weaponDamageString(weapon), 7)
        setFieldSized(form, s"NOTES - WEAPON $n", ch.weaponPropertiesSummary(weapon), 6)
    }

  private def fillSpellcasting(form: js.Dynamic, ch: Character): Unit =
    ch.dndClass.spellcastingAbility.foreach { ability =>
      setFieldSized(form, "SPELLCASTING ABILITY", ability.label, 8)
      setFieldSized(form, "SPELLCASTING MOD", modStr(ch.modifier(ability)), 10)
      ch.spellSaveDC.foreach(dc => setFieldSized(form, "SPELL SAVE DC", dc.toString, 10))
      ch.spellAttackBonus.foreach(b => setFieldSized(form, "SPELL ATTACK BONUS", modStr(b), 10))
    }
    ch.chosenCantrips.zipWithIndex.foreach { case (spell, idx) =>
      val fieldName = s"CANTRIP ${idx + 1}"
      trySetField(form, fieldName, spell.name, 8)
    }
    ch.preparedSpells.zipWithIndex.foreach { case (spell, idx) =>
      val fieldName = s"SPELL LVL1 ${idx + 1}"
      trySetField(form, fieldName, spell.name, 8)
    }
    if ch.dndClass.level1SpellSlots > 0 then
      trySetField(form, "SPELL SLOTS LVL1", ch.dndClass.level1SpellSlots.toString, 10)

  private def trySetField(form: js.Dynamic, name: String, value: String, pt: Int): Unit =
    try setFieldSized(form, name, value, pt)
    catch case _: Throwable => ()

  private def fillCurrency(form: js.Dynamic, ch: Character): Unit =
    setFieldSized(form, "GP", ch.background.startingGold.toString, 10)
    setFieldSized(form, "CP", "0", 10)
    setFieldSized(form, "SP", "0", 10)
    setFieldSized(form, "EP", "0", 10)
    setFieldSized(form, "PP", "0", 10)

  private def fillProficienciesAndFeatures(form: js.Dynamic, ch: Character): Unit =
    setFieldSized(form, "WEAPON PROF", ch.dndClass.weaponSummary, 10)
    setFieldSized(form, "TOOL PROF", ch.background.toolProficiency, 10)
    setFieldSized(form, "FEATS", ch.originFeat.name + "\n" + ch.originFeat.description, 10)
    setFieldSized(form, "EQUIPMENT", ch.equipmentSummary, 10)
    setFieldSized(form, "SPECIES TRAITS", ch.species.traits.map(t => s" * $t").mkString("\n"), 10)

    val features = ch.dndClass.level1Features
    val mid = (features.size + 1) / 2
    val col1 = features.take(mid)
    val col2 = features.drop(mid)

    setFieldSized(form, "CLASS FEATURES 1",
      col1.map(f => s" * ${f.name} - ${f.description}").mkString("\n\n"), 10)

    if col2.nonEmpty then
      setFieldSized(form, "CLASS FEATURES 2",
        col2.map(f => s" * ${f.name} - ${f.description}").mkString("\n\n"), 10)
