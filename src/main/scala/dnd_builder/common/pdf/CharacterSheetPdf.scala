package dndbuilder.common.pdf

import dndbuilder.dnd.DndTypes.Score
import dndbuilder.dnd.*

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

object CharacterSheetPdf {

  /** Bump this on every change to PDF filling logic so `testPdf()` output is visually distinguishable from stale builds. */
  val testPdfVersion: Int = 7

  /** Font size (pt) for all large content fields: Class Features, Species Traits, Feats, Languages, Equipment, Weapon Prof, Tool Prof, and weapon rows. Change this to adjust all at once. */
  val contentFontSizePt: Int = 10

  private val pdfUrl =
    "https://raw.githubusercontent.com/birddie721/5e2024Builder/main/Character-Sheet.pdf"

  /** Logs all PDF form field names as a single JSON array (easy to copy from console). Run in browser console to find exact names (e.g. for Hit Dice). */
  @JSExportTopLevel("listPdfFields")
  def listPdfFields(): Unit =
    PdfLib.loadFromUrl(pdfUrl) { doc =>
      val form = PdfLib.getForm(doc)
      val names = PdfLib.getFieldNames(form).sorted
      val json = "[" + names.map(n => "\"" + n.replace("\\", "\\\\").replace("\"", "\\\"") + "\"").mkString(", ") + "]"
      org.scalajs.dom.console.log(json)
    } { err => org.scalajs.dom.console.error(err) }

  /** Test character for testPdf(): must exercise every filled section (header, combat incl. hit dice, abilities, saves, skills, armor, weapons, spellcasting, currency, feats/traits/class features). When adding new form fields, add filling logic and ensure this character covers them. */
  @JSExportTopLevel("testPdf")
  def generateTest(): Unit = {
    val testCantrips = (1 to 5).toList.map(i =>
      Spell(f"Spell $i%02d", 0, SpellSchool.Evocation, Set("Cleric"), false)
    )
    val testLvl1Spells = (6 to 30).toList.map(i =>
      Spell(f"Spell $i%02d", 1, SpellSchool.Evocation, Set("Cleric"), false)
    )
    val testChar = Character(
      s"Thorn Ironfist v$testPdfVersion",
      Dwarf,
      List(ClassLevel(Cleric, 1)),
      Soldier,
      AbilityScores(Score(15), Score(14), Score(13), Score(8), Score(10), Score(12)),
      BackgroundBonus.TwoPlusOne(Ability.Strength, Ability.Constitution),
      Set(Skill.Perception, Skill.Survival),
      Some(Armor.all.find(_.name == "Chain Mail").get),
      true,
      List(
        Weapon.all.find(_.name == "Longsword").get,
        Weapon.all.find(_.name == "Handaxe").get
      ),
      testCantrips,
      testLvl1Spells,
      Nil,
      ClassFeatureSelections.empty.withChoices(
        None,
        Some(DivineOrder.Protector),
        None,
        None,
        Set.empty,
        Nil,
        None,
        None
      ),
      None,
      Dwarf.languages,
      Coins(Soldier.startingGold, 0, 0, 0, 0),
      Nil,
      Nil
    )
    generate(testChar)
  }

  def generate(ch: Character): Unit = {
    org.scalajs.dom.console.log("[Export PDF] generate() called")
    PdfLib.loadFromUrl(pdfUrl) { doc =>
      org.scalajs.dom.console.log("[Export PDF] PDF loaded, filling form")
      try {
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
        org.scalajs.dom.console.log("[Export PDF] saveAndOpen called")
      }
      catch {
        case t: Throwable =>
          org.scalajs.dom.console.error("PDF fill/save error:", t)
      }
    } { error =>
      org.scalajs.dom.console.error(s"PDF generation failed: $error")
    }
  }

  private def setField(form: js.Dynamic, name: String, value: String): Unit =
    PdfLib.setText(PdfLib.getTextField(form, name), value)

  private def setFieldSized(form: js.Dynamic, name: String, value: String, pt: Int): Unit = {
    val field = PdfLib.getTextField(form, name)
    PdfLib.removeMaxLength(field)
    PdfLib.setFontSize(field, pt)
    PdfLib.setText(field, value)
  }

  private def padToLines(text: String, targetLines: Int): String = {
    val currentLines = text.count(_ == '\n') + 1
    val padding = math.max(0, targetLines - currentLines)
    text + "\n" * padding
  }

  /** Like setField + padToLines but also sets font size (pt) for content fields. */
  private def setFieldPaddedSized(form: js.Dynamic, name: String, value: String, targetLines: Int, fontSizePt: Int): Unit = {
    val field = PdfLib.getTextField(form, name)
    PdfLib.removeMaxLength(field)
    PdfLib.setFontSize(field, fontSizePt)
    PdfLib.setText(field, padToLines(value, targetLines))
  }

  private def trySetFieldSized(form: js.Dynamic, name: String, value: String, pt: Int): Unit =
    try {
      val field = PdfLib.getTextField(form, name)
      PdfLib.removeMaxLength(field)
      PdfLib.setFontSize(field, pt)
      PdfLib.setText(field, value)
    } catch case _: Throwable => ()

  private def checkBox(form: js.Dynamic, name: String): Unit =
    PdfLib.check(PdfLib.getCheckBox(form, name))

  private def fillHeader(form: js.Dynamic, ch: Character): Unit = {
    setField(form, PdfFormFields.Name, ch.name)
    setField(form, PdfFormFields.Class, ch.classLabel)
    setField(form, PdfFormFields.Species, ch.species.displayName)
    setField(form, PdfFormFields.Background, ch.background.name)
    setField(form, PdfFormFields.Level, ch.characterLevel.toString)
    setField(form, PdfFormFields.XPPoints, "0")
  }

  private def fillCombatStats(form: js.Dynamic, ch: Character): Unit = {
    setField(form, PdfFormFields.ArmorClass, ch.armorClass.toString)
    setField(form, PdfFormFields.MaxHP, ch.maxHitPoints.toString)
    setField(form, PdfFormFields.MaxHD, ch.hitDiceString)
    // Spent HD: left empty for the player to fill during play (count spent since last long rest)
    setFieldSized(form, PdfFormFields.ProfBonus, ch.proficiencyBonus.format, 12)
    setField(form, PdfFormFields.PassivePerception, ch.passivePerception.toString)
    setFieldSized(form, PdfFormFields.Init, ch.initiative.format, 12)
    setField(form, PdfFormFields.Speed, s"${ch.speed}ft")
    setField(form, PdfFormFields.Size, ch.species.size.label)
  }

  private def fillAbilityScores(form: js.Dynamic, ch: Character): Unit = {
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
      setField(form, PdfFormFields.abilityScore(abbr), score.value.toString)
      setFieldSized(form, PdfFormFields.abilityMod(abbr), mod.format, 14)
    }
  }

  private val saveCheckboxes: Map[Ability, String] = Map(
    Ability.Strength     -> PdfFormFields.CheckBox18,
    Ability.Dexterity    -> PdfFormFields.CheckBox11,
    Ability.Constitution -> PdfFormFields.CheckBox7,
    Ability.Intelligence -> PdfFormFields.CheckBox25,
    Ability.Wisdom       -> PdfFormFields.CheckBox17,
    Ability.Charisma     -> PdfFormFields.CheckBox6
  )

  private val saveFields: Map[Ability, String] = Map(
    Ability.Strength     -> PdfFormFields.StrSave,
    Ability.Dexterity    -> PdfFormFields.DexSave,
    Ability.Constitution -> PdfFormFields.ConSave,
    Ability.Intelligence -> PdfFormFields.IntSave,
    Ability.Wisdom       -> PdfFormFields.WisSave,
    Ability.Charisma     -> PdfFormFields.ChaSave
  )

  private def fillSavingThrows(form: js.Dynamic, ch: Character): Unit =
    Ability.values.foreach { ability =>
      val bonus = ch.savingThrowBonus(ability)
      saveFields.get(ability).foreach(f => setFieldSized(form, f, bonus.format, 10))
      if ch.isProficientInSave(ability) then
        saveCheckboxes.get(ability).foreach(c => checkBox(form, c))
    }

  private val skillCheckboxes: Map[Skill, String] = Map(
    Skill.Athletics      -> PdfFormFields.CheckBox19,
    Skill.Acrobatics     -> PdfFormFields.CheckBox8,
    Skill.SleightOfHand  -> PdfFormFields.CheckBox9,
    Skill.Stealth        -> PdfFormFields.CheckBox10,
    Skill.Arcana         -> PdfFormFields.CheckBox24,
    Skill.History        -> PdfFormFields.CheckBox20,
    Skill.Investigation  -> PdfFormFields.CheckBox21,
    Skill.Nature         -> PdfFormFields.CheckBox22,
    Skill.Religion       -> PdfFormFields.CheckBox23,
    Skill.AnimalHandling -> PdfFormFields.CheckBox15,
    Skill.Insight        -> PdfFormFields.CheckBox13,
    Skill.Medicine       -> PdfFormFields.CheckBox12,
    Skill.Perception     -> PdfFormFields.CheckBox14,
    Skill.Survival       -> PdfFormFields.CheckBox16,
    Skill.Deception      -> PdfFormFields.CheckBox5,
    Skill.Intimidation   -> PdfFormFields.CheckBox4,
    Skill.Performance    -> PdfFormFields.CheckBox3,
    Skill.Persuasion     -> PdfFormFields.CheckBox2
  )

  private val skillFields: Map[Skill, String] = Map(
    Skill.Athletics      -> PdfFormFields.Athletics,
    Skill.Acrobatics     -> PdfFormFields.Acrobatics,
    Skill.SleightOfHand  -> PdfFormFields.SleightOfHand,
    Skill.Stealth        -> PdfFormFields.Stealth,
    Skill.Arcana         -> PdfFormFields.Arcana,
    Skill.History        -> PdfFormFields.History,
    Skill.Investigation  -> PdfFormFields.Investigation,
    Skill.Nature         -> PdfFormFields.Nature,
    Skill.Religion       -> PdfFormFields.Religion,
    Skill.AnimalHandling -> PdfFormFields.AnimalHandling,
    Skill.Insight        -> PdfFormFields.Insight,
    Skill.Medicine       -> PdfFormFields.Medicine,
    Skill.Perception     -> PdfFormFields.Perception,
    Skill.Survival       -> PdfFormFields.Survival,
    Skill.Deception      -> PdfFormFields.Deception,
    Skill.Intimidation   -> PdfFormFields.Intimidate,
    Skill.Performance    -> PdfFormFields.Performance,
    Skill.Persuasion     -> PdfFormFields.Persuasion
  )

  private def fillSkills(form: js.Dynamic, ch: Character): Unit =
    Skill.values.foreach { skill =>
      val bonus = ch.skillBonus(skill)
      skillFields.get(skill).foreach(f => setFieldSized(form, f, bonus.format, 10))
      if ch.isSkillProficient(skill) then
        skillCheckboxes.get(skill).foreach(c => checkBox(form, c))
    }

  private def fillArmorProficiencies(form: js.Dynamic, ch: Character): Unit = {
    val profs = ch.primaryClass.armorProficiencies
    if profs.contains(ArmorType.Light) then checkBox(form, PdfFormFields.CheckBox33)
    if profs.contains(ArmorType.Medium) then checkBox(form, PdfFormFields.CheckBox34)
    if profs.contains(ArmorType.Heavy) then checkBox(form, PdfFormFields.CheckBox35)
    if profs.contains(ArmorType.Shield) then checkBox(form, PdfFormFields.CheckBox36)
    if ch.equippedShield then checkBox(form, PdfFormFields.ShieldChk)
  }

  private def fillWeapons(form: js.Dynamic, ch: Character): Unit =
    ch.equippedWeapons.zipWithIndex.foreach { case (weapon, idx) =>
      val n = idx + 1
      if n <= 6 then {
        setFieldSized(form, PdfFormFields.nameWeapon(n), weapon.name, contentFontSizePt)
        setFieldSized(form, PdfFormFields.bonusWeapon(n), ch.weaponAttackBonus(weapon).format, contentFontSizePt)
        setFieldSized(form, PdfFormFields.damageWeapon(n), ch.weaponDamageString(weapon), contentFontSizePt)
        setFieldSized(form, PdfFormFields.notesWeapon(n), ch.weaponPropertiesSummary(weapon), contentFontSizePt)
      }
    }

  private def fillSpellcasting(form: js.Dynamic, ch: Character): Unit = {
    val grantCantrips = ch.spellGrants.flatMap(g => if g.spellLevel == 0 then g.chosen else Nil)
    val grantLvl1 = ch.spellGrants.flatMap(g => if g.spellLevel == 1 then g.chosen else Nil)
    val allCantrips = (ch.chosenCantrips ++ grantCantrips).sortBy(_.name)
    val allPreparedLvl1 = (ch.preparedSpells ++ grantLvl1).sortBy(_.name)

    ch.effectiveSpellcastingAbility.foreach { ability =>
      setField(form, PdfFormFields.SpellcastingAbility, ability.label)
      setField(form, PdfFormFields.SpellcastingMod, ch.modifier(ability).format)
    }
    ch.spellSaveDC.foreach(dc => setField(form, PdfFormFields.SpellSaveDC, dc.toString))
    ch.spellAttackBonus.foreach(atk => setField(form, PdfFormFields.SpellAttackBonus, atk.format))

    val slotsByLevel: List[Int] = ch.spellProgression match {
      case Some(row) => row.slots.padTo(9, 0)
      case None      => List.fill(9)(0)
    }
    PdfFormFields.spellSlotTotals.zip(slotsByLevel).foreach { case (name, n) =>
      if n > 0 then setField(form, name, n.toString)
    }

    val spellRows = allCantrips.map(s => (s.name, "0")) ++ allPreparedLvl1.map(s => (s.name, "1"))
    spellRows.zipWithIndex.foreach { case ((name, levelStr), visualPos) =>
      if visualPos < PdfFormFields.spellRowVisualOrder.length then {
        setField(form, PdfFormFields.spellName(visualPos), name)
        setField(form, PdfFormFields.spellLevel(visualPos), levelStr)
        val fieldIdx = PdfFormFields.spellRowVisualOrder(visualPos)
        if levelStr == "1" && fieldIdx >= 0 then
          tryCheckBox(form, PdfFormFields.spellPreparedCheckBox(fieldIdx))
      }
    }

    PdfFormFields.spellSlotExpendedCheckBoxes.foreach(name => tryUncheckBox(form, name))
  }

  private def tryCheckBox(form: js.Dynamic, name: String): Unit =
    try PdfLib.check(PdfLib.getCheckBox(form, name))
    catch case _: Throwable => ()

  private def tryUncheckBox(form: js.Dynamic, name: String): Unit =
    try PdfLib.uncheck(PdfLib.getCheckBox(form, name))
    catch case t: Throwable =>
      org.scalajs.dom.console.error(s"[PDF] tryUncheckBox failed for $name:", t.getMessage)

  private def fillCurrency(form: js.Dynamic, ch: Character): Unit = {
    def coinValue(n: Int): String = if n == 0 then "" else n.toString
    setField(form, PdfFormFields.GP, coinValue(ch.coins.gp))
    setField(form, PdfFormFields.SP, coinValue(ch.coins.sp))
    setField(form, PdfFormFields.EP, coinValue(ch.coins.ep))
    setField(form, PdfFormFields.CP, coinValue(ch.coins.cp))
    setField(form, PdfFormFields.PP, coinValue(ch.coins.pp))
  }

  private def fillProficienciesAndFeatures(form: js.Dynamic, ch: Character): Unit = {
    val langStr = ch.languages.toList.sortBy(_.label).map(_.label).mkString(", ")
    trySetFieldSized(form, PdfFormFields.Languages, langStr, contentFontSizePt)
    setFieldPaddedSized(form, PdfFormFields.WeaponProf, ch.primaryClass.weaponSummary, 4, contentFontSizePt)
    setFieldPaddedSized(form, PdfFormFields.ToolProf, ch.background.toolProficiency, 2, contentFontSizePt)
    val featText = ch.originFeat.name + ":\n" + ch.originFeat.description
    setFieldPaddedSized(form, PdfFormFields.Feats, featText, 28, contentFontSizePt)
    setFieldPaddedSized(form, PdfFormFields.Equipment, ch.equipmentSummary, 40, contentFontSizePt)
    val traitsText = ch.species.traits.map(t => s" * $t").mkString("\n")
    setFieldPaddedSized(form, PdfFormFields.SpeciesTraits, traitsText, 25, contentFontSizePt)

    val features = ClassProgression.featuresUpToLevel(ch.primaryClass, ch.primaryClassLevel)
    val featureLines = features.map(f => featureLine(f, ch))
    val mid = (featureLines.size + 1) / 2
    val col1Text = featureLines.take(mid).mkString("\n")
    val col2Text = featureLines.drop(mid)

    setFieldPaddedSized(form, PdfFormFields.ClassFeatures1, col1Text, 30, contentFontSizePt)

    if col2Text.nonEmpty then
      setFieldPaddedSized(form, PdfFormFields.ClassFeatures2, col2Text.mkString("\n"), 35, contentFontSizePt)
  }

  private def featureLine(f: ClassFeature, ch: Character): String = {
    val base = f.name + " - "
    val desc = resolvedFeatureDescription(f, ch)
    val tracking = f.uses match {
      case Some(n) => " " + ("o " * n).trim
      case None    => ""
    }
    s" * $base$desc$tracking"
  }

  private def resolvedFeatureDescription(f: ClassFeature, ch: Character): String = {
    val fs = ch.featureSelections
    f.name match {
      case "Fighting Style" =>
        fs.fightingStyle.fold(f.description)(s => s"${s.label} (${s.description})")
      case "Divine Order" =>
        fs.divineOrder.fold(f.description)(o => s"${o.label} (${o.description})")
      case "Primal Order" =>
        fs.primalOrder.fold(f.description)(o => s"${o.label} (${o.description})")
      case "Eldritch Invocations" | "Eldritch Invocation" =>
        fs.eldritchInvocation.fold(f.description)(i => s"${i.label} (${i.description})")
      case "Expertise" =>
        if fs.expertiseSkills.nonEmpty then
          fs.expertiseSkills.toList.sortBy(_.label).map(_.label).mkString(", ")
        else f.description
      case "Weapon Mastery" =>
        if fs.weaponMasteries.nonEmpty then
          fs.weaponMasteries.map(w => s"${w.name} (${w.mastery})").mkString(", ")
        else f.description
      case _ =>
        f.description
    }
  }
}
