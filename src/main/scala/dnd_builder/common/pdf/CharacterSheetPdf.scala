package dndbuilder.common.pdf

import dndbuilder.dnd.DndTypes.Score
import dndbuilder.dnd.*

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

object CharacterSheetPdf {

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
    val testChar = Character(
      "Thorn Ironfist",
      Dwarf,
      List(ClassLevel(Fighter, 1)),
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
      Nil, Nil, Nil,
      ClassFeatureSelections(
        fightingStyle = Some(FightingStyle.Defense),
        divineOrder = None,
        primalOrder = None,
        eldritchInvocation = None,
        expertiseSkills = Set.empty,
        weaponMasteries = List(
          Weapon.byName("Longsword").get,
          Weapon.byName("Handaxe").get,
          Weapon.byName("Greatsword").get
        ),
        landType = None,
        hunterPrey = None
      ),
      None,
      Dwarf.languages,
      Coins(Soldier.startingGold, 0, 0, 0, 0)
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

  private def setFieldPadded(form: js.Dynamic, name: String, value: String, targetLines: Int): Unit = {
    val field = PdfLib.getTextField(form, name)
    PdfLib.removeMaxLength(field)
    PdfLib.setText(field, padToLines(value, targetLines))
  }

  /** Like setFieldPadded but sets font size (pt) so text is a bit larger (e.g. tools/equipment). */
  private def setFieldPaddedSized(form: js.Dynamic, name: String, value: String, targetLines: Int, fontSizePt: Int): Unit = {
    val field = PdfLib.getTextField(form, name)
    PdfLib.removeMaxLength(field)
    PdfLib.setFontSize(field, fontSizePt)
    PdfLib.setText(field, padToLines(value, targetLines))
  }

  private def checkBox(form: js.Dynamic, name: String): Unit =
    PdfLib.check(PdfLib.getCheckBox(form, name))

  private def fillHeader(form: js.Dynamic, ch: Character): Unit = {
    setField(form, "Name", ch.name)
    setField(form, "Class", ch.classLabel)
    setField(form, "Species", ch.species.displayName)
    setField(form, "Background", ch.background.name)
    setField(form, "Level", ch.characterLevel.toString)
    setField(form, "XP Points", "0")
  }

  private def fillCombatStats(form: js.Dynamic, ch: Character): Unit = {
    setField(form, "Armor Class", ch.armorClass.toString)
    setField(form, "Max HP", ch.maxHitPoints.toString)
    setField(form, "Max HD", ch.hitDiceString)
    // Spent HD: left empty for the player to fill during play (count spent since last long rest)
    setFieldSized(form, "PROF BONUS", ch.proficiencyBonus.format, 12)
    setField(form, "PASSIVE PERCEPTION", ch.passivePerception.toString)
    setFieldSized(form, "init", ch.initiative.format, 12)
    setField(form, "SPEED", s"${ch.speed}ft")
    setField(form, "SIZE", ch.species.size.label)
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
      setField(form, s"$abbr SCORE", score.value.toString)
      setFieldSized(form, s"$abbr MOD", mod.format, 14)
    }
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
      saveFields.get(ability).foreach(f => setFieldSized(form, f, bonus.format, 10))
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
      skillFields.get(skill).foreach(f => setFieldSized(form, f, bonus.format, 10))
      if ch.isSkillProficient(skill) then
        skillCheckboxes.get(skill).foreach(c => checkBox(form, c))
    }

  private def fillArmorProficiencies(form: js.Dynamic, ch: Character): Unit = {
    val profs = ch.primaryClass.armorProficiencies
    if profs.contains(ArmorType.Light) then checkBox(form, "Check Box33")
    if profs.contains(ArmorType.Medium) then checkBox(form, "Check Box34")
    if profs.contains(ArmorType.Heavy) then checkBox(form, "Check Box35")
    if profs.contains(ArmorType.Shield) then checkBox(form, "Check Box36")
    if ch.equippedShield then checkBox(form, "shield chk")
  }

  private def fillWeapons(form: js.Dynamic, ch: Character): Unit =
    ch.equippedWeapons.zipWithIndex.foreach { case (weapon, idx) =>
      val n = idx + 1
      if n <= 6 then {
        setField(form, s"NAME - WEAPON $n", weapon.name)
        setField(form, s"BONUS/DC - WEAPON $n", ch.weaponAttackBonus(weapon).format)
        setField(form, s"DAMAGE/TYPE - WEAPON $n", ch.weaponDamageString(weapon))
        setField(form, s"NOTES - WEAPON $n", ch.weaponPropertiesSummary(weapon))
      }
    }

  private def fillSpellcasting(form: js.Dynamic, ch: Character): Unit = {
    ch.primaryClass.spellcastingAbility.foreach { ability =>
      setField(form, "SPELLCASTING ABILITY", ability.label)
      setField(form, "SPELLCASTING MOD", ch.modifier(ability).format)
      ch.spellSaveDC.foreach(dc => setField(form, "SPELL SAVE DC", dc.toString))
      ch.spellAttackBonus.foreach(b => setField(form, "SPELL ATTACK BONUS", b.format))
    }
    ch.chosenCantrips.zipWithIndex.foreach { case (spell, idx) =>
      trySetField(form, s"CANTRIP ${idx + 1}", spell.name)
    }
    ch.preparedSpells.zipWithIndex.foreach { case (spell, idx) =>
      trySetField(form, s"SPELL LVL1 ${idx + 1}", spell.name)
    }
    ch.spellProgression.foreach { row =>
      row.slots.zipWithIndex.foreach { case (n, i) =>
        if n > 0 then trySetField(form, s"SPELL SLOTS LVL${i + 1}", n.toString)
      }
    }
  }

  private def trySetField(form: js.Dynamic, name: String, value: String): Unit =
    try setField(form, name, value)
    catch case _: Throwable => ()

  private def fillCurrency(form: js.Dynamic, ch: Character): Unit = {
    def coinValue(n: Int): String = if n == 0 then "" else n.toString
    setField(form, "GP", coinValue(ch.coins.gp))
    setField(form, "SP", coinValue(ch.coins.sp))
    setField(form, "EP", coinValue(ch.coins.ep))
    setField(form, "CP", coinValue(ch.coins.cp))
    setField(form, "PP", coinValue(ch.coins.pp))
  }

  private def fillProficienciesAndFeatures(form: js.Dynamic, ch: Character): Unit = {
    val langStr = ch.languages.toList.sortBy(_.label).map(_.label).mkString(", ")
    trySetField(form, "LANGUAGES", langStr)
    setFieldPadded(form, "WEAPON PROF", ch.primaryClass.weaponSummary, 4)
    setFieldPaddedSized(form, "TOOL PROF", ch.background.toolProficiency, 2, 11)
    val featText = ch.originFeat.name + ":\n" + ch.originFeat.description
    setFieldPadded(form, "FEATS", featText, 28)
    setFieldPaddedSized(form, "EQUIPMENT", ch.equipmentSummary, 40, 11)
    val traitsText = ch.species.traits.map(t => s" * $t").mkString("\n")
    setFieldPadded(form, "SPECIES TRAITS", traitsText, 25)

    val features = ClassProgression.featuresUpToLevel(ch.primaryClass, ch.primaryClassLevel)
    val featureLines = features.map(f => featureLine(f, ch))
    val mid = (featureLines.size + 1) / 2
    val col1Text = featureLines.take(mid).mkString("\n")
    val col2Text = featureLines.drop(mid)

    setFieldPadded(form, "CLASS FEATURES 1", col1Text, 30)

    if col2Text.nonEmpty then
      setFieldPadded(form, "CLASS FEATURES 2", col2Text.mkString("\n"), 35)
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
