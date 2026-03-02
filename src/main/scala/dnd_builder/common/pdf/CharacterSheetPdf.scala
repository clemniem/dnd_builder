package dndbuilder.common.pdf

import dndbuilder.dnd.DndTypes.Score
import dndbuilder.dnd.*

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

object CharacterSheetPdf {

  /** Bump this on every change to PDF filling logic so `testPdf()` output is visually distinguishable from stale builds. */
  val testPdfVersion: Int = 35

  /** Font size (pt) for all large content fields: Class Features, Species Traits, Feats, Languages, Equipment, Weapon Prof, Tool Prof, and weapon rows. Change this to adjust all at once. */
  val contentFontSizePt: Int = 14

  /** Approximate characters per visual line in the Class Features PDF columns (for wrap-aware column split). Tune if columns look uneven. */
  private val classFeaturesCharsPerLine: Int = 42

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

  /** Logs every checkbox's physical position (page, x, y) so we can map field names to visual positions. Run: mapCheckboxPositions() in browser console. */
  @JSExportTopLevel("mapCheckboxPositions")
  def mapCheckboxPositions(): Unit =
    PdfLib.loadFromUrl(pdfUrl) { doc =>
      val form = PdfLib.getForm(doc)
      val fields = form.getFields().asInstanceOf[js.Array[js.Dynamic]]
      val entries = scala.collection.mutable.ArrayBuffer.empty[String]
      (0 until fields.length).foreach { i =>
        val field = fields(i)
        val name = field.getName().asInstanceOf[String]
        if name.startsWith("Check Box") then {
          try {
            val widgets = field.acroField.getWidgets().asInstanceOf[js.Array[js.Dynamic]]
            (0 until widgets.length).foreach { w =>
              val rect = widgets(w).getRectangle()
              val x = rect.x.asInstanceOf[Double]
              val y = rect.y.asInstanceOf[Double]
              val page = try {
                val pageRef = widgets(w).P()
                val pages = doc.getPages().asInstanceOf[js.Array[js.Dynamic]]
                (0 until pages.length).find(p => pages(p).ref == pageRef).getOrElse(-1)
              } catch { case _: Throwable => -1 }
              entries += f"""{"name":"$name","page":$page,"x":$x%.1f,"y":$y%.1f}"""
            }
          } catch { case _: Throwable => entries += s"""{"name":"$name","error":"no widgets"}""" }
        }
      }
      org.scalajs.dom.console.log("[\n" + entries.mkString(",\n") + "\n]")
    } { err => org.scalajs.dom.console.error(err) }

  /** Test character for testPdf(): must exercise every filled section (header, combat incl. hit dice, abilities, saves, skills, armor, weapons, spellcasting, currency, feats/traits/class features). When adding new form fields, add filling logic and ensure this character covers them. */
  @JSExportTopLevel("testPdf")
  def generateTest(): Unit = {
    val dragonbornRed = DragonbornOf(DragonAncestry.Red)
    val attackGrants = FeatureGrants.grantsForSpecies(dragonbornRed).attackGrants
    val greataxe = Weapon.all.find(_.name == "Greataxe").get
    val handaxe  = Weapon.all.find(_.name == "Handaxe").get
    val testChar = Character(
      s"Thorn Ironfist v$testPdfVersion",
      dragonbornRed,
      List(ClassLevel(DndClass.Barbarian, 3)),
      Soldier,
      AbilityScores(Score(15), Score(14), Score(13), Score(8), Score(10), Score(12)),
      BackgroundBonus.TwoPlusOne(Ability.Strength, Ability.Constitution),
      Set(Skill.Perception, Skill.Survival, Skill.Athletics),
      None,
      false,
      List(greataxe, handaxe),
      Nil,
      Nil,
      Nil,
      ClassFeatureSelections.empty.withChoices(
        None,
        None,
        None,
        None,
        Set(Skill.Athletics),
        List(greataxe, handaxe),
        None,
        None
      ),
      Some(Subclass.PathOfTheBerserker),
      dragonbornRed.languages,
      Coins(Soldier.startingGold, 0, 0, 0, 0),
      Nil,
      Nil,
      attackGrants
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
        fillSkills(form, doc, ch)
        fillArmorProficiencies(form, ch)
        fillWeapons(form, ch)
        fillSpellcasting(form, ch)
        fillCurrency(form, ch)
        fillProficienciesAndFeatures(form, ch)
        val safeName = ch.name.replaceAll("[^a-zA-Z0-9_\\- ]", "").trim
        val baseName = if safeName.isEmpty then "character-sheet" else safeName
        val filename = s"${baseName}_lv${ch.primaryClassLevel}.pdf"
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

  /**
   * Calibration baseline: targetLines values throughout the code were measured at this font size.
   * Changing contentFontSizePt rescales padding automatically.
   */
  private val referenceFontSizePt: Int = 10

  private def padToLines(text: String, targetLines: Int): String = {
    val currentLines = text.count(_ == '\n') + 1
    val padding = math.max(0, targetLines - currentLines)
    text + "\n" * padding
  }

  /**
   * Central function for big multiline content fields. Takes reference line count (calibrated
   * at 10pt), rescales to the current contentFontSizePt, then pads with newlines and sets.
   */
  private def setContentField(form: js.Dynamic, name: String, value: String, refLines: Int): Unit = {
    val scaledLines = (refLines * referenceFontSizePt) / contentFontSizePt
    val field = PdfLib.getTextField(form, name)
    PdfLib.removeMaxLength(field)
    PdfLib.setFontSize(field, contentFontSizePt)
    PdfLib.setText(field, padToLines(value, scaledLines))
  }

  /**
   * For single-line content fields (Languages, weapon cells). Pads with trailing spaces
   * so the auto-sizer picks a smaller font without adding newlines that push content out.
   * refChars = how many characters the field fits at 10pt.
   */
  private def setContentFieldSingleLine(form: js.Dynamic, name: String, value: String, refChars: Int): Unit = {
    val scaledChars = (refChars * referenceFontSizePt) / contentFontSizePt
    val padding = math.max(0, scaledChars - value.length)
    val field = PdfLib.getTextField(form, name)
    PdfLib.removeMaxLength(field)
    PdfLib.setFontSize(field, contentFontSizePt)
    PdfLib.setText(field, value + " " * padding)
  }

  private def checkBox(form: js.Dynamic, name: String): Unit =
    PdfLib.check(PdfLib.getCheckBox(form, name))

  private def fillHeader(form: js.Dynamic, ch: Character): Unit = {
    setField(form, PdfFormFields.Name, ch.name)
    val classOnly = if ch.classLevels.size == 1 then s"${ch.primaryClass.name} ${ch.primaryClassLevel}" else ch.classLevels.map(cl => s"${cl.dndClass.name} ${cl.classLevel}").mkString(" / ")
    setField(form, PdfFormFields.Class, classOnly)
    setField(form, PdfFormFields.Subclass, ch.subclass.fold("")(_.name))
    setField(form, PdfFormFields.Species, ch.species.displayName)
    setField(form, PdfFormFields.Background, ch.background.name)
    setField(form, PdfFormFields.Level, ch.characterLevel.toString)
    setField(form, PdfFormFields.XPPoints, "0")
  }

  private def fillCombatStats(form: js.Dynamic, ch: Character): Unit = {
    setField(form, PdfFormFields.ArmorClass, ch.armorClass.toString)
    setField(form, PdfFormFields.MaxHP, ch.maxHitPoints.toString)
    setField(form, PdfFormFields.MaxHD, ch.hitDiceString)
    // Current HP, Temp HP, Spent HD: never filled by us; clear any template default so the player fills during play
    setField(form, PdfFormFields.CurrentHP, "")
    setField(form, PdfFormFields.TempHP, "")
    setField(form, PdfFormFields.SpentHD, "")
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

  private def fillSkills(form: js.Dynamic, doc: js.Dynamic, ch: Character): Unit = {
    val page0 = PdfLib.getPage(doc, 0)
    Skill.values.foreach { skill =>
      skillFields.get(skill).foreach(f => setFieldSized(form, f, ch.skillBonus(skill).format, 10))
      ch.skillProficiencyLevel(skill) match {
        case SkillProficiency.Expert =>
          skillCheckboxes.get(skill).foreach(c => checkBox(form, c))
          PdfFormFields.skillCheckboxPositions.get(skill).foreach { case (x, y) =>
            PdfLib.drawFilledCircle(page0, x + 4.0, y + 4.0, 3.5)
          }
        case SkillProficiency.Proficient =>
          skillCheckboxes.get(skill).foreach(c => checkBox(form, c))
        case SkillProficiency.None => ()
      }
    }
  }

  private def fillArmorProficiencies(form: js.Dynamic, ch: Character): Unit = {
    val profs = ch.primaryClass.armorProficiencies
    if profs.contains(ArmorType.Light) then checkBox(form, PdfFormFields.CheckBox33)
    if profs.contains(ArmorType.Medium) then checkBox(form, PdfFormFields.CheckBox34)
    if profs.contains(ArmorType.Heavy) then checkBox(form, PdfFormFields.CheckBox35)
    if profs.contains(ArmorType.Shield) then checkBox(form, PdfFormFields.CheckBox36)
    if ch.equippedShield then checkBox(form, PdfFormFields.ShieldChk)
  }

  private val weaponRowFontSizePt: Int = 10

  private def fillWeapons(form: js.Dynamic, ch: Character): Unit =
    ch.allAttacks.zipWithIndex.foreach { case (atk, idx) =>
      val n = idx + 1
      if n <= 6 then {
        setFieldSized(form, PdfFormFields.nameWeapon(n), atk.name, weaponRowFontSizePt)
        setFieldSized(form, PdfFormFields.bonusWeapon(n), atk.delivery.format, weaponRowFontSizePt)
        setFieldSized(form, PdfFormFields.damageWeapon(n), atk.damage, weaponRowFontSizePt)
        setFieldSized(form, PdfFormFields.notesWeapon(n), atk.notes, weaponRowFontSizePt)
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
      setField(form, name, if n > 0 then n.toString else "")
    }

    val spellRows = allCantrips.map(s => (s.name, "0")) ++ allPreparedLvl1.map(s => (s.name, "1"))
    spellRows.zipWithIndex.foreach { case ((spellName, levelStr), visualPos) =>
      if visualPos < PdfFormFields.spellRowVisualOrder.length then {
        val displayName = s"O $spellName"
        setField(form, PdfFormFields.spellName(visualPos), displayName)
        setField(form, PdfFormFields.spellLevel(visualPos), levelStr)
      }
    }
  }

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
    setContentFieldSingleLine(form, PdfFormFields.Languages, langStr, 40)
    setContentField(form, PdfFormFields.WeaponProf, ch.primaryClass.weaponSummary, 5)
    setContentField(form, PdfFormFields.ToolProf, ch.background.toolProficiency, 2)
    val featText = ch.originFeat.name + ":\n" + ch.originFeat.description
    setContentField(form, PdfFormFields.Feats, featText, 28)
    setContentField(form, PdfFormFields.Equipment, ch.equipmentSummary, 18)
    val traitsText = ch.species.traits.map { t =>
      t.description.fold(s" * ${t.name}")(d => s" * ${t.name} - $d")
    }.mkString("\n")
    setContentField(form, PdfFormFields.SpeciesTraits, traitsText, 25)

    val features = ClassProgression.featuresUpToLevel(ch.primaryClass, ch.primaryClassLevel)
    val (actionable, informative) = FeatureDisplay.splitActionableInformative(features)
    def lineFor(f: Feature): String =
      " * " + f.name + " - " + FeatureDisplay.resolvedDescription(f, ch)
    val actionableLines = actionable.map(lineFor)
    val informativeLines = informative.map(lineFor)
    val lines = actionableLines ++ informativeLines
    // Account for wrapping: long lines occupy multiple visual lines in the PDF, so split by estimated wrapped height
    val wrappedCounts = lines.map { line =>
      val n = (line.length + classFeaturesCharsPerLine - 1) / classFeaturesCharsPerLine
      math.max(1, n)
    }
    val totalWrapped = wrappedCounts.sum
    val halfWrapped = totalWrapped / 2
    val (splitAt, _) =
      if lines.isEmpty then (0, 0)
      else
        (1 until lines.length).foldLeft((1, wrappedCounts(0))) { case ((splitAt, sum), i) =>
          val newSum = sum + wrappedCounts(i)
          if newSum <= halfWrapped then (i + 1, newSum) else (splitAt, sum)
        }
    val col1Text = lines.take(splitAt).mkString("\n")
    val col2Lines = lines.drop(splitAt)

    setContentField(form, PdfFormFields.ClassFeatures1, col1Text, 30)

    if col2Lines.nonEmpty then
      setContentField(form, PdfFormFields.ClassFeatures2, col2Lines.mkString("\n"), 35)
  }
}
