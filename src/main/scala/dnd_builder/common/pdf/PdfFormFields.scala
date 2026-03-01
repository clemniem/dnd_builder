package dndbuilder.common.pdf

/** All form field names for the 5e2024 character sheet PDF. Use these instead of magic strings. */
object PdfFormFields {

  // Header & identity
  val Name            = "Name"
  val Class           = "Class"
  val Species         = "Species"
  val Background      = "Background"
  val Level           = "Level"
  val XPPoints        = "XP Points"
  val Subclass        = "Subclass"

  // Combat
  val ArmorClass       = "Armor Class"
  val MaxHP            = "Max HP"
  val MaxHD            = "Max HD"
  val CurrentHP        = "Current HP"
  val TempHP           = "Temp HP"
  val SpentHD          = "Spent HD"
  val ProfBonus        = "PROF BONUS"
  val PassivePerception = "PASSIVE PERCEPTION"
  val Init             = "init"
  val Speed            = "SPEED"
  val Size             = "SIZE"

  // Ability scores (abbreviation + " SCORE" / " MOD")
  def abilityScore(abbr: String): String = s"$abbr SCORE"
  def abilityMod(abbr: String): String   = s"$abbr MOD"
  val StrScore         = "STR SCORE"
  val StrMod           = "STR MOD"
  val DexScore         = "DEX SCORE"
  val DexMod           = "DEX MOD"
  val ConScore         = "CON SCORE"
  val ConMod           = "CON MOD"
  val IntScore         = "INT SCORE"
  val IntMod           = "INT MOD"
  val WisScore         = "WIS SCORE"
  val WisMod           = "WIS MOD"
  val ChaScore         = "CHA SCORE"
  val ChaMod           = "CHA MOD"

  // Saving throws
  val StrSave          = "STR SAVE"
  val DexSave          = "DEX SAVE"
  val ConSave          = "CON SAVE"
  val IntSave          = "INT SAVE"
  val WisSave          = "Text Field71"
  val ChaSave          = "CHA SAVE"

  // Skills
  val Athletics        = "ATHLETICS"
  val Acrobatics       = "ACROBATICS"
  val SleightOfHand    = "SLEIGHT OF HAND"
  val Stealth          = "STEALTH"
  val Arcana           = "ARCANA"
  val History          = "HISTORY"
  val Investigation    = "INVESTIGATION"
  val Nature           = "NATURE"
  val Religion         = "RELIGION"
  val AnimalHandling   = "ANIMAL HANDLING"
  val Insight          = "INSIGHT"
  val Medicine         = "MEDICINE"
  val Perception       = "PERCEPTION"
  val Survival         = "SURVIVAL"
  val Deception        = "DECEPTION"
  val Intimidate       = "INTIMIDATE"
  val Performance      = "PERFORMANCE"
  val Persuasion       = "PERSUASION"

  // Armor & shield
  val ShieldChk        = "shield chk"

  // Weapons (1–6)
  def nameWeapon(n: Int): String       = s"NAME - WEAPON $n"
  def bonusWeapon(n: Int): String      = s"BONUS/DC - WEAPON $n"
  def damageWeapon(n: Int): String     = s"DAMAGE/TYPE - WEAPON $n"
  def notesWeapon(n: Int): String     = s"NOTES - WEAPON $n"

  // Spellcasting
  val SpellcastingAbility = "SPELLCASTING ABILITY"
  val SpellcastingMod     = "SPELLCASTING MOD"
  val SpellSaveDC         = "SPELL SAVE DC"
  val SpellAttackBonus    = "SPELL ATTACK BONUS"
  val Lvl1Total           = "LVL1 TOTAL"
  val Lvl2Total            = "LVL2 TOTAL"
  val Lvl3Total            = "LVL3 TOTAL"
  val Lvl4Total            = "LVL4 TOTAL"
  val Lvl5Total            = "LVL5 TOTAL"
  val Lvl6Total            = "LVL6 TOTAL"
  val Lvl7Total            = "LVL7 TOTAL"
  val Lvl8Total            = "LVL8 TOTAL"
  val Lvl9Total            = "LVL9 TOTAL"
  def spellSlotTotal(level: Int): String = s"LVL$level TOTAL"
  /**
   * PDF spell rows are NOT rendered in field-index order. The template
   * lays them out as: bare, 0, 1, then groups of three reversed:
   * (4,3,2), (7,6,5), (10,9,8), … (28,27,26).
   * This array maps visual position (0–29) → field index (-1 = bare).
   */
  val spellRowVisualOrder: IndexedSeq[Int] = {
    val first = IndexedSeq(-1, 0, 1)
    val groups = (0 until 9).flatMap { n =>
      IndexedSeq(3 * n + 4, 3 * n + 3, 3 * n + 2)
    }
    first ++ groups
  }

  def spellName(visualPos: Int): String = {
    val idx = spellRowVisualOrder(visualPos)
    if idx < 0 then "SPELL NAME" else s"SPELL NAME$idx"
  }
  def spellLevel(visualPos: Int): String = {
    val idx = spellRowVisualOrder(visualPos)
    if idx < 0 then "SPELL LEVEL" else s"SPELL LEVEL$idx"
  }

  // Spell slots total field names (ordered 1–9)
  val spellSlotTotals: List[String]    = List(Lvl1Total, Lvl2Total, Lvl3Total, Lvl4Total, Lvl5Total, Lvl6Total, Lvl7Total, Lvl8Total, Lvl9Total)

  // Checkboxes (by index)
  def checkBox(i: Int): String         = s"Check Box$i"

  // ---------------------------------------------------------------------------
  // Spell slot EXPENDED diamonds (y ≈ 650–680, above the spell table).
  // These are for the player to mark during play — never touch them in code.
  // ---------------------------------------------------------------------------
  // Level 1 (4): 52, 46, 40, 37     Level 4 (3): 50, 53, 39     Level 7 (2): 55, 56
  // Level 2 (3): 49, 38, 43         Level 5 (3): 48, 45, 42     Level 8 (1): 58
  // Level 3 (3): 47, 44, 41         Level 6 (2): 51, 54         Level 9 (1): 57

  // ---------------------------------------------------------------------------
  // Spell table C/R/M diamonds (x ≈ 239/261/283, 30 rows top→bottom).
  // These mark Concentration, Ritual, and Required Material — not filled by us.
  // ---------------------------------------------------------------------------
  // C column (x≈239): 0, 64, 67, 70, 73, 76, 79, 85, 82, 88, 91, 94, 97,
  //   100, 103, 106, 109, 112, 115, 118, 121, 124, 127, 130, 133, 136, 139, 142, 145, 148
  // R column (x≈261): 59, 65, 68, 71, 74, 77, 80, 86, 83, 89, 92, 95, 98,
  //   101, 104, 107, 110, 113, 116, 119, 122, 125, 128, 131, 134, 137, 140, 143, 146 (149 missing)
  // M column (x≈283): 60, 66, 69, 72, 75, 78, 81, 87, 84, 90, 93, 96, 99,
  //   102, 105, 108, 111, 114, 117, 120, 123, 126, 129, 132, 135, 138, 141, 144, 147, 150

  // NOTE: This template has NO "prepared" checkbox column. The spell table title
  // is "CANTRIPS & PREPARED SPELLS" — being listed IS being prepared.

  // Armor proficiency checkboxes
  val CheckBox33 = "Check Box33"  // Light
  val CheckBox34 = "Check Box34"  // Medium
  val CheckBox35 = "Check Box35"  // Heavy
  val CheckBox36 = "Check Box36"  // Shield

  // Saving throw checkboxes
  val CheckBox18 = "Check Box18"   // STR
  val CheckBox11 = "Check Box11"   // DEX
  val CheckBox7  = "Check Box7"    // CON
  val CheckBox25 = "Check Box25"  // INT
  val CheckBox17 = "Check Box17"  // WIS
  val CheckBox6  = "Check Box6"    // CHA

  // Skill checkboxes (name, page-0 x, page-0 y from mapCheckboxPositions)
  val CheckBox19 = "Check Box19"  // Athletics
  val CheckBox8  = "Check Box8"   // Acrobatics
  val CheckBox9  = "Check Box9"   // Sleight of Hand
  val CheckBox10 = "Check Box10"  // Stealth
  val CheckBox24 = "Check Box24"  // Arcana
  val CheckBox20 = "Check Box20"  // History
  val CheckBox21 = "Check Box21"  // Investigation
  val CheckBox22 = "Check Box22"  // Nature
  val CheckBox23 = "Check Box23"  // Religion
  val CheckBox15 = "Check Box15"  // Animal Handling
  val CheckBox13 = "Check Box13"  // Insight
  val CheckBox12 = "Check Box12"  // Medicine
  val CheckBox14 = "Check Box14"  // Perception
  val CheckBox16 = "Check Box16"  // Survival
  val CheckBox5  = "Check Box5"   // Deception
  val CheckBox4  = "Check Box4"   // Intimidation
  val CheckBox3  = "Check Box3"   // Performance
  val CheckBox2  = "Check Box2"   // Persuasion

  /** Physical (x, y) position of each skill checkbox on page 0, for drawing expertise filled circles. */
  import dndbuilder.dnd.Skill
  val skillCheckboxPositions: Map[Skill, (Double, Double)] = Map(
    Skill.Athletics      -> (17.8, 485.1),
    Skill.Acrobatics     -> (17.7, 367.3),
    Skill.SleightOfHand  -> (17.6, 353.3),
    Skill.Stealth        -> (17.7, 339.3),
    Skill.Arcana         -> (123.8, 562.4),
    Skill.History        -> (123.8, 548.5),
    Skill.Investigation  -> (123.8, 534.3),
    Skill.Nature         -> (123.8, 520.4),
    Skill.Religion       -> (123.9, 506.3),
    Skill.AnimalHandling -> (123.8, 388.4),
    Skill.Insight        -> (123.9, 374.4),
    Skill.Medicine       -> (123.9, 360.5),
    Skill.Perception     -> (123.9, 346.5),
    Skill.Survival       -> (123.9, 332.4),
    Skill.Deception      -> (123.7, 214.7),
    Skill.Intimidation   -> (123.8, 200.5),
    Skill.Performance    -> (123.8, 186.5),
    Skill.Persuasion     -> (123.8, 172.3)
  )

  // Currency
  val GP = "GP"
  val SP = "SP"
  val EP = "EP"
  val CP = "CP"
  val PP = "PP"

  // Proficiencies & features
  val Languages     = "LANGUAGES"
  val WeaponProf    = "WEAPON PROF"
  val ToolProf      = "TOOL PROF"
  val Feats         = "FEATS"
  val Equipment     = "EQUIPMENT"
  val SpeciesTraits = "SPECIES TRAITS"
  val ClassFeatures1 = "CLASS FEATURES 1"
  val ClassFeatures2 = "CLASS FEATURES 2"
}
