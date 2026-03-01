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
  def spellName(i: Int): String        = s"SPELL NAME$i"
  def spellLevel(i: Int): String      = s"SPELL LEVEL$i"

  // Spell slots total field names (ordered 1–9)
  val spellSlotTotals: List[String]    = List(Lvl1Total, Lvl2Total, Lvl3Total, Lvl4Total, Lvl5Total, Lvl6Total, Lvl7Total, Lvl8Total, Lvl9Total)

  // Checkboxes (by index)
  def checkBox(i: Int): String         = s"Check Box$i"
  /** Prepared tickbox for spell row i (0–28) in CANTRIPS & PREPARED SPELLS table. */
  def spellPreparedCheckBox(rowIndex: Int): String = checkBox(37 + rowIndex)

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

  // Skill checkboxes
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
