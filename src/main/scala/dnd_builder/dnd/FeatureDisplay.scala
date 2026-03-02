package dndbuilder.dnd

import scala.util.matching.Regex

/** Shared formatting and value resolution for class feature text (actionable vs informative, O circles, placeholders). */
object FeatureDisplay {

  private val usePhrasePrefix: Regex = """^\d+ use(s)?\.? ?(per (Short/Long|Short|Long) rest\.?)? ?""".r

  /** Raw description as defined — for web-page general info. No circles, no placeholder resolution. */
  def generalDescription(f: Feature): String = f.description

  /** Character-resolved description: placeholders filled, actionable uses shown as "O O per LR - ...". */
  def resolvedDescription(f: Feature, ch: Character): String = {
    val desc = descriptionForDisplay(f, ch)
    descriptionWithUsesNotation(f, desc, Some(ch))
  }

  /** Splits features into actionable (false informative) and informative (true). */
  def splitActionableInformative(features: List[Feature]): (List[Feature], List[Feature]) =
    features.partition(!_.informative)

  private def stripLeadingUsesPhrase(description: String): String =
    usePhrasePrefix.replaceFirstIn(description.trim, "").trim

  private def inferPerRest(description: String): String =
    if description.contains("Short/Long") then "SR/LR"
    else if description.contains("Short Rest") then "SR"
    else "LR"

  private def descriptionForDisplay(f: Feature, ch: Character): String =
    resolveDescription(choiceSubstitutedDescription(f, ch), ch)

  private def choiceSubstitutedDescription(f: Feature, ch: Character): String = {
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

  private def resolveDescription(description: String, ch: Character): String = {
    val level = ch.primaryClassLevel
    val prof = ch.proficiencyBonus.toInt
    val halfProf = prof / 2

    // Ability modifiers: replace both "X mod" and "X Mod" so we catch any casing
    val abilityPairs: List[(String, String)] = List(
      "CHA mod" -> ch.modifier(Ability.Charisma).toInt.toString,
      "WIS mod"  -> ch.modifier(Ability.Wisdom).toInt.toString,
      "INT mod"  -> ch.modifier(Ability.Intelligence).toInt.toString,
      "DEX mod"  -> ch.modifier(Ability.Dexterity).toInt.toString,
      "STR mod"  -> ch.modifier(Ability.Strength).toInt.toString,
      "CON mod"  -> ch.modifier(Ability.Constitution).toInt.toString
    )
    val afterAbility = abilityPairs.foldLeft(description) { case (acc, (phrase, value)) =>
      val capMod = phrase.take(3) + " Mod"  // e.g. "CHA Mod"
      acc.replace(phrase, value).replace(capMod, value)
    }

    val levelReplacements: List[(String, String)] = List(
      "Fighter level", "Wizard level", "Paladin level", "Bard level", "Barbarian level",
      "Cleric level", "Druid level", "Ranger level", "Rogue level", "Sorcerer level",
      "Warlock level", "Monk level"
    ).map(_ -> level.toString)

    // Order matters: longer phrases first so "half your proficiency bonus" before "proficiency bonus"
    val otherReplacements: List[(String, String)] = List(
      "half your proficiency bonus" -> halfProf.toString,
      "proficiency bonus"           -> prof.toString,
      "prof bonus"                  -> prof.toString
    )

    val all = otherReplacements ++ levelReplacements
    all.foldLeft(afterAbility) { case (acc, (from, to)) => acc.replace(from, to) }
  }

  private def resolveUses(u: Uses, ch: Character): Int = u match {
    case Uses.Static(n)          => n
    case Uses.AbilityMod(ab)     => math.max(1, ch.modifier(ab).toInt)
    case Uses.ProfBonus()        => ch.proficiencyBonus.toInt
    case Uses.LevelMultiplier(f) => f * ch.primaryClassLevel
  }

  private def descriptionWithUsesNotation(
      f: Feature, resolvedDesc: String, ch: Option[Character]
  ): String = {
    val usesCount: Option[Int] = f.uses.flatMap(u => ch.map(c => resolveUses(u, c)))
    if !f.informative && usesCount.isDefined then {
      val n = usesCount.get
      val circles = ("O " * n).trim
      val perRest = inferPerRest(resolvedDesc)
      val stripped = stripLeadingUsesPhrase(resolvedDesc)
      s"$circles per $perRest - $stripped"
    } else
      resolvedDesc
  }
}
