package dndbuilder.dnd

import io.circe.*
import io.circe.syntax.*

object ConfigLoader {
  import Codecs.given

  given dndClassContentEncoder: Encoder[DndClass] = Encoder.forProduct20(
    "name", "hitDie", "primaryAbilities", "savingThrows", "armorProficiencies", "weaponProficiencies",
    "skillPool", "numSkillChoices", "spellcastingAbility", "spellCasterType", "fullCasterVariant",
    "level1Features", "description", "recommendedScores", "weaponMasteryCount", "extraLanguageChoices",
    "unarmoredDefenseAbility", "usesSpellbook", "jackOfAllTradesAtLevel", "grantedAttacks"
  )(d => (
    d.name, d.hitDie, d.primaryAbilities, d.savingThrows, d.armorProficiencies, d.weaponProficiencies,
    d.skillPool, d.numSkillChoices, d.spellcastingAbility, d.spellCasterType, d.fullCasterVariant,
    d.level1Features, d.description, d.recommendedScores, d.weaponMasteryCount, d.extraLanguageChoices,
    d.unarmoredDefenseAbility, d.usesSpellbook, d.jackOfAllTradesAtLevel, d.grantedAttacks
  ))

  given dndClassContentDecoder: Decoder[DndClass] = Decoder.instance { c =>
    for {
      name                    <- c.downField("name").as[String]
      hitDie                  <- c.downField("hitDie").as[HitDie]
      primaryAbilities        <- c.downField("primaryAbilities").as[List[Ability]]
      savingThrows            <- c.downField("savingThrows").as[Set[Ability]]
      armorProficiencies      <- c.downField("armorProficiencies").as[Set[ArmorType]]
      weaponProficiencies     <- c.downField("weaponProficiencies").as[Set[WeaponProficiency]]
      skillPool               <- c.downField("skillPool").as[Set[Skill]]
      numSkillChoices         <- c.downField("numSkillChoices").as[Int]
      spellcastingAbility     <- c.downField("spellcastingAbility").as[Option[Ability]]
      spellCasterType         <- c.downField("spellCasterType").as[SpellCasterType]
      fullCasterVariant       <- c.downField("fullCasterVariant").as[Option[FullCasterVariant]]
      level1Features         <- c.downField("level1Features").as[List[ClassFeature]]
      description             <- c.downField("description").as[String]
      recommendedScores      <- c.downField("recommendedScores").as[AbilityScores]
      weaponMasteryCount     <- c.downField("weaponMasteryCount").as[Int]
      extraLanguageChoices   <- c.downField("extraLanguageChoices").as[Int]
      unarmoredDefenseAbility <- c.downField("unarmoredDefenseAbility").as[Option[Ability]]
      usesSpellbook          <- c.downField("usesSpellbook").as[Boolean]
      jackOfAllTradesAtLevel <- c.downField("jackOfAllTradesAtLevel").as[Option[Int]]
      grantedAttacks         <- c.downField("grantedAttacks").as[Option[List[AttackGrant]]].map(_.getOrElse(Nil))
    } yield DndClass(
      name = name,
      hitDie = hitDie,
      primaryAbilities = primaryAbilities,
      savingThrows = savingThrows,
      armorProficiencies = armorProficiencies,
      weaponProficiencies = weaponProficiencies,
      skillPool = skillPool,
      numSkillChoices = numSkillChoices,
      spellcastingAbility = spellcastingAbility,
      spellCasterType = spellCasterType,
      fullCasterVariant = fullCasterVariant,
      level1Features = level1Features,
      description = description,
      recommendedScores = recommendedScores,
      weaponMasteryCount = weaponMasteryCount,
      extraLanguageChoices = extraLanguageChoices,
      unarmoredDefenseAbility = unarmoredDefenseAbility,
      usesSpellbook = usesSpellbook,
      jackOfAllTradesAtLevel = jackOfAllTradesAtLevel,
      grantedAttacks = grantedAttacks
    )
  }

  given Encoder[RuleSet] = Encoder.instance { r =>
    Json.obj(
      "id" -> r.id.asJson,
      "name" -> r.name.asJson,
      "classes" -> r.classes.asJson(using Encoder.encodeList(using dndClassContentEncoder))
    )
  }
  given Decoder[RuleSet] = Decoder.instance { c =>
    for {
      id      <- c.downField("id").as[String]
      name    <- c.downField("name").as[String]
      classes <- c.downField("classes").as[List[DndClass]](using Decoder.decodeList(using dndClassContentDecoder))
    } yield RuleSet(id, name, classes)
  }
}
