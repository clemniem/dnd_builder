package dndbuilder.dnd

import dndbuilder.dnd.DndTypes.{Modifier, Score}
import io.circe.*
import io.circe.syntax.*

object Codecs {

  private def mkEncoder[A](toStr: A => String): Encoder[A] =
    Encoder.encodeString.contramap(toStr)

  private def mkDecoder[A](values: Array[A], toStr: A => String): Decoder[A] =
    Decoder.decodeString.emap { s =>
      values.find(v => toStr(v) == s).toRight(s"Unknown value: $s")
    }

  given Encoder[Ability] = mkEncoder(_.toString)
  given Decoder[Ability] = mkDecoder(Ability.values, _.toString)

  private given Decoder[(Ability, Int)] = Decoder.instance { c =>
    for {
      a <- c.downField("ability").as[Ability]
      n <- c.downField("amount").as[Int]
    } yield (a, n)
  }

  given Encoder[Skill] = mkEncoder(_.toString)
  given Decoder[Skill] = mkDecoder(Skill.values, _.toString)

  given Encoder[HitDie] = mkEncoder(_.toString)
  given Decoder[HitDie] = mkDecoder(HitDie.values, _.toString)

  given Encoder[Size] = mkEncoder(_.toString)
  given Decoder[Size] = mkDecoder(Size.values, _.toString)

  given Encoder[ArmorType] = mkEncoder(_.toString)
  given Decoder[ArmorType] = mkDecoder(ArmorType.values, _.toString)

  given Encoder[WeaponCategory] = mkEncoder(_.toString)
  given Decoder[WeaponCategory] = mkDecoder(WeaponCategory.values, _.toString)
  given Encoder[WeaponRange] = mkEncoder(_.toString)
  given Decoder[WeaponRange] = mkDecoder(WeaponRange.values, _.toString)
  given Encoder[WeaponProperty] = mkEncoder(_.toString)
  given Decoder[WeaponProperty] = mkDecoder(WeaponProperty.values, _.toString)
  given Encoder[MasteryProperty] = mkEncoder(_.toString)
  given Decoder[MasteryProperty] = mkDecoder(MasteryProperty.values, _.toString)

  given Encoder[Weapon] = Encoder.encodeString.contramap(_.name)
  given Decoder[Weapon] = Decoder.decodeString.emap { s =>
    Weapon.byName(s).toRight(s"Unknown weapon: $s")
  }

  given Encoder[Armor] = Encoder.encodeString.contramap(_.name)
  given Decoder[Armor] = Decoder.decodeString.emap { s =>
    Armor.byName(s).toRight(s"Unknown armor: $s")
  }

  given Encoder[WeaponProficiency] = Encoder.instance {
    case WeaponProficiency.AllSimple       => Json.obj("type" -> "AllSimple".asJson)
    case WeaponProficiency.AllMartial      => Json.obj("type" -> "AllMartial".asJson)
    case WeaponProficiency.MartialIf(props) => Json.obj("type" -> "MartialIf".asJson, "props" -> props.toList.asJson)
  }
  given Decoder[WeaponProficiency] = Decoder.instance { c =>
    c.downField("type").as[String].flatMap {
      case "AllSimple"  => Right(WeaponProficiency.AllSimple)
      case "AllMartial" => Right(WeaponProficiency.AllMartial)
      case "MartialIf"  => c.downField("props").as[Set[WeaponProperty]].map(WeaponProficiency.MartialIf.apply)
      case other        => Left(DecodingFailure(s"Unknown WeaponProficiency: $other", c.history))
    }
  }

  given Encoder[SpellList] = mkEncoder(_.toString)
  given Decoder[SpellList] = mkDecoder(SpellList.values, _.toString)

  given Encoder[SpellSchool] = mkEncoder(_.toString)
  given Decoder[SpellSchool] = mkDecoder(SpellSchool.values, _.toString)

  given Encoder[Spell] = Encoder.encodeString.contramap(_.name)
  given Decoder[Spell] = Decoder.decodeString.emap { s =>
    Spell.byName.get(s).toRight(s"Unknown spell: $s")
  }

  given Encoder[ElvenLineage] = mkEncoder(_.toString)
  given Decoder[ElvenLineage] = mkDecoder(ElvenLineage.values, _.toString)

  given Encoder[GnomishLineage] = mkEncoder(_.toString)
  given Decoder[GnomishLineage] = mkDecoder(GnomishLineage.values, _.toString)

  given Encoder[GiantAncestry] = mkEncoder(_.toString)
  given Decoder[GiantAncestry] = mkDecoder(GiantAncestry.values, _.toString)

  given Encoder[FiendishLegacy] = mkEncoder(_.toString)
  given Decoder[FiendishLegacy] = mkDecoder(FiendishLegacy.values, _.toString)

  given Encoder[DragonAncestry] = mkEncoder(_.toString)
  given Decoder[DragonAncestry] = mkDecoder(DragonAncestry.values, _.toString)

  given Encoder[Language] = mkEncoder(_.label)
  given Decoder[Language] = mkDecoder(Language.values, _.label)

  given Encoder[FightingStyle] = mkEncoder(_.label)
  given Decoder[FightingStyle] = mkDecoder(FightingStyle.values, _.label)
  given Encoder[DivineOrder] = mkEncoder(_.label)
  given Decoder[DivineOrder] = mkDecoder(DivineOrder.values, _.label)
  given Encoder[PrimalOrder] = mkEncoder(_.label)
  given Decoder[PrimalOrder] = mkDecoder(PrimalOrder.values, _.label)
  given Encoder[EldritchInvocation] = mkEncoder(_.label)
  given Decoder[EldritchInvocation] = mkDecoder(EldritchInvocation.values, _.label)

  given Encoder[LandType] = mkEncoder(_.label)
  given Decoder[LandType] = mkDecoder(LandType.values, _.label)

  given Encoder[HunterPreyChoice] = mkEncoder(_.label)
  given Decoder[HunterPreyChoice] = mkDecoder(HunterPreyChoice.values, _.label)

  given Encoder[SpellCasterType] = mkEncoder(_.toString)
  given Decoder[SpellCasterType] = mkDecoder(SpellCasterType.values, _.toString)

  given Encoder[FullCasterVariant] = mkEncoder(_.toString)
  given Decoder[FullCasterVariant] = mkDecoder(FullCasterVariant.values, _.toString)

  given Encoder[Subclass] = Encoder.encodeString.contramap(_.name)
  given Decoder[Subclass] = Decoder.decodeString.emap { s =>
    Subclass.byName(s).toRight(s"Unknown subclass: $s")
  }

  given Encoder[AttackKind] = mkEncoder(_.toString)
  given Decoder[AttackKind] = mkDecoder(AttackKind.values, _.toString)

  given Encoder[AttackGrantDelivery] = Encoder.instance {
    case AttackGrantDelivery.MeleeAttack(a)   => Json.obj("type" -> "MeleeAttack".asJson, "ability" -> a.toString.asJson)
    case AttackGrantDelivery.RangedAttack(a)  => Json.obj("type" -> "RangedAttack".asJson, "ability" -> a.toString.asJson)
    case AttackGrantDelivery.SaveDC(save, dc) => Json.obj("type" -> "SaveDC".asJson, "save" -> save.toString.asJson, "dcAbility" -> dc.toString.asJson)
  }
  given Decoder[AttackGrantDelivery] = Decoder.instance { c =>
    c.downField("type").as[String].flatMap {
      case "MeleeAttack"  => c.downField("ability").as[Ability].map(AttackGrantDelivery.MeleeAttack.apply)
      case "RangedAttack" => c.downField("ability").as[Ability].map(AttackGrantDelivery.RangedAttack.apply)
      case "SaveDC"       =>
        for {
          save <- c.downField("save").as[Ability]
          dc   <- c.downField("dcAbility").as[Ability]
        } yield AttackGrantDelivery.SaveDC(save, dc)
      case other => Left(DecodingFailure(s"Unknown AttackGrantDelivery: $other", c.history))
    }
  }

  given Encoder[DiceScaling] = mkEncoder(_.toString)
  given Decoder[DiceScaling] = mkDecoder(DiceScaling.values, _.toString)

  given Encoder[AttackGrant] = Encoder.forProduct9(
    "name", "kind", "baseDamageDice", "damageType", "delivery",
    "diceScaling", "usesPerLR", "range", "sourceLabel"
  )(g => (g.name, g.kind, g.baseDamageDice, g.damageType, g.delivery, g.diceScaling, g.usesPerLR, g.range, g.sourceLabel))
  given Decoder[AttackGrant] = Decoder.instance { c =>
    val diceScalingResult: Decoder.Result[DiceScaling] =
      c.downField("diceScaling").as[Option[DiceScaling]].flatMap {
        case Some(s) => Right(s)
        case None =>
          for {
            cantrip <- c.downField("scalesLikeCantrip").as[Option[Boolean]].map(_.getOrElse(false))
            martial <- c.downField("scalesLikeMartialArts").as[Option[Boolean]].map(_.getOrElse(false))
          } yield if martial then DiceScaling.MartialArts else if cantrip then DiceScaling.Cantrip else DiceScaling.None
      }
    for {
      name           <- c.downField("name").as[String]
      kind           <- c.downField("kind").as[AttackKind]
      baseDamageDice <- c.downField("baseDamageDice").as[String]
      damageType     <- c.downField("damageType").as[String]
      delivery       <- c.downField("delivery").as[AttackGrantDelivery]
      diceScaling    <- diceScalingResult
      usesPerLR      <- c.downField("usesPerLR").as[Boolean]
      range          <- c.downField("range").as[String]
      sourceLabel    <- c.downField("sourceLabel").as[String]
    } yield AttackGrant(name, kind, baseDamageDice, damageType, delivery, diceScaling, usesPerLR, range, sourceLabel)
  }

  given Encoder[FeatureGrant] = Encoder.instance {
    case FeatureGrant.SkillChoice(c, p)           => Json.obj("type" -> "SkillChoice".asJson, "count" -> c.asJson, "pool" -> p.asJson)
    case FeatureGrant.ExpertiseChoice(c)          => Json.obj("type" -> "ExpertiseChoice".asJson, "count" -> c.asJson)
    case FeatureGrant.Attack(g)                   => Json.obj("type" -> "Attack".asJson, "grant" -> g.asJson)
    case FeatureGrant.WeaponMasteryChoice(c)      => Json.obj("type" -> "WeaponMasteryChoice".asJson, "count" -> c.asJson)
    case FeatureGrant.FightingStyleChoice()       => Json.obj("type" -> "FightingStyleChoice".asJson)
    case FeatureGrant.ACFormula(ab)              => Json.obj("type" -> "ACFormula".asJson, "abilities" -> ab.asJson)
    case FeatureGrant.SpellChoice(c, lv, list)    => Json.obj("type" -> "SpellChoice".asJson, "count" -> c.asJson, "level" -> lv.asJson, "list" -> list.asJson)
    case FeatureGrant.HalfProfUnproficientChecks() => Json.obj("type" -> "HalfProfUnproficientChecks".asJson)
    case FeatureGrant.InitiativeBonus()           => Json.obj("type" -> "InitiativeBonus".asJson)
    case FeatureGrant.SubclassGate()              => Json.obj("type" -> "SubclassGate".asJson)
    case FeatureGrant.DivineOrderChoice()          => Json.obj("type" -> "DivineOrderChoice".asJson)
    case FeatureGrant.PrimalOrderChoice()         => Json.obj("type" -> "PrimalOrderChoice".asJson)
    case FeatureGrant.EldritchInvocationChoice()   => Json.obj("type" -> "EldritchInvocationChoice".asJson)
    case FeatureGrant.LandTypeChoice()            => Json.obj("type" -> "LandTypeChoice".asJson)
    case FeatureGrant.HunterPreyChoice()          => Json.obj("type" -> "HunterPreyChoice".asJson)
    case FeatureGrant.ExtraSkillsChoice(c, p)     => Json.obj("type" -> "ExtraSkillsChoice".asJson, "count" -> c.asJson, "pool" -> p.asJson)
  }
  given Decoder[FeatureGrant] = Decoder.instance { c =>
    c.downField("type").as[String].flatMap {
      case "SkillChoice"             => Decoder.forProduct2("count", "pool")(FeatureGrant.SkillChoice.apply).tryDecode(c)
      case "ExpertiseChoice"         => c.downField("count").as[Int].map(FeatureGrant.ExpertiseChoice.apply)
      case "Attack"                  => c.downField("grant").as[AttackGrant].map(FeatureGrant.Attack.apply)
      case "WeaponMasteryChoice"     => c.downField("count").as[Int].map(FeatureGrant.WeaponMasteryChoice.apply)
      case "FightingStyleChoice"    => Right(FeatureGrant.FightingStyleChoice())
      case "ACFormula"              => c.downField("abilities").as[List[Ability]].map(FeatureGrant.ACFormula.apply)
      case "SpellChoice"            => Decoder.forProduct3("count", "level", "list")(FeatureGrant.SpellChoice.apply).tryDecode(c)
      case "HalfProfUnproficientChecks" => Right(FeatureGrant.HalfProfUnproficientChecks())
      case "InitiativeBonus"        => Right(FeatureGrant.InitiativeBonus())
      case "SubclassGate"           => Right(FeatureGrant.SubclassGate())
      case "DivineOrderChoice"      => Right(FeatureGrant.DivineOrderChoice())
      case "PrimalOrderChoice"      => Right(FeatureGrant.PrimalOrderChoice())
      case "EldritchInvocationChoice" => Right(FeatureGrant.EldritchInvocationChoice())
      case "LandTypeChoice"         => Right(FeatureGrant.LandTypeChoice())
      case "HunterPreyChoice"       => Right(FeatureGrant.HunterPreyChoice())
      case "ExtraSkillsChoice"      => Decoder.forProduct2("count", "pool")(FeatureGrant.ExtraSkillsChoice.apply).tryDecode(c)
      case other                    => Left(DecodingFailure(s"Unknown FeatureGrant: $other", c.history))
    }
  }

  given Encoder[Uses] = Encoder.instance {
    case Uses.Static(n)          => Json.obj("type" -> "static".asJson, "n" -> n.asJson)
    case Uses.AbilityMod(ab)     => Json.obj("type" -> "abilityMod".asJson, "ability" -> ab.asJson)
    case Uses.ProfBonus()        => Json.obj("type" -> "profBonus".asJson)
    case Uses.LevelMultiplier(f) => Json.obj("type" -> "levelMul".asJson, "factor" -> f.asJson)
  }
  given Decoder[Uses] = Decoder.instance { c =>
    c.downField("type").as[String].flatMap {
      case "static"     => c.downField("n").as[Int].map(Uses.Static.apply)
      case "abilityMod" => c.downField("ability").as[Ability].map(Uses.AbilityMod.apply)
      case "profBonus"  => Right(Uses.ProfBonus())
      case "levelMul"  => c.downField("factor").as[Int].map(Uses.LevelMultiplier.apply)
      case other        => Left(DecodingFailure(s"Unknown Uses type: $other", c.history))
    }
  }

  private def optionUsesDecoder(using du: Decoder[Uses], di: Decoder[Int]): Decoder[Option[Uses]] =
    Decoder.decodeOption(using du).or(Decoder.decodeOption(using di).map(_.map(Uses.Static.apply)))

  given Encoder[Feature] = Encoder.forProduct6("id", "name", "description", "uses", "grants", "informative")(f => (f.id, f.name, f.description, f.uses, f.grants, f.informative))
  given Decoder[Feature] = Decoder.instance { c =>
    for {
      name        <- c.downField("name").as[String]
      description <- c.downField("description").as[String]
      uses        <- c.downField("uses").as(using optionUsesDecoder)
      id          <- c.downField("id").as[Option[String]].map(_.getOrElse(name.toLowerCase.replace(' ', '-')))
      grants      <- c.downField("grants").as[Option[List[FeatureGrant]]].map(_.getOrElse(Nil))
      informative <- c.downField("informative").as[Option[Boolean]].map(_.getOrElse(false))
    } yield Feature(id, name, description, uses, grants, informative)
  }

  given Encoder[Modifier] = Encoder.encodeInt.contramap(_.toInt)
  given Decoder[Modifier] = Decoder.decodeInt.map(Modifier.apply)

  given Encoder[AttackDelivery] = Encoder.instance {
    case AttackDelivery.AttackRoll(bonus) =>
      Json.obj("type" -> "AttackRoll".asJson, "bonus" -> bonus.toInt.asJson)
    case AttackDelivery.SaveDC(dc, save) =>
      Json.obj("type" -> "SaveDC".asJson, "dc" -> dc.asJson, "save" -> save.toString.asJson)
  }
  given Decoder[AttackDelivery] = Decoder.instance { c =>
    c.downField("type").as[String].flatMap {
      case "AttackRoll" => c.downField("bonus").as[Int].map(b => AttackDelivery.AttackRoll(Modifier(b)))
      case "SaveDC"     =>
        for {
          dc   <- c.downField("dc").as[Int]
          save <- c.downField("save").as[Ability]
        } yield AttackDelivery.SaveDC(dc, save)
      case other => Left(DecodingFailure(s"Unknown AttackDelivery: $other", c.history))
    }
  }

  given Encoder[Attack] = Encoder.forProduct5("name", "kind", "delivery", "damage", "notes")(a =>
    (a.name, a.kind, a.delivery, a.damage, a.notes)
  )
  given Decoder[Attack] = Decoder.forProduct5("name", "kind", "delivery", "damage", "notes")(Attack.apply)

  given Encoder[SpellDelivery] = Encoder.instance {
    case SpellDelivery.RangedAttack     => Json.obj("type" -> "RangedAttack".asJson)
    case SpellDelivery.MeleeAttack      => Json.obj("type" -> "MeleeAttack".asJson)
    case SpellDelivery.Save(ability)    => Json.obj("type" -> "Save".asJson, "ability" -> ability.toString.asJson)
  }
  given Decoder[SpellDelivery] = Decoder.instance { c =>
    c.downField("type").as[String].flatMap {
      case "RangedAttack" => Right(SpellDelivery.RangedAttack)
      case "MeleeAttack"  => Right(SpellDelivery.MeleeAttack)
      case "Save"         => c.downField("ability").as[Ability].map(SpellDelivery.Save.apply)
      case other          => Left(DecodingFailure(s"Unknown SpellDelivery: $other", c.history))
    }
  }

  given Encoder[SpellGrant] = Encoder.instance { g =>
    Json.obj(
      "count"           -> g.count.asJson,
      "spellLevel"      -> g.spellLevel.asJson,
      "spellListLabel"  -> g.spellListLabel.asJson,
      "sourceLabel"     -> g.sourceLabel.asJson,
      "chosen"          -> g.chosen.asJson
    )
  }
  given Decoder[SpellGrant] = Decoder.instance { c =>
    for {
      count          <- c.downField("count").as[Int]
      spellLevel     <- c.downField("spellLevel").as[Int]
      spellListLabel <- c.downField("spellListLabel").as[String]
      sourceLabel    <- c.downField("sourceLabel").as[String]
      chosen         <- c.downField("chosen").as[List[Spell]]
    } yield SpellGrant(count, spellLevel, spellListLabel, sourceLabel, chosen)
  }

  given Encoder[SkillGrant] = Encoder.instance { g =>
    Json.obj(
      "count"       -> g.count.asJson,
      "pool"        -> g.pool.toList.asJson,
      "sourceLabel" -> g.sourceLabel.asJson,
      "chosen"      -> g.chosen.toList.asJson
    )
  }
  given Decoder[SkillGrant] = Decoder.instance { c =>
    for {
      count       <- c.downField("count").as[Int]
      pool        <- c.downField("pool").as[List[Skill]]
      sourceLabel <- c.downField("sourceLabel").as[String]
      chosen      <- c.downField("chosen").as[List[Skill]]
    } yield SkillGrant(count, pool.toSet, sourceLabel, chosen.toSet)
  }

  given Encoder[ClassFeatureSelections] = Encoder.instance { fs =>
    Json.obj(
      "fightingStyle"      -> fs.fightingStyle.asJson,
      "divineOrder"       -> fs.divineOrder.asJson,
      "primalOrder"       -> fs.primalOrder.asJson,
      "eldritchInvocation" -> fs.eldritchInvocation.asJson,
      "expertiseSkills"   -> fs.expertiseSkills.toList.asJson,
      "weaponMasteries"   -> fs.weaponMasteries.map(_.name).asJson,
      "landType"          -> fs.landType.asJson,
      "hunterPrey"        -> fs.hunterPrey.asJson
    )
  }
  given Decoder[ClassFeatureSelections] = Decoder.instance { c =>
    for {
      fightingStyle      <- c.downField("fightingStyle").as[Option[FightingStyle]]
      divineOrder        <- c.downField("divineOrder").as[Option[DivineOrder]]
      primalOrder        <- c.downField("primalOrder").as[Option[PrimalOrder]]
      eldritchInvocation <- c.downField("eldritchInvocation").as[Option[EldritchInvocation]]
      expertiseSkills    <- c.downField("expertiseSkills").as[Option[List[Skill]]].map(_.fold(Set.empty[Skill])(_.toSet))
      weaponMasteries    <- c.downField("weaponMasteries").as[Option[List[String]]].map(_.fold(List.empty[Weapon])(_.flatMap(Weapon.byName)))
      landType           <- c.downField("landType").as[Option[LandType]]
      hunterPrey         <- c.downField("hunterPrey").as[Option[HunterPreyChoice]]
    } yield {
      val selections = List(
        fightingStyle.map(FeatureSelection.FightingStyleChoice.apply),
        divineOrder.map(FeatureSelection.DivineOrderChoice.apply),
        primalOrder.map(FeatureSelection.PrimalOrderChoice.apply),
        eldritchInvocation.map(FeatureSelection.EldritchInvocationChoice.apply),
        Some(FeatureSelection.ExpertiseChoice(expertiseSkills)),
        Some(FeatureSelection.WeaponMasteryChoice(weaponMasteries)),
        landType.map(FeatureSelection.LandTypeChoice.apply),
        hunterPrey.map(FeatureSelection.HunterPreyChoiceSelection.apply)
      ).flatten
      ClassFeatureSelections(selections)
    }
  }

  given Encoder[AbilityScores] = Encoder.instance { s =>
    Json.obj(
      "strength"     -> s.strength.value.asJson,
      "dexterity"    -> s.dexterity.value.asJson,
      "constitution" -> s.constitution.value.asJson,
      "intelligence" -> s.intelligence.value.asJson,
      "wisdom"       -> s.wisdom.value.asJson,
      "charisma"     -> s.charisma.value.asJson
    )
  }

  given Decoder[AbilityScores] = Decoder.instance { c =>
    for {
      str <- c.downField("strength").as[Int]
      dex <- c.downField("dexterity").as[Int]
      con <- c.downField("constitution").as[Int]
      int <- c.downField("intelligence").as[Int]
      wis <- c.downField("wisdom").as[Int]
      cha <- c.downField("charisma").as[Int]
    }
    yield AbilityScores(Score(str), Score(dex), Score(con), Score(int), Score(wis), Score(cha))
  }

  given Encoder[BackgroundBonus] = Encoder.instance {
    case BackgroundBonus.TwoPlusOne(p2, p1) =>
      Json.obj("type" -> "TwoPlusOne".asJson, "plus2" -> p2.asJson, "plus1" -> p1.asJson)
    case BackgroundBonus.ThreePlusOnes(a1, a2, a3) =>
      Json.obj("type" -> "ThreePlusOnes".asJson, "a1" -> a1.asJson, "a2" -> a2.asJson, "a3" -> a3.asJson)
    case BackgroundBonus.Flexible(increases) =>
      Json.obj(
        "type"      -> "Flexible".asJson,
        "increases" -> increases.map { case (a, n) => Json.obj("ability" -> a.asJson, "amount" -> n.asJson) }.asJson
      )
  }

  given Decoder[BackgroundBonus] = Decoder.instance { c =>
    c.downField("type").as[String].flatMap {
      case "TwoPlusOne" =>
        for {
          p2 <- c.downField("plus2").as[Ability]
          p1 <- c.downField("plus1").as[Ability]
        }
        yield BackgroundBonus.TwoPlusOne(p2, p1)
      case "ThreePlusOnes" =>
        for {
          a1 <- c.downField("a1").as[Ability]
          a2 <- c.downField("a2").as[Ability]
          a3 <- c.downField("a3").as[Ability]
        }
        yield BackgroundBonus.ThreePlusOnes(a1, a2, a3)
      case "Flexible" =>
        c.downField("increases").as[List[(Ability, Int)]].map(BackgroundBonus.Flexible.apply)
      case other => Left(DecodingFailure(s"Unknown BackgroundBonus type: $other", c.history))
    }
  }

  given Encoder[Species] = Encoder.instance { sp =>
    val base = Json.obj("species" -> sp.name.asJson)
    sp match {
      case DragonbornOf(a) => base.deepMerge(Json.obj("dragonAncestry" -> a.asJson))
      case Elf(l)          => base.deepMerge(Json.obj("elvenLineage" -> l.asJson))
      case Gnome(l)        => base.deepMerge(Json.obj("gnomishLineage" -> l.asJson))
      case Goliath(a)      => base.deepMerge(Json.obj("giantAncestry" -> a.asJson))
      case Tiefling(l)     => base.deepMerge(Json.obj("fiendishLegacy" -> l.asJson))
      case _               => base
    }
  }

  given Decoder[Species] = Decoder.instance { c =>
    c.downField("species").as[String].flatMap {
      case "Dragonborn" =>
        c.downField("dragonAncestry").as[DragonAncestry]
          .map(DragonbornOf.apply)
          .orElse(Right(Dragonborn))
      case "Dwarf"    => Right(Dwarf)
      case "Elf"      => c.downField("elvenLineage").as[ElvenLineage].map(Elf.apply)
      case "Gnome"    => c.downField("gnomishLineage").as[GnomishLineage].map(Gnome.apply)
      case "Goliath"  => c.downField("giantAncestry").as[GiantAncestry].map(Goliath.apply)
      case "Halfling" => Right(Halfling)
      case "Human"    => Right(Human)
      case "Orc"      => Right(Orc)
      case "Tiefling" => c.downField("fiendishLegacy").as[FiendishLegacy].map(Tiefling.apply)
      case other      => Left(DecodingFailure(s"Unknown species: $other", c.history))
    }
  }

  given Encoder[DndClass] = Encoder.encodeString.contramap(_.name)

  given Decoder[DndClass] = Decoder.decodeString.emap { s =>
    DndClass.all.find(_.name == s).toRight(s"Unknown class: $s")
  }

  given Encoder[OriginFeat] = Encoder.instance {
    case Alert              => Json.obj("feat" -> "Alert".asJson)
    case MagicInitiate(sl)  => Json.obj("feat" -> "MagicInitiate".asJson, "spellList" -> sl.asJson)
    case SavageAttacker     => Json.obj("feat" -> "SavageAttacker".asJson)
    case Skilled            => Json.obj("feat" -> "Skilled".asJson)
  }

  given Decoder[OriginFeat] = Decoder.instance { c =>
    c.downField("feat").as[String].flatMap {
      case "Alert"          => Right(Alert)
      case "MagicInitiate"  => c.downField("spellList").as[SpellList].map(MagicInitiate.apply)
      case "SavageAttacker" => Right(SavageAttacker)
      case "Skilled"        => Right(Skilled)
      case other            => Left(DecodingFailure(s"Unknown feat: $other", c.history))
    }
  }

  given Encoder[Background] = Encoder.encodeString.contramap(_.name)

  given Decoder[Background] = Decoder.decodeString.emap { s =>
    Background.all.find(_.name == s).toRight(s"Unknown background: $s")
  }

  given Encoder[Coins] = Encoder.instance { c =>
    Json.obj(
      "gp" -> c.gp.asJson,
      "sp" -> c.sp.asJson,
      "ep" -> c.ep.asJson,
      "cp" -> c.cp.asJson,
      "pp" -> c.pp.asJson
    )
  }

  given Decoder[Coins] = Decoder.instance { c =>
    for {
      gp <- c.downField("gp").as[Int]
      sp <- c.downField("sp").as[Int]
      ep <- c.downField("ep").as[Int]
      cp <- c.downField("cp").as[Int]
      pp <- c.downField("pp").as[Int]
    } yield Coins(gp, sp, ep, cp, pp)
  }

  given Encoder[ClassLevel] = Encoder.instance { cl =>
    Json.obj("dndClass" -> cl.dndClass.asJson, "classLevel" -> cl.classLevel.asJson)
  }

  given Decoder[ClassLevel] = Decoder.instance { c =>
    for {
      cls <- c.downField("dndClass").as[DndClass]
      lvl <- c.downField("classLevel").as[Int]
    } yield ClassLevel(cls, lvl)
  }

  given Encoder[Character] = Encoder.instance { ch =>
    Json.obj(
      "name"             -> ch.name.asJson,
      "species"          -> ch.species.asJson,
      "classLevels"      -> ch.classLevels.asJson,
      "background"       -> ch.background.asJson,
      "baseScores"       -> ch.baseScores.asJson,
      "backgroundBonus"  -> ch.backgroundBonus.asJson,
      "chosenSkills"     -> ch.chosenSkills.toList.asJson,
      "equippedArmor"    -> ch.equippedArmor.asJson,
      "equippedShield"   -> ch.equippedShield.asJson,
      "equippedWeapons"  -> ch.equippedWeapons.asJson,
      "chosenCantrips"   -> ch.chosenCantrips.asJson,
      "preparedSpells"   -> ch.preparedSpells.asJson,
      "spellbookSpells"  -> ch.spellbookSpells.asJson,
      "featureSelections" -> ch.featureSelections.asJson,
      "subclass"         -> ch.subclass.asJson,
      "languages"        -> ch.languages.toList.asJson,
      "coins"            -> ch.coins.asJson,
      "spellGrants"      -> ch.spellGrants.asJson,
      "skillGrants"      -> ch.skillGrants.asJson,
      "attackGrants"     -> ch.attackGrants.asJson
    
    )
  }

  given Decoder[Character] = Decoder.instance { c =>
    for {
      name   <- c.downField("name").as[String]
      sp     <- c.downField("species").as[Species]
      classLevels <- c.downField("classLevels").as[List[ClassLevel]].flatMap { list =>
        if list.nonEmpty then Right(list)
        else Left(DecodingFailure("classLevels cannot be empty", c.history))
      }
      bg     <- c.downField("background").as[Background]
      scores <- c.downField("baseScores").as[AbilityScores]
      bonus  <- c.downField("backgroundBonus").as[BackgroundBonus]
      skills <- c.downField("chosenSkills").as[List[Skill]]
      armor  <- c.downField("equippedArmor").as[Option[Armor]]
      shield <- c.downField("equippedShield").as[Option[Boolean]].map(_.getOrElse(false))
      weapons <- c.downField("equippedWeapons").as[Option[List[Weapon]]].map(_.getOrElse(Nil))
      cantrips <- c.downField("chosenCantrips").as[Option[List[Spell]]].map(_.getOrElse(Nil))
      prepared <- c.downField("preparedSpells").as[Option[List[Spell]]].map(_.getOrElse(Nil))
      spellbook <- c.downField("spellbookSpells").as[Option[List[Spell]]].map(_.getOrElse(Nil))
      featureSelections <- c.downField("featureSelections").as[Option[ClassFeatureSelections]].map(_.getOrElse(ClassFeatureSelections.empty))
      subclass <- c.downField("subclass").as[Option[Subclass]].orElse(Right(None))
      languages <- c.downField("languages").as[Option[List[Language]]].map(_.fold(sp.languages.toList)(identity))
      coins     <- c.downField("coins").as[Option[Coins]].map(_.getOrElse(Coins(bg.startingGold, 0, 0, 0, 0)))
      spellGrants <- c.downField("spellGrants").as[Option[List[SpellGrant]]].map(_.getOrElse(Nil))
      skillGrants <- c.downField("skillGrants").as[Option[List[SkillGrant]]].map(_.getOrElse(Nil))
      attackGrants <- c.downField("attackGrants").as[Option[List[AttackGrant]]].map(_.getOrElse(Nil))
    } yield {
      val allGrants: List[Grant] = spellGrants ++ skillGrants ++ attackGrants
      Character(name, sp, classLevels, bg, scores, bonus, skills.toSet, armor, shield, weapons, cantrips, prepared, spellbook,
        featureSelections, subclass, languages.toSet, coins, allGrants)
    }
  }
}
