package dndbuilder.dnd

import dndbuilder.dnd.DndTypes.Score
import munit.FunSuite

/** Unit tests for scaledDice validation in Character (exercised via allAttacks when grants use DiceScaling.Cantrip). */
class CharacterScaledDiceSuite extends FunSuite {

  private def minimalCharacter(attackGrants: List[AttackGrant]): Character = {
    val threeWeapons = Weapon.all.take(3)
    val fs = ClassFeatureSelections.empty.withChoices(
      Some(FightingStyle.Archery),
      None,
      None,
      None,
      Set.empty,
      threeWeapons,
      None,
      None
    )
    Character(
      "Test",
      Dwarf,
      List(ClassLevel(DndClass.Fighter, 1)),
      Soldier,
      AbilityScores(Score(10), Score(10), Score(10), Score(10), Score(10), Score(10)),
      BackgroundBonus.TwoPlusOne(Ability.Strength, Ability.Constitution),
      Set(Skill.Acrobatics, Skill.Perception),
      None,
      false,
      Nil,
      Nil,
      Nil,
      Nil,
      fs,
      None,
      Dwarf.languages,
      Coins(Soldier.startingGold, 0, 0, 0, 0),
      Nil,
      Nil,
      attackGrants
    )
  }

  private def cantripGrant(baseDamageDice: String): AttackGrant =
    AttackGrant(
      "Test",
      AttackKind.Spell,
      baseDamageDice,
      "fire",
      AttackGrantDelivery.SaveDC(Ability.Dexterity, Ability.Constitution),
      DiceScaling.Cantrip,
      false,
      "",
      "Test"
    )

  test("valid dice string 1d6 produces scaled damage in allAttacks") {
    val ch = minimalCharacter(List(cantripGrant("1d6")))
    val attacks = ch.allAttacks
    val grantAttack = attacks.find(_.name == "Test").get
    assert(grantAttack.damage.contains("d6"), clue(grantAttack.damage))
  }

  test("valid dice string 2d8 produces scaled damage in allAttacks") {
    val ch = minimalCharacter(List(cantripGrant("2d8")))
    val attacks = ch.allAttacks
    val grantAttack = attacks.find(_.name == "Test").get
    assert(grantAttack.damage.contains("d8"), clue(grantAttack.damage))
  }

  test("invalid dice string with non-digit before d (X1d6) throws with clear message") {
    val ch = minimalCharacter(List(cantripGrant("X1d6")))
    val ex = intercept[IllegalArgumentException](ch.allAttacks)
    assert(ex.getMessage.contains("Invalid dice format"), clue(ex.getMessage))
    assert(ex.getMessage.contains("X1d6"), clue(ex.getMessage))
  }

  test("invalid dice string with non-digit after d (1dX) throws with clear message") {
    val ch = minimalCharacter(List(cantripGrant("1dX")))
    val ex = intercept[IllegalArgumentException](ch.allAttacks)
    assert(ex.getMessage.contains("Invalid dice format"), clue(ex.getMessage))
    assert(ex.getMessage.contains("1dX"), clue(ex.getMessage))
  }

  test("invalid dice string with no digits before d (d6) throws") {
    val ch = minimalCharacter(List(cantripGrant("d6")))
    val ex = intercept[IllegalArgumentException](ch.allAttacks)
    assert(ex.getMessage.contains("Invalid dice format"), clue(ex.getMessage))
  }

  test("invalid dice string with nothing after d (1d) throws") {
    val ch = minimalCharacter(List(cantripGrant("1d")))
    val ex = intercept[IllegalArgumentException](ch.allAttacks)
    assert(ex.getMessage.contains("Invalid dice format"), clue(ex.getMessage))
  }
}
