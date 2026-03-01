package dndbuilder.dnd

import munit.FunSuite

/** Sanity tests for grant-based choice extraction (LevelChoice removed in Phase C5). */
class GrantChoiceEquivalenceSuite extends FunSuite {

  test("Barbarian level 1: 2 weapon mastery, no expertise") {
    val grants = ClassProgression.grantChoicesUpToLevel(DndClass.Barbarian, 1)
    assertEquals(ClassProgression.weaponMasteryCountFromGrants(grants), 2)
    assertEquals(ClassProgression.expertiseCountFromGrants(grants), 0)
  }

  test("Rogue level 1: 2 expertise, 2 weapon mastery") {
    val grants = ClassProgression.grantChoicesUpToLevel(DndClass.Rogue, 1)
    assertEquals(ClassProgression.expertiseCountFromGrants(grants), 2)
    assertEquals(ClassProgression.weaponMasteryCountFromGrants(grants), 2)
  }

  test("Ranger level 3: has SubclassGate and HunterPreyChoice") {
    val grants = ClassProgression.grantChoicesUpToLevel(DndClass.Ranger, 3)
    assert(grants.exists { case FeatureGrant.SubclassGate() => true; case _ => false })
    assert(grants.exists { case FeatureGrant.HunterPreyChoice() => true; case _ => false })
  }

  test("Druid level 3: has SubclassGate and LandTypeChoice") {
    val grants = ClassProgression.grantChoicesUpToLevel(DndClass.Druid, 3)
    assert(grants.exists { case FeatureGrant.SubclassGate() => true; case _ => false })
    assert(grants.exists { case FeatureGrant.LandTypeChoice() => true; case _ => false })
  }

  test("Bard level 3: has ExtraSkillsChoice(3, _)") {
    val grants = ClassProgression.grantChoicesUpToLevel(DndClass.Bard, 3)
    val extra = grants.collect { case FeatureGrant.ExtraSkillsChoice(c, _) => c }
    assert(extra.contains(3), s"expected one ExtraSkillsChoice(3, _), got $grants")
  }
}
