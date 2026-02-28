# Levels 4–20 Roadmap

Phase 1 (Level 3) is implemented: subclass assignment, spell updates, and level-3-specific class feature choices. This file documents deferred work for levels 4–20.

## General progression (all classes)

- **Ability Score Improvement (ASI)** at levels 4, 8, 12, 16, 19 (Fighter 6/14, Rogue 10).
- **Feats** as optional replacement for ASI (2024 rules: one feat or +2 to one ability or +1 to two).
- **Proficiency bonus** steps at 5, 9, 13, 17.

## Class-specific deferred features

### Barbarian
- Primal Knowledge (L3) — done in Phase 1.
- ASI at 4, 8, 12, 16, 19.
- Extra Attack (5), Fast Movement (5), Feral Instinct (7), etc.
- Subclass: Path of the Berserker — Frenzy (3), Mindless Rage (6), Intimidating Presence (10), Retaliation (14).

### Bard
- College of Lore: Bonus Proficiencies (L3) — done in Phase 1.
- ASI at 4, 8, 12, 16, 19.
- Bardic Inspiration uses, Font of Inspiration (5), Countercharm (6), etc.
- Expertise (3, 10).

### Cleric
- Life Domain (L3) — done in Phase 1.
- ASI at 4, 8, 12, 16, 19.
- Destroy Undead improvements, Divine Intervention (10), etc.

### Druid
- Circle of the Land: Land type (L3) — done in Phase 1.
- ASI at 4, 8, 12, 16, 19.
- Wild Shape improvements, etc.

### Fighter
- Champion (L3) — done in Phase 1.
- ASI at 4, 6, 8, 12, 14, 16, 19.
- Extra Attack (5, 11), Indomitable (9, 13, 17).

### Monk
- Open Hand (L3) — done in Phase 1.
- ASI at 4, 8, 12, 16, 19.
- Slow Fall (4), Extra Attack (5), Stunning Strike (5), Ki-Empowered Strikes (6), Evasion (7), etc.

### Paladin
- Oath of Devotion (L3) — done in Phase 1.
- ASI at 4, 8, 12, 16, 19.
- Extra Attack (5), Aura of Devotion (7), etc.

### Ranger
- Hunter: Hunter's Prey (L3) — done in Phase 1.
- ASI at 4, 8, 12, 16, 19.
- Extra Attack (5), etc.

### Rogue
- Thief (L3) — done in Phase 1.
- ASI at 4, 8, 10, 12, 16, 19.
- Expertise (1, 6), Uncanny Dodge (5), Evasion (7), etc.

### Sorcerer
- Draconic Sorcery (L3) — done in Phase 1.
- ASI at 4, 8, 12, 16, 19.
- **Metamagic** (3): choose options (e.g. Twinned, Quickened, Subtle). More options at 10, 17.

### Warlock
- Fiend Patron (L3) — done in Phase 1.
- ASI at 4, 8, 12, 16, 19.
- **Eldritch Invocations**: multiple choices at 2, 5, 7, 9, 12, 15, 18. Already have one in ClassFeatureSelections; extend to list/set.

### Wizard
- School of Evocation (L3) — done in Phase 1.
- ASI at 4, 8, 12, 16, 19.
- Spellbook growth per level, Evocation features (Sculpt Spells at 6, etc.).

## Implementation notes

- Add `level4Entries` … `level20Entries` (or a single map) in `ClassProgression.scala`.
- Extend `LevelChoice` as needed: ASI, ChooseFeat, ChooseMetamagic, ChooseInvocations, etc.
- Level-up flow: extend `LevelUpScreen` phases per level (e.g. ASI vs Feat, metamagic picker, invocation picker).
- Subclass features: extend `Subclass.features` and `alwaysPreparedByLevel` for higher levels in `Subclass.scala`.
