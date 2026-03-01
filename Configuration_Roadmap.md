# RuleSet Configuration Architecture

## Scope Assessment

The codebase has **~1,700 lines of hardcoded D&D content** across 10+ files:

- **12 classes** (`DndClass.scala`, sealed trait + 12 case objects, 328 lines)
- **9 species** (`Species.scala`, sealed trait + case objects/classes, 255 lines)
- **4 backgrounds** (`Background.scala`, sealed trait + 4 case objects, 66 lines)
- **~255 spells** (`Spell.scala`, case class list, 309 lines)
- **39 weapons + 13 armor** (`Equipment.scala`, case classes, 147 lines)
- **12 subclasses** (`Subclass.scala`, sealed trait + 12 case objects, 297 lines)
- **Feats** (`Feat.scala`, sealed trait + case objects, 78 lines)
- **Class progression** (`ClassProgression.scala`, hardcoded maps, 197 lines)
- **Class feature choices** (`ClassFeatureChoices.scala`, enums, 136 lines)

These are referenced from **every screen** in the wizard flow (14 screens), plus `Character.scala`, `CharacterDraft.scala`, `Codecs.scala`, and PDF generation.

The full migration is a **large refactoring** (~25-30 files, ~2000+ lines changed). The plan below breaks it into safe, testable phases.

---

## Key Architectural Decisions

- **Format:** JSON (Circe already in project; no extra dependencies)
- **Storage:** RuleSets stored in LocalStorage alongside characters
- **Built-in content:** Eventually becomes a loadable RuleSet too, but starts hardcoded
- **Core types stay fixed initially:** `Ability`, `Skill`, `HitDie`, `Size`, `ArmorType` remain enums (these are mechanical primitives)
- **Cross-references by name:** Spells already reference classes by `String` name. All cross-references in config will be string-based, resolved at validation time.

---

## Phase 1: DndClass as Data (Learning Phase)

**Goal:** Convert `DndClass` from a sealed trait to a case class. Keep all 12 built-in classes as `val`s. Add one custom test class loaded from JSON. This teaches us the exact scope of changes for the full migration.

### 1a. Convert DndClass to case class

Currently (`src/main/scala/dnd_builder/dnd/DndClass.scala`):

```scala
sealed trait DndClass:
  val name: String
  val hitDie: HitDie
  // ...
case object Barbarian extends DndClass:
  val name = "Barbarian"
  val hitDie = HitDie.D12
  // ...
```

Becomes:

```scala
case class DndClass(
  name: String,
  hitDie: HitDie,
  primaryAbilities: List[Ability],
  savingThrows: Set[Ability],
  armorProficiencies: Set[ArmorType],
  weaponProficiencies: Set[WeaponProficiency],
  skillPool: Set[Skill],
  numSkillChoices: Int,
  spellcastingAbility: Option[Ability],
  spellCasterType: SpellCasterType,
  fullCasterVariant: Option[FullCasterVariant],
  level1Features: List[ClassFeature],
  description: String,
  recommendedScores: List[Ability],
  weaponMasteryCount: Int,
  extraLanguageChoices: Int
)
object DndClass:
  val Barbarian: DndClass = DndClass(name = "Barbarian", hitDie = HitDie.D12, ...)
  val Bard: DndClass = DndClass(name = "Bard", hitDie = HitDie.D8, ...)
  // ... all 12
  val builtIn: List[DndClass] = List(Barbarian, Bard, ...)
```

Helper methods like `hitPoints`, `weaponSummary`, `isSpellcaster` stay as extension methods or methods on the case class.

**Files affected (~10):**

- `DndClass.scala` -- restructure
- `ClassProgression.scala` -- key by `name: String` instead of type identity
- `Subclass.scala` -- `dndClass` field stays, but compare by name
- `Codecs.scala` -- update DndClass codec (encode by name, decode by lookup from available classes)
- `Character.scala` -- no structural change (ClassLevel already holds DndClass)
- `ClassSelectScreen.scala` -- use `availableClasses` list instead of `DndClass.all`
- `ClassFeaturesScreen.scala`, `SkillsScreen.scala`, `SpellsScreen.scala`, `ReviewScreen.scala` -- minor

**Low risk because:** Most screens already access DndClass through field accessors (`.name`, `.hitDie`, `.skillPool`), not pattern matching. `Spell.availableToClass` already uses string-based `dndClass.name`. The codecs already encode by name string.

### 1b. JSON schema for custom classes

Create a JSON schema and Circe decoder for loading custom classes:

```json
{
  "name": "Artificer",
  "hitDie": "d8",
  "primaryAbilities": ["Intelligence"],
  "savingThrows": ["Constitution", "Intelligence"],
  "armorProficiencies": ["Light", "Medium", "Shield"],
  "weaponProficiencies": [{"type": "AllSimple"}],
  "skillPool": ["Arcana", "History", "Investigation", "Medicine", "Nature", "Perception"],
  "numSkillChoices": 2,
  "spellcastingAbility": "Intelligence",
  "spellCasterType": "HalfCaster",
  "level1Features": [
    {"name": "Magical Tinkering", "description": "You can invest a spark of magic..."}
  ],
  "description": "Masters of invention...",
  "recommendedScores": ["Intelligence", "Constitution", "Dexterity"],
  "weaponMasteryCount": 0,
  "extraLanguageChoices": 0
}
```

New file: `dnd/ConfigLoader.scala` -- generic JSON-to-domain parser.

### 1c. RuleSet container (minimal)

```scala
case class RuleSet(
  id: String,
  name: String,
  classes: List[DndClass]
  // Later: species, backgrounds, feats, spells, equipment...
)
object RuleSet:
  val default: RuleSet = RuleSet("srd", "Standard (SRD)", DndClass.builtIn)
```

New file: `dnd/RuleSet.scala`.

### 1d. Thread RuleSet through the wizard

- Add `ruleSet: RuleSet` to `CharacterDraft` (or pass it via `ScreenOutput.Draft`)
- `ScreenOutput.Draft` gets a `ruleSet` field
- `ClassSelectScreen` reads `ruleSet.classes` instead of `DndClass.all`
- Other screens receive the RuleSet but don't use it yet (they still use hardcoded species, backgrounds, etc.)

### 1e. RuleSet selection step

- Add a step before Species selection (or as first step) where the user picks which RuleSet to use
- If only "Standard (SRD)" exists, auto-select and skip
- New screen: `RuleSetSelectScreen` (simple list of available rulesets)
- Or: add a dropdown to the existing HomeScreen "Create Character" flow

---

## Phase 2: RuleSet Upload and Management

**New screen:** `RuleSetScreen` accessible from the main menu.

- Upload JSON text (paste or file input via `<input type="file">`)
- Validate:
  - JSON syntax (Circe parse)
  - Schema compliance (required fields, correct types)
  - Cross-reference integrity (e.g. spell class names must match defined class names)
- Display validation errors or success
- Store valid RuleSets in LocalStorage under `StorageKeys.ruleSets`
- List/delete existing custom rulesets

New files:

- `screens/RuleSetScreen.scala`
- `dnd/RuleSetValidation.scala`

Add `RuleSetScreen` to the `ScreenRegistry` in `App.scala`, plus a "Manage Rulesets" button on the Home screen.

---

## Phase 3: Species as Data (repeat the pattern)

Same approach as Phase 1 for `Species`:

- Convert sealed trait to case class
- Built-in species become vals
- Add to RuleSet: `species: List[SpeciesDef]`
- Add JSON schema for custom species
- Update SpeciesScreen, Codecs, Character

**Challenge:** Species have sub-choices (ElvenLineage, DragonAncestry, etc.). These need a generic "sub-choice" mechanism in the JSON schema:

```json
{
  "name": "Elf",
  "subChoices": [
    {"name": "High Elf", "traits": ["Darkvision", "Fey Ancestry", "High Elf Cantrip"]},
    {"name": "Wood Elf", "traits": ["Darkvision", "Fey Ancestry", "Fleet of Foot"]}
  ]
}
```

---

## Phase 4+: Backgrounds, Feats, Spells, Equipment, Subclasses

Each follows the same pattern. Estimated order by complexity:

1. **Backgrounds** (simplest -- 4 entries, flat structure)
2. **Feats** (small, but has categories: Origin, General, FightingStyle)
3. **Spells** (already a case class with `Set[String]` class refs -- mostly just needs a loader)
4. **Equipment** (weapons + armor, already case classes)
5. **Subclasses + ClassProgression** (most complex -- features per level, spell lists)

---

## Estimated Effort Per Phase

- **Phase 1 (DndClass + RuleSet infra):** ~10 files, ~500 lines changed/added. Medium effort. This is the hardest phase because it establishes the pattern and threading.
- **Phase 2 (Upload screen):** ~3 new files, ~300 lines. Small-medium effort.
- **Phase 3 (Species):** ~8 files, ~400 lines. Medium effort (sub-choices add complexity).
- **Phase 4+ (rest):** ~5-8 files each, ~200-400 lines each. Gets faster as the pattern is established.

**Total for full migration:** ~25-30 files, ~2000-3000 lines changed/added across all phases.

---

## Risk Mitigation

- **Backward compatibility:** Character save format must remain decodable. Since DndClass is already encoded by name string in Codecs, converting to case class won't break existing saves as long as the lookup resolves. Custom classes not found in the current RuleSet should degrade gracefully (show name, mark as "unknown class").
- **No sealed trait exhaustiveness:** Converting sealed traits to case classes loses compile-time exhaustive match checking. This is acceptable since the code already uses field access (not pattern matching) for almost everything.
- **Incremental testing:** Each phase produces a working app. Built-in content always works. Custom content is additive.
