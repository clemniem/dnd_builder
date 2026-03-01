---
name: Typed Types Roadmap
overview: Audit the codebase for stringly-typed and other primitive types that should be replaced with dedicated types, then produce a dedicated roadmap document with working packages (one type per package) and Scalafix strategy for cheaper, token-efficient refactors.
todos: []
isProject: false
---

# Typed Types Roadmap

## Summary of findings

The codebase already uses typed abstractions in [DndTypes.scala](src/main/scala/dnd_builder/dnd/DndTypes.scala) (`Score`, `Modifier`), [CoreTypes.scala](src/main/scala/dnd_builder/dnd/CoreTypes.scala) (`HitDie`, `Ability`, `Skill`, `DamageType` for weapons, etc.), and [Equipment.scala](src/main/scala/dnd_builder/dnd/Equipment.scala) (`DamageType` for physical: Slashing/Piercing/Bludgeoning). Several concepts are still represented as plain `String` or `Set[String]`, and a few as `Option[String]`.


| Current type                 | Where used                                                                                                                                                      | Suggested typed abstraction                                                                                                                 |
| ---------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------- |
| **Dice as `String`**         | `AttackGrant.baseDamageDice`, `Weapon.damageDice`, `Spell.damageDice`, `Character.scaledDice` / `martialArtsDiceForLevel` / `hitDiceString` (derived), `Codecs` | `Dice` (e.g. `num: Int`, `sides: Int` with parse/format)                                                                                    |
| **Spell/attack class list**  | `Spell.classes: Set[String]` (class names), `Spell.availableToClass` uses `dndClass.name`                                                                       | `Set[ClassName]` (opaque type wrapping String) or keep as `Set[String]` and rely on data-driven convention                                  |
| **Spell/attack damage type** | `Spell.damageType: Option[String]`, `AttackGrant.damageType: String`, `DragonAncestry.damageType: String`                                                       | `SpellDamageType` enum (Acid, Cold, Fire, Force, Lightning, Necrotic, Poison, Psychic, Radiant, Varies) shared by spells and breath weapons |
| **Attack range**             | `AttackGrant.range: String`                                                                                                                                     | `AttackRange` (e.g. Melee, Ranged(feet: Int) or simple enum + label for display)                                                            |
| **Source label**             | `sourceLabel: String` on grants                                                                                                                                 | Could stay as `String` (free text for UI); optional later: `GrantSource` if we want structured source                                       |


Recommended order for **working packages** (one type per package, minimal cross-deps):

1. **Dice** — highest impact: used in `Attack`, `Equipment`, `Spell`, `Character`, `Codecs`. Unblocks safer parsing and scaling.
2. **SpellDamageType** — used in `Spell`, `AttackGrant`, `Species` (DragonAncestry), `Character` (display). Clear finite set.
3. **ClassName** (opaque) — used in `Spell.classes` and anywhere we pass class names; keeps data-driven `.name` convention but adds type safety at API boundaries.
4. **AttackRange** — used only in `AttackGrant` and display; smaller scope.

The project already has **Scalafix** enabled ([.scalafix.conf](.scalafix.conf), [build.sbt](build.sbt): `scalafixOnCompile`, `semanticdbEnabled`). No custom rules exist yet; only `OrganizeImports` and `DisableSyntax` are used.

---

## 1. Dice type (Package 1)

**Goal:** Replace all damage-dice and hit-dice-as-string with a single `Dice` type.

**Current usage (concise):**

- [Attack.scala](src/main/scala/dnd_builder/dnd/Attack.scala): `AttackGrant.baseDamageDice: String`
- [Equipment.scala](src/main/scala/dnd_builder/dnd/Equipment.scala): `Weapon.damageDice: String` (many literals e.g. `"1d4"`, `"1d8"`, `"2d6"`)
- [Spell.scala](src/main/scala/dnd_builder/dnd/Spell.scala): `Spell.damageDice: Option[String]`, and all `Spell.attack(..., "1d6", ...)`-style calls
- [Character.scala](src/main/scala/dnd_builder/dnd/Character.scala): `scaledDice(baseDice: String)`, `martialArtsDiceForLevel(level: Int): String`, and `hitDiceString` (derived from `HitDie` + level, can stay as method that returns `String` for PDF/UI or build from `Dice` later)
- [Codecs.scala](src/main/scala/dnd_builder/dnd/Codecs.scala): `AttackGrant` decoder/encoder for `baseDamageDice`

**Proposed type (e.g. in DndTypes or new `Dice.scala`):**

- Opaque type or case class: `Dice(num: Int, sides: Int)` with:
  - `def format: String` (e.g. `"1d6"`, `"2d8"`)
  - `def parse(s: String): Option[Dice]` (regex or split on `d`)
  - Optional: `def scaleByLevel(characterLevel: Int): Dice` for cantrip-style scaling (num increases at 5/11/17)

**Working package steps:**

1. Add `Dice` type and companion (parse, format, Circe codecs in Codecs.scala).
2. Replace `AttackGrant.baseDamageDice` with `Dice`; update Codecs and all `AttackGrant` construction sites (e.g. [DndClass.scala](src/main/scala/dnd_builder/dnd/DndClass.scala) Monk, [Species.scala](src/main/scala/dnd_builder/dnd/Species.scala) breath weapons).
3. Replace `Weapon.damageDice` with `Dice`; update `Weapon.all` and any code that reads `weapon.damageDice` (e.g. [Character.scala](src/main/scala/dnd_builder/dnd/Character.scala) `weaponDamageString`).
4. Replace `Spell.damageDice: Option[String]` with `Option[Dice]`; update `Spell.attack` and all spell list literals; update `Character.scaledDice` to accept `Dice` and return `Dice` or formatted string as needed.
5. Update `martialArtsDiceForLevel` to return `Dice` (or keep internal and format at use site).
6. Keep `hitDiceString` as a derived string for PDF/display (it’s built from `classLevels` and `HitDie`, not from a single dice string).

**Scalafix for Dice:**

- **symbolReplacements** in `.scalafix.conf` can replace a *type symbol* (e.g. `String` in a specific field) only if the type is uniquely identified by symbol; here we’re changing *field types* and adding a new type, so symbolReplacements are of limited use.
- A **custom syntactic rule** could:
  - Find string literals matching `"NdM"` and replace with `Dice(N, M).format` or leave as literal and add a helper that parses at call sites. Safer approach: **don’t** auto-replace literals; add `Dice` and migrate call sites by hand or with a one-off script, then add a custom rule that *warns* when a `String` literal matches `\d+d\d+` in a position where `Dice` is expected (optional, later).
- **Recommendation:** Introduce `Dice` and migrate manually (or with a small script). Use Scalafix only for follow-up lint (e.g. “no String in field X”) if you add a custom rule. This keeps the refactor predictable and avoids fragile AST rewrites of literals.

---

## 2. SpellDamageType (Package 2)

**Goal:** Replace spell/attack/breath damage type strings with an enum (or sealed type) used everywhere non-physical damage is expressed.

**Current usage:**

- [Spell.scala](src/main/scala/dnd_builder/dnd/Spell.scala): `damageType: Option[String]`, and literals like `"Acid"`, `"Fire"`, `"Radiant"`, `"Necrotic"`, `"Psychic"`, `"varies"`, etc.
- [Attack.scala](src/main/scala/dnd_builder/dnd/Attack.scala): `AttackGrant.damageType: String`
- [Species.scala](src/main/scala/dnd_builder/dnd/Species.scala): `DragonAncestry.damageType: String`
- [Character.scala](src/main/scala/dnd_builder/dnd/Character.scala): `grant.damageType`, `spell.damageType.getOrElse("")` for display

**Proposed type:**

- Enum `SpellDamageType(val label: String)` with: Acid, Cold, Fire, Force, Lightning, Necrotic, Poison, Psychic, Radiant, Thunder, Varies (and any others present in spell list). Add `Encoder`/`Decoder` in Codecs (by label or by name).

**Working package steps:**

1. Add `SpellDamageType` enum (e.g. in [CoreTypes.scala](src/main/scala/dnd_builder/dnd/CoreTypes.scala) or next to `DamageType` in Equipment if you want to keep physical vs spell damage separate).
2. `Spell.damageType` → `Option[SpellDamageType]`; update all `Spell.attack` and utility constructors; update `Character` display to use `.label`.
3. `AttackGrant.damageType` → `SpellDamageType` (or a sum type Physical | Spell(SpellDamageType) if some grants use weapon DamageType — currently AttackGrant is used for unarmed/breath/cantrips, so spell damage type is enough).
4. `DragonAncestry.damageType` → `SpellDamageType`; update Species and any breath-weapon construction.
5. Codecs: decode/encode `SpellDamageType` by label (or enum name) for backward compatibility.

**Scalafix:** Same as Dice: type and usages are straightforward; manual (or script) replacement of string literals with enum members. Optional custom rule to forbid string literals in `damageType` fields once the type is in place.

---

## 3. ClassName (Package 3)

**Goal:** Type the set of class names so that `Spell.classes` and similar are `Set[ClassName]` instead of `Set[String]`, preserving data-driven lookup by `.name` and enabling future RuleSet/JSON where class is identified by string.

**Current usage:**

- [Spell.scala](src/main/scala/dnd_builder/dnd/Spell.scala): `classes: Set[String]`, `availableToClass(dndClass: DndClass)` uses `classes.contains(dndClass.name)`.
- Any code that builds or passes class name sets (currently only in Spell definitions).

**Proposed type:**

- Opaque type: `object ClassName { opaque type ClassName = String; def apply(s: String): ClassName = s; extension (c: ClassName) def value: String = c }` (or a value class). Use `Set[ClassName]` in `Spell` and `ClassName(dndClass.name)` at call sites; comparison stays via `classes.contains(ClassName(dndClass.name))`.

**Working package steps:**

1. Add `ClassName` opaque type (e.g. in DndTypes.scala) with Circe codec (encode/decode as string).
2. Change `Spell.classes` to `Set[ClassName]`; update `Spell.availableToClass` to `classes.contains(ClassName(dndClass.name))`.
3. Update all `Set("Bard", "Cleric", ...)` in Spell definitions to `Set(ClassName("Bard"), ...)` (or add a helper `ClassName.from(List("Bard", "Cleric"))`).
4. If any other API exposes or accepts “class name” strings, add `ClassName` there for consistency.

**Scalafix:** A **symbolReplacement** could in theory replace `String` with `ClassName` for the specific parameter of `Spell.classes`, but Scalafix’s symbolReplacements typically target type *references* (e.g. fully qualified type name). Safer: manual change of the field type and then run a simple find/replace for `Set("X")` → `Set(ClassName("X"))` in Spell.scala (or a small script). No custom rule required unless you want to enforce “no raw String where ClassName is expected” later.

---

## 4. AttackRange (Package 4)

**Goal:** Replace `AttackGrant.range: String` with a typed `AttackRange` (e.g. Melee, Ranged(feet: Int), or enum + label for display).

**Current usage:**

- [Attack.scala](src/main/scala/dnd_builder/dnd/Attack.scala): `AttackGrant.range: String`
- [Codecs.scala](src/main/scala/dnd_builder/dnd/Codecs.scala): encode/decode `range` as string
- Display in UI/PDF likely uses the string as-is.

**Proposed type:**

- Sealed trait or enum: e.g. `Melee`, `Ranged(feet: Int)`, with `def label: String` for backward-compatible serialization and display.

**Working package steps:**

1. Add `AttackRange` type; add Codecs (encode as string, decode from string or structured JSON).
2. Replace `AttackGrant.range` with `AttackRange`; update all construction sites (Species, DndClass) and any place that displays `grant.range`.
3. Ensure PDF/UI still get a string via `.label` or equivalent.

**Scalafix:** Manual migration is small (one field, limited call sites). No Scalafix needed unless you add a lint rule later.

---

## Roadmap document to add

Create a single markdown file (e.g. `**TypedTypes_Roadmap.md`** in the repo root, alongside `GrantSystem_Roadmap.md`) containing:

1. **Overview** — same table as above (current type → where used → suggested type).
2. **Order and rationale** — Package 1 (Dice) through 4 (AttackRange), with short rationale (impact, dependencies).
3. **Per-package sections** — for each:
  - Goal
  - Proposed type (signature/location)
  - Steps (numbered)
  - Scalafix strategy (what to do manually vs what to automate, and any optional custom rule).
4. **Scalafix notes** — project already has Scalafix; for type migrations, prefer:
  - Manual (or scripted) replacement of types and literals.
  - Optional: custom rule to *warn* when a string literal appears in a field that’s now typed (e.g. `Dice` or `SpellDamageType`), to prevent regressions.
  - Document that `symbolReplacements` are better for renaming existing types/symbols than for introducing new types and changing field types.

No code or config changes in this plan — only the creation of the roadmap file and the above content. Implementation of each package would be done in follow-up work.