# Grant System Refactoring Roadmap

**See [docs/GRANTS_ARCHITECTURE.md](docs/GRANTS_ARCHITECTURE.md) for the current grants architecture and flow (collect → resolve → visualise) in creation and level-up.**

## Current State

All grant types extend a `sealed trait Grant` with shared `sourceLabel` and `isFilled`:

| Grant Type | Type | Has Player Choice | Source |
|---|---|---|---|
| Spell grants | `SpellGrant` | Yes (pick N spells from a list) | Background feat (Magic Initiate), Fighting Style (Blessed/Druidic Warrior) |
| Skill grants | `SkillGrant` | Yes (pick N skills from a pool) | Background feat (Skilled) |
| Attack grants | `AttackGrant` | No (auto-resolved) | Species, Class |

`CharacterDraft` and `Character` carry a single `grants: List[Grant]` field with typed convenience accessors (`spellGrants`, `skillGrants`, `attackGrants`) and partial-update helpers (`withSpellGrants`, `withSkillGrants`).

They flow through the same pipeline:
1. `FeatureGrants.fromSpecies/fromClass/fromBackground` produces a `Grants` container (view over `List[Grant]`)
2. `grants: List[Grant]` is stored on `CharacterDraft`
3. Wizard screens fill player choices where needed (filter by type)
4. `Character` carries the resolved grants as `List[Grant]`
5. Output (PDF, review screen) reads via typed accessors
6. JSON serialization writes/reads three separate fields for backward compat, merging into `List[Grant]` on decode

## Goal: Unified Grant System

Replace the three separate types with a single `Grant` ADT:

```scala
sealed trait Grant {
  def sourceLabel: String
  def isFilled: Boolean
}
case class SpellGrant(...) extends Grant
case class SkillGrant(...) extends Grant
case class AttackGrant(...) extends Grant
case class LanguageGrant(...) extends Grant   // future
case class EquipmentGrant(...) extends Grant  // future
case class FeatureGrant(...) extends Grant    // future (class features at level-up)
```

Benefits:
- `CharacterDraft` and `Character` carry a single `grants: List[Grant]` instead of N separate fields
- `FeatureGrants.from*` methods return `List[Grant]` uniformly
- New grant types are added by extending `Grant` -- no signature changes
- The wizard can have a generic "resolve grants" phase that handles any unfilled grant
- Config-driven content (RuleSets) can specify grants as a heterogeneous list in JSON

## Migration Phases

### Phase 1: AttackGrant ✅

Added `AttackGrant` as a third grant type. Introduced `Grants` container to group the three types.

### Phase 2: Unify into `List[Grant]` ✅

- `sealed trait Grant { def sourceLabel: String; def isFilled: Boolean }` in `FeatureGrants.scala`
- `SpellGrant`, `SkillGrant`, `AttackGrant` all extend `Grant` (all in same file for sealed requirement)
- `CharacterDraft` and `Character` carry `grants: List[Grant]` with typed accessors and `withSpellGrants`/`withSkillGrants` helpers
- `Grants` container is now a view over `List[Grant]` with typed accessors; overloaded `apply` for backward compat
- `CharacterValidator.validate()` takes `grants: List[Grant]`
- JSON encoder writes three fields; decoder reads three fields and merges into `List[Grant]`
- All screens updated to use the unified API

### Phase 3: LanguageGrant

Species currently grant languages via `def languages: Set[Language]` on the `Species` trait. This could become `LanguageGrant` entries:

```scala
case class LanguageGrant(
    count: Int,
    pool: Set[Language],
    sourceLabel: String,
    chosen: Set[Language]
) extends Grant
```

- Fixed languages (e.g. Common, Draconic for Dragonborn) would be grants with `count = 1, pool = Set(Draconic), chosen = Set(Draconic)` (pre-filled)
- Extra language choices (e.g. from class `extraLanguageChoices`) would have an empty `chosen` for the player to fill

### Phase 4: EquipmentGrant (optional)

Starting equipment could be modeled as grants from the class/background. This is lower priority since equipment selection is already a full wizard screen.

### Phase 5: FeatureGrant (level-up)

Class features at each level could be grants. This connects to the `ClassProgression` system and level-up flow. Most complex phase -- depends on the Configuration Roadmap Phase 5 (Subclasses + ClassProgression as data).

## Alignment with Configuration Roadmap

The grant system dovetails with the RuleSet migration:

- When `DndClass` becomes a JSON case class, `grantedAttacks` is just `List[AttackGrant]` in the JSON
- When `Species` becomes a JSON case class, both `grantedAttacks` and language grants are JSON arrays
- When `Background`/`Feat` become JSON, spell and skill grants are JSON arrays
- A unified `List[Grant]` serializes naturally as a heterogeneous JSON array with a `type` discriminator

The grant pipeline is the "glue" that connects building blocks (species, class, background, feat) to the character. Making it generic and data-driven is essential for the configuration system.
