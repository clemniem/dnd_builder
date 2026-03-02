# Grant System Refactoring Roadmap

**See [docs/GRANTS_ARCHITECTURE.md](docs/GRANTS_ARCHITECTURE.md) for the current grants architecture and flow (collect → resolve → visualise) in creation and level-up.**

## Current State

Three separate grant types exist in parallel, each with their own type:

| Grant Type | Type | Has Player Choice | Source |
|---|---|---|---|
| Spell grants | `SpellGrant` | Yes (pick N spells from a list) | Background feat (Magic Initiate) |
| Skill grants | `SkillGrant` | Yes (pick N skills from a pool) | Background feat (Skilled) |
| Attack grants | `AttackGrant` | No (auto-resolved) | Species, Class |

They share a common shape:
- Template fields (what can be granted)
- `sourceLabel: String` (who granted it)
- `isFilled: Boolean` (are player choices complete)

They flow through the same pipeline:
1. `FeatureGrants.fromSpecies/fromClass/fromBackground` produces a `Grants` container
2. `Grants` is stored on `CharacterDraft` (three separate fields)
3. Wizard screens fill player choices where needed
4. `Character` carries the resolved grants
5. Output (PDF, review screen) reads from the character

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

### Phase 1: AttackGrant (current task)

Add `AttackGrant` as a third grant type. Introduce `Grants` container to group the three types. Keep separate fields on `CharacterDraft`/`Character` for now.

This establishes the pattern without breaking existing code.

### Phase 2: Unify into `List[Grant]`

- Convert `SpellGrant`, `SkillGrant`, `AttackGrant` into cases of a sealed trait `Grant`
- Replace `spellGrants`, `skillGrants`, `attackGrants` fields with a single `grants: List[Grant]`
- Update `CharacterDraft`, `Character`, `Codecs` to use `List[Grant]`
- Update wizard screens to filter grants by type: `grants.collect { case sg: SpellGrant => sg }`
- `Grants` container becomes a view helper over `List[Grant]` rather than a data type

**Estimated scope:** ~8 files, ~200 lines changed. Medium effort. Low risk since the types are already following the same pattern.

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
