# Grants Architecture & Flow

This document describes how **grants** work: how they are chosen and collected, resolved step by step, and finally visualized in the overview and PDF. It distinguishes the **creation** flow from the **level-up** flow.

---

## Main idea

1. **Choose and collect grants** – Decide which sources apply (class, species, background, and choices like fighting style), and build the list of *resolvable* grants (spell/skill/attack) that require player input.
2. **Resolve grants step by step** – Each grant is a “pick N from pool/list”; the wizard shows the right screens and records `chosen` until every grant is filled.
3. **Visualise grants** – The final character (and PDF) shows resolved grants: grant cantrips with class cantrips, grant skills in the skill list, grant attacks in the attack block.

---

## Types

### Declarative: `FeatureGrant` (on features)

Defined on **class features** (and derived from species/background). They describe *what kind* of choice is required, not the concrete “pick N from X” form.

- **Choice gates** (resolved on Class Features / Level-up Choices):  
  `FightingStyleChoice`, `DivineOrderChoice`, `PrimalOrderChoice`, `EldritchInvocationChoice`, `SubclassGate`, `LandTypeChoice`, `HunterPreyChoice`, `ExpertiseChoice`, `WeaponMasteryChoice`, `ExtraSkillsChoice`
- **Converted into resolvable grants** (see below):  
  `SpellChoice`, `SkillChoice`, `Attack`  
  These are turned into `SpellGrant` / `SkillGrant` / `AttackGrant` with a `sourceLabel` and empty `chosen`.

Defined in: `DndClass.scala` (`FeatureGrant`), `SRD_registry.scala` (features list their `grants`), `ClassProgression.scala` (level gains and `grantChoicesAtLevel`).

### Resolvable: `sealed trait Grant` with `SpellGrant`, `SkillGrant`, `AttackGrant`

All concrete grant types extend a shared sealed trait (defined in `FeatureGrants.scala`):

```scala
sealed trait Grant {
  def sourceLabel: String
  def isFilled: Boolean
}
```

Concrete “pick N from pool/list” entries. They carry:

- Template: count, pool or spell list, level (for spells)
- `sourceLabel: String` (e.g. class name, "Blessed Warrior", "Acolyte: Magic Initiate")
- `chosen`: the player’s selection; grant is **filled** when `chosen.size >= count`

Defined in: `FeatureGrants.scala`. Stored on `CharacterDraft` and `Character` as a single `grants: List[Grant]` field. Typed convenience accessors (`spellGrants`, `skillGrants`, `attackGrants`) filter by subtype. Partial-update helpers (`withSpellGrants`, `withSkillGrants`) replace one grant type in the list.

### Conversion: `FeatureGrant` → resolvable grants

- **`FeatureGrants.buildGrantsFromFeatureGrants(grants, sourceLabel)`**  
  Maps only the “resolvable” feature grants to the three grant types:
  - `FeatureGrant.SpellChoice(c, l, list)` → `SpellGrant(c, l, list, sourceLabel, Nil)`
  - `FeatureGrant.SkillChoice(c, pool)` → `SkillGrant(c, pool, sourceLabel, Set.empty)`
  - `FeatureGrant.Attack(a)` → included in `attackGrants` (no `chosen`; auto-resolved)

Choice gates (e.g. `FightingStyleChoice`) are **not** converted here; they are resolved elsewhere and their result can *conditionally* add more resolvable grants (see “Special case” below).

---

## Creation flow (step-by-step)

### 1. Collect grants

- **When:** After the user picks **Background** (Background screen → Next).
- **Where:** `BackgroundScreen`: `FeatureGrants.allGrantsForDraft(draftWithBg)`.
- **What:** Merges:
  - **Class:** `grantsFromClass(draft.resolvedClass, level)` → from `ClassProgression.featuresUpToLevel` → each feature’s `grants`; only `SpellChoice`, `SkillChoice`, `Attack` are turned into `SpellGrant`/`SkillGrant`/`AttackGrant`.
  - **Species:** `grantsFromSpecies(draft.resolvedSpecies)` → e.g. `Attack` from species.
  - **Background feat:** `grantsFromOriginFeat(bg.feat)` → e.g. Magic Initiate → `SpellChoice`, Skilled → `SkillChoice`.
- **Result:** Draft is updated with `grants: List[Grant]` (containing all spell, skill, and attack grants) and passed to Abilities.

**Later, after Class Features:** If the user chose **Blessed Warrior** or **Druidic Warrior**, `ClassFeaturesScreen` appends the corresponding spell grant (2 Cleric / 2 Druid cantrips) to `draft.spellGrants` when navigating to Equipment (see “Special case” below). So by the time we reach the Spells screen, those grants are already in the list.

### 2. Resolve grants (step by step)

Resolution happens on dedicated screens; each screen only advances when the grants it cares about are filled.

| Step | Screen        | What is resolved |
|------|---------------|------------------|
| Class features | **ClassFeaturesScreen** | Choice gates: Fighting Style, Divine/Primal Order, Eldritch Invocation, Expertise, Weapon Mastery, Land Type, Hunter’s Prey, Extra Skills. Stored in `draft.featureSelections`. No spell/skill/attack grants are filled here. |
| Skills         | **SkillsScreen**       | `draft.skillGrants`: pick N skills per grant. Proceed when `skillGrants.forall(_.isFilled)`. |
| Equipment      | **EquipmentScreen**    | No grants; equipment and coins. Routing: if `FeatureGrants.needsSpellScreen(draft)` then next = Spells else Review. |
| Spells         | **SpellsScreen**       | (1) **Grant spells:** each `draft.spellGrants(i)` – pick `count` spells from the grant’s list/level; (2) class cantrips; (3) spellbook (Wizard); (4) prepared spells. Proceed when all grant spells are filled and class spell counts are satisfied. |
| Review         | **ReviewScreen**       | No resolution; validation ensures all grants are filled before save. |

So: **collect** (Background + later ClassFeatures for fighting-style spell grant) → **resolve** (ClassFeatures for choices, then Skills, then Spells for grant spells + class spells) → **visualise** (Review and PDF).

### 3. Visualise grants

- **Review screen:** Builds `Character` from draft; summary and validation use `draft.spellGrants`, `draft.skillGrants`, `draft.attackGrants`.
- **Character detail / PDF:**  
  - **Spells:** `ch.chosenCantrips ++ grantCantrips` (grant cantrips = `ch.spellGrants` where `spellLevel == 0`); same for level-1 grant spells.  
  - **Attacks:** `Character.attacks` uses `attackGrants` (resolved) and class/spell attacks.  
  - Grant skills are part of the chosen skill set used for proficiency display.

---

## Level-up flow (step-by-step)

### 1. Collect grants

- **When:** At the start of each level-up, per target level.
- **Where:** `LevelSummary.forClassAtLevel(cls, level)` and `ClassProgression.atLevel` / `grantChoicesAtLevel`.
- **What:**  
  - **Features** and **grant choices** for that level: subclass gate, fighting style, land type, hunter’s prey, expertise, weapon mastery, extra skills, etc.  
  - **Spell progression** (slots, cantrips, prepared) from `SpellProgression.forClass(cls, level)`.

There is no single “merge all grants” call like in creation. Level-up collects *choice gates* and *spell progression*; resolvable spell grants from choices (e.g. Blessed/Druidic Warrior) are handled in the Spells phase (see below).

### 2. Resolve grants (step by step)

Phases and order are in `LevelUpScreen.phasesForLevel`: Preview → (Subclass if level ≥ 3) → **ClassFeatures** → **Spells** → Confirm.

| Phase          | What is resolved |
|----------------|------------------|
| **Subclass**   | Subclass choice (stored in model, then in `featureSelections` / `subclass` on confirm). |
| **ClassFeatures** | Fighting Style, Land Type, Hunter’s Prey, Expertise, Extra Skills, etc. Stored in model and then merged into `ch.featureSelections` on confirm. |
| **Spells**     | (1) **Fighting-style cantrips:** If resolved fighting style is Blessed Warrior or Druidic Warrior, the UI shows “Choose 2 Cleric/Druid cantrips”; choices stored in `model.fightingStyleGrantCantrips`. (2) Class spell progression: spellbook (Wizard), prepared spells. On confirm, a `SpellGrant(2, 0, "Cleric"/"Druid", "Blessed Warrior"/"Druidic Warrior", chosen)` is appended to `ch.spellGrants` when applicable. |
| **Confirm**    | Character is updated: level, features, `featureSelections`, `spellGrants` (including the new fighting-style grant if present), prepared spells, spellbook, etc. |

So: **collect** (level summary: choice gates + spell progression) → **resolve** (Subclass → ClassFeatures → Spells, including fighting-style cantrips when relevant) → **visualise** (saved character and PDF show updated `spellGrants` and attacks/cantrips).

---

## Where grants are stored

All grants live in a single `grants: List[Grant]` field on both `CharacterDraft` and `Character`. Typed accessors (`spellGrants`, `skillGrants`, `attackGrants`) filter by subtype.

| Data           | Creation | Level-up |
|----------------|----------|----------|
| Choice gates   | `CharacterDraft.featureSelections` | Model then `Character.featureSelections` on confirm |
| All grants     | `CharacterDraft.grants` (unified list) | `Character.grants` (merged on confirm via `withSpellGrants`) |

JSON serialization writes three separate fields (`spellGrants`, `skillGrants`, `attackGrants`) for backward compatibility; the decoder reads them and merges into `List[Grant]`.

---

## Special case: Fighting-style cantrips (Blessed / Druidic Warrior)

- **Creation:**  
  - `FeatureGrant.FightingStyleChoice()` is a *choice gate* only; it does **not** produce a `SpellGrant` in `allGrantsForDraft` (which only converts `SpellChoice`, `SkillChoice`, `Attack`).  
  - When the user picks Blessed Warrior or Druidic Warrior on **ClassFeaturesScreen**, we append the corresponding spell grant via `FeatureGrants.spellGrantsForFightingStyle(fs)`. That grant is then resolved on the **Spells** screen like any other `draft.spellGrants` entry (generic “pick 2 from list” UI).

- **Level-up:**  
  - The same fighting style is chosen in the Class Features phase. The Spells phase has **dedicated** state (`fightingStyleGrantCantrips`) and UI for “Choose 2 Cleric/Druid cantrips”, and on confirm we push a new `SpellGrant(…, chosen)` onto `ch.spellGrants`. So level-up does not currently use the generic “list of spell grants”; it special-cases this one.

**Unifying with the generic design:**  
In a fully generic design, “Fighting Style: Blessed/Druidic Warrior” would be represented as a *conditional* spell grant: when the choice gate `FightingStyleChoice` is resolved to one of those two, a `SpellChoice(2, 0, "Cleric")` / `SpellChoice(2, 0, "Druid")` would be added to the **resolvable** grants for that level. Then:

- **Creation:** After Class Features, `allGrantsForDraft` (or a post-step that takes `featureSelections` into account) would include this spell grant in `draft.spellGrants`, and the existing Spells screen would resolve it with no extra code.
- **Level-up:** The level-up flow would build a single list of “spell grants for this level” (including the fighting-style one when applicable); the same generic “resolve each spell grant” step would handle it, without a separate `fightingStyleGrantCantrips` or special-case UI.

Until that refactor, fighting-style cantrips remain a special case in both flows (creation: append grant when leaving Class Features; level-up: dedicated state and merge on confirm).

---

## Summary diagram

```
Creation:
  [Species] → [Class] → [Background] ──► COLLECT: allGrantsForDraft(draft) → spell/skill/attack grants on draft
       ↓
  [Abilities] → [Skills] ──► RESOLVE: skillGrants filled on Skills screen
       ↓
  [Equipment] ← [Class Features] ──► choice gates resolved; append fighting-style spell grant if Blessed/Druidic
       ↓
  [Spells] ──► RESOLVE: spellGrants (incl. fighting-style) + class cantrips/prepared/spellbook
       ↓
  [Languages] → [Review] ──► VISUALISE: build Character; validate; save
       ↓
  [Detail / PDF] ──► VISUALISE: grant cantrips/spells in spell list; grant attacks in attack block

Level-up:
  [Preview] → [Subclass?] → [Class Features] ──► RESOLVE: fighting style, land, hunter’s prey, expertise, …
       ↓
  [Spells] ──► RESOLVE: fighting-style cantrips (if Blessed/Druidic) + class prepared/spellbook
       ↓
  [Confirm] ──► Merge into Character (featureSelections, spellGrants); save
       ↓
  [Detail / PDF] ──► VISUALISE: same as creation
```

---

## Key files

| Concern | File(s) |
|--------|---------|
| Grant types | `dnd/FeatureGrants.scala` (`sealed trait Grant`, SpellGrant, SkillGrant, AttackGrant, Grants), `dnd/DndClass.scala` (FeatureGrant) |
| Collection | `dnd/FeatureGrants.scala` (allGrantsForDraft, grantsFromClass/Species/OriginFeat, spellGrantsForFightingStyle) |
| Class/level gains | `dnd/ClassProgression.scala`, `dnd/LevelSummary.scala` |
| Creation: collect | `screens/BackgroundScreen.scala` (sets draft grants), `screens/ClassFeaturesScreen.scala` (appends fighting-style spell grant) |
| Creation: resolve | `screens/ClassFeaturesScreen.scala`, `screens/SkillsScreen.scala`, `screens/SpellsScreen.scala` |
| Creation: visualise | `screens/ReviewScreen.scala`, `common/pdf/CharacterSheetPdf.scala`, `screens/CharacterDetailScreen.scala` |
| Level-up: collect | `screens/LevelUpScreen.scala` (phasesForLevel, LevelSummary), `dnd/ClassProgression.scala` |
| Level-up: resolve | `screens/LevelUpScreen.scala` (Class Features phase, Spells phase, Confirm) |
| Level-up: visualise | Same Character / PDF as creation |
| Validation | `dnd/CharacterValidator.scala` (validateSpellGrants, validateSkillGrants) |
