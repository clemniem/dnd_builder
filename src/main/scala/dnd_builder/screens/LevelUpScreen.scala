package dndbuilder.screens

import cats.effect.IO
import dndbuilder.{NavigateNext, Screen, ScreenId, ScreenOutput, StorageKeys, StoredCharacter}
import dndbuilder.common.LocalStorageUtils
import dndbuilder.dnd.*
import tyrian.Html.*
import tyrian.*

object LevelUpScreen extends Screen {
  type Model = LevelUpModel
  type Msg   = LevelUpMsg | NavigateNext

  val screenId: ScreenId = ScreenId.LevelUpId

  private val targetLevel = 3

  def init(previous: Option[ScreenOutput]): (Model, Cmd[IO, Msg]) = {
    val sc = previous match {
      case Some(ScreenOutput.LevelUp(c)) => c
      case _ =>
        StoredCharacter("?", Character(
          "Unknown", Human, List(ClassLevel(Barbarian, 1)),
          Acolyte, AbilityScores.default,
          BackgroundBonus.ThreePlusOnes(Ability.Intelligence, Ability.Wisdom, Ability.Charisma),
          Set.empty, None, false, Nil, Nil, Nil, Nil, ClassFeatureSelections.empty, None, Human.languages, Coins.empty
        ))
    }
    val ch = sc.character
    val cls = ch.primaryClass
    val nextProg = SpellProgression.forClass(cls, targetLevel)
    val initialSpells = nextProg match {
      case Some(prog) =>
        val fromBook = if cls == Wizard then ch.spellbookSpells else Nil
        val prepared = if ch.preparedSpells.size <= prog.preparedSpells then ch.preparedSpells else ch.preparedSpells.take(prog.preparedSpells)
        (fromBook, prepared)
      case None => (Nil, Nil)
    }
    val model = LevelUpModel(
      storedCharacter = sc,
      phase = LevelUpPhase.Preview,
      chosenSubclass = None,
      chosenCantrips = ch.chosenCantrips,
      preparedSpells = initialSpells._2,
      spellbookSpells = initialSpells._1,
      extraSkills = Set.empty,
      landType = None,
      hunterPrey = None,
      saving = false,
      errors = Nil
    )
    (model, Cmd.None)
  }

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = {
    case LevelUpMsg.Next =>
      val ch = model.storedCharacter.character
      val cls = ch.primaryClass
      val nextPhase = nextPhaseFrom(model.phase, cls)
      nextPhase match {
        case Some(LevelUpPhase.Confirm) =>
          (model.copy(phase = LevelUpPhase.Confirm), Cmd.None)
        case Some(p) =>
          (model.copy(phase = p), Cmd.None)
        case None =>
          (model, Cmd.None)
      }

    case LevelUpMsg.AcceptSubclass =>
      val ch = model.storedCharacter.character
      val cls = ch.primaryClass
      val sub = Subclass.forClass(cls)
      val withSub = model.copy(chosenSubclass = sub)
      nextPhaseFrom(LevelUpPhase.Subclass, cls) match {
        case Some(p) => (withSub.copy(phase = p), Cmd.None)
        case None    => (withSub.copy(phase = LevelUpPhase.Confirm), Cmd.None)
      }

    case LevelUpMsg.ToggleExtraSkill(skill) =>
      val ch = model.storedCharacter.character
      val cls = ch.primaryClass
      val (count, pool) = extraSkillChoiceAtLevel3(cls)
      if count == 0 then (model, Cmd.None)
      else {
        val current = model.extraSkills
        val newSet =
          if current.contains(skill) then current - skill
          else if current.size < count && pool.contains(skill) then current + skill
          else current
        (model.copy(extraSkills = newSet), Cmd.None)
      }

    case LevelUpMsg.SetLandType(lt) =>
      (model.copy(landType = Some(lt)), Cmd.None)

    case LevelUpMsg.SetHunterPrey(hp) =>
      (model.copy(hunterPrey = Some(hp)), Cmd.None)

    case LevelUpMsg.TogglePrepared(spell) =>
      val ch = model.storedCharacter.character
      val cls = ch.primaryClass
      val maxPrepared = SpellProgression.forClass(cls, targetLevel).map(_.preparedSpells).getOrElse(0)
      val current = model.preparedSpells
      val newList =
        if current.exists(_.name == spell.name) then current.filterNot(_.name == spell.name)
        else if current.size < maxPrepared then current :+ spell
        else current
      (model.copy(preparedSpells = newList), Cmd.None)

    case LevelUpMsg.ToggleSpellbook(spell) =>
      val maxBook = SpellProgression.wizardSpellbookSize(targetLevel)
      val current = model.spellbookSpells
      val newList =
        if current.exists(_.name == spell.name) then current.filterNot(_.name == spell.name)
        else if current.size < maxBook then current :+ spell
        else current
      (model.copy(spellbookSpells = newList), Cmd.None)

    case LevelUpMsg.Confirm =>
      val ch = model.storedCharacter.character
      val cls = ch.primaryClass
      val subOpt = model.chosenSubclass.orElse(Subclass.forClass(cls))
      subOpt match {
        case Some(sub) =>
          val subclassSpells = sub.alwaysPreparedByLevel.getOrElse(targetLevel, Nil)
          val newClassLevels = ch.classLevels match {
            case head :: tail => ClassLevel(head.dndClass, targetLevel) :: tail
            case Nil          => List(ClassLevel(cls, targetLevel))
          }
          val newPrepared = (model.preparedSpells ++ subclassSpells).distinctBy(_.name)
          val newFeatures = ch.featureSelections.copy(
            landType = model.landType.orElse(ch.featureSelections.landType),
            hunterPrey = model.hunterPrey.orElse(ch.featureSelections.hunterPrey)
          )
          val updated = ch.copy(
            classLevels = newClassLevels,
            subclass = Some(sub),
            chosenSkills = ch.chosenSkills ++ model.extraSkills,
            preparedSpells = newPrepared,
            spellbookSpells = if cls == Wizard then model.spellbookSpells else ch.spellbookSpells,
            featureSelections = newFeatures
          )
          val updatedStored = StoredCharacter(model.storedCharacter.id, updated)
          (model.copy(saving = true),
            LocalStorageUtils.loadList[StoredCharacter, Msg](StorageKeys.characters)(
              existing => LevelUpMsg.Loaded(existing, updatedStored),
              _ => LevelUpMsg.Loaded(Nil, updatedStored),
              (msg, _) => LevelUpMsg.Error(msg)
            ))

        case None =>
          (model.copy(errors = List("Subclass not found for class.")), Cmd.None)
      }

    case LevelUpMsg.Loaded(existing, updatedChar) =>
      val newList = existing.map { sc =>
        if sc.id == updatedChar.id then updatedChar else sc
      }
      (model,
        LocalStorageUtils.saveList(StorageKeys.characters, newList)(
          _ => LevelUpMsg.Saved(updatedChar),
          (msg, _) => LevelUpMsg.Error(msg)
        ))

    case LevelUpMsg.Saved(updatedChar) =>
      (model, Cmd.Emit(NavigateNext(ScreenId.DetailId, Some(ScreenOutput.ViewCharacter(updatedChar)))))

    case LevelUpMsg.Error(msg) =>
      (model.copy(saving = false, errors = List(msg)), Cmd.None)

    case LevelUpMsg.Cancel =>
      (model, Cmd.Emit(NavigateNext(ScreenId.DetailId,
        Some(ScreenOutput.ViewCharacter(model.storedCharacter)))))

    case _: NavigateNext =>
      (model, Cmd.None)
  }

  private def nextPhaseFrom(current: LevelUpPhase, cls: DndClass): Option[LevelUpPhase] =
    current match {
      case LevelUpPhase.Preview       => Some(LevelUpPhase.Subclass)
      case LevelUpPhase.Subclass     => if hasSpellChangesAtLevel3(cls) then Some(LevelUpPhase.Spells) else if hasClassFeatureChoicesAtLevel3(cls) then Some(LevelUpPhase.ClassFeatures) else Some(LevelUpPhase.Confirm)
      case LevelUpPhase.Spells       => if hasClassFeatureChoicesAtLevel3(cls) then Some(LevelUpPhase.ClassFeatures) else Some(LevelUpPhase.Confirm)
      case LevelUpPhase.ClassFeatures => Some(LevelUpPhase.Confirm)
      case LevelUpPhase.Confirm      => None
    }

  private def hasSpellChangesAtLevel3(cls: DndClass): Boolean =
    SpellProgression.forClass(cls, targetLevel).isDefined

  private def hasClassFeatureChoicesAtLevel3(cls: DndClass): Boolean =
    extraSkillChoiceAtLevel3(cls)._1 > 0 || cls == Druid || cls == Ranger

  private def extraSkillChoiceAtLevel3(cls: DndClass): (Int, Set[Skill]) =
    cls match {
      case Barbarian => (1, Barbarian.skillPool)
      case Bard      => (3, Skill.values.toSet)
      case _         => (0, Set.empty)
    }

  def view(model: Model): Html[Msg] = {
    val ch = model.storedCharacter.character
    div(`class` := "screen-container")(
      div(`class` := "screen-header")(
        h1(`class` := "screen-title")(text(s"Level Up: ${ch.name}")),
        button(`class` := "btn-ghost", onClick(LevelUpMsg.Cancel))(text("< Back"))
      ),
      model.phase match {
        case LevelUpPhase.Preview       => previewView(model)
        case LevelUpPhase.Subclass     => subclassView(model)
        case LevelUpPhase.Spells       => spellsView(model)
        case LevelUpPhase.ClassFeatures => classFeaturesView(model)
        case LevelUpPhase.Confirm      => confirmView(model)
      },
      errorsView(model.errors)
    )
  }

  private def previewView(model: Model): Html[Msg] = {
    val ch = model.storedCharacter.character
    val cls = ch.primaryClass
    val nextClassLevel = targetLevel
    val gain = ClassProgression.atLevel(cls, nextClassLevel)
    val conMod = ch.modifier(Ability.Constitution)
    val hpGain = cls.hitDie.avgGain + conMod.toInt + ch.species.hpBonusPerLevel
    val oldProf = ch.proficiencyBonus.toInt
    val newProf = if targetLevel <= 4 then 2 else 3
    val oldProg = ch.spellProgression
    val newProg = SpellProgression.forClass(cls, nextClassLevel)
    val sub = Subclass.forClass(cls)
    val nextLabel = sub match {
      case Some(s) => s"Next: ${s.name} >"
      case None    => "Next >"
    }
    div(
      div(`class` := "flex-row", style := "margin-bottom: 1rem; gap: 0.5rem;")(
        span(`class` := "badge")(text(s"${cls.name} ${ch.primaryClassLevel}")),
        span(style := "font-size: 1.2rem; font-weight: bold; color: var(--color-text-muted);")(text("->")),
        span(`class` := "badge", style := "font-weight: bold;")(text(s"${cls.name} $nextClassLevel"))
      ),
      div(`class` := "section-title")(text("Stats Comparison")),
      combatTable(ch, hpGain, oldProf, newProf, targetLevel, cls),
      spellTable(oldProg, newProg),
      (if gain.features.nonEmpty then
        div(
          div(`class` := "section-title")(text("New Features")),
          div(`class` := "feature-list")(
            gain.features.map { f =>
              div(`class` := "feature-item")(
                div(`class` := "feature-name")(text(f.name)),
                div(`class` := "feature-desc")(text(f.description))
              )
            }*
          )
        )
      else div()),
      sub match {
        case Some(s) =>
          div(style := "margin-top: 1rem; padding: 0.75rem; background: var(--color-bg-muted); border-radius: 4px;")(
            text(s"You will gain subclass: ${s.name}")
          )
        case None => div()
      },
      StepNav("Cancel", LevelUpMsg.Cancel, nextLabel, LevelUpMsg.Next, true)
    )
  }

  private def subclassView(model: Model): Html[Msg] = {
    val ch = model.storedCharacter.character
    val cls = ch.primaryClass
    val subOpt = Subclass.forClass(cls)
    subOpt match {
      case Some(sub) =>
        val l3Features = sub.features.getOrElse(3, Nil)
        val alwaysPrepared = sub.alwaysPreparedByLevel.getOrElse(3, Nil)
        div(
          h2(`class` := "screen-title")(text("Your Subclass")),
          div(`class` := "card", style := "margin-bottom: 1rem; padding: 1rem;")(
            div(`class` := "card-title")(text(sub.name)),
            p(`class` := "card-desc")(text(sub.description)),
            div(`class` := "section-title", style := "margin-top: 0.75rem;")(text("Level 3 Features")),
            div(`class` := "feature-list")(
              l3Features.map { f =>
                div(`class` := "feature-item")(
                  div(`class` := "feature-name")(text(f.name)),
                  div(`class` := "feature-desc")(text(f.description))
                )
              }*
            ),
            (if alwaysPrepared.nonEmpty then
              div(style := "margin-top: 0.5rem;")(
                div(`class` := "feature-name")(text("Always prepared: " + alwaysPrepared.map(_.name).mkString(", ")))
              )
            else div())
          ),
          StepNav("< Back", LevelUpMsg.Cancel, "Accept", LevelUpMsg.AcceptSubclass, true)
        )
      case None =>
        div(StepNav("< Back", LevelUpMsg.Cancel, "Next >", LevelUpMsg.Next, true))
    }
  }

  private def spellsView(model: Model): Html[Msg] = {
    val ch = model.storedCharacter.character
    val cls = ch.primaryClass
    val prog = SpellProgression.forClass(cls, targetLevel).get
    val maxPrepared = prog.preparedSpells
    val isWizard = cls == Wizard
    val spellbookSize = SpellProgression.wizardSpellbookSize(targetLevel)
    val maxSpellLvl = SpellProgression.maxSpellLevelForSlots(cls, targetLevel)
    val availableForPrepared =
      if isWizard then model.spellbookSpells
      else (1 to maxSpellLvl).flatMap(l => Spell.forClass(cls, l)).toList
    val remainingPrepared = maxPrepared - model.preparedSpells.size
    val remainingBook = if isWizard then spellbookSize - model.spellbookSpells.size else 0

    val needSpellbookPhase = isWizard && remainingBook > 0
    val spellbookSection =
      if !needSpellbookPhase then div()
      else {
        val availableBook = (1 to maxSpellLvl).flatMap(l => Spell.forClass(cls, l)).toList
        div(style := "margin-bottom: 1.5rem;")(
          div(`class` := "section-title")(text("Add 2 spells to your spellbook")),
          div(`class` := "points-pool", style := "margin-bottom: 0.5rem;")(text(s"Remaining: $remainingBook")),
          spellListGrouped(availableBook, model.spellbookSpells, LevelUpMsg.ToggleSpellbook.apply)
        )
      }

    div(
      h2(`class` := "screen-title")(text("Spell Changes")),
      p(`class` := "screen-intro")(
        text(if isWizard then s"Add 2 spells to your spellbook, then choose $maxPrepared prepared spells." else s"Choose $maxPrepared prepared spells.")
      ),
      spellbookSection,
      div(
        div(`class` := "section-title")(text(if isWizard then "Prepared spells (from spellbook)" else "Prepared spells")),
        div(`class` := "points-pool", style := "margin-bottom: 0.5rem;")(text(s"Chosen: ${model.preparedSpells.size} / $maxPrepared")),
        spellListGrouped(availableForPrepared, model.preparedSpells, LevelUpMsg.TogglePrepared.apply)
      ),
      StepNav(
        "< Back",
        LevelUpMsg.Cancel,
        "Next >",
        LevelUpMsg.Next,
        remainingPrepared == 0 && (!needSpellbookPhase || remainingBook == 0)
      )
    )
  }

  private def spellListGrouped(
      available: List[Spell],
      selected: List[Spell],
      toggleMsg: Spell => LevelUpMsg
  ): Html[Msg] = {
    val bySchool = available.groupBy(_.school).toList.sortBy(_._1.label)
    div(
      bySchool.map { case (school, spells) =>
        div(`class` := "skill-group")(
          div(`class` := "skill-group-title")(text(school.label)),
          div(
            spells.sortBy(_.name).map { spell =>
              val isChosen = selected.exists(_.name == spell.name)
              val clsName = if isChosen then "skill-item skill-item--selected" else "skill-item"
              div(`class` := clsName, onClick(toggleMsg(spell)))(
                div(`class` := "skill-checkbox")(text(if isChosen then "*" else "")),
                span(`class` := "skill-label")(text(spell.name)),
                span(`class` := "skill-ability-tag")(text(spell.school.label.take(4)))
              )
            }*
          )
        )
      }*
    )
  }

  private def classFeaturesView(model: Model): Html[Msg] = {
    val ch = model.storedCharacter.character
    val cls = ch.primaryClass
    val (extraCount, extraPool) = extraSkillChoiceAtLevel3(cls)
    val canProceed = (extraCount == 0 || model.extraSkills.size == extraCount) &&
      (cls != Druid || model.landType.isDefined) &&
      (cls != Ranger || model.hunterPrey.isDefined)

    val skillSection =
      if extraCount == 0 then div()
      else
        div(style := "margin-bottom: 1.5rem;")(
          div(`class` := "section-title")(text(s"Choose $extraCount extra skill(s)")),
          div(`class` := "points-pool", style := "margin-bottom: 0.5rem;")(text(s"Chosen: ${model.extraSkills.size} / $extraCount")),
          div(
            Skill.byAbility.toList.sortBy(_._1.ordinal).flatMap { case (ability, skills) =>
              val inPool = skills.filter(extraPool.contains)
              if inPool.isEmpty then Nil
              else
                List(div(`class` := "skill-group")(
                  div(`class` := "skill-group-title")(text(ability.label)),
                  div(
                    inPool.map { skill =>
                      val isChosen = model.extraSkills.contains(skill)
                      val clsName = if isChosen then "skill-item skill-item--selected" else "skill-item"
                      div(`class` := clsName, onClick(LevelUpMsg.ToggleExtraSkill(skill)))(
                        div(`class` := "skill-checkbox")(text(if isChosen then "*" else "")),
                        span(`class` := "skill-label")(text(skill.label)),
                        span(`class` := "skill-ability-tag")(text(ability.abbreviation))
                      )
                    }*
                  )
                ))
            }*
          )
        )

    val landSection =
      if cls != Druid then div()
      else
        div(style := "margin-bottom: 1.5rem;")(
          div(`class` := "section-title")(text("Circle of the Land: choose land type")),
          div(`class` := "card-grid")(
            LandType.values.toList.map { lt =>
              val selected = model.landType.contains(lt)
              div(
                `class` := (if selected then "card card--selected" else "card"),
                onClick(LevelUpMsg.SetLandType(lt))
              )(
                div(`class` := "card-title")(text(lt.label))
              )
            }*
          )
        )

    val hunterSection =
      if cls != Ranger then div()
      else
        div(style := "margin-bottom: 1.5rem;")(
          div(`class` := "section-title")(text("Hunter's Prey")),
          div(`class` := "card-grid")(
            HunterPreyChoice.values.toList.map { hp =>
              val selected = model.hunterPrey.contains(hp)
              div(
                `class` := (if selected then "card card--selected" else "card"),
                onClick(LevelUpMsg.SetHunterPrey(hp))
              )(
                div(`class` := "card-title")(text(hp.label)),
                div(`class` := "card-desc")(text(hp.description))
              )
            }*
          )
        )

    div(
      h2(`class` := "screen-title")(text("Class Feature Choices")),
      skillSection,
      landSection,
      hunterSection,
      StepNav("< Back", LevelUpMsg.Cancel, "Next >", LevelUpMsg.Next, canProceed)
    )
  }

  private def confirmView(model: Model): Html[Msg] = {
    val cls = model.storedCharacter.character.primaryClass
    val sub = model.chosenSubclass.orElse(Subclass.forClass(cls))
    val summaryItems: List[Html[Msg]] =
      sub.map(s => div(`class` := "feature-item")(div(`class` := "feature-name")(text(s"Subclass: ${s.name}")))).toList ++
        (if model.extraSkills.nonEmpty then List(div(`class` := "feature-item")(div(`class` := "feature-name")(text(s"Extra skills: ${model.extraSkills.map(_.label).mkString(", ")}")))) else Nil) ++
        model.landType.map(lt => div(`class` := "feature-item")(div(`class` := "feature-name")(text(s"Land type: ${lt.label}")))).toList ++
        model.hunterPrey.map(hp => div(`class` := "feature-item")(div(`class` := "feature-name")(text(s"Hunter's Prey: ${hp.label}")))).toList
    div(
      div(`class` := "section-title")(text("Summary")),
      div(`class` := "feature-list")(summaryItems*),
      StepNav("Cancel", LevelUpMsg.Cancel, if model.saving then "Saving..." else s"Level Up to $targetLevel", LevelUpMsg.Confirm, !model.saving)
    )
  }

  private def combatTable(
      ch: Character,
      hpGain: Int,
      oldProf: Int,
      newProf: Int,
      nextLevel: Int,
      cls: DndClass
  ): Html[Msg] = {
    val oldHp = ch.maxHitPoints
    val newHp = oldHp + hpGain
    val oldHitDice = ch.hitDiceString
    val newHitDice = s"${nextLevel}d${cls.hitDie.sides}"
    val rows: List[Html[Msg]] = List(
      compareRow("Level", ch.characterLevel.toString, nextLevel.toString, ch.characterLevel != nextLevel),
      compareRow("Hit Points", oldHp.toString, s"$newHp (+$hpGain)", true),
      compareRow("Hit Dice", oldHitDice, newHitDice, oldHitDice != newHitDice),
      compareRow("Proficiency Bonus", s"+$oldProf", s"+$newProf", oldProf != newProf)
    )
    table(`class` := "compare-table")(
      thead(tr(th(text("")), th(text("Current")), th(text("New")))),
      tbody(rows*)
    )
  }

  private def spellTable(old: Option[SpellSlotRow], next: Option[SpellSlotRow]): Html[Msg] =
    (old, next) match {
      case (Some(o), Some(n)) =>
        val rows: List[Html[Msg]] = List(
          if o.cantrips > 0 || n.cantrips > 0 then
            Some(compareRow("Cantrips Known", o.cantrips.toString, n.cantrips.toString, o.cantrips != n.cantrips))
          else None,
          if o.preparedSpells > 0 || n.preparedSpells > 0 then
            Some(compareRow("Prepared Spells", o.preparedSpells.toString, n.preparedSpells.toString, o.preparedSpells != n.preparedSpells))
          else None
        ).flatten ++ slotRows(o.slots, n.slots)
        if rows.isEmpty then div()
        else div(
          div(`class` := "section-title")(text("Spellcasting")),
          table(`class` := "compare-table")(
            thead(tr(th(text("")), th(text("Current")), th(text("New")))),
            tbody(rows*)
          )
        )
      case (None, Some(n)) =>
        val rows: List[Html[Msg]] = List(
          if n.cantrips > 0 then Some(compareRow("Cantrips Known", "-", n.cantrips.toString, true)) else None,
          if n.preparedSpells > 0 then Some(compareRow("Prepared Spells", "-", n.preparedSpells.toString, true)) else None
        ).flatten ++ n.slots.zipWithIndex.collect { case (cnt, i) if cnt > 0 =>
          compareRow(s"Level ${i + 1} Slots", "-", cnt.toString, true)
        }
        if rows.isEmpty then div()
        else div(
          div(`class` := "section-title")(text("Spellcasting (New!)")),
          table(`class` := "compare-table")(
            thead(tr(th(text("")), th(text("Current")), th(text("New")))),
            tbody(rows*)
          )
        )
      case _ => div()
    }

  private def slotRows(oldSlots: List[Int], newSlots: List[Int]): List[Html[Msg]] =
    oldSlots.zipAll(newSlots, 0, 0).zipWithIndex.collect {
      case ((ov, nv), i) if ov > 0 || nv > 0 =>
        compareRow(s"Level ${i + 1} Slots", ov.toString, nv.toString, ov != nv)
    }

  private def compareRow(label: String, oldVal: String, newVal: String, changed: Boolean): Html[Msg] =
    tr(
      td(text(label)),
      td(`class` := "val-old")(text(oldVal)),
      td(`class` := (if changed then "val-changed" else "val-new"))(text(newVal))
    )

  private def errorsView(errors: List[String]): Html[Msg] =
    if errors.isEmpty then div()
    else div(`class` := "error-box")(
      errors.map(e => div(text(e)))*
    )
}

enum LevelUpPhase {
  case Preview, Subclass, Spells, ClassFeatures, Confirm
}

final case class LevelUpModel(
    storedCharacter: StoredCharacter,
    phase: LevelUpPhase,
    chosenSubclass: Option[Subclass],
    chosenCantrips: List[Spell],
    preparedSpells: List[Spell],
    spellbookSpells: List[Spell],
    extraSkills: Set[Skill],
    landType: Option[LandType],
    hunterPrey: Option[HunterPreyChoice],
    saving: Boolean,
    errors: List[String]
)

enum LevelUpMsg {
  case Next
  case AcceptSubclass
  case ToggleExtraSkill(skill: Skill)
  case SetLandType(landType: LandType)
  case SetHunterPrey(choice: HunterPreyChoice)
  case TogglePrepared(spell: Spell)
  case ToggleSpellbook(spell: Spell)
  case Confirm
  case Cancel
  case Loaded(existing: List[StoredCharacter], updatedChar: StoredCharacter)
  case Saved(updatedChar: StoredCharacter)
  case Error(msg: String)
}
