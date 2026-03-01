package dndbuilder.screens

import cats.effect.IO
import dndbuilder.{NavigateNext, Screen, ScreenId, ScreenOutput}
import dndbuilder.dnd.*
import tyrian.Html.*
import tyrian.*

object LevelSelectScreen extends Screen {
  type Model = LevelSelectModel
  type Msg   = LevelSelectMsg | NavigateNext

  val screenId: ScreenId = ScreenId.LevelSelectId

  def init(previous: Option[ScreenOutput]): (Model, Cmd[IO, Msg]) = {
    val draft = previous match {
      case Some(ScreenOutput.Draft(d)) => d
      case _ => CharacterDraft.empty
    }
    (LevelSelectModel(draft, draft.level.getOrElse(1)), Cmd.None)
  }

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = {
    case LevelSelectMsg.SetLevel(lvl) =>
      val clamped = math.max(1, math.min(maxLevel(model), lvl))
      (model.copy(selectedLevel = clamped), Cmd.None)
    case LevelSelectMsg.Increment =>
      val max = maxLevel(model)
      val next = math.min(max, model.selectedLevel + 1)
      (model.copy(selectedLevel = next), Cmd.None)
    case LevelSelectMsg.Decrement =>
      val next = math.max(1, model.selectedLevel - 1)
      (model.copy(selectedLevel = next), Cmd.None)
    case LevelSelectMsg.Next =>
      val updated = model.draft.copy(level = Some(model.selectedLevel))
      (model, Cmd.Emit(NavigateNext(ScreenId.BackgroundId, Some(ScreenOutput.Draft(updated)))))
    case LevelSelectMsg.Back =>
      val updated = model.draft.copy(level = Some(model.selectedLevel))
      (model, Cmd.Emit(NavigateNext(ScreenId.ClassSelectId, Some(ScreenOutput.Draft(updated)))))
    case _: NavigateNext =>
      (model, Cmd.None)
  }

  private def maxLevel(model: LevelSelectModel): Int =
    model.draft.dndClass.map(ClassProgression.maxSupportedLevel).getOrElse(1)

  def view(model: Model): Html[Msg] = {
    val cls = model.draft.resolvedClass
    val max = maxLevel(model)
    val lvl = model.selectedLevel
    val prog = SpellProgression.forClass(cls, lvl)
    val features = ClassProgression.featuresUpToLevel(cls, lvl)

    div(`class` := "screen-container")(
      StepIndicator(3, cls.isSpellcaster),
      StepNav(StepIndicator.backLabel(3, cls.isSpellcaster), LevelSelectMsg.Back, StepIndicator.nextLabel(3, cls.isSpellcaster), LevelSelectMsg.Next, true),
      h1(`class` := "screen-title")(text("Starting Level")),
      p(`class` := "screen-intro")(
        text(s"Choose the level for your ${cls.name}. Currently supports up to level $max.")
      ),
      div(`class` := "level-picker", style := "display: flex; align-items: center; gap: 1rem; margin: 1.5rem 0;")(
        button(
          `class` := (if lvl <= 1 then "btn btn--secondary btn-disabled" else "btn btn--secondary"),
          onClick(LevelSelectMsg.Decrement)
        )(text("-")),
        div(style := "text-align: center; min-width: 5rem;")(
          div(style := "font-size: 2.5rem; font-weight: bold;")(text(lvl.toString)),
          div(style := "font-size: 0.85rem; color: var(--color-text-muted);")(text("Level"))
        ),
        button(
          `class` := (if lvl >= max then "btn btn--secondary btn-disabled" else "btn btn--secondary"),
          onClick(LevelSelectMsg.Increment)
        )(text("+"))
      ),
      levelSummary(cls, lvl, prog, features)
    )
  }

  private def levelSummary(
      cls: DndClass,
      lvl: Int,
      prog: Option[SpellSlotRow],
      features: List[Feature]
  ): Html[Msg] = {
    val conMod = 0
    val hp = cls.hitDie.sides + conMod + (if lvl > 1 then (lvl - 1) * (cls.hitDie.avgGain + conMod) else 0)
    val profBonus = lvl match {
      case l if l <= 4  => 2
      case l if l <= 8  => 3
      case l if l <= 12 => 4
      case l if l <= 16 => 5
      case _            => 6
    }

    div(
      div(`class` := "stat-block", style := "margin-bottom: 1rem;")(
        statBox("Base HP", hp.toString, s"d${cls.hitDie.sides} + CON"),
        statBox("Prof. Bonus", s"+$profBonus", ""),
        statBox("Hit Dice", s"${lvl}d${cls.hitDie.sides}", "")
      ),
      prog match {
        case Some(row) =>
          val slotsStr = row.slots.zipWithIndex.collect { case (n, i) if n > 0 => s"Lv${i + 1}: $n" }.mkString("  ")
          div(`class` := "stat-block", style := "margin-bottom: 1rem;")(
            (if row.cantrips > 0 then statBox("Cantrips", row.cantrips.toString, "") else div()),
            statBox("Prepared", row.preparedSpells.toString, "spells"),
            statBox("Spell Slots", slotsStr, "")
          )
        case None => div()
      },
      div(`class` := "section-title")(text("Class Features")),
      div(`class` := "feature-list")(
        features.map { f =>
          div(`class` := "feature-item")(
            div(`class` := "feature-name")(text(f.name)),
            div(`class` := "feature-desc")(text(f.description))
          )
        }*
      )
    )
  }

  private def statBox(label: String, value: String, sub: String): Html[Msg] =
    div(`class` := "stat-box")(
      div(`class` := "stat-box-label")(text(label)),
      div(`class` := "stat-box-value")(text(value)),
      (if sub.nonEmpty then div(`class` := "stat-box-sub")(text(sub)) else div())
    )
}

final case class LevelSelectModel(
    draft: CharacterDraft,
    selectedLevel: Int)

enum LevelSelectMsg {
  case SetLevel(lvl: Int)
  case Increment
  case Decrement
  case Next
  case Back
}
