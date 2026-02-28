package dndbuilder.screens

import cats.effect.IO
import dndbuilder.{NavigateNext, Screen, ScreenId, ScreenOutput}
import dndbuilder.dnd.*
import tyrian.Html.*
import tyrian.*

object AbilityScoresScreen extends Screen {
  type Model = AbilityScoresModel
  type Msg   = AbilityScoresMsg | NavigateNext

  val screenId: ScreenId = ScreenId.AbilitiesId

  def init(previous: Option[ScreenOutput]): (Model, Cmd[IO, Msg]) = {
    val draft = previous match {
      case Some(ScreenOutput.Draft(d)) => d
      case _ => CharacterDraft.empty
    }

    val cls = draft.dndClass.getOrElse(Barbarian)
    val bg  = draft.background.getOrElse(Acolyte)
    val initialScores = draft.baseScores.getOrElse(cls.recommendedScores)
    val opts = bg.abilityOptionsList
    val initialBonus = draft.backgroundBonus.getOrElse(BackgroundBonus.TwoPlusOne(opts.head, opts(1)))

    (AbilityScoresModel(draft, initialScores, initialBonus, ScoreMethod.StandardArray, Nil), Cmd.None)
  }

  private val stdArraySum: Int = AbilityScores.standardArray.sum

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = {
    case AbilityScoresMsg.SetMethod(m) =>
      val cls = model.draft.dndClass.getOrElse(Barbarian)
      val newScores = m match {
        case ScoreMethod.StandardArray => cls.recommendedScores
        case ScoreMethod.PointBuy      => AbilityScores.default
      }
      (model.copy(baseScores = newScores, method = m), Cmd.None)

    case AbilityScoresMsg.Increment(ability) =>
      val current = model.baseScores.get(ability)
      if current < 15 then {
        val newScores = model.baseScores.adjust(ability, 1)
        val valid = model.method match {
          case ScoreMethod.PointBuy =>
            newScores.totalPointBuyCost match {
              case Right(cost) => cost <= AbilityScores.pointBuyTotal
              case Left(_) => false
            }
          case ScoreMethod.StandardArray =>
            newScores.toList.map(_._2).sum <= stdArraySum
        }
        if valid then (model.copy(baseScores = newScores), Cmd.None)
        else (model, Cmd.None)
      }
      else (model, Cmd.None)

    case AbilityScoresMsg.Decrement(ability) =>
      val current = model.baseScores.get(ability)
      if current > 8 then
        (model.copy(baseScores = model.baseScores.adjust(ability, -1)), Cmd.None)
      else (model, Cmd.None)

    case AbilityScoresMsg.SetBonus(bonus) =>
      (model.copy(backgroundBonus = bonus), Cmd.None)

    case AbilityScoresMsg.IncrementBonus(ability) =>
      val bg = model.draft.background.getOrElse(Acolyte)
      val currentIncreases = model.backgroundBonus.increases
      val currentForAbility = currentIncreases.find(_._1 == ability).map(_._2).getOrElse(0)
      val totalUsed = model.backgroundBonus.totalPoints
      if totalUsed >= 3 || currentForAbility >= 2 then (model, Cmd.None)
      else {
        val allowed = bg.abilityOptionsList.toSet
        if !allowed.contains(ability) then (model, Cmd.None)
        else {
          val newBonus = rebuildBonus(model.backgroundBonus, ability, 1, bg)
          (model.copy(backgroundBonus = newBonus), Cmd.None)
        }
      }

    case AbilityScoresMsg.DecrementBonus(ability) =>
      val bg = model.draft.background.getOrElse(Acolyte)
      val currentIncreases = model.backgroundBonus.increases
      val currentForAbility = currentIncreases.find(_._1 == ability).map(_._2).getOrElse(0)
      if currentForAbility <= 0 then (model, Cmd.None)
      else {
        val newBonus = rebuildBonus(model.backgroundBonus, ability, -1, bg)
        (model.copy(backgroundBonus = newBonus), Cmd.None)
      }

    case AbilityScoresMsg.AssignStdArray(ability, score) =>
      (model.copy(baseScores = model.baseScores.set(ability, score)), Cmd.None)

    case AbilityScoresMsg.Next =>
      val updated = model.draft.copy(
        baseScores = Some(model.baseScores),
        backgroundBonus = Some(model.backgroundBonus)
      )
      (model, Cmd.Emit(NavigateNext(ScreenId.SkillsId, Some(ScreenOutput.Draft(updated)))))

    case AbilityScoresMsg.Back =>
      val updated = model.draft.copy(
        baseScores = Some(model.baseScores),
        backgroundBonus = Some(model.backgroundBonus)
      )
      (model, Cmd.Emit(NavigateNext(ScreenId.BackgroundId, Some(ScreenOutput.Draft(updated)))))

    case _: NavigateNext =>
      (model, Cmd.None)
  }

  private def rebuildBonus(current: BackgroundBonus, ability: Ability, delta: Int, bg: Background): BackgroundBonus = {
    val opts = bg.abilityOptionsList
    val currentMap: Map[Ability, Int] = current.increases.toMap
    val newMap = currentMap.updated(ability, currentMap.getOrElse(ability, 0) + delta)
    val filled = opts.map(a => (a, newMap.getOrElse(a, 0)))
    val total = filled.map(_._2).sum
    if total == 3 then
      filled.filter(_._2 > 0) match {
        case (a1, 2) :: (a2, 1) :: Nil => BackgroundBonus.TwoPlusOne(a1, a2)
        case (a1, 1) :: (a2, 2) :: Nil => BackgroundBonus.TwoPlusOne(a2, a1)
        case (a1, 1) :: (a2, 1) :: (a3, 1) :: Nil => BackgroundBonus.ThreePlusOnes(a1, a2, a3)
        case _ => current
      }
    else current.increases.toMap.updated(ability, newMap.getOrElse(ability, 0)) match {
      case m =>
        val all = opts.map(a => (a, m.getOrElse(a, 0)))
        all.filter(_._2 > 0) match {
          case (a1, 2) :: (a2, v2) :: _ if v2 >= 1 => BackgroundBonus.TwoPlusOne(a1, a2)
          case (a1, v1) :: (a2, 2) :: _ if v1 >= 1 => BackgroundBonus.TwoPlusOne(a2, a1)
          case items if items.map(_._2).sum == 3 && items.forall(_._2 <= 1) && items.size >= 3 =>
            val filtered = items.filter(_._2 == 1)
            if filtered.size == 3 then BackgroundBonus.ThreePlusOnes(filtered(0)._1, filtered(1)._1, filtered(2)._1)
            else current
          case _ => current
        }
    }
  }

  def view(model: Model): Html[Msg] = {
    val cls = model.draft.dndClass.getOrElse(Barbarian)
    val bg  = model.draft.background.getOrElse(Acolyte)
    val finalScores = AbilityScores.applyBonus(model.baseScores, model.backgroundBonus)
    val pointsUsed = model.baseScores.totalPointBuyCost.getOrElse(0)
    val bonusUsed  = model.backgroundBonus.totalPoints

    div(`class` := "screen-container")(
      StepIndicator(5, cls.isSpellcaster),
      StepNav("< Background", AbilityScoresMsg.Back, "Next: Skills >", AbilityScoresMsg.Next, bonusUsed == 3),
      h1(`class` := "screen-title")(text("Ability Scores")),
      p(`class` := "screen-intro")(text("Assign your ability scores and distribute background bonuses.")),
      div(`class` := "flex-row", style := "margin-bottom: 1rem;")(
        methodToggle(model.method),
        (model.method match {
          case ScoreMethod.PointBuy =>
            div(`class` := (if pointsUsed > AbilityScores.pointBuyTotal then "points-pool points-pool--over" else "points-pool"))(
              text("Points: "),
              span(`class` := "points-pool-value")(text(s"$pointsUsed / ${AbilityScores.pointBuyTotal}"))
            )
          case ScoreMethod.StandardArray =>
            val currentSum = model.baseScores.toList.map(_._2).sum
            val spare = stdArraySum - currentSum
            val clsName = if spare > 0 then "points-pool points-pool--over" else "points-pool"
            div(`class` := clsName)(
              text("Unassigned: "),
              span(`class` := "points-pool-value")(text(s"$spare"))
            )
        }
        )
      ),
      scoresTable(model, finalScores),
      div(`class` := "flex-row", style := "margin-top: 1.25rem; margin-bottom: 0.5rem; align-items: baseline; gap: 0.75rem;")(
        h3(style := "margin: 0;")(text("Background Bonus")),
        span(style := "font-size: 0.85rem; color: var(--color-text-muted);")(
          text(s"${bg.abilityOptionsList.map(_.abbreviation).mkString(", ")} — max +2 each")
        ),
        div(`class` := "points-pool")(
          text("Used: "),
          span(`class` := "points-pool-value")(text(s"$bonusUsed / 3"))
        )
      ),
      bonusControls(model)
    )
  }

  private def methodToggle(current: ScoreMethod): Html[Msg] =
    div(`class` := "toggle-group")(
      button(
        `class` := (if current == ScoreMethod.StandardArray then "toggle-option toggle-option--active" else "toggle-option"),
        onClick(AbilityScoresMsg.SetMethod(ScoreMethod.StandardArray))
      )(text("Standard Array")),
      button(
        `class` := (if current == ScoreMethod.PointBuy then "toggle-option toggle-option--active" else "toggle-option"),
        onClick(AbilityScoresMsg.SetMethod(ScoreMethod.PointBuy))
      )(text("Point Buy"))
    )

  private def scoresTable(model: Model, finalScores: AbilityScores): Html[Msg] =
    table(`class` := "ability-table")(
      thead(
        tr(
          th(text("Ability")),
          th(text("Base")),
          th(text("")),
          th(text("Bonus")),
          th(text("Final")),
          th(text("Mod"))
        )
      ),
      tbody(
        Ability.values.toList.map { ability =>
          val base = model.baseScores.get(ability)
          val bonusAmt = model.backgroundBonus.increases.find(_._1 == ability).map(_._2).getOrElse(0)
          val final_ = finalScores.get(ability)
          val mod = AbilityScores.modifierString(final_)
          tr(
            td(`class` := "ability-name")(text(ability.label)),
            td(`class` := "ability-score")(text(base.toString)),
            td(
              div(`class` := "ability-controls")(
                button(onClick(AbilityScoresMsg.Decrement(ability)))(text("-")),
                button(onClick(AbilityScoresMsg.Increment(ability)))(text("+"))
              )
            ),
            td(style := "text-align: center; color: var(--color-success);")(
              text(if bonusAmt > 0 then s"+$bonusAmt" else "-")
            ),
            td(`class` := "ability-score")(text(final_.toString)),
            td(`class` := "ability-mod")(text(mod))
          )
        }*
      )
    )

  private def bonusControls(model: Model): Html[Msg] = {
    val bg = model.draft.background.getOrElse(Acolyte)
    val opts = bg.abilityOptionsList
    val currentMap = model.backgroundBonus.increases.toMap
    div(
      opts.map { ability =>
        val amount = currentMap.getOrElse(ability, 0)
        div(`class` := "bonus-row")(
          span(`class` := "bonus-ability-name")(text(ability.label)),
          div(`class` := "bonus-controls")(
            button(onClick(AbilityScoresMsg.DecrementBonus(ability)))(text("-")),
            span(`class` := "bonus-value")(text(s"+$amount")),
            button(onClick(AbilityScoresMsg.IncrementBonus(ability)))(text("+"))
          )
        )
      }*
    )
  }
}

final case class AbilityScoresModel(
    draft: CharacterDraft,
    baseScores: AbilityScores,
    backgroundBonus: BackgroundBonus,
    method: ScoreMethod,
    errors: List[String])

enum ScoreMethod {
  case StandardArray, PointBuy
}

enum AbilityScoresMsg {
  case SetMethod(m: ScoreMethod)
  case Increment(ability: Ability)
  case Decrement(ability: Ability)
  case IncrementBonus(ability: Ability)
  case DecrementBonus(ability: Ability)
  case SetBonus(bonus: BackgroundBonus)
  case AssignStdArray(ability: Ability, score: Int)
  case Next
  case Back
}
