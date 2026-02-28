package dndbuilder.screens

import cats.effect.IO
import dndbuilder.{NavigateNext, Screen, ScreenId, ScreenOutput}
import dndbuilder.dnd.*
import tyrian.Html.*
import tyrian.*

object BackgroundScreen extends Screen {
  type Model = BackgroundModel
  type Msg   = BackgroundMsg | NavigateNext

  val screenId: ScreenId = ScreenId.BackgroundId

  def init(previous: Option[ScreenOutput]): (Model, Cmd[IO, Msg]) = {
    val draft = previous match {
      case Some(ScreenOutput.Draft(d)) => d
      case _ => CharacterDraft.empty
    }
    (BackgroundModel(draft, draft.background, draft.chosenExtraLanguages), Cmd.None)
  }

  private def canProceed(model: Model): Boolean = {
    val cls = model.draft.dndClass.getOrElse(Barbarian)
    model.selectedBackground.isDefined &&
      (cls.extraLanguageChoices == 0 || model.chosenExtraLanguages.size == cls.extraLanguageChoices)
  }

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = {
    case BackgroundMsg.SelectBackground(bg) =>
      (model.copy(selectedBackground = Some(bg)), Cmd.None)

    case BackgroundMsg.Next =>
      model.selectedBackground match {
        case Some(bg) =>
          val updated = model.draft.copy(background = Some(bg), chosenExtraLanguages = model.chosenExtraLanguages)
          (model, Cmd.Emit(NavigateNext(ScreenId.AbilitiesId, Some(ScreenOutput.Draft(updated)))))
        case None =>
          (model, Cmd.None)
      }

    case BackgroundMsg.ToggleExtraLanguage(lang) =>
      val cls = model.draft.dndClass.getOrElse(Barbarian)
      val n = cls.extraLanguageChoices
      val current = model.chosenExtraLanguages
      val next =
        if current.contains(lang) then current - lang
        else if current.size < n then current + lang
        else current
      (model.copy(chosenExtraLanguages = next), Cmd.None)

    case BackgroundMsg.Back =>
      (model, Cmd.Emit(NavigateNext(ScreenId.ClassSelectId, Some(ScreenOutput.Draft(model.draft)))))

    case _: NavigateNext =>
      (model, Cmd.None)
  }

  def view(model: Model): Html[Msg] = {
    val cls = model.draft.dndClass.getOrElse(Barbarian)
    div(`class` := "screen-container")(
      StepIndicator(3, cls.isSpellcaster),
      StepNav("< Class", BackgroundMsg.Back, "Next: Abilities >", BackgroundMsg.Next, canProceed(model)),
      h1(`class` := "screen-title")(text("Background & Languages")),
      p(`class` := "screen-intro")(text("Choose your background (ability bonuses, skills, origin feat) and any extra languages from your class.")),
      div(`class` := "card-grid--2col card-grid")(
        Background.all.map { bg =>
          val isSel = model.selectedBackground.contains(bg)
          div(
            `class` := (if isSel then "card card--selected" else "card"),
            onClick(BackgroundMsg.SelectBackground(bg))
          )(
            div(`class` := "card-title")(text(bg.name)),
            div(`class` := "card-desc")(text(bg.description)),
            div(`class` := "flex-row", style := "margin-top: 0.5rem; gap: 0.4rem;")(
              span(`class` := "badge badge--feat")(text(bg.feat.name)),
              span(`class` := "badge")(text(bg.skillProficiencySet.map(_.label).mkString(", ")))
            ),
            div(style := "margin-top: 0.3rem; font-size: 0.8rem; color: var(--color-text-dim);")(
              text(s"Abilities: ${bg.abilityOptionsList.map(_.abbreviation).mkString(", ")}"),
              text(s" | Tools: ${bg.toolProficiency}")
            ),
            div(style := "margin-top: 0.2rem; font-size: 0.8rem; color: var(--color-text-dim);")(
              text(s"Starting gold: ${bg.startingGold} GP")
            )
          )
        }*
      ),
      selectedBackgroundDetail(model),
      languagesSection(model)
    )
  }

  private def languagesSection(model: Model): Html[Msg] = {
    val sp = model.draft.species.getOrElse(Human)
    val cls = model.draft.dndClass.getOrElse(Barbarian)
    val granted = sp.languages
    val needChoices = cls.extraLanguageChoices
    div(style := "margin-top: 1.5rem;")(
      div(`class` := "section-title")(text("Languages")),
      p(style := "font-size: 0.9rem; color: var(--color-text-dim); margin-bottom: 0.5rem;")(
        text("Everyone knows Common. Your species grants the following:")
      ),
      div(`class` := "prof-list", style := "margin-bottom: 0.75rem;")(
        granted.toList.sortBy(_.label).map { lang =>
          div(`class` := "prof-item prof-item--proficient")(text(lang.label))
        }*
      ),
      if needChoices <= 0 then div()
      else
        div(
          h2(`class` := "about-heading", style := "margin-top: 0.5rem;")(text(s"Choose $needChoices additional language(s) from your class")),
          div(`class` := "points-pool", style := "margin-bottom: 0.5rem;")(
            text("Chosen: "),
            span(`class` := "points-pool-value")(text(s"${model.chosenExtraLanguages.size} / $needChoices"))
          ),
          div(
            Language.choicePool.filterNot(granted.contains).map { lang =>
              val isChosen = model.chosenExtraLanguages.contains(lang)
              val clsName = if isChosen then "skill-item skill-item--selected" else "skill-item"
              div(`class` := clsName, onClick(BackgroundMsg.ToggleExtraLanguage(lang)), style := "cursor: var(--nes-pointer);")(
                div(`class` := "skill-checkbox")(text(if isChosen then "*" else "")),
                span(`class` := "skill-label")(text(lang.label))
              )
            }*
          )
        )
    )
  }

  private def selectedBackgroundDetail(model: Model): Html[Msg] =
    model.selectedBackground match {
      case Some(bg) =>
        div(style := "margin-top: 1rem;")(
          div(`class` := "section-title")(text(s"${bg.name} Details")),
          div(`class` := "feature-list")(
            div(`class` := "feature-item")(
              div(`class` := "feature-name")(text("Origin Feat")),
              div(`class` := "feature-desc")(text(s"${bg.feat.name}: ${bg.feat.description}"))
            ),
            div(`class` := "feature-item")(
              div(`class` := "feature-name")(text("Skill Proficiencies")),
              div(`class` := "feature-desc")(text(bg.skillProficiencySet.map(_.label).mkString(", ")))
            ),
            div(`class` := "feature-item")(
              div(`class` := "feature-name")(text("Ability Score Bonuses")),
              div(`class` := "feature-desc")(
                text(s"Choose from ${bg.abilityOptionsList.map(_.label).mkString(", ")}: either +2/+1 or +1/+1/+1")
              )
            ),
            div(`class` := "feature-item")(
              div(`class` := "feature-name")(text("Tool Proficiency")),
              div(`class` := "feature-desc")(text(bg.toolProficiency))
            )
          )
        )
      case None => div()
    }
}

final case class BackgroundModel(
    draft: CharacterDraft,
    selectedBackground: Option[Background],
    chosenExtraLanguages: Set[Language])

enum BackgroundMsg {
  case SelectBackground(bg: Background)
  case ToggleExtraLanguage(lang: Language)
  case Next
  case Back
}
