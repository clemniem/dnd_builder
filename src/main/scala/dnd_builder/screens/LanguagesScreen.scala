package dndbuilder.screens

import cats.effect.IO
import dndbuilder.{NavigateNext, Screen, ScreenId, ScreenOutput}
import dndbuilder.dnd.*
import tyrian.Html.*
import tyrian.*

final case class LanguagesModel(
    draft: CharacterDraft,
    chosenExtraLanguages: Set[Language]
)

enum LanguagesMsg {
  case Next
  case Back
  case ToggleExtraLanguage(lang: Language)
}

object LanguagesScreen extends Screen {
  type Model = LanguagesModel
  type Msg   = LanguagesMsg | NavigateNext

  val screenId: ScreenId = ScreenId.LanguagesId

  private def canProceed(model: Model): Boolean = {
    val cls = model.draft.dndClass.getOrElse(Barbarian)
    model.chosenExtraLanguages.size == cls.extraLanguageChoices
  }

  def init(previous: Option[ScreenOutput]): (Model, Cmd[IO, Msg]) = {
    val draft = previous match {
      case Some(ScreenOutput.Draft(d)) => d
      case _ => CharacterDraft.empty
    }
    (LanguagesModel(draft, draft.chosenExtraLanguages), Cmd.None)
  }

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = {
    case LanguagesMsg.Next =>
      val updated = model.draft.copy(chosenExtraLanguages = model.chosenExtraLanguages)
      (model, Cmd.Emit(NavigateNext(ScreenId.ReviewId, Some(ScreenOutput.Draft(updated)))))

    case LanguagesMsg.Back =>
      val cls = model.draft.dndClass.getOrElse(Barbarian)
      val updated = model.draft.copy(chosenExtraLanguages = model.chosenExtraLanguages)
      if FeatureGrants.needsSpellScreen(cls, model.draft.background) then
        (model, Cmd.Emit(NavigateNext(ScreenId.SpellsId, Some(ScreenOutput.Draft(updated)))))
      else
        (model, Cmd.Emit(NavigateNext(ScreenId.FeaturesId, Some(ScreenOutput.Draft(updated)))))

    case LanguagesMsg.ToggleExtraLanguage(lang) =>
      val cls = model.draft.dndClass.getOrElse(Barbarian)
      val n = cls.extraLanguageChoices
      val current = model.chosenExtraLanguages
      val next =
        if current.contains(lang) then current - lang
        else if current.size < n then current + lang
        else current
      (model.copy(chosenExtraLanguages = next), Cmd.None)

    case _: NavigateNext =>
      (model, Cmd.None)
  }

  def view(model: Model): Html[Msg] = {
    val sp = model.draft.species.getOrElse(Human)
    val cls = model.draft.dndClass.getOrElse(Barbarian)
    val needsSpells = FeatureGrants.needsSpellScreen(cls, model.draft.background)
    val granted = sp.languages
    val needChoices = cls.extraLanguageChoices
    val step = if needsSpells then 9 else 8
    div(`class` := "screen-container")(
      StepIndicator(step, needsSpells),
      StepNav(
        StepIndicator.backLabel(step, needsSpells),
        LanguagesMsg.Back,
        StepIndicator.nextLabel(step, needsSpells),
        LanguagesMsg.Next,
        canProceed(model)
      ),
      h1(`class` := "screen-title")(text("Languages")),
      p(`class` := "screen-intro")(
        text("Everyone knows Common. Your species and class may grant additional languages.")
      ),
      div(style := "margin-bottom: 1rem;")(
        h2(`class` := "about-heading")(text("Languages you know")),
        div(`class` := "prof-list")(
          granted.toList.sortBy(_.label).map { lang =>
            div(`class` := "prof-item prof-item--proficient")(
              text(lang.label)
            )
          }*
        )
      ),
      if needChoices <= 0 then div()
      else {
        val pool = Language.choicePool.filterNot(granted.contains)
        val chosen = model.chosenExtraLanguages
        div(style := "margin-bottom: 1rem;")(
          h2(`class` := "about-heading")(text(s"Choose $needChoices additional language(s) from your class")),
          div(`class` := "points-pool", style := "margin-bottom: 0.5rem;")(
            text("Chosen: "),
            span(`class` := "points-pool-value")(text(s"${chosen.size} / $needChoices"))
          ),
          div(
            pool.map { lang =>
              val isChosen = chosen.contains(lang)
              val clsName = if isChosen then "skill-item skill-item--selected" else "skill-item"
              div(`class` := clsName, onClick(LanguagesMsg.ToggleExtraLanguage(lang)), style := "cursor: var(--nes-pointer);")(
                div(`class` := "skill-checkbox")(text(if isChosen then "*" else "")),
                span(`class` := "skill-label")(text(lang.label))
              )
            }*
          )
        )
      }
    )
  }
}
