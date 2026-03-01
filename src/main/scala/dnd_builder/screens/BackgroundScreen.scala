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
          val (spellFromSp, skillFromSp) = FeatureGrants.fromSpecies(model.draft.species.getOrElse(Human))
          val (spellFromBg, skillFromBg) = FeatureGrants.fromBackground(bg)
          val spellGrants = spellFromSp ++ spellFromBg
          val skillGrants = skillFromSp ++ skillFromBg
          val updated = model.draft.copy(
            background = Some(bg),
            chosenExtraLanguages = model.chosenExtraLanguages,
            spellGrants = spellGrants,
            skillGrants = skillGrants
          )
          (model, Cmd.Emit(NavigateNext(ScreenId.AbilitiesId, Some(ScreenOutput.Draft(updated)))))
        case None =>
          (model, Cmd.None)
      }

    case BackgroundMsg.SelectExtraLanguage(maybeLang) =>
      val next = maybeLang.fold(Set.empty[Language])(Set(_))
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
      StepNav(StepIndicator.backLabel(3, cls.isSpellcaster), BackgroundMsg.Back, StepIndicator.nextLabel(3, cls.isSpellcaster), BackgroundMsg.Next, canProceed(model)),
      h1(`class` := "screen-title")(text("Background & Languages")),
      p(`class` := "screen-intro")(text("Choose your background (ability bonuses, skills, origin feat) and any extra languages from your class.")),
      languagesSection(model),
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
      selectedBackgroundDetail(model)
    )
  }

  private def languagesSection(model: Model): Html[Msg] = {
    val sp = model.draft.species.getOrElse(Human)
    val cls = model.draft.dndClass.getOrElse(Barbarian)
    val granted = sp.languages
    val needChoices = cls.extraLanguageChoices
    val pool = Language.choicePool.filterNot(granted.contains)
    val selectedValue = model.chosenExtraLanguages.headOption.map(_.label).getOrElse("")

    def onSelectValue(s: String): Msg =
      if s.isEmpty then BackgroundMsg.SelectExtraLanguage(None)
      else BackgroundMsg.SelectExtraLanguage(pool.find(_.label == s))

    div(style := "margin-bottom: 1.5rem;")(
      if needChoices <= 0 then div()
      else
        div(`class` := "field-block", style := "max-width: 20rem;")(
          label(`class` := "label-block")(text("Additional language (from class)")),
          {
            val opts =
              option(value := "")(text("Choose a language...")) :: pool.map(lang =>
                option(value := lang.label)(text(lang.label))
              )
            select(value := selectedValue, onInput(onSelectValue))(opts*)
          }
        ),
      div(`class` := "section-title")(text("Languages")),
      p(style := "font-size: 0.9rem; color: var(--color-text-dim); margin-bottom: 0.5rem;")(
        text("Everyone knows Common. Your species grants:")
      ),
      div(`class` := "prof-list", style := "margin-bottom: 0.75rem;")(
        granted.toList.sortBy(_.label).map { lang =>
          div(`class` := "prof-item prof-item--proficient")(text(lang.label))
        }*
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
  case SelectExtraLanguage(lang: Option[Language])
  case Next
  case Back
}
