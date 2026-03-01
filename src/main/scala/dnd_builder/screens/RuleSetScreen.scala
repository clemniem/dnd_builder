package dndbuilder.screens

import cats.effect.IO
import dndbuilder.{NavigateNext, Screen, ScreenId, ScreenOutput, StorageKeys, StoredRuleSet}
import dndbuilder.common.LocalStorageUtils
import dndbuilder.dnd.{ConfigLoader, RuleSet, RuleSetValidation}
import dndbuilder.dnd.ConfigLoader.given
import io.circe.parser
import tyrian.Html.*
import tyrian.*

object RuleSetScreen extends Screen {
  type Model = RuleSetModel
  type Msg   = RuleSetMsg | NavigateNext

  val screenId: ScreenId = ScreenId.RuleSetsId

  def init(previous: Option[ScreenOutput]): (Model, Cmd[IO, Msg]) =
    (
      RuleSetModel(None, "", None, None),
      LocalStorageUtils.loadList[StoredRuleSet, Msg](StorageKeys.ruleSets)(
        RuleSetMsg.LoadedRuleSets.apply,
        _ => RuleSetMsg.LoadedRuleSets(Nil),
        (msg, _) => RuleSetMsg.LoadError(msg)
      )
    )

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = {
    case RuleSetMsg.LoadedRuleSets(list) =>
      (model.copy(storedRuleSets = Some(list)), Cmd.None)

    case RuleSetMsg.UpdateJsonInput(s) =>
      (model.copy(jsonInput = s, validationResult = None), Cmd.None)

    case RuleSetMsg.ValidateAndSave =>
      val parseResult = parser.decode[RuleSet](model.jsonInput)
      val validated = parseResult.fold(
        e => Left(List(e.getMessage)),
        RuleSetValidation.validate
      )
      val newModel = model.copy(validationResult = Some(validated))
      validated match {
        case Right(rs) =>
          val stored = StoredRuleSet(rs.id, rs)
          val newList = model.storedRuleSets.getOrElse(Nil).filter(_.id != rs.id) :+ stored
          (
            newModel.copy(jsonInput = "", validationResult = Some(Right(rs))),
            LocalStorageUtils.saveList(StorageKeys.ruleSets, newList)(
              _ => RuleSetMsg.LoadedRuleSets(newList),
              (msg, _) => RuleSetMsg.LoadError(msg)
            )
          )
        case Left(_) =>
          (newModel, Cmd.None)
      }

    case RuleSetMsg.ConfirmDelete(id) =>
      (model.copy(confirmDeleteId = Some(id)), Cmd.None)

    case RuleSetMsg.DoDelete(id) =>
      val listOpt = model.storedRuleSets.map(_.filterNot(_.id == id))
      (
        model.copy(storedRuleSets = listOpt, confirmDeleteId = None),
        listOpt match {
          case Some(newList) =>
            LocalStorageUtils.saveList(StorageKeys.ruleSets, newList)(
              _ => RuleSetMsg.LoadedRuleSets(newList),
              (_, _) => RuleSetMsg.CancelDelete
            )
          case None => Cmd.None
        }
      )

    case RuleSetMsg.CancelDelete =>
      (model.copy(confirmDeleteId = None), Cmd.None)

    case RuleSetMsg.Back =>
      (model, Cmd.Emit(NavigateNext(ScreenId.HomeId, None)))

    case RuleSetMsg.LoadError(_) =>
      (model, Cmd.None)

    case _: NavigateNext =>
      (model, Cmd.None)
  }

  def view(model: Model): Html[Msg] =
    div(`class` := "screen-container")(
      div(`class` := "screen-header")(
        h1(`class` := "screen-title")(text("Manage RuleSets")),
        button(
          `class` := "btn-ghost",
          onClick(RuleSetMsg.Back),
          style := "cursor: var(--nes-pointer);"
        )(text("< Home"))
      ),
      div(`class` := "flex-col flex-col--gap-md", style := "margin-top: 1rem;")(
        sectionRuleSets(model),
        sectionImport(model),
        validationResultView(model.validationResult)
      )
    )

  private def sectionRuleSets(model: RuleSetModel): Html[Msg] =
    div(
      h2(`class` := "section-title", style := "margin-bottom: 0.5rem;")(text("Custom RuleSets")),
      model.storedRuleSets match {
        case None =>
          div(text("Loading..."))
        case Some(Nil) =>
          div(`class` := "empty-state", style := "padding: 0.5rem 0;")(text("No custom rulesets. Paste JSON below to import one."))
        case Some(list) =>
          div(`class` := "card-grid", style := "display: flex; flex-wrap: wrap; gap: 0.75rem;")(
            list.map(srs => ruleSetCard(srs, model.confirmDeleteId))*
          )
      }
    )

  private def ruleSetCard(srs: StoredRuleSet, confirmDeleteId: Option[String]): Html[Msg] =
    div(
      `class` := "card",
      style := "min-width: 200px; max-width: 280px;"
    )(
      div(`class` := "card-title")(text(srs.ruleSet.name)),
      div(`class` := "card-subtitle")(
        text(s"${srs.ruleSet.classes.size} class(es)")
      ),
      if confirmDeleteId.contains(srs.id) then
        div(style := "margin-top: 0.5rem; display: flex; gap: 0.3rem; align-items: center;")(
          span(style := "font-size: 0.9rem;")(text("Delete?")),
          button(
            `class` := "btn btn--secondary btn-sm",
            onClick(RuleSetMsg.DoDelete(srs.id)),
            style := "cursor: var(--nes-pointer);"
          )(text("Yes")),
          button(
            `class` := "btn btn--secondary btn-sm",
            onClick(RuleSetMsg.CancelDelete),
            style := "cursor: var(--nes-pointer);"
          )(text("Cancel"))
        )
      else
        button(
          `class` := "btn btn--secondary btn-sm",
          style := "margin-top: 0.5rem; cursor: var(--nes-pointer);",
          onClick(RuleSetMsg.ConfirmDelete(srs.id))
        )(text("Delete"))
    )

  private def sectionImport(model: RuleSetModel): Html[Msg] =
    div(
      h2(`class` := "section-title", style := "margin-bottom: 0.5rem;")(text("Import RuleSet")),
      p(style := "font-size: 0.9rem; color: var(--color-text-muted); margin-bottom: 0.5rem;")(
        text("Paste JSON with id, name, and classes array. Then click Validate & Save.")
      ),
      textarea(
        `class` := "screen-intro",
        value := model.jsonInput,
        onInput(RuleSetMsg.UpdateJsonInput.apply),
        style := "width: 100%; min-height: 120px; font-family: monospace; font-size: 0.85rem; padding: 0.5rem; box-sizing: border-box; cursor: text;"
      )(),
      button(
        `class` := "btn btn-primary",
        onClick(RuleSetMsg.ValidateAndSave),
        style := "margin-top: 0.5rem; cursor: var(--nes-pointer);"
      )(text("Validate & Save"))
    )

  private def validationResultView(result: Option[Either[List[String], RuleSet]]): Html[Msg] =
    result match {
      case None => div()
      case Some(Right(rs)) =>
        div(
          `class` := "card",
          style := "border: 2px solid var(--color-success, green); padding: 0.5rem;"
        )(text(s"Saved: ${rs.name} (${rs.classes.size} classes)"))
      case Some(Left(errors)) =>
        div(
          `class` := "card",
          style := "border: 2px solid var(--color-error, #c00); padding: 0.5rem;"
        )(
          div(style := "font-weight: bold; margin-bottom: 0.3rem;")(text("Errors:")),
          ul(style := "margin: 0; padding-left: 1.2rem;")(
            errors.map(e => li(text(e)))*
          )
        )
    }
}

final case class RuleSetModel(
    storedRuleSets: Option[List[StoredRuleSet]],
    jsonInput: String,
    validationResult: Option[Either[List[String], RuleSet]],
    confirmDeleteId: Option[String]
)

enum RuleSetMsg {
  case LoadedRuleSets(list: List[StoredRuleSet])
  case UpdateJsonInput(s: String)
  case ValidateAndSave
  case ConfirmDelete(id: String)
  case DoDelete(id: String)
  case CancelDelete
  case Back
  case LoadError(msg: String)
}
