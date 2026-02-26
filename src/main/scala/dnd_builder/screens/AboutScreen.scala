package dndbuilder.screens

import cats.effect.IO
import dndbuilder.{NavigateNext, Screen, ScreenId, ScreenOutput}
import dndbuilder.common.CmdUtils
import org.scalajs.dom
import tyrian.Html.*
import tyrian.*
import scala.scalajs.js

object AboutScreen extends Screen {
  type Model = Boolean
  type Msg   = AboutMsg | NavigateNext

  val screenId: ScreenId = ScreenId.AboutId

  def init(previous: Option[ScreenOutput]): (Model, Cmd[IO, Msg]) =
    (false, Cmd.None)

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = {
    case AboutMsg.ShowRefreshConfirm =>
      (true, Cmd.None)
    case AboutMsg.ConfirmRefresh =>
      (model, CmdUtils.fireAndForget(refreshAppFromSW, AboutMsg.NoOp, _ => AboutMsg.NoOp))
    case AboutMsg.CancelRefresh =>
      (false, Cmd.None)
    case AboutMsg.Back =>
      (model, Cmd.Emit(NavigateNext(ScreenId.HomeId, None)))
    case AboutMsg.NoOp =>
      (model, Cmd.None)
    case _: NavigateNext =>
      (model, Cmd.None)
  }

  private def refreshAppFromSW: IO[Unit] = IO.delay {
    val f = js.Dynamic.global.selectDynamic("refreshApp")
    if (js.typeOf(f) == "function") {
      f.asInstanceOf[js.Function0[Unit]]()
    } else {
      dom.window.location.reload()
    }
  }

  def view(model: Model): Html[Msg] =
    div(`class` := "screen-container")(
      div(`class` := "screen-header")(
        h1(`class` := "screen-title")(text("About")),
        button(`class` := "btn-ghost", onClick(AboutMsg.Back))(text("< Home"))
      ),
      div(`class` := "about-content")(
        p(text("D&D Character Creator — a type-safe 5e (2024) character builder.")),
        p(text("Built with Scala 3, Scala.js, Tyrian (Elm architecture), and Circe.")),
        h2(`class` := "about-heading")(text("Libraries & tools")),
        div(`class` := "about-table-wrap")(toolsTable),
        h2(`class` := "about-heading")(text("Get latest version")),
        p(text("After a deploy, the browser may serve cached files.")),
        p(text("Use the button below to unregister the cache and reload.")),
        (
          if (model)
            div(`class` := "about-refresh-confirm")(
              p(`class` := "about-refresh-confirm-text")(
                text("Unregister cache and reload now? The page will refresh.")
              ),
              div(`class` := "flex-row flex-row--tight")(
                button(`class` := "btn-primary", onClick(AboutMsg.ConfirmRefresh))(text("Yes, refresh")),
                button(`class` := "btn-ghost", onClick(AboutMsg.CancelRefresh))(text("Cancel"))
              )
            )
          else
            button(`class` := "btn-secondary", onClick(AboutMsg.ShowRefreshConfirm))(text("Refresh app"))
        )
      )
    )

  private def toolsTable: Html[Msg] = {
    val rows = List(
      ("Tyrian", "Elm-style UI framework for Scala.js", "https://github.com/PurpleKingdomGames/tyrian"),
      ("Circe", "JSON encoding / decoding", "https://circe.github.io/circe/"),
      ("Scala.js", "Scala compiled to JavaScript", "https://www.scala-js.org/"),
      ("Parcel", "Dev server and bundler", "https://parceljs.org/")
    )
    table(`class` := "about-table")(
      thead(
        tr(
          th(text("Library")),
          th(text("Purpose"))
        )
      ),
      tbody(
        rows.map { case (name, purpose, url) =>
          tr(
            td(
              a(
                href := url,
                Attribute("target", "_blank"),
                Attribute("rel", "noopener noreferrer"),
                `class` := "about-link"
              )(text(name))
            ),
            td(text(purpose))
          )
        }*
      )
    )
  }
}

enum AboutMsg {
  case ShowRefreshConfirm
  case ConfirmRefresh
  case CancelRefresh
  case Back
  case NoOp
}
