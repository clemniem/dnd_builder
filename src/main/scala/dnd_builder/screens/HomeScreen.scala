package dndbuilder.screens

import cats.effect.IO
import dndbuilder.{NavigateNext, Screen, ScreenId, ScreenOutput}
import tyrian.Html.*
import tyrian.*

object HomeScreen extends Screen {
  type Model = Unit
  type Msg   = HomeMsg | NavigateNext

  val screenId: ScreenId = ScreenId.HomeId

  def init(previous: Option[ScreenOutput]): (Model, Cmd[IO, Msg]) =
    ((), Cmd.None)

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = {
    case HomeMsg.GoTo(target) =>
      (model, Cmd.Emit(NavigateNext(target, None)))
    case HomeMsg.NoOp =>
      (model, Cmd.None)
    case _: NavigateNext =>
      (model, Cmd.None)
  }

  def view(model: Model): Html[Msg] =
    div(`class` := "screen-container")(
      div(`class` := "home-title-block")(
        h1(text("D&D Character Creator")),
        p(text("Build type-safe 5e (2024) characters"))
      ),
      div(`class` := "flex-col flex-col--gap-md")(
        linkCard("Create Character", ScreenId.SpeciesId,
          "Build a new Level 1 character step by step"),
        linkCard("My Characters", ScreenId.GalleryId,
          "View, manage, and export your saved characters"),
        // linkCard("Manage RuleSets", ScreenId.RuleSetsId,
        //   "Import and manage custom class collections"),
        linkCard("About", ScreenId.AboutId,
          "Tech stack and tools")
      )
    )

  private def linkCard(title: String, target: ScreenId, desc: String): Html[Msg] =
    button(
      `class` := "link-card",
      onClick(HomeMsg.GoTo(target))
    )(
      span(`class` := "link-card-title")(text(title)),
      span(`class` := "link-card-desc")(text(desc))
    )
}

enum HomeMsg {
  case GoTo(screenId: ScreenId)
  case NoOp
}
