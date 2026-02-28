package dndbuilder.screens

import tyrian.Html.*
import tyrian.*

object StepNav {

  def apply[Msg](
      backLabel: String,
      backMsg: Msg,
      nextLabel: String,
      nextMsg: Msg,
      nextEnabled: Boolean
  ): Html[Msg] =
    div(`class` := "nav-row")(
      button(`class` := "btn-ghost", onClick(backMsg))(text(backLabel)),
      button(
        `class` := (if nextEnabled then "btn-primary btn-lg" else "btn-primary btn-lg btn-disabled"),
        onClick(nextMsg)
      )(text(nextLabel))
    )
}
