package scaffold.screens

import scaffold.common.nescss.NesCss
import tyrian.Html.*
import tyrian.*

object GalleryEmptyState {

  def apply[Msg](emptyText: String, buttonLabel: String, createMsg: Msg): Html[Msg] =
    div(`class` := s"${NesCss.container} empty-state")(
      p(`class` := NesCss.text)(text(emptyText)),
      button(`class` := NesCss.btnPrimary, onClick(createMsg))(text(buttonLabel))
    )
}
