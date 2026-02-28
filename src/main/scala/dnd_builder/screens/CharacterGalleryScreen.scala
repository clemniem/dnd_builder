package dndbuilder.screens

import cats.effect.IO
import dndbuilder.{NavigateNext, Screen, ScreenId, ScreenOutput, StorageKeys, StoredCharacter}
import dndbuilder.common.LocalStorageUtils
import tyrian.Html.*
import tyrian.*

object CharacterGalleryScreen extends Screen {
  type Model = GalleryModel
  type Msg   = GalleryMsg | NavigateNext

  val screenId: ScreenId = ScreenId.GalleryId

  private val pageSize = 6

  def init(previous: Option[ScreenOutput]): (Model, Cmd[IO, Msg]) =
    (
      GalleryModel(None, 1, None),
      LocalStorageUtils.loadList[StoredCharacter, Msg](StorageKeys.characters)(
        GalleryMsg.Loaded.apply,
        _ => GalleryMsg.Loaded(Nil),
        (msg, _) => GalleryMsg.Error(msg)
      )
    )

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = {
    case GalleryMsg.Loaded(chars) =>
      (model.copy(characters = Some(chars)), Cmd.None)

    case GalleryMsg.AskDelete(id) =>
      (model.copy(pendingDeleteId = Some(id)), Cmd.None)

    case GalleryMsg.CancelDelete =>
      (model.copy(pendingDeleteId = None), Cmd.None)

    case GalleryMsg.ConfirmDelete(id) =>
      val (newList, newPage, cmd) =
        LocalStorageUtils.confirmDelete[StoredCharacter, Msg](
          model.characters, id, StorageKeys.characters,
          pageSize, model.currentPage, GalleryMsg.CancelDelete, _.id
        )
      (model.copy(characters = newList, currentPage = newPage, pendingDeleteId = None), cmd)

    case GalleryMsg.ViewDetail(id) =>
      model.characters.flatMap(_.find(_.id == id)) match {
        case Some(sc) =>
          (model, Cmd.Emit(NavigateNext(ScreenId.DetailId, Some(ScreenOutput.ViewCharacter(sc)))))
        case None =>
          (model, Cmd.None)
      }

    case GalleryMsg.PreviousPage =>
      (model.copy(currentPage = (model.currentPage - 1).max(1)), Cmd.None)

    case GalleryMsg.NextPage =>
      val total = GalleryLayout.totalPagesFor(model.characters.map(_.size).getOrElse(0), pageSize)
      (model.copy(currentPage = (model.currentPage + 1).min(total)), Cmd.None)

    case GalleryMsg.Home =>
      (model, Cmd.Emit(NavigateNext(ScreenId.HomeId, None)))

    case GalleryMsg.CreateNew =>
      (model, Cmd.Emit(NavigateNext(ScreenId.ClassSelectId, None)))

    case GalleryMsg.Error(_) =>
      (model, Cmd.None)

    case _: NavigateNext =>
      (model, Cmd.None)
  }

  def view(model: Model): Html[Msg] =
    div(`class` := "screen-container")(
      div(`class` := "screen-header")(
        h1(`class` := "screen-title")(text("My Characters")),
        div(`class` := "flex-row")(
          button(`class` := "btn-ghost", onClick(GalleryMsg.Home))(text("< Home")),
          button(`class` := "btn-primary", onClick(GalleryMsg.CreateNew))(text("+ New Character"))
        )
      ),
      viewContent(model)
    )

  private def viewContent(model: Model): Html[Msg] =
    model.characters match {
      case None =>
        div(text("Loading..."))
      case Some(chars) if chars.isEmpty =>
        div(`class` := "empty-state")(
          p(text("No characters yet.")),
          button(`class` := "btn-primary", onClick(GalleryMsg.CreateNew))(text("Create Your First Character"))
        )
      case Some(chars) =>
        val (slice, page, totalPages) = GalleryLayout.sliceForPage(chars, model.currentPage, pageSize)
        div(`class` := "gallery-list")(
          div(`class` := "gallery-list-entries")(
            slice.map(sc => characterCard(sc, model.pendingDeleteId))*
          ),
          (
            if totalPages > 1 then
              div(`class` := "gallery-pagination")(
                button(
                  `class` := (if page <= 1 then "btn-ghost btn-sm btn-disabled" else "btn-ghost btn-sm"),
                  onClick(GalleryMsg.PreviousPage)
                )(text("< Prev")),
                span(`class` := "gallery-pagination-label")(text(s"Page $page of $totalPages")),
                button(
                  `class` := (if page >= totalPages then "btn-ghost btn-sm btn-disabled" else "btn-ghost btn-sm"),
                  onClick(GalleryMsg.NextPage)
                )(text("Next >"))
              )
            else div()
          )
        )
    }

  private def characterCard(sc: StoredCharacter, pendingDeleteId: Option[String]): Html[Msg] = {
    val ch = sc.character
    div(`class` := "gallery-card")(
      div(`class` := "gallery-card-body")(
        div(`class` := "gallery-card-title")(text(ch.name)),
        div(`class` := "gallery-card-meta")(
          text(s"Level ${ch.characterLevel} ${ch.species.displayName} ${ch.classLabel}")
        ),
        div(`class` := "flex-row", style := "margin-top: 0.3rem; gap: 0.3rem;")(
          span(`class` := "badge")(text(s"HP ${ch.maxHitPoints}")),
          span(`class` := "badge")(text(s"AC ${ch.armorClass}")),
          span(`class` := "badge badge--feat")(text(ch.originFeat.name))
        )
      ),
      div(
        if pendingDeleteId.contains(sc.id) then
          div(`class` := "gallery-delete-confirm")(
            span(`class` := "delete-confirm-text")(text("Delete?")),
            button(`class` := "btn-danger btn-sm", onClick(GalleryMsg.ConfirmDelete(sc.id)))(text("Yes")),
            button(`class` := "btn-ghost btn-sm", onClick(GalleryMsg.CancelDelete))(text("Cancel"))
          )
        else
          div(`class` := "gallery-actions")(
            button(`class` := "btn-secondary btn-sm", onClick(GalleryMsg.ViewDetail(sc.id)))(text("View")),
            button(`class` := "btn-ghost btn-sm", onClick(GalleryMsg.AskDelete(sc.id)))(text("Delete"))
          )
      )
    )
  }
}

final case class GalleryModel(
    characters: Option[List[StoredCharacter]],
    currentPage: Int,
    pendingDeleteId: Option[String])

enum GalleryMsg {
  case Loaded(chars: List[StoredCharacter])
  case AskDelete(id: String)
  case CancelDelete
  case ConfirmDelete(id: String)
  case ViewDetail(id: String)
  case PreviousPage
  case NextPage
  case Home
  case CreateNew
  case Error(msg: String)
}
