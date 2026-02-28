package dndbuilder

import cats.effect.IO
import dndbuilder.dnd.*
import tyrian.{Cmd, Html, Sub}

trait ScreenId {
  def name: String
  def title: String
}

object ScreenId {
  case object HomeId        extends ScreenId { val name = "home"; val title = "Home" }
  case object AboutId       extends ScreenId { val name = "about"; val title = "About" }
  case object SpeciesId     extends ScreenId { val name = "species"; val title = "Choose Race" }
  case object ClassSelectId extends ScreenId { val name = "class-select"; val title = "Choose Class" }
  case object BackgroundId  extends ScreenId { val name = "background"; val title = "Choose Background" }
  case object AbilitiesId   extends ScreenId { val name = "abilities"; val title = "Ability Scores" }
  case object SkillsId      extends ScreenId { val name = "skills"; val title = "Skills" }
  case object EquipmentId   extends ScreenId { val name = "equipment"; val title = "Equipment" }
  case object ClassFeaturesId extends ScreenId { val name = "class-features"; val title = "Class Features" }
  case object SpellsId      extends ScreenId { val name = "spells"; val title = "Spells" }
  case object LanguagesId   extends ScreenId { val name = "languages"; val title = "Languages" }
  case object ReviewId      extends ScreenId { val name = "review"; val title = "Review" }
  case object GalleryId     extends ScreenId { val name = "gallery"; val title = "My Characters" }
  case object DetailId      extends ScreenId { val name = "detail"; val title = "Character Detail" }
}

sealed trait ScreenOutput
object ScreenOutput {
  case class Draft(draft: CharacterDraft) extends ScreenOutput
  case class ViewCharacter(storedCharacter: StoredCharacter) extends ScreenOutput
}

sealed trait RootMsg
object RootMsg {
  case class NavigateTo(screenId: ScreenId, output: Option[ScreenOutput]) extends RootMsg
  case class HandleScreenMsg(screenId: ScreenId, msg: Any)                extends RootMsg
}

case class NavigateNext(screenId: ScreenId, output: Option[ScreenOutput])

trait Screen {
  type Model
  type Msg

  val screenId: ScreenId

  def init(previous: Option[ScreenOutput]): (Model, Cmd[IO, Msg])

  def update(model: Model): Msg => (Model, Cmd[IO, Msg])

  def view(model: Model): Html[Msg]

  def subscriptions(model: Model): Sub[IO, Msg] = Sub.None

  def wrapMsg(msg: Msg): RootMsg = RootMsg.HandleScreenMsg(screenId, msg)
}
