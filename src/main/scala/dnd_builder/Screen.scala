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
  case object ReviewId      extends ScreenId { val name = "review"; val title = "Review" }
  case object GalleryId     extends ScreenId { val name = "gallery"; val title = "My Characters" }
  case object DetailId      extends ScreenId { val name = "detail"; val title = "Character Detail" }
}

sealed trait ScreenOutput
object ScreenOutput {
  case class SpeciesChosen(species: Species) extends ScreenOutput
  case class ClassChosen(species: Species, dndClass: DndClass) extends ScreenOutput
  case class BackgroundChosen(species: Species, dndClass: DndClass, background: Background) extends ScreenOutput
  case class AbilitiesChosen(
      species: Species, dndClass: DndClass, background: Background,
      baseScores: AbilityScores, backgroundBonus: BackgroundBonus) extends ScreenOutput
  case class SkillsChosen(
      species: Species, dndClass: DndClass, background: Background,
      baseScores: AbilityScores, backgroundBonus: BackgroundBonus,
      chosenSkills: Set[Skill]) extends ScreenOutput
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
