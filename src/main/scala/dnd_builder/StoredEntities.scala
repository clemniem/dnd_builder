package dndbuilder

import dndbuilder.dnd.{Character, Codecs}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

final case class StoredCharacter(id: String, character: Character)
object StoredCharacter {
  import Codecs.given
  given Encoder[StoredCharacter] = deriveEncoder
  given Decoder[StoredCharacter] = deriveDecoder
}

object StorageKeys {
  val characters = "characters"
}
