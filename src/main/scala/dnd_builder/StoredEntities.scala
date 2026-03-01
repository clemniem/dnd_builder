package dndbuilder

import dndbuilder.dnd.{Character, Codecs, ConfigLoader, RuleSet}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

final case class StoredCharacter(id: String, character: Character)
object StoredCharacter {
  import Codecs.given
  given Encoder[StoredCharacter] = deriveEncoder
  given Decoder[StoredCharacter] = deriveDecoder
}

final case class StoredRuleSet(id: String, ruleSet: RuleSet)
object StoredRuleSet {
  import ConfigLoader.given
  given Encoder[StoredRuleSet] = deriveEncoder
  given Decoder[StoredRuleSet] = deriveDecoder
}

object StorageKeys {
  val characters = "characters"
  val ruleSets   = "ruleSets"
}
