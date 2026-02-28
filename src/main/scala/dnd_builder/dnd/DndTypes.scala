package dndbuilder.dnd

import scala.annotation.targetName
import io.circe.{Decoder, Encoder}

object DndTypes {

  opaque type Score = Int
  object Score {
    def apply(v: Int): Score = v
    given Encoder[Score] = Encoder.encodeInt.contramap(_.value)
    given Decoder[Score] = Decoder.decodeInt.map(Score.apply)
  }
  extension (s: Score) {
    def value: Int = s
    @targetName("scorePlus") def +(delta: Int): Score = (s: Int) + delta
    def >=(threshold: Int): Boolean = (s: Int) >= threshold
  }

  opaque type Modifier = Int
  object Modifier {
    def apply(v: Int): Modifier = v
    def zero: Modifier = 0
  }
  extension (m: Modifier) {
    def toInt: Int = m
    @targetName("modifierPlus") def +(other: Modifier): Modifier = (m: Int) + (other: Int)
    def format: String = {
      val v = m.toInt
      if v >= 0 then s"+$v" else s"$v"
    }
  }
}
