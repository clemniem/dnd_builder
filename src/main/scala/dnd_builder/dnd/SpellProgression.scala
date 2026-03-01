package dndbuilder.dnd

final case class SpellSlotRow(
    cantrips: Int,
    preparedSpells: Int,
    slots: List[Int])

final case class PactMagicRow(
    cantrips: Int,
    preparedSpells: Int,
    numSlots: Int,
    slotLevel: Int)

object SpellProgression {

  // Full caster progression (Bard, Cleric, Druid, Sorcerer, Wizard)
  // Each entry: (cantrips, prepared, level1, level2, ..., level9)
  private val fullCasterBase: Map[Int, (Int, Int, List[Int])] = Map(
    1  -> (0, 4,  List(2, 0, 0, 0, 0, 0, 0, 0, 0)),
    2  -> (0, 5,  List(3, 0, 0, 0, 0, 0, 0, 0, 0)),
    3  -> (0, 6,  List(4, 2, 0, 0, 0, 0, 0, 0, 0)),
    4  -> (0, 7,  List(4, 3, 0, 0, 0, 0, 0, 0, 0)),
    5  -> (0, 9,  List(4, 3, 2, 0, 0, 0, 0, 0, 0)),
    6  -> (0, 10, List(4, 3, 3, 0, 0, 0, 0, 0, 0)),
    7  -> (0, 11, List(4, 3, 3, 1, 0, 0, 0, 0, 0)),
    8  -> (0, 12, List(4, 3, 3, 2, 0, 0, 0, 0, 0)),
    9  -> (0, 14, List(4, 3, 3, 3, 1, 0, 0, 0, 0)),
    10 -> (0, 15, List(4, 3, 3, 3, 2, 0, 0, 0, 0)),
    11 -> (0, 16, List(4, 3, 3, 3, 2, 1, 0, 0, 0)),
    12 -> (0, 16, List(4, 3, 3, 3, 2, 1, 0, 0, 0)),
    13 -> (0, 17, List(4, 3, 3, 3, 2, 1, 1, 0, 0)),
    14 -> (0, 17, List(4, 3, 3, 3, 2, 1, 1, 0, 0)),
    15 -> (0, 18, List(4, 3, 3, 3, 2, 1, 1, 1, 0)),
    16 -> (0, 18, List(4, 3, 3, 3, 2, 1, 1, 1, 0)),
    17 -> (0, 19, List(4, 3, 3, 3, 2, 1, 1, 1, 1)),
    18 -> (0, 20, List(4, 3, 3, 3, 3, 1, 1, 1, 1)),
    19 -> (0, 21, List(4, 3, 3, 3, 3, 2, 1, 1, 1)),
    20 -> (0, 22, List(4, 3, 3, 3, 3, 2, 2, 1, 1))
  )

  private val bardCantrips: Map[Int, Int] = Map(
    1 -> 2, 2 -> 2, 3 -> 2, 4 -> 3, 5 -> 3, 6 -> 3, 7 -> 3, 8 -> 3, 9 -> 3, 10 -> 4,
    11 -> 4, 12 -> 4, 13 -> 4, 14 -> 4, 15 -> 4, 16 -> 4, 17 -> 4, 18 -> 4, 19 -> 4, 20 -> 4
  )
  private val bardPrepared: Map[Int, Int] = Map(
    1 -> 4, 2 -> 5, 3 -> 6, 4 -> 7, 5 -> 9, 6 -> 10, 7 -> 11, 8 -> 12, 9 -> 14, 10 -> 15,
    11 -> 16, 12 -> 16, 13 -> 17, 14 -> 17, 15 -> 18, 16 -> 18, 17 -> 19, 18 -> 20, 19 -> 21, 20 -> 22
  )

  private val clericCantrips: Map[Int, Int] = Map(
    1 -> 3, 2 -> 3, 3 -> 3, 4 -> 4, 5 -> 4, 6 -> 4, 7 -> 4, 8 -> 4, 9 -> 4, 10 -> 5,
    11 -> 5, 12 -> 5, 13 -> 5, 14 -> 5, 15 -> 5, 16 -> 5, 17 -> 5, 18 -> 5, 19 -> 5, 20 -> 5
  )

  private val druidCantrips: Map[Int, Int] = Map(
    1 -> 2, 2 -> 2, 3 -> 2, 4 -> 3, 5 -> 3, 6 -> 3, 7 -> 3, 8 -> 3, 9 -> 3, 10 -> 4,
    11 -> 4, 12 -> 4, 13 -> 4, 14 -> 4, 15 -> 4, 16 -> 4, 17 -> 4, 18 -> 4, 19 -> 4, 20 -> 4
  )

  private val sorcererCantrips: Map[Int, Int] = Map(
    1 -> 4, 2 -> 4, 3 -> 4, 4 -> 5, 5 -> 5, 6 -> 5, 7 -> 5, 8 -> 5, 9 -> 5, 10 -> 6,
    11 -> 6, 12 -> 6, 13 -> 6, 14 -> 6, 15 -> 6, 16 -> 6, 17 -> 6, 18 -> 6, 19 -> 6, 20 -> 6
  )
  private val sorcererPrepared: Map[Int, Int] = Map(
    1 -> 4, 2 -> 5, 3 -> 6, 4 -> 7, 5 -> 9, 6 -> 10, 7 -> 11, 8 -> 12, 9 -> 14, 10 -> 15,
    11 -> 16, 12 -> 16, 13 -> 17, 14 -> 17, 15 -> 18, 16 -> 18, 17 -> 19, 18 -> 20, 19 -> 21, 20 -> 22
  )

  private val wizardCantrips: Map[Int, Int] = Map(
    1 -> 3, 2 -> 3, 3 -> 3, 4 -> 4, 5 -> 4, 6 -> 4, 7 -> 4, 8 -> 4, 9 -> 4, 10 -> 5,
    11 -> 5, 12 -> 5, 13 -> 5, 14 -> 5, 15 -> 5, 16 -> 5, 17 -> 5, 18 -> 5, 19 -> 5, 20 -> 5
  )
  private val wizardPrepared: Map[Int, Int] = Map(
    1 -> 4, 2 -> 5, 3 -> 6, 4 -> 7, 5 -> 9, 6 -> 10, 7 -> 11, 8 -> 12, 9 -> 14, 10 -> 15,
    11 -> 16, 12 -> 16, 13 -> 17, 14 -> 18, 15 -> 19, 16 -> 21, 17 -> 22, 18 -> 23, 19 -> 24, 20 -> 26
  )

  private val fullCasterCantrips: Map[FullCasterVariant, Map[Int, Int]] = Map(
    FullCasterVariant.Bard    -> bardCantrips,
    FullCasterVariant.Cleric  -> clericCantrips,
    FullCasterVariant.Druid   -> druidCantrips,
    FullCasterVariant.Sorcerer -> sorcererCantrips,
    FullCasterVariant.Wizard  -> wizardCantrips
  )
  private val fullCasterBasePrepared: Map[Int, Int] =
    fullCasterBase.map { case (lvl, (_, prepared, _)) => lvl -> prepared }
  private val fullCasterPrepared: Map[FullCasterVariant, Map[Int, Int]] = Map(
    FullCasterVariant.Bard     -> bardPrepared,
    FullCasterVariant.Cleric   -> fullCasterBasePrepared,
    FullCasterVariant.Druid    -> fullCasterBasePrepared,
    FullCasterVariant.Sorcerer -> sorcererPrepared,
    FullCasterVariant.Wizard   -> wizardPrepared
  )

  // Half caster progression (Paladin, Ranger): no cantrips, spell slots up to level 5
  private val halfCasterSlots: Map[Int, List[Int]] = Map(
    1  -> List(2, 0, 0, 0, 0, 0, 0, 0, 0),
    2  -> List(2, 0, 0, 0, 0, 0, 0, 0, 0),
    3  -> List(3, 0, 0, 0, 0, 0, 0, 0, 0),
    4  -> List(3, 0, 0, 0, 0, 0, 0, 0, 0),
    5  -> List(4, 2, 0, 0, 0, 0, 0, 0, 0),
    6  -> List(4, 2, 0, 0, 0, 0, 0, 0, 0),
    7  -> List(4, 3, 0, 0, 0, 0, 0, 0, 0),
    8  -> List(4, 3, 0, 0, 0, 0, 0, 0, 0),
    9  -> List(4, 3, 2, 0, 0, 0, 0, 0, 0),
    10 -> List(4, 3, 2, 0, 0, 0, 0, 0, 0),
    11 -> List(4, 3, 3, 0, 0, 0, 0, 0, 0),
    12 -> List(4, 3, 3, 0, 0, 0, 0, 0, 0),
    13 -> List(4, 3, 3, 1, 0, 0, 0, 0, 0),
    14 -> List(4, 3, 3, 1, 0, 0, 0, 0, 0),
    15 -> List(4, 3, 3, 2, 0, 0, 0, 0, 0),
    16 -> List(4, 3, 3, 2, 0, 0, 0, 0, 0),
    17 -> List(4, 3, 3, 3, 1, 0, 0, 0, 0),
    18 -> List(4, 3, 3, 3, 1, 0, 0, 0, 0),
    19 -> List(4, 3, 3, 3, 2, 0, 0, 0, 0),
    20 -> List(4, 3, 3, 3, 2, 0, 0, 0, 0)
  )

  private val halfCasterPrepared: Map[Int, Int] = Map(
    1 -> 2, 2 -> 3, 3 -> 4, 4 -> 5, 5 -> 6, 6 -> 6, 7 -> 7, 8 -> 7, 9 -> 9, 10 -> 9,
    11 -> 10, 12 -> 10, 13 -> 11, 14 -> 11, 15 -> 12, 16 -> 12, 17 -> 14, 18 -> 14, 19 -> 15, 20 -> 15
  )

  // Warlock Pact Magic: unique slot system
  private val warlockTable: Map[Int, PactMagicRow] = Map(
    1  -> PactMagicRow(2, 2,  1, 1),
    2  -> PactMagicRow(2, 3,  2, 1),
    3  -> PactMagicRow(2, 4,  2, 2),
    4  -> PactMagicRow(3, 5,  2, 2),
    5  -> PactMagicRow(3, 6,  2, 3),
    6  -> PactMagicRow(3, 7,  2, 3),
    7  -> PactMagicRow(3, 8,  2, 4),
    8  -> PactMagicRow(3, 9,  2, 4),
    9  -> PactMagicRow(3, 10, 2, 5),
    10 -> PactMagicRow(4, 10, 2, 5),
    11 -> PactMagicRow(4, 11, 3, 5),
    12 -> PactMagicRow(4, 11, 3, 5),
    13 -> PactMagicRow(4, 12, 3, 5),
    14 -> PactMagicRow(4, 12, 3, 5),
    15 -> PactMagicRow(4, 13, 3, 5),
    16 -> PactMagicRow(4, 13, 3, 5),
    17 -> PactMagicRow(4, 14, 4, 5),
    18 -> PactMagicRow(4, 14, 4, 5),
    19 -> PactMagicRow(4, 15, 4, 5),
    20 -> PactMagicRow(4, 15, 4, 5)
  )

  def forClass(dndClass: DndClass, characterLevel: Int): Option[SpellSlotRow] = {
    val lvl = math.max(1, math.min(20, characterLevel))
    dndClass.spellCasterType match {
      case SpellCasterType.NonCaster => None
      case SpellCasterType.FullCaster =>
        dndClass.fullCasterVariant.flatMap { variant =>
          fullCasterBase.get(lvl).flatMap { case (_, _, slots) =>
            fullCasterCantrips.get(variant).flatMap(_.get(lvl)).flatMap { cantrips =>
              fullCasterPrepared.get(variant).flatMap(_.get(lvl)).map { prepared =>
                SpellSlotRow(cantrips, prepared, slots)
              }
            }
          }
        }
      case SpellCasterType.HalfCaster =>
        halfCasterSlots.get(lvl).map { slots =>
          SpellSlotRow(0, halfCasterPrepared(lvl), slots)
        }
      case SpellCasterType.PactMagic =>
        warlockTable.get(lvl).map { pm =>
          val slots = List.fill(9)(0).updated(pm.slotLevel - 1, pm.numSlots)
          SpellSlotRow(pm.cantrips, pm.preparedSpells, slots)
        }
    }
  }

  def pactMagicForLevel(characterLevel: Int): Option[PactMagicRow] =
    warlockTable.get(math.max(1, math.min(20, characterLevel)))

  def wizardSpellbookSize(characterLevel: Int): Int = {
    val lvl = math.max(1, math.min(20, characterLevel))
    if lvl == 1 then 6 else 6 + (lvl - 1) * 2
  }

  def maxSpellLevelForSlots(dndClass: DndClass, characterLevel: Int): Int =
    forClass(dndClass, characterLevel) match {
      case None => 0
      case Some(row) =>
        val lastNonZero = row.slots.lastIndexWhere(_ > 0)
        if lastNonZero < 0 then 0 else lastNonZero + 1
    }
}
