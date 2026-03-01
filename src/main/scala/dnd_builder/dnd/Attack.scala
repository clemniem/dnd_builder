package dndbuilder.dnd

import dndbuilder.dnd.DndTypes.Modifier

enum AttackKind {
  case Weapon, Spell
}

enum AttackDelivery {
  case AttackRoll(bonus: Modifier)
  case SaveDC(dc: Int, save: Ability)

  def format: String = this match {
    case AttackRoll(bonus) => bonus.format
    case SaveDC(dc, _)     => s"DC $dc"
  }
}

enum SpellDelivery {
  case RangedAttack
  case MeleeAttack
  case Save(ability: Ability)
}

enum AttackGrantDelivery {
  case MeleeAttack(ability: Ability)
  case RangedAttack(ability: Ability)
  case SaveDC(save: Ability, dcAbility: Ability)
}

final case class AttackGrant(
    name: String,
    kind: AttackKind,
    baseDamageDice: String,
    damageType: String,
    delivery: AttackGrantDelivery,
    scalesLikeCantrip: Boolean,
    scalesLikeMartialArts: Boolean,
    usesPerLR: Boolean,
    range: String,
    sourceLabel: String
) {
  def isFilled: Boolean = true
}

case class Attack(
    name: String,
    kind: AttackKind,
    delivery: AttackDelivery,
    damage: String,
    notes: String
)
