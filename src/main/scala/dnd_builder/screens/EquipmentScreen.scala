package dndbuilder.screens

import cats.effect.IO
import dndbuilder.{NavigateNext, Screen, ScreenId, ScreenOutput}
import dndbuilder.dnd.*
import tyrian.Html.*
import tyrian.*

object EquipmentScreen extends Screen:
  type Model = EquipmentModel
  type Msg   = EquipmentMsg | NavigateNext

  val screenId: ScreenId = ScreenId.EquipmentId

  def init(previous: Option[ScreenOutput]): (Model, Cmd[IO, Msg]) =
    val (cls, sp, bg, scores, bonus, skills) = previous match
      case Some(ScreenOutput.SkillsChosen(s, c, b, sc, bn, sk)) => (c, s, b, sc, bn, sk)
      case _ =>
        (Barbarian, Human, Acolyte, AbilityScores.default,
          BackgroundBonus.ThreePlusOnes(Ability.Strength, Ability.Dexterity, Ability.Constitution), Set.empty[Skill])
    (EquipmentModel(cls, sp, bg, scores, bonus, skills, None, false, Nil, 4), Cmd.None)

  private def totalStars(model: EquipmentModel): Int =
    model.selectedArmor.fold(0)(_.stars) +
      (if model.selectedShield then Armor.shieldStars else 0) +
      model.selectedWeapons.map(_.stars).sum

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) =
    case EquipmentMsg.SetMaxStars(n) =>
      (model.copy(maxStars = math.max(1, math.min(10, n))), Cmd.None)
    case EquipmentMsg.SelectArmor(armor) =>
      val newArmorStars = armor.fold(0)(_.stars)
      val otherStars = (if model.selectedShield then Armor.shieldStars else 0) + model.selectedWeapons.map(_.stars).sum
      if otherStars + newArmorStars <= model.maxStars then (model.copy(selectedArmor = armor), Cmd.None)
      else (model, Cmd.None)
    case EquipmentMsg.ToggleShield =>
      val shieldCost = Armor.shieldStars
      val current = totalStars(model)
      val newTotal = if model.selectedShield then current - shieldCost else current + shieldCost
      if newTotal <= model.maxStars then (model.copy(selectedShield = !model.selectedShield), Cmd.None)
      else (model, Cmd.None)
    case EquipmentMsg.AddWeapon(weapon) =>
      if model.selectedWeapons.contains(weapon) then (model, Cmd.None)
      else if model.selectedWeapons.size >= 2 then (model, Cmd.None)
      else if totalStars(model) + weapon.stars <= model.maxStars then
        (model.copy(selectedWeapons = model.selectedWeapons :+ weapon), Cmd.None)
      else (model, Cmd.None)
    case EquipmentMsg.RemoveWeapon(weapon) =>
      (model.copy(selectedWeapons = model.selectedWeapons.filter(_ != weapon)), Cmd.None)
    case EquipmentMsg.Next =>
      val output = ScreenOutput.EquipmentChosen(
        model.species, model.dndClass, model.background,
        model.baseScores, model.backgroundBonus, model.chosenSkills,
        model.selectedArmor, model.selectedShield, model.selectedWeapons
      )
      (model, Cmd.Emit(NavigateNext(ScreenId.ClassFeaturesId, Some(output))))
    case EquipmentMsg.Back =>
      val output = ScreenOutput.SkillsChosen(
        model.species, model.dndClass, model.background,
        model.baseScores, model.backgroundBonus, model.chosenSkills
      )
      (model, Cmd.Emit(NavigateNext(ScreenId.SkillsId, Some(output))))
    case EquipmentMsg.NoOp =>
      (model, Cmd.None)
    case _: NavigateNext =>
      (model, Cmd.None)

  private def starsDisplay(n: Int): String = "★" * n + "☆" * (5 - n)

  def view(model: Model): Html[Msg] =
    val proficientArmors = Armor.all.filter(a => model.dndClass.armorProficiencies.contains(a.armorType))
    val proficientWeapons = Weapon.all.filter(w => WeaponProficiency.isProficient(w, model.dndClass.weaponProficiencies))
    val usedStars = totalStars(model)
    val nextEnabled = model.selectedWeapons.nonEmpty

    div(`class` := "screen-container")(
      StepIndicator(6, model.dndClass.isSpellcaster),
      StepNav("< Skills", EquipmentMsg.Back,
        "Next: Class Features >",
        EquipmentMsg.Next, nextEnabled),
      h1(`class` := "screen-title")(text("Choose Equipment")),
      p(`class` := "screen-intro")(text("Select weapons and armor you are proficient with. Each item has a star cost (1–5); stay within your budget.")),
      div(`class` := "flex-row", style := "align-items: center; gap: 0.75rem; margin-bottom: 1rem; flex-wrap: wrap;")(
        label(style := "font-weight: 500;")(text("Star budget: ")),
        input(
          `type` := "number",
          value := model.maxStars.toString,
          min := "1",
          max := "10",
          style := "width: 4rem; padding: 0.35rem;",
          onInput(s => s.toIntOption.fold(EquipmentMsg.SetMaxStars(model.maxStars))(EquipmentMsg.SetMaxStars.apply))
        ),
        span(style := "color: var(--color-text-muted);")(text("stars per character")),
        div(`class` := "points-pool")(
          text("Used: "),
          span(`class` := "points-pool-value")(text(s"$usedStars / ${model.maxStars}"))
        )
      ),
      h2(`class` := "about-heading", style := "margin-top: 0.5rem;")(text("Weapons (choose 1–2)")),
      div(`class` := "points-pool", style := "margin-bottom: 0.5rem;")(
        text("Selected: "),
        span(`class` := "points-pool-value")(text(s"${model.selectedWeapons.size} / 2"))
      ),
      weaponGroups(proficientWeapons, model),
      h2(`class` := "about-heading", style := "margin-top: 1.5rem;")(text("Armor")),
      div(`class` := "card-grid")(
        (List(
          div(
            `class` := (if model.selectedArmor.isEmpty then "card card--selected" else "card"),
            onClick(EquipmentMsg.SelectArmor(None))
          )(
            div(`class` := "card-title")(text("Unarmored")),
            div(`class` := "card-desc")(text("AC = 10 + DEX (or class feature) • 0 ★"))
          )
        ) ++ proficientArmors.map { armor =>
          val wouldExceed = usedStars - model.selectedArmor.fold(0)(_.stars) + armor.stars > model.maxStars
          div(
            `class` := (if model.selectedArmor.contains(armor) then "card card--selected" else if wouldExceed then "card card--disabled" else "card"),
            onClick(if wouldExceed then EquipmentMsg.NoOp else EquipmentMsg.SelectArmor(Some(armor)))
          )(
            div(`class` := "card-title")(text(armor.name)),
            div(`class` := "card-desc")(
              text(s"AC ${armor.baseAC}" + (if armor.addDex then " + DEX" else "") + armor.maxDexBonus.fold("")(m => s" (max +$m)") + s" • ${starsDisplay(armor.stars)}")
            )
          )
        })*
      ),
      (if model.dndClass.armorProficiencies.contains(ArmorType.Shield) then
        div(style := "margin-top: 1rem;")(
          button(
            `class` := (if model.selectedShield then "toggle-option toggle-option--active" else "toggle-option"),
            onClick(EquipmentMsg.ToggleShield)
          )(text(if model.selectedShield then s"Shield: equipped (+2 AC) • ${starsDisplay(Armor.shieldStars)}" else s"Shield: not equipped • ${starsDisplay(Armor.shieldStars)}"))
        )
      else div())
    )

  private def weaponGroups(weapons: List[Weapon], model: EquipmentModel): Html[Msg] =
    import WeaponCategory.*
    import WeaponRange.*
    val simpleMelee   = weapons.filter(w => w.category == Simple && w.range == Melee)
    val simpleRanged  = weapons.filter(w => w.category == Simple && w.range == Ranged)
    val martialMelee  = weapons.filter(w => w.category == Martial && w.range == Melee)
    val martialRanged = weapons.filter(w => w.category == Martial && w.range == Ranged)
    div(
      groupSection("Simple Melee", simpleMelee, model),
      groupSection("Simple Ranged", simpleRanged, model),
      groupSection("Martial Melee", martialMelee, model),
      groupSection("Martial Ranged", martialRanged, model)
    )

  private def groupSection(title: String, weapons: List[Weapon], model: EquipmentModel): Html[Msg] =
    if weapons.isEmpty then div()
    else
      val usedStars = totalStars(model)
      div(`class` := "skill-group")(
        div(`class` := "section-title")(text(title)),
        div(`class` := "card-grid")(
          weapons.map { w =>
            val isSelected = model.selectedWeapons.contains(w)
            val canAdd = model.selectedWeapons.size < 2 && usedStars + w.stars <= model.maxStars
            val wouldExceed = !isSelected && model.selectedWeapons.size < 2 && usedStars + w.stars > model.maxStars
            div(
              `class` := (if isSelected then "card card--selected" else if wouldExceed then "card card--disabled" else "card"),
              onClick(
                if isSelected then EquipmentMsg.RemoveWeapon(w)
                else if canAdd then EquipmentMsg.AddWeapon(w)
                else EquipmentMsg.NoOp
              )
            )(
              div(`class` := "card-title")(text(w.name)),
              div(`class` := "card-desc")(text(s"${w.damage} • ${w.mastery} • ${starsDisplay(w.stars)}"))
            )
          }*
        )
      )

final case class EquipmentModel(
    dndClass: DndClass,
    species: Species,
    background: Background,
    baseScores: AbilityScores,
    backgroundBonus: BackgroundBonus,
    chosenSkills: Set[Skill],
    selectedArmor: Option[Armor],
    selectedShield: Boolean,
    selectedWeapons: List[Weapon],
    maxStars: Int)

enum EquipmentMsg:
  case SetMaxStars(n: Int)
  case SelectArmor(armor: Option[Armor])
  case ToggleShield
  case AddWeapon(weapon: Weapon)
  case RemoveWeapon(weapon: Weapon)
  case Next
  case Back
  case NoOp
