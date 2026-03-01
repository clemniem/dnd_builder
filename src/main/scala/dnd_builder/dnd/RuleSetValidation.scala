package dndbuilder.dnd

object RuleSetValidation {

  def validate(rs: RuleSet): Either[List[String], RuleSet] = {
    val errors = List.newBuilder[String]
    if (rs.id.isEmpty) errors += "RuleSet id is required"
    if (rs.name.isEmpty) errors += "RuleSet name is required"
    if (rs.classes.isEmpty) errors += "RuleSet must have at least one class"
    val dupes = rs.classes.groupBy(_.name).filter(_._2.size > 1).keys
    dupes.foreach(n => errors += s"Duplicate class name: $n")
    rs.classes.foreach { cls =>
      if (cls.name.isEmpty) errors += "Class name is required"
      if (cls.primaryAbilities.isEmpty) errors += s"${cls.name}: at least one primary ability required"
      if (cls.savingThrows.size != 2) errors += s"${cls.name}: exactly 2 saving throws required"
    }
    val errs = errors.result()
    if (errs.isEmpty) Right(rs) else Left(errs)
  }
}
