case class Skill(skillName: String, isCool: Boolean)
case class Employee(employeeName: String, specialSkills: List[Skill])
case class Team(teamName: String, teamMembers: List[Employee])

val someTeams = List(
  Team(
    "Civic Apps",
    List(
      Employee(
        "Alice",
        List(
          Skill("possum identification", false),
          Skill("terrifying martial arts", true)
        )
      ),
      Employee(
        "Arianna",
        List(
          Skill("making cents", false),
          Skill("frisbee", true)
        )
      )
    )
  )
)

/** Tons of data copying, most of it pretty boilerplatey */
def makePossumIdentificationCool(teams: List[Team]): List[Team] = teams match {
  case Nil => Nil
  case (team +: teams) =>
    team.copy(
      teamMembers = team.teamMembers.head.copy(
        specialSkills = team.teamMembers.head.specialSkills.head.copy(
          isCool = true
        ) +: team.teamMembers.head.specialSkills.tail
      ) +: team.teamMembers.tail
    ) +: teams
}

/** Better since it more specifically matches our intent, but still tons of boilerplatey data copying */
def makePossumIdentificationCool2(teams: List[Team]): List[Team] = teams match {
  case (Team(teamName,
             (Employee(employeeName,
                       (Skill(skillName@"possum identification", false) +: tailSkills)) +: tailEmployees))
          +: tailTeams) =>
    Team(teamName, (Employee(employeeName, (Skill(skillName, true) +: tailSkills)) +: tailEmployees)) +: tailTeams
  case teams => teams
}

println(someTeams)
println(makePossumIdentificationCool(someTeams))
println(makePossumIdentificationCool2(someTeams))


/** Lenses zone
  import monocle.Lens
  import monocle.macros.GenLens
  import monocle.function._

  val teamMembers: Lens[Team, List[Employee]] = GenLens[Team](_.teamMembers)
  val specialSkills: Lens[Employee, List[Skill]] = GenLens[Employee](_.specialSkills)

  */
