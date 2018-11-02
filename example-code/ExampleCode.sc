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
          Skill("possums", false),
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
  ),
  Team("Inchoate Team", List.empty)
)

/** Tons of data copying, most of it pretty boilerplatey */
def makePossumsCool(teams: List[Team]): List[Team] = teams match {
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
def makePossumsCool2(teams: List[Team]): List[Team] = teams match {
  case (Team(teamName,
             (Employee(employeeName,
                       (Skill(skillName@"possums", false) +: tailSkills)) +: tailEmployees))
          +: tailTeams) =>
    Team(teamName, (Employee(employeeName, (Skill(skillName, true) +: tailSkills)) +: tailEmployees)) +: tailTeams
  case teams => teams
}

println(someTeams)
println(makePossumsCool(someTeams))
println(makePossumsCool2(someTeams))


/** Lenses zone
  // skip setting just the first element since it seems really hard
  import monocle.Lens
  import monocle.macros.GenLens
  import monocle.function._

  val teamMembers: Lens[Team, List[Employee]] = GenLens[Team](_.teamMembers)
  val specialSkills: Lens[Employee, List[Skill]] = GenLens[Employee](_.specialSkills)

  */

import monocle.{Lens, Traversal}
import monocle.macros.GenLens
import monocle.function.all._

val teamMembers: Lens[Team, List[Employee]] = GenLens[Team](_.teamMembers)
val specialSkills: Lens[Employee, List[Skill]] = GenLens[Employee](_.specialSkills)
val coolLens: Lens[Skill, Boolean] = GenLens[Skill](_.isCool)

val composed: Traversal[List[Team], List[Employee]] = each[List[Team], Team] composeLens teamMembers
