---
title: Lenses
author: James Santucci
patat:
    wrap: true
    margins:
        left: 20
        right: 20
---

# TODO #

- move js code to [`monocle-ts`](https://github.com/gcanti/monocle-ts)

# Patat #

> - because if you need to leave a terminal you're obviously not doing real work?
> - no!
- fully terminal based -> tmux or screen or whatever else you like
- good backronym -- *P*resentations *At*op the *A*NSI *T*erminal
- french fries
- styling!
- tools are fun

# Lenses #

> - But really "optics"
> - There are a lot of kinds
> - `Lens`, `Prism`, `Traversal`, ...
> - but we don't really have to care about their fancy names, just what we can do with them
> - but I'm still going to say "lenses" to stand in for optics

# Why lenses #

- lenses / prisms / traversals let us specify intent to view / modify data more clearly and concisely
> - with composition
> - and in a way that we'll be able to recognize after reading what we've written a while later
> - no really I'll prove it

# Some data in haskell #

```haskell
data Skill =
  Skill { _skillName :: String
        , _isCool :: Bool}

data Employee =
  Employee { _employeeName :: String
           , _specialSkills :: [Skill] }

data Team =
  Team { _teamName :: String
       , _teamMembers :: [Employee] }

someTeams :: [Team]
someTeams =
  [ Team "Civic Apps"
    [ Employee "Alice" [ Skill "possums" False
                       , Skill "terrifying martial arts" True
                       ]
    , Employee "Arianna" [ Skill "making cents" False
                         , Skill "frisbee" True ]]
  , Team "Inchoate Team" []
  ]
```

# Some data in scala #

```scala
case class Skill(skillName: String, isCool: Boolean)
case class Employee(employeeName: String, specialSkills: List[String])
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
  )
)
```

# Some data in javascript #

```javascript
let someTeams = [
    {
        name: 'Civic Apps',
        employees: [
            {
                name: 'Alice',
                skills: [
                    {
                        name: 'possums',
                        isCool: false
                    },
                    {
                        name: 'terrifying martial arts',
                        isCool: true
                    }
                ]
            },
            {
                name: 'Arianna',
                skills: [
                    {
                        name: 'making cents',
                        isCool: false
                    },
                    {
                        name: 'frisbee',
                        isCool: true
                    }
                ]
            }
        ]
    }
];
```

# How do we update a deeply nested field #

# Without lenses -- haskell #

```haskell
{- We're just going to make the first employee's first skill cool, to simplify things,
and also do it unsafely -- again, for simplicity. This is the simple version
-}
makePossumsCool :: [Team] -> [Team]
makePossumsCool [] = []
makePossumsCool (team:teams) =
  (Team (_teamName team)
  $ (Employee (_employeeName . head . _teamMembers $ team)
     ((Skill ( _skillName . head . _specialSkills . head . _teamMembers $ team) True)
     : (_specialSkills . head . _teamMembers $ team))
    ) : (tail . _teamMembers $ team)) : teams

{- Similar, but we're more specifically going to match our intent, to show the insanity of the necessary
pattern match
-}
makePossumsCool' :: [Team] -> [Team]
makePossumsCool' ((Team teamName (Employee eName (headSkill@(Skill "possums" False):tailSkills):tailEs)):tailTeams) =
  (Team teamName
   $ Employee eName (Skill (_skillName headSkill) True : tailSkills) : tailEs) : tailTeams
makePossumsCool' teams = teams
```

# Without lenses -- scala #

```scala
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
  case (Team(teamName, (Employee(employeeName, (Skill(skillName@"possums", false) +: tailSkills)) +: tailEmployees)) +: tailTeams) =>
    Team(teamName, (Employee(employeeName, (Skill(skillName, true) +: tailSkills)) +: tailEmployees)) +: tailTeams
  case teams => teams
}
```

# Without lenses -- javascript #
```javascript
someTeams[0].employees[0].skills[0].isCool = true
```

# Why's this bad #

> - :thinking:
- not checking to make sure we're updating what we actually care about (for complexity reasons, but obviously that makes complexity worse)
- lots of data copying
- without the function name, we'd have no real way of knowing what the purpose of all that was by looking at it
- in the javascript case, it's _extremely unsafe_ -- we could mistype any of those things, and they wouldn't have item access or we could accidentally introduce a new property instead of updating the existing property or man who even knows

# Updating the first team _with_ lenses #

# With lenses #

- haskell and scala both let us derive lenses automatically because we live in a great future
- ramda lives in a not-typesafe-by-default universe so writing lenses is easy
- `monocle`: http://julien-truffaut.github.io/Monocle/
- `lens`: http://hackage.haskell.org/package/lens
- `ramda`: `R.compose(R.lensProp('foo'), R.lensIndex(n), R.lensProp('bar'))`

# With lenses -- haskell #

```haskell
makePossumsCoolWithLens :: [Team] -> [Team]
makePossumsCoolWithLens =
  set (ix 0 . teamMembers . ix 0 . specialSkills . ix 0 . isCool) True
```

# With lenses -- scala #

```scala

...
```

# With lenses -- javascript #

# What happens if we lens onto something that's not there? #

# Haskell #

```haskell
bogusLens :: [Team] -> [Skill]
bogusLens = view (ix 1 . teamMembers . ix 99 . specialSkills . ix 0)
```

Can we do the following?

```haskell
bogusLens :: [Team] -> Skill
bogusLens = view (ix 1 . teamMembers . ix 99 . specialSkills . ix 0)
```

# Scala #

# Javascript #

# Let's look at some types #

- this is a bad idea
- include Edward Kmett quote here to prove it

# Requirements changed #

- we don't want to create a culture of contempt, so we believe everyone that the special skills they have are really cool
- so we want to update all of the skills
- how?
> - we'd probably make some helper function for updating a team then map
> - but that just moves the mess somewhere else
> - we've all been in the position of "oh this looks simple I wonder what this nice-looking helper function does -- oh no"

# Where can I see lenses in the wild? #
