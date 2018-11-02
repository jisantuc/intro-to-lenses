{-# LANGUAGE TemplateHaskell #-}

module ExampleCode where

import           Control.Lens
import           Control.Lens.TH

data Skill =
  Skill { _skillName :: String
        , _isCool    :: Bool } deriving (Eq, Show)

data Employee =
  Employee { _employeeName  :: String
           , _specialSkills :: [Skill] } deriving (Eq, Show)

data Team =
  Team { _teamName    :: String
       , _teamMembers :: [Employee] } deriving (Eq, Show)

makeLenses ''Skill
makeLenses ''Employee
makeLenses ''Team

someTeams :: [Team]
someTeams =
  [ Team "Civic Apps"
    [ Employee "Alice" [ Skill "possums" False
                       , Skill "terrifying martial arts" True
                       ]
    , Employee "Arianna" [ Skill "making cents" False
                         , Skill "sending slack messages" True
                         , Skill "frisbee" True ]]
  , Team "Inchoate Team" []
  ]

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

{- There is no 99th member of the second team -- what should happen? Subject to some constraints
(Monoid b => a -> b) we just get back the empty case for any lens that "fails"
-}
bogusLens :: [Team] -> [Skill]
bogusLens = view (ix 1 . teamMembers . ix 99 . specialSkills)

makePossumsCoolWithLens :: [Team] -> [Team]
makePossumsCoolWithLens =
  set (ix 0 . teamMembers . ix 0 . specialSkills . ix 0 . isCool) True

theyreTheSame :: Bool
theyreTheSame = makePossumsCool' someTeams == makePossumsCoolWithLens someTeams

makeAllTheSkillsCool :: [Team] -> [Team]
makeAllTheSkillsCool = undefined
