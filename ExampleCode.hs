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
    [ Employee "Alice" [ Skill "possum identification" False
                       , Skill "terrifying martial arts" True
                       ]
    , Employee "Arianna" [ Skill "making cents" False
                         , Skill "sending slack messages" True
                         , Skill "frisbee" True ]]
  ]

{- We're just going to make the first employee's first skill cool, to simplify things,
and also do it unsafely -- again, for simplicity. This is the simple version
-}
makePossumIdentificationCool :: [Team] -> [Team]
makePossumIdentificationCool [] = []
makePossumIdentificationCool (team:teams) =
  (Team (_teamName team)
  $ (Employee (_employeeName . head . _teamMembers $ team)
     ((Skill ( _skillName . head . _specialSkills . head . _teamMembers $ team) True)
     : (_specialSkills . head . _teamMembers $ team))
    ) : (tail . _teamMembers $ team)) : teams

{- Similar, but we're more specifically going to match our intent, to show the insanity of the necessary
pattern match
-}
makePossumIdentificationCool' :: [Team] -> [Team]
makePossumIdentificationCool' ((Team teamName (Employee eName (headSkill@(Skill "possum identification" False):tailSkills):tailEs)):tailTeams) =
  (Team teamName
   $ Employee eName (Skill (_skillName headSkill) True : tailSkills) : tailEs) : tailTeams
makePossumIdentificationCool' teams = teams

makePossumIdentificationCoolWithLens :: [Team] -> [Team]
makePossumIdentificationCoolWithLens =
  set (ix 0 . teamMembers . ix 0 . specialSkills . ix 0 . isCool) True

theyreTheSame :: Bool
theyreTheSame = makePossumIdentificationCool' someTeams == makePossumIdentificationCoolWithLens someTeams

makeAllTheSkillsCool :: [Team] -> [Team]
makeAllTheSkillsCool = undefined
