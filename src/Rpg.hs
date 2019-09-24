module Rpg where

import Numeric.Natural (Natural)

newtype Health =
  H Natural
  deriving (Eq, Show)

newtype Level =
  L Natural
  deriving (Eq, Show)

data State
  = Alive
  | Dead
  deriving (Eq, Show)

type Character = (Health, Level, State)

newCharacter :: Character
newCharacter = (H 1000, L 1, Alive)

damage :: (Character, Character, Natural) -> Character
damage ((_, _, Alive), (H def_h, def_l, Alive), amount) =
  if amount >= def_h
    then (H 0, def_l, Dead)
    else (H (def_h - amount), def_l, Alive)

heal :: (Character, Character, Natural) -> Character
heal ((_, _, Alive), (H h, l, Alive), amount) = (H (min 1000 (amount + h)), l, Alive)
heal ((_, _, Dead), (_, _, _), _) = error "A dead character cannot heal"
heal ((_, _, _), (_, _, Dead), _) = error "A dead character cannot be healed"

healthOf :: Character -> Health
healthOf (h, _, _) = h

levelOf :: Character -> Level
levelOf (_, l, _) = l

stateOf :: Character -> State
stateOf (_, _, s) = s