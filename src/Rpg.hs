module Rpg where

import Numeric.Natural (Natural)

data State
  = Alive
  | Dead
  deriving (Eq, Show)

data Character =
  Character
    { name :: String 
    , health :: Natural
    , level :: Natural
    , state :: State
    }
  deriving (Eq, Show)

newCharacter :: String -> Character
newCharacter name = Character {name = name, health = 1000, level = 1, state = Alive}

damage :: (Character, Character, Natural) -> Character
damage (Character {state = Alive}, defender@Character {health = def_h, state = Alive}, amount) =
  if amount >= def_h
    then defender {health = 0, state = Dead}
    else defender {health = def_h - amount, state = Alive}

heal :: (Character, Character, Natural) -> Character
heal (Character {state = Alive}, blessed@Character {health = h, state = Alive}, amount) =
  blessed {health = min 1000 (amount + h)}
heal (Character {state = Dead}, _, _) = error "A dead character cannot heal"
heal (_, Character {state = Dead}, _) = error "A dead character cannot be healed"
