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
damage (Character {name = att_n, state = Alive}, defender@Character {name = def_n, health = def_h, state = Alive}, amount)
  | att_n == def_n = error "A Character cannot Deal Damage to itself."
  | amount >= def_h = defender {health = 0, state = Dead}
  | otherwise = defender {health = def_h - amount, state = Alive}
heal :: (Character, Character, Natural) -> Character
heal (Character {state = Alive}, blessed@Character {health = h, state = Alive}, amount) =
  blessed {health = min 1000 (amount + h)}
heal (Character {state = Dead}, _, _) = error "A dead character cannot heal"
heal (_, Character {state = Dead}, _) = error "A dead character cannot be healed"
