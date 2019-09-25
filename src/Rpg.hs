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
damage (Character {name = att_n, level = att_l, state = Alive}
  , target@Character { name = tar_n, health = tar_h, level = tar_l
  , state = Alive}, amount)
  | att_n == tar_n = error "A Character cannot Deal Damage to itself."
  | real_dmg >= tar_h = target {health = 0, state = Dead}
  | otherwise = target {health = tar_h - real_dmg, state = Alive}
  where
    real_dmg
      | att_l > tar_l && att_l - tar_l >= 5 = round (toRational amount * 1.5)
      | tar_l > att_l && (tar_l - att_l) >= 5 = round (toRational amount * 0.5)
      | otherwise = amount
heal :: (Character, Natural) -> Character
heal (blessed@Character {health = h, state = Alive}, amount) =
  blessed {health = min 1000 (amount + h)}
heal (Character {state = Dead}, _) = error "A dead character cannot heal or be healed"
