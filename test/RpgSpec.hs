module RpgSpec where

import Control.Exception.Base (evaluate)
import Numeric.Natural (Natural)
import Rpg
import Test.Hspec
import Text.Printf (printf)

testDamage :: (Character, Natural) -> (Character -> Bool) -> Spec
testDamage (before, amount) expectation =
   it (printf "%s -> damage %3d" (show before) amount) $
   damage (newCharacter, before, amount) `shouldSatisfy` expectation

testHeal :: (Character, Natural) -> (Character -> Bool) -> Spec
testHeal (before, amount) expectation =
   it (printf "%s -> heal %3d" (show before) amount) $
   heal (newCharacter, before, amount) `shouldSatisfy` expectation

spec :: Spec
spec = do
  describe "All Characters, when created, have" $ do
    it "Health, starting at 1000" $ newCharacter `shouldSatisfy` (\Character {health = h} -> h == 1000)
    it "Level, starting at 1" $ newCharacter `shouldSatisfy` (\Character {level = l} -> l == 1)
    it "May be Alive or Dead, starting Alive (Alive may be a true/false)" $
      newCharacter `shouldSatisfy` (\Character {state = s} -> s == Alive)
  describe "Characters can Deal Damage to Characters" $ do
    describe "Damage is subtracted from Health" $ do
      testDamage (newCharacter, 0) (\Character {health = h, state = s} -> h == 1000 && s == Alive)
      testDamage (newCharacter, 10) (\Character {health = h, state = s} -> h == 990 && s == Alive)
      testDamage (newCharacter, 999) (\Character {health = h, state = s} -> h == 1 && s == Alive)
    describe "When damage received exceeds current Health, Health becomes 0 and the character dies" $ do
      testDamage (newCharacter, 1000) (\Character {health = h, state = s} -> h == 0 && s == Dead)
      testDamage (newCharacter, 1001) (\Character {health = h, state = s} -> h == 0 && s == Dead)
  describe "A Character can Heal a Character" $ do
    testHeal (Character {health = 1, level = 1, state = Alive}, 1) (\Character {health = h, state = s} -> h == 2 && s == Alive)
    testHeal (Character {health = 1, level = 1, state = Alive}, 999) (\Character {health = h, state = s} -> h == 1000 && s == Alive)
    testHeal (Character {health = 999, level = 1, state = Alive}, 1) (\Character {health = h, state = s} -> h == 1000 && s == Alive)
    it "A dead character cannot heal" $
      evaluate (heal (Character {health = 0, level = 1, state = Dead}, newCharacter, 1)) `shouldThrow`
      errorCall "A dead character cannot heal"
    it "A dead character cannot be healed" $
      evaluate (heal (newCharacter, Character {health = 0, level = 1, state = Dead}, 1)) `shouldThrow`
      errorCall "A dead character cannot be healed"
    describe "Healing cannot raise health above 1000" $ do
      testHeal (Character {health = 1000, level = 1, state = Alive}, 1) (\Character {health = h} -> h == 1000)
      testHeal (Character {health = 999, level = 2, state = Alive}, 1) (\Character {health = h} -> h == 1000)
