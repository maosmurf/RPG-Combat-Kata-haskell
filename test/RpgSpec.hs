module RpgSpec where

import Control.Exception.Base (evaluate)
import Numeric.Natural (Natural)
import Rpg
import Test.Hspec
import Text.Printf (printf)

testDamage :: (Character, Natural) -> Character -> Spec
testDamage (before, amount) after =
  it (printf "%s -> damage %3d --> %s" (show before) amount (show after)) $
  damage (newCharacter, before, amount) `shouldBe` after

testHeal :: (Character, Natural) -> Character -> Spec
testHeal (before, amount) after =
  it (printf "%s -> heal %3d --> %s" (show before) amount (show after)) $
  heal (newCharacter, before, amount) `shouldBe` after

spec :: Spec
spec = do
  describe "All Characters, when created, have" $ do
    it "Health, starting at 1000" $ healthOf newCharacter `shouldBe` 1000
    it "Level, starting at 1" $ levelOf newCharacter `shouldBe` 1
    it "May be Alive or Dead, starting Alive (Alive may be a true/false)" $ stateOf newCharacter `shouldBe` Alive
  describe "Characters can Deal Damage to Characters" $ do
    describe "Damage is subtracted from Health" $ do
      testDamage (newCharacter, 0) Character {health = 1000, level = 1, state = Alive}
      testDamage (newCharacter, 10) Character {health = 990, level = 1, state = Alive}
      testDamage (newCharacter, 999) Character {health = 1, level = 1, state = Alive}
    describe "When damage received exceeds current Health, Health becomes 0 and the character dies" $ do
      testDamage (newCharacter, 1000) Character {health = 0, level = 1, state = Dead}
      testDamage (newCharacter, 1001) Character {health = 0, level = 1, state = Dead}
  describe "A Character can Heal a Character" $ do
    testHeal (Character {health = 1, level = 1, state = Alive}, 1) Character {health = 2, level = 1, state = Alive}
    testHeal (Character {health = 1, level = 1, state = Alive}, 999) Character {health = 1000, level = 1, state = Alive}
    testHeal (Character {health = 999, level = 1, state = Alive}, 1) Character {health = 1000, level = 1, state = Alive}
    it "A dead character cannot heal" $
      evaluate (heal (Character {health = 0, level = 1, state = Dead}, newCharacter, 1)) `shouldThrow`
      errorCall "A dead character cannot heal"
    it "A dead character cannot be healed" $
      evaluate (heal (newCharacter, Character {health = 0, level = 1, state = Dead}, 1)) `shouldThrow`
      errorCall "A dead character cannot be healed"
    describe "Healing cannot raise health above 1000" $ do
      testHeal (Character {health = 1000, level = 1, state = Alive}, 1) Character {health = 1000, level = 1, state = Alive}
      testHeal (Character {health = 999, level = 1, state = Alive}, 1) Character {health = 1000, level = 1, state = Alive}
