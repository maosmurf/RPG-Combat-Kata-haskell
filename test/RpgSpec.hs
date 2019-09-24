module RpgSpec where

import Control.Exception.Base (evaluate)
import Numeric.Natural (Natural)
import Rpg (Character, Health(H), Level(L), State(Alive, Dead), damage, heal, healthOf, levelOf, newCharacter, stateOf)
import Test.Hspec
import Text.Printf (printf)

testNewCharacter :: (Health, Level, State) -> Spec
testNewCharacter (H h, L l, s) =
  it (printf "newCharacter: --> %d %d %s" h l (show s)) $ newCharacter `shouldBe` (H h, L l, s)

testDamage :: (Character, Character, Natural) -> Character -> Spec
testDamage (attacker, defender, amount) attacked =
  it (printf "damage:  %s %s %d --> %s" (show attacker) (show defender) amount (show attacked)) $
  damage (attacker, defender, amount) `shouldBe` attacked

testHeal :: (Character, Character, Natural) -> Character -> Spec
testHeal (healer, heallee, amount) healed =
  it (printf "heal:  %s %s %d --> %s" (show healer) (show heallee) amount (show healed)) $
  heal (healer, heallee, amount) `shouldBe` healed

spec :: Spec
spec = do
  describe "All Characters, when created, have" $ do
    it "Health, starting at 1000" $ healthOf newCharacter `shouldBe` H 1000
    it "Level, starting at 1" $ levelOf newCharacter `shouldBe` L 1
    it "May be Alive or Dead, starting Alive (Alive may be a true/false)" $ stateOf newCharacter `shouldBe` Alive
  describe "Characters can Deal Damage to Characters" $ do
    describe "Damage is subtracted from Health" $ do
      testDamage (newCharacter, newCharacter, 0) (H 1000, L 1, Alive)
      testDamage (newCharacter, newCharacter, 10) (H 990, L 1, Alive)
      testDamage (newCharacter, newCharacter, 999) (H 1, L 1, Alive)
    describe "When damage received exceeds current Health, Health becomes 0 and the character dies" $ do
      testDamage (newCharacter, newCharacter, 1000) (H 0, L 1, Dead)
      testDamage (newCharacter, newCharacter, 1001) (H 0, L 1, Dead)
  describe "A Character can Heal a Character" $ do
    testHeal (newCharacter, damage (newCharacter, newCharacter, 100), 10) (H 910, L 1, Alive)
    testHeal (newCharacter, damage (newCharacter, newCharacter, 999), 1) (H 2, L 1, Alive)
    it "A dead character cannot heal" $
      evaluate (heal (damage (newCharacter, newCharacter, 1000), newCharacter, 1)) `shouldThrow`
      errorCall "A dead character cannot heal"
    it "A dead character cannot be healed" $
      evaluate (heal (newCharacter, damage (newCharacter, newCharacter, 1000), 1)) `shouldThrow`
      errorCall "A dead character cannot be healed"
    describe "A Character can Heal a Character" $ do
      testHeal (newCharacter, newCharacter, 1) (H 1000, L 1, Alive)
      testHeal (newCharacter, damage (newCharacter, newCharacter, 10), 20) (H 1000, L 1, Alive)
