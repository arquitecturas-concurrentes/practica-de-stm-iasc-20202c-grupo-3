-- Tasty makes it easy to test your code. It is a test framework that can
-- combine many different types of tests into one suite. See its website for
-- help: <http://documentup.com/feuerbach/tasty>.
import qualified Test.Tasty
-- Hspec is one of the providers for Tasty. It provides a nice syntax for
-- writing tests. Its website has more info: <https://hspec.github.io>.
import Test.Tasty.Hspec
import Control.Monad
import Control.Concurrent.STM
import Practica

newPlayer :: Int -> HitPoint -> InventoryOut -> STM Player
newPlayer account health inventory =
    Player `liftM` newTVar account
              `ap` newTVar health
              `ap` newTVar inventory

transferTest = do
  caballero <- newTVar 30
  mago <- newTVar 10
  transfer 10 caballero mago
  -- Lifteo el valor de dos resultados separados por coma
  liftM2 (,) (readTVar caballero) (readTVar mago) 

giveItemTest = do
    caballero <- newPlayer 20 (100 :: HitPoint) [Axe, RedScroll]
    mago <- newPlayer 20 (100 :: HitPoint) []
    giveItem RedScroll (inventory caballero) (inventory mago)
    liftM2 (,) (readTVar (inventory caballero)) (readTVar (inventory mago)) 

sellItemTestCheckInventory = do
    caballero <- newPlayer 20 (100 :: HitPoint) [Axe, RedScroll]
    mago <- newPlayer 20 (100 :: HitPoint) []
    sellItem RedScroll 10 mago caballero
    liftM2 (,) (readTVar (inventory caballero)) (readTVar (inventory mago))

sellItemTestCheckAccount = do
    caballero <- newPlayer 20 (100 :: HitPoint) [Axe, RedScroll]
    mago <- newPlayer 20 (100 :: HitPoint) []
    sellItem RedScroll 10 mago caballero
    liftM2 (,) (readTVar (account caballero)) (readTVar (account mago))
    
main :: IO ()
main = do
    test <- testSpec "iasc-stm-practica" spec
    Test.Tasty.defaultMain test

spec :: Spec
spec = parallel $ do
    describe "transferencia de dinero" $ do
        it "Transfer Test" $ do
            atomically (transferTest) `shouldReturn` (20, 20)

    describe "give Item" $ do
        it "Should be able to transfer an item" $ do
            atomically (giveItemTest) `shouldReturn` ([Axe], [RedScroll])

    describe "sell Item" $ do
        it "Should transfer the item sold" $ do
            atomically (sellItemTestCheckInventory) `shouldReturn` ([Axe], [RedScroll])
        
        it "Should transfer the gold from the sold" $ do
            atomically (sellItemTestCheckAccount) `shouldReturn` (30, 10)
