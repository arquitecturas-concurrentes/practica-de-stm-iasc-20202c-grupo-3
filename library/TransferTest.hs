--para usar el ejemplo
-- atomically transferTest
import Practica
import Control.Concurrent.STM
import Control.Monad

newPlayer :: Int -> HitPoint -> InventoryOut -> STM Player
newPlayer balance health inventory =
    Player `liftM` newTVar balance
              `ap` newTVar health
              `ap` newTVar inventory

transferTest = do
  caballero <- newTVar 20
  mago <- newTVar 10
  transfer 10 caballero mago
  -- Lifteo el valor de dos resultados separados por coma
  liftM2 (,) (readTVar caballero) (readTVar mago) 

giveItemTest = do
  caballero <- newPlayer 20 (100 :: HitPoint) [Axe, RedScroll]
  mago <- newPlayer 20 (100 :: HitPoint) []
  giveItem RedScroll (inventory caballero) (inventory mago)
  liftM2 (,) (readTVar (inventory caballero)) (readTVar (inventory mago)) 

sellItemTest = do
  caballero <- newPlayer 20 (100 :: HitPoint) [Axe, RedScroll]
  mago <- newPlayer 20 (100 :: HitPoint) []
  sellItem RedScroll 10 mago caballero
  liftM2 (,) (readTVar (inventory caballero)) (readTVar (inventory mago))