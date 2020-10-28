-- la linea de abajo es para usar el typeclass Num
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Practica where

import Control.Concurrent.STM
import Control.Monad

data Item = RedScroll
            | BlueScroll
            | Axe
            deriving (Eq, Ord, Show)

newtype HitPoint = HitPoint Int
    deriving (Eq, Ord, Show, Num)

type InventoryOut = [Item]
type Inventory = TVar [Item]
type Health = TVar HitPoint
type Account = TVar Int

data Player = Player {
    account :: Account,
    health :: Health,
    inventory :: Inventory
}

removeInv :: Eq a => a -> [a] -> Maybe [a]
removeInv x xs =
    case span (/= x) xs of
      (_, [])                -> Nothing
      (prefix, (_ : suffix)) -> Just $ prefix ++ suffix

-- Transferir oro de una cuenta a otra
-- Nota: Lo que hay es una intuicion, el codigo esta incompleto y hay cosas por agregar
transfer :: Int -> Account -> Account -> STM ()
transfer gold fromAcc toAcc = do
    fromQty <- readTVar fromAcc
    writeTVar toAcc (fromQty + qty)

-- Transferir un item
-- Nota: Lo que hay es una intuicion, el codigo esta incompleto y hay cosas por agregar
giveItem :: Item -> Inventory -> Inventory -> STM Bool
giveItem item fromInv toInv = do
    fromList <- readTVar fromInv
    case removeInv item fromList of
      Nothing      -> return False
      Just newList -> do
        return True

-- Vender un Item, ver si la firma es la correcta
-- Nota: Lo que hay es una intuicion, el codigo esta incompleto y hay cosas por agregar  
sellItem :: Item -> Int -> Player -> Player -> ()
sellItem item price buyer seller = do
  transfer price (balance buyer) (balance seller)