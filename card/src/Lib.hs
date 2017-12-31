module Lib (someFunc) where

import Control.Monad.Trans.State (State, get, put, runState)
import Data.List (sort)

someFunc :: IO ()
someFunc = print $ runState game [1 .. 50]
someFunc = print $ runState game [1 .. 50]


type Card = Int
type Score = Int
type Hand = [Card]
type Stock = [Card]
type Player = String


drawCards :: Int -> State Stock Hand
drawCards n = do
  deck <- get
  put $ drop n deck
  return $ take n deck

game :: State Stock [(Score, Hand, Player)]
game = do
  taroHand <- drawCards 5
  hanakoHand <- drawCards 5
  takashiHand <- drawCards 5
  yumiHand <- drawCards 5

  return . reverse . sort $
    [ (sum taroHand, taroHand, "Taro")
    , (sum hanakoHand, hanakoHand, "Hanako")
    , (sum takashiHand, takashiHand, "Takashi")
    , (sum yumiHand, yumiHand, "Yumi")
    ]
