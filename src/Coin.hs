module Coin ( flipCoin ) where
import System.Random

data Coin = Heads | Tails deriving (Show, Enum, Bounded)

instance Random Coin where
  randomR (a, b) g =
    case randomR (fromEnum a, fromEnum b) g of
      (x, g) -> (toEnum x, g)
  random g = randomR (minBound, maxBound) g

flipCoin :: IO()
flipCoin = do
  g <- newStdGen
  print . take 5 $ (randoms g :: [Coin])

