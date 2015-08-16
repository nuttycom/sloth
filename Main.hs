{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Random
import Data.Map.Strict (Map, insert, findWithDefault, empty)
import Data.Foldable
import Web.Scotty
import Data.Text.Lazy (Text, pack)
import Data.Monoid (mconcat)
import Control.Monad.IO.Class

main :: IO ()
main = scotty 3000 $ do
  let letters = ["foo", "bar", "baz", "gweep", "gwaaak"]
      --letters = "0123456789ABCDEF"
  get "/" $ do 
    gen <- liftIO getStdGen
    html . page . take 10 $ randSeq gen letters

page :: Show a => [a] -> Text
page chars = mconcat
  [ "<html><head/><body>"
  , "<h1><code>"
  , pack $ show chars
  , "</code></h1>"
  , "</body></html>"
  ]

randSeq :: RandomGen g => g -> [a] -> [a]
randSeq gen letters = 
  let lettersLen = length letters - 1
  in  fmap (letters !!) (randomRs (0, lettersLen) gen)

hist :: Ord a => [a] -> Map a Int
hist = foldl' histAcc empty 

histAcc :: Ord a => Map a Int -> a -> Map a Int
histAcc m c = insert c (1 + findWithDefault 0 c m) m
