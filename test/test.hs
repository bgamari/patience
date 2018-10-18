-- Simple test for Data.Algorithm.Patience
--
-- Invoke as: ./test r n
-- for ints r, n
--
-- Reads lines of standard input, then repeats r times:
--   - Generate two documents of n lines each, by picking
--     randomly from the stdin lines, with replacement
--   - Compute their patience diff
--   - Check that each document is recovered by keeping the
--     respective side of the diff
module Main(main) where

import Control.Monad
import Data.Array
import Data.Maybe
import System.Environment
import System.Random

import Data.Algorithm.Patience

keepOld :: [Item a] -> [a]
keepOld = catMaybes . map f where
  f (Old  x  ) = Just x
  f (New    _) = Nothing
  f (Both x _) = Just x

keepNew :: [Item a] -> [a]
keepNew = catMaybes . map f where
  f (Old  _  ) = Nothing
  f (New    x) = Just x
  f (Both _ x) = Just x

main :: IO ()
main = do
  [r,n] <- map read `fmap` getArgs
  xs    <- lines `fmap` getContents
  let ar   = listArray (0, length xs - 1) xs
      pick = replicateM n ((ar !) `fmap` randomRIO (bounds ar))
  replicateM_ r $ do
    da <- pick
    db <- pick
    let d    = diff da db
        good = (da == keepOld d) && (db == keepNew d)
    when (not good) $ print (da, db, d)
