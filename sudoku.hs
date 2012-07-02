module Main where

import Control.Monad
import Data.Maybe
import Data.IORef

import Puzzle

main :: IO ()
main = do
  s0 <- getContents
  let ls = lines s0
  count <- newIORef 0
  mapM_ (\s -> do
           let mp = readPuzzle s
           (case mp of
              Nothing -> return ()
              Just p -> do
                putStr $ showPuzzle p ++ "\n"
                let p' = fromJust $ solveOnce p
                print $ verifyPuzzle p'
                putStr $ showPuzzle p' ++ "\n\n"
                modifyIORef count (+1))
        ) ls
  print =<< readIORef count













