module Main where

import qualified FoldVersion
import qualified ReactiveVersion
import qualified ReflexVersion
import qualified ReflexVersion2

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["fold"] -> FoldVersion.exec
    ["reactive"] -> ReactiveVersion.exec
    ["reflex"] -> ReflexVersion.exec
    ["reflex2"] -> ReflexVersion2.exec

    _ -> do putStrLn "Usage: slow-frp <test-type>"
            putStrLn " where"
            putStrLn "   <test-type> one of 'fold', 'reactive', or 'reflex'"
