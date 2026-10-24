module Main (main) where

import Cli (cli)
import Commands (commands)
import Examples (examples)
import Properties (properties)
import Solvers (solvers)
import Storage (storage)
import Test.Tasty

main :: IO ()
main = defaultMain (testGroup "quandary" [examples, properties, solvers, storage, commands, cli])
