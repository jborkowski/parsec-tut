module Main where

import Lib
import ShowParser (parseShow)

recStr =  show [rec1,rec2]

main :: IO ()
--main = putStrLn $ show [rec1,rec2]
main = putStrLn $ parseShow recStr
