module Main where

import System.Environment

--main :: IO ()
--main = do
--  args <- getArgs
--  let num1 = read $ args !! 0 :: Int
--      num2 = read $ args !! 1 :: Int
--    in putStrLn $ show $ num1+num2

main :: IO ()
main = do
  putStrLn "What is your name?"
  name <- getLine
  putStrLn $ "Hi, " ++ name ++ "!"