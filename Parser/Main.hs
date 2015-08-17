module Main where

{-

This is an interpreter for our programming language runxy.

Compile this file with

  $ ghc --make Main.hs

and then run it with

  $ ./Main <filename> <int>

-}

import System.Environment
import Interpreter
import Parser

main :: IO()
main = do 
  args <- getArgs
  if length args == 2 then do
     concreteProgram <- readFile (head args)
     let abstractProgram = parseProgram concreteProgram
     let x = read(head(tail args))
     case runxy abstractProgram x of
       Nothing -> putStrLn "Run-time error"  
       Just y ->  putStrLn (show y)
  else do putStrLn "Usage: ./Main <filename> <int>" 
