module Lib where

helloWorld :: IO ()
helloWorld = putStrLn "Hello World"

addOne :: Int -> Int
addOne n = n + 1

subOne :: Int -> Int
subOne n = n - 1
