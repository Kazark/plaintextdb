module Main where

import PlainTextDb

main :: IO ()
main = putStrLn $ unlines $ format []
