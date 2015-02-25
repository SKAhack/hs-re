module Main where

import Re
import Data.Array ((!))

main :: IO ()
main = do
    putStrLn $ replace "abc" "abcde abcde" ""
    putStrLn $ show $ matchCount "abc" "abcde abcde"
    putStrLn $ show $ matchTest "abc" "abcde abcde"
    putStrLn $ concatMap (\m -> fst $ fst m ) $ matchAll "a(b.c)" "abbcdeabmcde"
    putStrLn $ replace "(b)(c)" "abcd abcd" "--\\1--\\2--"
    putStrLn $ replaceMap "(b)(c)" "abcd abcd" (\ps -> "--" ++ (ps!!1) ++ "--" ++ (ps!!2) ++ "--")
