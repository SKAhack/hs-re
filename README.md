# Re

Easy to use Regex for Haskell

```hs
import Re
import Data.Array ((!))

main :: IO ()
main = do
    putStrLn $ show $ matchCount "abc" "abcde abcde"
    -- 2
    putStrLn $ show $ matchTest "abc" "abcde abcde"
    -- True
    putStrLn $ concatMap (\m -> fst $ fst m ) $ matchAll "a(b.c)" "abbcdeabmcde"
    -- abbcabmc
    putStrLn $ replace "abc" "abcde abcde" ""
    -- de de
    putStrLn $ replace "(b)(c)" "abcd abcd" "--\\1--\\2--"
    -- a--b--c--d a--b--c--d
    putStrLn $ replaceMap "(b)(c)" "abcd abcd" (\ps -> "--" ++ (ps!!1) ++ "--" ++ (ps!!2) ++ "--")
    -- a--b--c--d a--b--c--d
```
