module Re (
    matchOnce,
    matchAll,
    matchCount,
    matchTest,
    replace,
    replaceMap
) where

import qualified Text.Regex.Base as B
    ( RegexMaker(..)
    , RegexLike(..)
    , RegexContext(..)
    , defaultExecOpt
    , MatchText
    , MatchArray
    )
import Text.Regex.Posix as P
    ( Regex
    , compNewline
    , compIgnoreCase
    , compExtended
    )
import Data.Array ((!), indices, bounds, elems)

makeRegex :: String -> Regex
makeRegex = B.makeRegex

matchOnce :: String -> String -> Maybe (String, B.MatchText String, String)
matchOnce r s = B.matchOnceText (makeRegex r) s

matchAll :: String -> String -> [((String, Int), [String])]
matchAll r s = do
    let matches = B.matchAllText (makeRegex r) s
    map (\m -> ((substr m, count m), values m)) matches
    where
        values m = map (\n -> fst (m!n)) (drop 1 $ indices m)
        count m = snd $ bounds m
        substr m = fst (m!0)

matchCount :: String -> String -> Int
matchCount r s = B.matchCount (makeRegex r) s

matchTest :: String -> String -> Bool
matchTest r s = B.matchTest (makeRegex r) s

replace :: String -> String -> String -> String
replace r s m = replaceMap r s (func m)
    where
        func tmpl ps =
            compile "\\\\([0-9])" tmpl ps

compile :: String -> String -> [String] -> String
compile r s m = replaceMap r s (func m)
    where
        func ms ps = ms !! (read $ (ps!!1) :: Int)

replaceMap :: String -> String -> ([String] -> String) -> String
replaceMap r s f = do
    let matches = B.matchAllText (makeRegex r) s
    let splited = _split 0 s (map (\m -> m!0) matches)
    concat $ _replacer splited matches f
    where
        _replacer [] _ _ = []
        _replacer s [] _ = s
        _replacer ss ms f = do
            let ps = map (\m -> fst m) $ elems (head ms)
            [head ss] ++ [f ps] ++ (_replacer (tail ss) (tail ms) f)

        _split _ s [] = [s]
        _split i s m  = do
            let (offset, len) = snd (head m)
            let pre = take (offset - i) s
            let left = drop (offset + len - i) s
            pre : ( _split (offset + len) left (tail m) )
