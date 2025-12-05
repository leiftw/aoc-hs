module Lib where

import Data.Char (isDigit)
import Data.Maybe (listToMaybe)

import Text.ParserCombinators.ReadP

-- TODO: unintuitive order, either postpend or `foldr` with initial `reverse`?
accum :: Foldable fo => (b -> a -> b) -> b -> fo a -> [b]
accum f i as = snd $ foldl' (\(c,ras) a -> (f c a,(f c a):ras)) (i,[]) as

intenner :: Int -> Integer
intenner i = read ('1':replicate i '0') -- `10^^l` is a `Fractional`

-- TODO: generalize?
convoluteWith3 :: (Num n) => [n] -> [n] -> [n]
convoluteWith3 [up,he,dn] xs = zipWith3 ((+) .|. (+)) (map (*up) xs ++ [0,0]) -- TODO: inelegant `++`
                                                      (map (*he) (0:xs ++ [0]))
                                                      (map (*dn) (0:0:xs))

-- TODO: find in some library?
padLeft :: Char -> Int -> String -> String
padLeft c l str = replicate (l - length str) c ++ str

-- can't really be used in `silliesInRange` but probably elsewhere
inRange :: (Ord o) => (o,o) -> o -> Bool
inRange (from,to) x = (from <= x) && (x <= to)

-- variant using `Ordering` a bit freely
orderRange :: (Ord o) => (o,o) -> o -> Ordering
orderRange (from,to) x | x < from = GT -- unintuitive, but follows the order of `compare`
                       | x > to   = LT
                       | otherwise = EQ

-- `matchWith` would take unsorted input, but sorting them is nlogn while checking any pair is nn
-- "match" could be misleading, here targets can be matched multiple times
matchWithSorted :: (a -> b -> Ordering) -> [a] -> [b] -> [b]
matchWithSorted _ [] _ = []
matchWithSorted _ _ [] = []
matchWithSorted f (t:ts) (x:xs) = case f t x of
                                       EQ -> x : matchWithSorted f (t:ts) xs
                                          -- record a match and move on to the next shot
                                       LT -> matchWithSorted f ts (x:xs) -- skip too low target
                                       GT -> matchWithSorted f (t:ts) xs -- skip too low shot

parseInteger :: ReadP Integer
parseInteger = read <$> munch1 isDigit

parseRange :: ReadP (Integer,Integer)
parseRange = (,) <$> parseInteger <*> (char '-' >> parseInteger)

-- from `Utils.ReadPMaybe`
tryParser :: ReadP a -> String -> Maybe a
tryParser = fmap fst . listToMaybe . reverse .|. readP_to_S

-- from `Utils.Util`
infixr 8 .|. -- in (f .|. g . h) precedence does not matter
             --    (f . g .|. h) does not make sense
(.|.) :: (c -> d) -> (a -> b -> c) -> a -> b -> d -- rename?
f .|. g = (f .) . g
