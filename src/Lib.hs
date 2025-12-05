module Lib where

import Data.Maybe (listToMaybe)

import Text.ParserCombinators.ReadP

-- TODO: unintuitive order, either postpend or `foldr` with initial `reverse`?
accum :: Foldable fo => (b -> a -> b) -> b -> fo a -> [b]
accum f i as = snd $ foldl' (\(c,ras) a -> (f c a,(f c a):ras)) (i,[]) as

intenner :: Int -> Integer
intenner i = read ('1':replicate i '0') -- `10^^l` is a `Fractional`

-- TODO: generalize?
convoluteWith3 :: [Integer] -> [Integer] -> [Integer]
convoluteWith3 [up,he,dn] xs = zipWith3 ((+) .|. (+)) (map (*up) xs ++ [0,0])
                                                      (map (*he) (0:xs ++ [0]))
                                                      (map (*dn) (0:0:xs))

padLeft :: Char -> Int -> String -> String
padLeft c l str = replicate (l - length str) c ++ str

-- from `Utils.ReadPMaybe`
tryParser :: ReadP a -> String -> Maybe a
tryParser = fmap fst . listToMaybe . reverse .|. readP_to_S

-- from `Utils.Util`
infixr 8 .|. -- in (f .|. g . h) precedence does not matter
             --    (f . g .|. h) does not make sense
(.|.) :: (c -> d) -> (a -> b -> c) -> a -> b -> d -- rename?
f .|. g = (f .) . g
