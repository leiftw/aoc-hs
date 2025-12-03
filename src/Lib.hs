module Lib where

import Data.Maybe (listToMaybe)

import Text.ParserCombinators.ReadP

-- TODO: unintuitive order, either postpend or `foldr` with initial `reverse`?
accum :: Foldable fo => (b -> a -> b) -> b -> fo a -> [b]
accum f i as = snd $ foldl' (\(c,ras) a -> (f c a,(f c a):ras)) (i,[]) as

intenner :: Int -> Integer
intenner i = read ('1':replicate i '0') -- `10^^l` is a `Fractional`

-- from `Utils.ReadPMaybe`
tryParser :: ReadP a -> String -> Maybe a
tryParser = fmap fst . listToMaybe . reverse .|. readP_to_S

-- from `Utils.Util`
infixr 8 .|. -- in (f .|. g . h) precedence does not matter
             --    (f . g .|. h) does not make sense
(.|.) :: (c -> d) -> (a -> b -> c) -> a -> b -> d -- rename?
f .|. g = (f .) . g
