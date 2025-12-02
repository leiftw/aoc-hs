module Main (main) where

import Data.Maybe (listToMaybe)
import Data.Char (isDigit)

import Text.ParserCombinators.ReadP

import Lib

main :: IO ()
main = do
         input1 <- readFile "input1.txt"
         let rots = map parseRot $ words input1
         print $ length $ filter (==0) $ map (`mod`100) $ accum (+) 50 rots
         print $ snd $ foldl' (\(c,tot) rot -> ((c+rot) `mod` 100,tot + hits c rot)) (50,0) rots
         input2 <- readFile "input2.txt"
         let Just ranges = tryParser (parseRange `sepBy` string ",") input2
         let sillies = concatMap silliesInRange ranges
         print sillies
         print $ length sillies
         print $ sum sillies

parseRot :: String -> Int
parseRot ('L':r) = 0 - read r
parseRot ('R':r) =     read r

-- TODO: unintuitive order, either postpend or `foldr` with initial `reverse`?
accum :: Foldable fo => (b -> a -> b) -> b -> fo a -> [b]
accum f i as = snd $ foldl' (\(c,ras) a -> (f c a,(f c a):ras)) (i,[]) as

hits :: Int -> Int -> Int
hits old rot = abs (rot`quot`100) + fromEnum (((old`div`100 /= new`div`100)
                                            || (new`mod`100 == 0))
                                           && (not (old`mod`100 == 0)))
 where new = (old+(rot`rem`100))

parseInteger :: ReadP Integer
parseInteger = read <$> munch1 isDigit

parseRange :: ReadP (Integer,Integer)
parseRange = (,) <$> parseInteger <*> (char '-' >> parseInteger)

silliesInRange :: (Integer,Integer) -> [Integer]
silliesInRange (from,to) | (l`mod`2==1) = silliesInRange (intenner l,to)
                         | otherwise = dropWhile (<from)
                                     $ takeWhile (<=to)
                                    -- $ map (read.(\s -> s++s).show) [h..]
                                     $ [hh,hh+step..]
 where l = length (show from)
       hs = take (l`div`2) (show from)
       -- h = read hs :: Integer
       hh = read (hs++hs)
       step = intenner (l`div`2) + 1

intenner :: Int -> Integer
intenner i = read ('1':replicate i '0') --10^^l

-- from `Utils.ReadPMaybe`
tryParser :: ReadP a -> String -> Maybe a
tryParser = fmap fst . listToMaybe . reverse .|. readP_to_S
-- from `Utils.Util`
infixr 8 .|. -- in (f .|. g . h) precedence does not matter
             --    (f . g .|. h) does not make sense
(.|.) :: (c -> d) -> (a -> b -> c) -> a -> b -> d -- rename?
f .|. g = (f .) . g
