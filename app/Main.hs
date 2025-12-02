module Main (main) where

import Data.Char (isDigit)
import Data.List (nub)

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
         let sillies = concatMap (\range -> nub $ concatMap (flip silliesInRange range) [2..10]) ranges
         print $ length sillies
         print $ sum sillies

parseRot :: String -> Int
parseRot ('L':r) = 0 - read r
parseRot ('R':r) =     read r

hits :: Int -> Int -> Int
hits old rot = abs (rot`quot`100) + fromEnum (((old`div`100 /= new`div`100)
                                            || (new`mod`100 == 0))
                                           && (not (old`mod`100 == 0)))
 where new = (old+(rot`rem`100))

parseInteger :: ReadP Integer
parseInteger = read <$> munch1 isDigit

parseRange :: ReadP (Integer,Integer)
parseRange = (,) <$> parseInteger <*> (char '-' >> parseInteger)

silliesInRange :: Int -> (Integer,Integer) -> [Integer]
silliesInRange times (from,to) | (l`mod`times/=0) = silliesInRange times (intenner l,to)
                               | otherwise = dropWhile (<from)
                                           $ takeWhile (<=to)
                                           $ map (read.concat.(replicate times).show) [h..]
                                          -- $ [hh,hh+step..]
 where l = length (show from)
       hs = take (l`div`times) (show from)
       h = read hs :: Integer
       --hh = read (hs++hs)
       --step = intenner (l`div`times) + 1
