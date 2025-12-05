module Main (main) where

import Data.List (nub, unfoldr, sort)
import Data.Maybe (fromJust)

import Text.ParserCombinators.ReadP

import Lib


main :: IO ()
main = do
         input1 <- readFile "input1.txt"
         let rots = map parseRot $ words input1
         print $ length $ filter (==0) $ map (`mod`100) $ accum (+) 50 rots
            -- could instead of `accum` also `foldl'` with a running total like below
         print $ snd $ foldl' (\(c,tot) rot -> ((c+rot) `mod` 100,tot + hits c rot)) (50,0) rots
         input2 <- readFile "input2.txt"
         let Just ranges = tryParser (parseRange `sepBy` string ",") input2
         let sillies2 = concatMap (silliesInRange 2) ranges
         let silliesX = concatMap (\range -> nub $ concatMap (flip silliesInRange range) [2..10]) ranges
            -- `nub` removes the duplicates that can be read as different numbers of repetitions
         print $ sum sillies2
         print $ sum silliesX
         input3 <- readFile "input3x.txt"
         let joltages = map (map (\c -> fromEnum c - fromEnum '0')) $ lines input3
                                    -- `fromEnum` trick faster than `read [c]`?
         print $ map (maxJoltages 3) joltages
         input4 <- readFile "input4.txt"
         let charbits = map (map roll_bit) $ lines input4
         print $ fst $ forklift charbits
         print $ sum $ unfoldr forkliftOrNothing charbits
         input5 <- readFile "input5.txt"
         let (input5rs,input5ids) = span (/="") $ lines input5
         let idranges = map (fromJust . tryParser parseRange) input5rs
         let ids = map read $ tail input5ids
         print $ length $ matchWithSorted orderRange (sort idranges) (sort ids)
         print $ sum $ map (\(a,b) -> 1+b-a) $ mergeRanges (sort idranges)

-- DAY 1

-- hacky parser, runs faster than a `ReadP`
parseRot :: String -> Int
parseRot ('L':r) = 0 - read r
parseRot ('R':r) =     read r

hits :: Int -> Int -> Int
hits old rot = abs (rot`quot`100) -- how many whole turns
             + fromEnum (((old`div`100 /= new`div`100) -- if crossed to a different period
                       || (new`mod`100 == 0)) -- or arrived at 0,
                      && (not (old`mod`100 == 0))) -- but not departed from 0!
                                                   -- to not double-count departing downwards from 0
 where new = old + (rot`rem`100) -- `mod` or `rem` on `old + rot` would erase the needed information,
                                 --  which is whether 0 or 100 is crossed;
                                 -- `mod` on `rot` would overcount when going down from a high point,
                                 --  misrepresenting it as going further up over 100

-- DAY 2

silliesInRange :: Int -> (Integer,Integer) -> [Integer]
silliesInRange times (from,to) | (l`mod`times/=0) = silliesInRange times (intenner l,to)
                                 -- digit length does not fit, go to the start of the next length
                               | otherwise = dropWhile (<from) -- repeat beginning may lower `from`
                                           $ takeWhile (<=to)
                                           $ map (read.concat.(replicate times).show) [n..]
                                          -- $ [nn,nn+step..]
                                          -- increment the repeated part from the beginning of `from`
 where ms = show from
       l = length ms
       ns = take (l`div`times) ms -- the beginning of `from`, which repeated is a lower bound
       n = read ns :: Integer
       -- ALT: this saves many `read`s and `show`s,
       --       but finds false positives when ranges go over different digit lengths
       --nn = read (concat $ replicate times ns)
       --step = intenner (l`div`times) + 1

-- DAY 3

maxJoltage :: [Int] -> Int
maxJoltage l = maxJoltage' (0,0) l

maxJoltage' :: (Int,Int) -> [Int] -> Int
maxJoltage' (x,y) (a:b:r) | a > x = maxJoltage' (a,b) (b:r)
                          | a > y = maxJoltage' (x,a) (b:r)
                          | otherwise = maxJoltage' (x,y) (b:r)
-- could merge these clauses with `isEmpty`, `head` and `tail`
maxJoltage' (x,y) (a:r) | a > y = maxJoltage' (x,a) r
                        | otherwise = maxJoltage' (x,y) r
maxJoltage' (x,y) [] = (10*x)+y

maxJoltages :: Int -> [Int] -> Integer
maxJoltages k l = maxJoltages' k (replicate k 0) l

maxJoltages' :: Int -> [Int] -> [Int] -> Integer
maxJoltages' k js l@(a:r) = maxJoltages' k
                            (take k $ safes ++ staying ++ l)
                            (drop (max 1 $ length displaced) l) -- start with l where it starts to increase the number
 where (safes,unsafes) = splitAt (k - length l) js
     -- safe not because high enough but because there aren't enough batteries left to displace them
       (staying,displaced) = span (>=a) unsafes
maxJoltages' _ js [] = sum $ zipWith (\i j -> toInteger j * intenner i) [0..] $ reverse js

-- DAY 4

roll_bit :: Char -> Char
roll_bit '@' = '1'
roll_bit '.' = '0'

neighborify :: [[Char]] -> [[Char]]
neighborify charbits = tail $ init $ map (tail . init . padLeft ' ' padLength . show) neighbor_decbits
 where decbits = map read charbits :: [Integer]
       neighbor_decbits = convoluteWith3 [111,101,111] decbits -- adds 1 to every neighboring digit
       padLength = 2 + maximum (map length charbits) -- 2 extra because the neighbor digits spill over

forklift :: [[Char]] -> (Int,[[Char]])
forklift charbits = (length $ filter (\(b,n) -> b=='1' && n<'4') $ concat zipped
                    ,map (map (\(b,n) -> head $ show $ fromEnum $ b=='1' && n>='4')) zipped)
 where zipped = zipWith (zip) charbits (neighborify charbits)

forkliftOrNothing :: [[Char]] -> Maybe (Int,[[Char]])
forkliftOrNothing charbits = case forklift charbits of
                              (0,_) -> Nothing
                              res   -> Just res
-- ALT: could check for `any` removable rolls first,
--       requiring `neighborify` and all the `zip`ping twice if some are removed but
--       avoiding computing characters when none are removed
