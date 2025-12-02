module Main (main) where

import Lib

main :: IO ()
main = do
         input1a <- readFile "input1a.txt"
         let rots = map parse $ words input1a
         print $ length $ filter (==0) $ map (`mod`100) $ accum (+) 50 rots
         print $ snd $ foldl' (\(c,tot) rot -> ((c+rot) `mod` 100,tot + hits c rot)) (50,0) rots

parse :: String -> Int
parse ('L':r) = 0 - read r
parse ('R':r) =     read r

-- TODO: unintuitive order, either postpend or `foldr` with initial `reverse`?
accum :: Foldable fo => (b -> a -> b) -> b -> fo a -> [b]
accum f i as = snd $ foldl' (\(c,ras) a -> (f c a,(f c a):ras)) (i,[]) as

hits :: Int -> Int -> Int
hits old rot = abs (rot`quot`100) + fromEnum (((old`div`100 /= new`div`100)
                                            || (new`mod`100 == 0))
                                           && (not (old`mod`100 == 0)))
 where new = (old+(rot`rem`100))
