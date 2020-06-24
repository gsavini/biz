module Biz.Helper where

import Biz.Data
import qualified Data.Map as M
import Data.List
import Data.Char

delete :: Int -> [a] -> [a]
delete index list =
  let pair = splitAt index list
  in fst pair ++ (drop 1 . snd $ pair)

replace :: Eq a => a -> a -> [a] -> [a]
replace _ _ [] = []
replace a b (x:xs)
  | x == a = b : replace a b xs
  | otherwise = x : replace a b xs

mergeStands :: BizVal -> BizVal -> BizVal
mergeStands new@(Stand name abilities1 sp) old@(Stand _ abilities2 _) = Stand name (M.union abilities1 abilities2) sp

-- https://stackoverflow.com/a/5852820
replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
  | n == 0 = newVal:xs
  | otherwise = x:replaceNth (n - 1) newVal xs

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

-- https://stackoverflow.com/a/4981265
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                  "" -> []
                  s' -> w : wordsWhen p s''
                    where (w, s'') = break p s'
