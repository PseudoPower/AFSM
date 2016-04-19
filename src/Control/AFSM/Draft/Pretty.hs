-----------------------------------------------------------------------------
-- |
-- Module      :  Control.AFSM.Pretty
-- Copyright   :  (c) Hanzhong Xu, Meng Meng 2016,
-- License     :  MIT License
--
-- Maintainer  :  hanzh.xu@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- Pretty Print - unfinished
-- Perhaps, Using FFI and C is a better way.
-----------------------------------------------------------------------------

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE IncoherentInstances #-}

module Control.AFSM.Draft.Pretty where

import Data.List
import Control.Category
import Control.Arrow

import Control.AFSM.CoreType
import Control.AFSM.Core
import Control.AFSM.Draft.SMMonoid


simpleSMS :: String -> (s -> a -> (s, b)) -> s -> SM String a b
simpleSMS name f s = newSM (f' s) ("- " ++ name ++ " -\n")
  where
    f' s' _ a' = (newSM (f' s'') name, b)
      where
        (s'', b) = f s' a'
        
printSM :: SM String a b -> IO ()
printSM sm = putStrLn (st sm)


instance {-# OVERLAPPING #-} SMMonoid String where
  smempty = ppse
  smid = ppsid
  smappend = ppsa


  
ppse :: Int -> String
ppse 1 = "- id -\n"
ppse 2 = "- arr -\n"
ppse _ = "- unknown -\n"
  
ppsid :: Int -> String -> String
ppsid x s = case x of
  1 -> s ++ (replicate n '-') ++ "\n"
  2 -> (replicate n '-') ++ "\n" ++ s
  where 
    n = length (head $ lines s)

ppsa :: Int -> String -> String -> String
ppsa 1 s0 s1 = ppsa1 s0 s1
ppsa 2 s0 s1 = s0 ++ " *** " ++ s1
ppsa 3 s0 s1 = s0 ++ " &&& " ++ s1

    
findInputs :: [String] -> [Int]
findInputs ls = elemIndices '-' (map head ls)

findOuputs :: [String] -> [Int]
findOuputs ls = elemIndices '-' (map head ls)  


solvePipesHelper :: [(Int, Int)] -> ([(Int, Int)], [(Int, Int)])
solvePipesHelper ((px, py):(x, y):[]) =
  if y >= x then ([(y, y)], [(x, y)])
  else let x' = max (px + 1) y in ([(x', y)], [(x, x')])
solvePipesHelper ((px, py):(x, y):(nx, ny):pps) =
  if y >= x then let x' = min (nx - 1) y in ((x', y):xs, (x, x'):ys)
  else let x' = max (px + 1) y in ((x', y):xs, (x, x'):ys)
  where
    (xs, ys) = solvePipesHelper ((x, y):(nx, ny):pps)

pipesIsDone :: [(Int, Int)] -> Bool
pipesIsDone [] = True
pipesIsDone ((x, y):xs) = if x /= y then False else pipesIsDone xs
        
solvePipes :: [(Int, Int)] -> [[(Int, Int)]]
solvePipes pps = if pipesIsDone pps then [] else b:(solvePipes pps')
  where 
    (pps', b) = solvePipesHelper $ (-1, -1):pps

    
copePipes :: [(Int, Int)] -> [(Int, Int)]
copePipes s = map (\(x, y) -> (x, x)) s

solvePipesA :: [(Int, Int)] -> [[(Int, Int)]]
solvePipesA pps = if pipesIsDone pps then [pps] else (copePipes pps):b:(solvePipesA pps')
  where 
    (pps', b) = solvePipesHelper $ (-1, -1):pps
    
drawOnePipe :: Int -> Int -> [(Int, Int)] -> String
drawOnePipe n m [] = replicate (n - m) ' '
drawOnePipe n m ((x, y):xs) = l1 ++ l2 ++ (drawOnePipe n (y'+1) xs)
  where
    x' = min x y
    y' = max x y
    l1 = if m < x' then replicate (x' - m) ' ' else []
    l2 = if x == y then 
           "-" 
         else if x < y then
           "+" ++ (replicate (y' - x' - 1) '|') ++ "+" 
         else 
            "+" ++ (replicate (y' - x' - 1) '|') ++ "+"
drawPipesHelper :: Int -> [[(Int, Int)]] -> [[Char]]
drawPipesHelper n [] = []
drawPipesHelper n (x:xs) = (drawOnePipe n 0 x):(drawPipesHelper n xs)
    
    
-- testP :: [(Int, Int)]
testP = [(1,3), (2,5), (3,6), (9,7),(10,8)]

ret0 = solvePipesA testP

ret1 = (drawPipesHelper 11 ret0) ++ [replicate 11 '\n']

ret2 = transpose ret1

ret3 = concat ret2

oneFuncSolvePipes :: Int -> [(Int, Int)] -> [String]
oneFuncSolvePipes n ps = ret2
  where
    ret0 = solvePipesA ps
    ret1 = drawPipesHelper n ret0
    ret2 = transpose ret1


addMoreLines :: Int -> [String] -> [String]
addMoreLines n ls = if ln >= n then ls else ls ++ (replicate (n - ln) (replicate lm ' '))
  where
    ln = length ls
    lm = length (head ls)
    
ppsa1 :: String -> String -> String
ppsa1 s0 s1 = ret1
  where
    ls0 = lines s0
    ls1 = lines s1
    n = max (length ls0) (length ls1)
    ls0' = addMoreLines n ls0
    ls1' = addMoreLines n ls1
    out0 = findOuputs ls0'
    in1 = findInputs ls1'
    tt = zip out0 in1
    pipes = oneFuncSolvePipes n tt
    ret0 = zipWith3 (\s0 s1 s2 -> s0 ++ s1 ++ s2 ++ "\n") ls0' pipes ls1'
    ret1 = concat ret0
    

    



-- basic machines

sumSM  = simpleSMS "sum" (\s a -> (s+a, s+a)) 0 

plusOneSM  = simpleSMS "plus" (\() a -> ((), a + 1)) ()

timesTwoSM  = simpleSMS "times" (\() a -> ((), a * 2)) ()

-- combination machine
ttSM = timesTwoSM >>> sumSM 

ptSM = plusOneSM &&& timesTwoSM

ptsSM = plusOneSM *** timesTwoSM

    

    