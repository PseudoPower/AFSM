{-# LANGUAGE Arrows #-}

module Main where

import Control.AFSM

data Op = Add | Sub | Mul | Div | L | R
  deriving (Eq)
  
instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show L = "("
  show R = ")"

data Token = Num Int | Op Op | End 
  deriving (Eq)

instance Show Token where
  show (Num x) = show x
  show (Op op) = show op
  show End = ""


trans0 :: [Token] -> Token -> ([Token], [Token])
trans0 xs End = ([], xs)
trans0 xs (Num x) = (xs, [(Num x)])
trans0 xs op@(Op L) = (op:xs, [])
trans0 xs op@(Op R) = (tail $ dropWhile ((Op L) /= ) xs, takeWhile ((Op L) /= ) xs)
trans0 xs op = 
  (op:(dropWhile f0 xs), takeWhile f0 xs)
  where
    f0 = (\x -> x == (Op Mul) || x == (Op Div))
    
in2post = simpleSM [] trans0

-- 3 * (2 - 3) + (4 - 2 * 3)
test1 = [Num 3, Op Mul, Op L, Num 2, Op Sub, Num 3, Op R, Op Add, Op L, Num 4, Op Sub, Num 2, Op Mul, Num 3, Op R, End]

-- 3 + 4 * 2 / (1 - 5) * 2 + 3
test2 = [Num 3, Op Add, Num 4, Op Mul, Num 2, Op Div, Op L, Num 1, Op Sub, Num 5, Op R, Op Mul, Num 2, Op Add, Num 3, End]

out1 = concat $ snd $ exec in2post test1
out2 = concat $ snd $ exec in2post test2

f :: Op -> Int -> Int -> Int
f Add x y = x + y
f Sub x y = x - y
f Mul x y = x * y
f Div x y = quot x y

trans1 :: [Int] -> Token -> ([Int], Int)
trans1 xs End = if (null xs) then (xs, 0) else (tail xs, head xs)
trans1 xs (Num x) = (x:xs, x)
trans1 (x:y:xs) (Op o) = ((f o y x):xs, (f o y x))

post2ret = execSM $ simpleSM [] trans1

in2ret = proc x -> do
   y <- in2post -< x
   post2ret -< y

historySM :: SM a [a]
historySM = simpleSM [] (\xs a -> (a:xs, reverse (a:xs)))

lst2str:: Show a => SM [a] String
lst2str = arr (let f = \xs -> if (null xs) then "" else (show (head xs)) ++ f (tail xs) in f)

fooSM :: SM Token String
fooSM = proc x -> do
  h <- (historySM >>> lst2str) -< x
  r <- in2ret -< x
  returnA -< (h ++ if null r then "" else "=" ++ show (last r) )

getRet :: SM a b -> [a] -> [b]
getRet sm xs = snd $ exec sm xs