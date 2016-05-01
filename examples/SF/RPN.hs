-----------------------------------------------------------------------------
-- Copyright   :  (c) Hanzhong Xu, Meng Meng 2016,
-- License     :  MIT License
--
-- Maintainer  :  hanzh.xu@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
-- A simple calculator
-----------------------------------------------------------------------------

-- {-# LANGUAGE Arrows #-}

module Main where

import Control.Arrow
import Control.Monad

import Data.SF
import Data.SF.Draft.IOSF
import Data.Maybe

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Data.IORef

-- import Data.Map (fromList, (!))


data Op = Add | Sub | Mul | Div
  deriving (Eq)

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"


data Token = Num Int | Op Op | L | R | End
  deriving (Eq)

instance Show Token where
  show (Num x) = show x
  show (Op op) = show op
  show L = "("
  show R = ")"
  show End = ""

-- 3 * (2 - 3) + (4 - 2 * 3)
test1 = [Num 3, Op Mul, L, Num 2, Op Sub, Num 3, R, Op Add, L, Num 4, Op Sub, Num 2, Op Mul, Num 3, R, End]

-- 3 + 4 * 2 / (1 - 5) * 2 + 3
test2 = [Num 3, Op Add, Num 4, Op Mul, Num 2, Op Div, L, Num 1, Op Sub, Num 5, R, Op Mul, Num 2, Op Add, Num 3, End]


-- State machines

trans0 :: [Token] -> Token -> ([Token], [Token])
trans0 xs End = ([End], xs)
trans0 xs (Num x) = (xs, [(Num x)])
trans0 xs L = (L:xs, [])
trans0 xs R = let (x0, x1) = span (L /= ) xs in (tail $ x1, x0)
trans0 xs op =
  (op:x1, x0)
  where
    f0 = (\x -> x == (Op Mul) || x == (Op Div))
    (x0, x1) = span f0 xs


-- the SF converting infix to postfix
--
--   Token /---------\ [Token]
--  >----->| in2post |>------->
--         \---------/
--
in2post :: SF Token [Token]
in2post = simpleSF trans0 [End]

post1 = concat $ getRet in2post test1
post2 = concat $ getRet in2post test2

f :: Op -> Int -> Int -> Int
f Add x y = x + y
f Sub x y = x - y
f Mul x y = x * y
f Div x y = quot x y

trans1 :: [Int] -> Token -> ([Int], Maybe Int)
trans1 xs End = if (null xs) then (xs, Just 0) else ([], Just $ head xs)
trans1 xs (Num x) = (x:xs, Nothing)
trans1 (x:y:xs) (Op o) = ((f o y x):xs, Nothing)

-- the SF evaluating postfix expression
--
--   Token /-----------\ Maybe Int
--  >----->| post2ret' |>--------->
--         \-----------/
--
post2ret' :: SF Token (Maybe Int)
post2ret' = simpleSF trans1 []


--
--   [Token] /----------\ [Maybe Int]
--  >------->| post2ret |>----------->
--           \----------/
--
post2ret :: SF [Token] [Maybe Int]
post2ret = execSF post2ret'


-- an example to use Arrow notation
--   the SF composed of in2post and post2ret
--
--         /----------------------------\
--   Token |          [Token]           | [Maybe Int]
--  >----->| in2post >-------> post2ret |>----------->
--         |                            |
--         \----------------------------/
--                     in2ret
--
in2ret :: SF Token [Maybe Int]
in2ret = in2post >>> post2ret
{-
hg = hideStorage

in2ret :: SFH Token [Maybe Int]
in2ret = hg $ proc x -> do
   y <- (hg in2post) -< x
   (hg post2ret) -< y
-}

-- Parsing and evaluating

getRet :: SF a b -> [a] -> [b]
getRet sf xs = snd $ sfexec sf xs

calc :: [Token] -> [Int]
calc xs = catMaybes $ concat $ getRet in2ret xs

isNum :: Char -> Bool
isNum x = elem x "0123456789"

-- parseOp x = (fromList $ zip "()+-*/" [L, R, Op Add, Op Sub, Op Mul, Op Div])!x
parseOp :: Char -> Token
parseOp '(' = L
parseOp ')' = R
parseOp '+' = Op Add
parseOp '-' = Op Sub
parseOp '*' = Op Mul
parseOp '/' = Op Div

parseStr :: String -> [Token]
parseStr [] = [End]
parseStr (x:xs) =
  if elem x ",\n" then End : (parseStr xs)
  else if x == ' ' then parseStr xs
  else if isNum x then
    let (ys, zs) = span isNum xs in (Num $ read (x:ys)):(parseStr zs)
  else if elem x "()+-*/" then
    (parseOp x):(parseStr xs)
  else
    parseStr xs

{-
main = do
  getContents >>= (mapM_ putStrLn).(map show).(calc.parseStr)
-}

main = do
  s <- getContents
  tc <- newBroadcastTChanIO
  tf <- newThreadSF in2ret
  ret <- tf tc
  forM_ (parseStr s) (\t -> do (putStrLn.show) t; atomically $ writeTChan tc t; tryOutputTChan ret (putStrLn.show))
  
-- input samples
-- 3 * (2 - 3) + (4 - 2 * 3), 3 + 4 * 2 / (1 - 5) * 2 + 3
