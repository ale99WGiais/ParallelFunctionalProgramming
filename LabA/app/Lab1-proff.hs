module Main where

import Control.Parallel
import Control.DeepSeq

main = print $ runEval $ epMap (map fib) $ chunksOf 3 $ replicate 12 35

mSwap :: (a,b) -> (b,a)
mSwap (a,b) = (b,a)

mLength :: [a] -> Int
mLength []     = 0
mLength (_:xs) = 1 + mLength xs

mSum :: Num a => [a] -> a
mSum []     = 0
mSum (x:xs) = x + mSum xs

mTake :: Int -> [a] -> [a]
mTake 0 _      = []
mTake n (x:xs) = x : mTake (n-1) xs

--

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

pMap :: NFData b => (a -> b) -> [a] -> [b]
pMap _ [] = []
pMap f (x:xs) = x' `par` xs' `pseq` x' : xs'
  where
    x'  = force $ f x
    xs' = pMap f xs

chunksOf :: Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

badfib :: Int -> Int
badfib 0 = 0
badfib 1 = 1
badfib n = if xs == reverse xs
    then badfib (n - 1) + badfib (n - 2)
    else badfib (n - 1) + badfib (n - 2)
  where xs = [1..1000000]

--

data Eval a = Done a

runEval :: Eval a -> a
runEval (Done a) = a

instance Functor Eval where
    fmap f (Done a) = Done (f a)

instance Applicative Eval where
    pure x = Done x
    Done f <*> Done x = Done (f x)

instance Monad Eval where
    Done x >>= k = k x

rpar :: a -> Eval a
rpar a = a `par` Done a

rseq :: a -> Eval a
rseq a = a `pseq` Done a

epMap :: NFData b => (a -> b) -> [a] -> Eval [b]
epMap _ [] = return []
epMap f (x:xs) = do
    x'  <- rpar $ force (f x)
    xs' <- epMap f xs
    return (x' : xs')