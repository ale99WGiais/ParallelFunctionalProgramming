import Data.List
import System.Random
import Criterion.Main

import Control.Parallel
import Control.DeepSeq
import Control.Monad
import Control.Parallel.Strategies (using, parListChunk, rdeepseq)
import Control.Monad.Par (get, spawn, runPar, Par)

-- code borrowed from the Stanford Course 240h (Functional Systems in Haskell)
-- I suspect it comes from Bryan O'Sullivan, author of Criterion

data T a = T !a !Int


mean :: (RealFrac a) => [a] -> a
mean = fini . foldl' go (T 0 0)
  where
    fini (T a _) = a
    go (T m n) x = T m' n'
      where m' = m + (x - m) / fromIntegral n'
            n' = n + 1


resamples :: Int -> [a] -> [[a]]
resamples k xs =
    take (length xs - k) $
    zipWith (++) (inits xs) (map (drop k) (tails xs))


jackknife :: ([a] -> b) -> [a] -> [b]
jackknife f = map f . resamples 500

jackknife_pmap :: NFData b => ([a] -> b) -> [a] -> [b]
--jackknife_pmap f = pMap f . resamples 500
jackknife_pmap f = (map_chunked 20 pMap) f . resamples 500

jackknife_epmap :: NFData b => ([a] -> b) -> [a] -> [b]
jackknife_epmap f l = (epMap f . resamples 500) l

jackknife_strategy :: NFData b => ([a] -> b) -> [a] -> [b]
jackknife_strategy f l = (map f . resamples 500 ) l `using` parListChunk 20 rdeepseq
 
pMap :: NFData b => (a -> b) -> [a] -> [b]
pMap _ [] = []
pMap f (x:xs) = x' `par` xs' `pseq` x' : xs'
  where
    x'  = force $ f x
    xs' = pMap f xs

parMap :: NFData b => (a -> b) -> [a] -> Par [b]
parMap _ [] = return []
parMap f (x:xs) = do
            leftVar <- spawn $ parMap f xs 
            left <- get leftVar
            return $ (f x):left

map_chunked _ _ _ [] = []
map_chunked chuckSize mapper f xs = foldr (++) [] l
  where
    l = mapper (map f) (chunksOf chuckSize xs)

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

epMap :: NFData b => (a -> b) -> [a] -> [b]
epMap _ [] = runEval $ return []
epMap f (x:xs) = runEval $ do
    x'  <- rpar $ force (f x)
    xs' <- rseq $ epMap f xs
    return (x' : xs')

parMap2 :: NFData b => (a -> b) -> a -> [b]
parMap2 _ [] = runPar $ return []
parMap2 f (x:xs) = runPar $ do
    i <- spawn $ f x
    j <- spawn $ parMap2 f xs
    a <- get i
    b <- get j          
    return (a : b)

crud = zipWith (\x a -> sin (x / 300)**2 + a) [0..]

chunksOf :: Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

main = do
  let numbers = map (`mod` 1000) $ take 1000000 (randoms (mkStdGen 211570155)) :: [Integer]
  print $ parsort 100 $ take 100 numbers

  let numbers2 = map (`mod` 1000) $ take 10000000 (randoms (mkStdGen 211570155)) :: [Integer]
  print $ parcountoccurrencies 100 (head numbers) numbers2

  let (xs,ys) = splitAt 1500  (take 6000
                               (randoms (mkStdGen 211570155)) :: [Float] )
  -- handy (later) to give same input different parallel functions

  let rs = crud xs ++ ys
  putStrLn $ "sample mean:    " ++ show (mean rs)

  let j = jackknife_epmap mean rs :: [Float]
  putStrLn $ "jack mean min:  " ++ show (minimum j)
  putStrLn $ "jack mean max:  " ++ show (maximum j)
  defaultMain
        [
          --bench "jackknife" (nf (jackknife  mean) rs),
          --bench "jackknife_pmap" (nf (jackknife_pmap  mean) rs),
          --bench "jackknife_epmap" (nf (jackknife_epmap  mean) rs),
          --bench "jackknife_strategy" (nf (jackknife_strategy  mean) rs),
          --bench "sort" (nf sort numbers),
          --bench "parsort" (nf (parsort 1000) numbers),
          bench "countoccurrencies" (nf (countoccurrencies 0) numbers2),
          bench "parcountoccurrencies" (nf (parcountoccurrencies 1000000 0) numbers2)
        ]



parsort :: Int -> [Integer] -> [Integer] 
parsort thresh xs    = divConq indiv divide merge sort xs
  where     
    indiv xs = (length xs) <= thresh
    divide xs = (as, bs) 
      where 
        n = (length xs) `div` 2
        as = take n xs 
        bs = drop n xs

divConq :: NFData sol         
  => (prob -> Bool)        -- indivisible?         
  -> (prob -> (prob,prob)) -- split into subproblems         
  -> (sol -> sol -> sol)   -- join solutions         
  -> (prob -> sol)                 
  -> (prob -> sol) 
divConq indiv split join f prob  
  = runPar $ go prob 
  where    
    go prob -- solve a subproblem      
      | indiv prob = return (f prob)      
      | otherwise = do          
        let (a,b) = split prob          
        i <- spawn $ go a          
        j <- spawn $ go b          
        a <- get i          
        b <- get j          
        return (join a b) 

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = (elem) : (merge xs' ys') where
  elem = min x y
  xs' = if x <= y then xs else (x:xs)
  ys' = if x > y then ys else (y:ys)



countoccurrencies :: (Eq a) => a -> [a] -> Int
countoccurrencies e = sum . map (\x -> if x == e then 1 else 0)

parcountoccurrencies :: (Eq a) => Int -> a -> [a] -> Int
parcountoccurrencies thresh e = divConq indiv divide (+) (countoccurrencies e)
  where     
    indiv xs = (length xs) <= thresh
    divide xs = (as, bs) 
      where 
        n = (length xs) `div` 2
        as = take n xs 
        bs = drop n xs

