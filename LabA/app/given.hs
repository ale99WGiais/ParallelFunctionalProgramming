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


main = do
  let numbers = map (`mod` 1000) $ take 1000000 (randoms (mkStdGen 211570155)) :: [Integer]
  print $ parsort 100 $ take 100 numbers

  --numbers2 are all 1 <= x <= 1000000001 (collatz holds on positive integers)
  let numbers2 = map ((+1) . (`mod` 1000000000)) $ take 1000000 (randoms (mkStdGen 211570155)) :: [Int]    
  print $ parensurecollatz 100 numbers2

  let (xs,ys) = splitAt 1500  (take 6000
                               (randoms (mkStdGen 211570155)) :: [Float] )
  -- handy (later) to give same input different parallel functions

  let rs = crud xs ++ ys
  putStrLn $ "sample mean:    " ++ show (mean rs)

  let j = jackknife_epmap 1 mean rs :: [Float]
  putStrLn $ "jack mean min:  " ++ show (minimum j)
  putStrLn $ "jack mean max:  " ++ show (maximum j)
  defaultMain
        [
          bench "jackknife" (nf (jackknife mean) rs),
          bench "jackknife_pmap" (nf (jackknife_pmap 1 mean) rs),
          bench "jackknife_pmap5" (nf (jackknife_pmap 5 mean) rs),
          bench "jackknife_pmap10" (nf (jackknife_pmap 10  mean) rs),
          bench "jackknife_pmap20" (nf (jackknife_pmap 20 mean) rs),
          bench "jackknife_pmap40" (nf (jackknife_pmap 40 mean) rs)
          --bench "jackknife_epmap" (nf (jackknife_epmap  mean) rs)
          --bench "jackknife_strategy" (nf (jackknife_strategy  mean) rs),
          --bench "sort" (nf sort numbers),
          --bench "parsort10" (nf (parsort 10) numbers),
          --bench "parsort100" (nf (parsort 100) numbers),
          --bench "parsort1000" (nf (parsort 1000) numbers),
          --bench "parsort10000" (nf (parsort 10000) numbers),
          --bench "ensurecollatz" (nf ensurecollatz numbers2),
          --bench "parensurecollatz10" (nf (parensurecollatz 10) numbers2),
          --bench "parensurecollatz100" (nf (parensurecollatz 100) numbers2),
          --bench "parensurecollatz1000" (nf (parensurecollatz 1000) numbers2),
          --bench "parensurecollatz10000" (nf (parensurecollatz 10000) numbers2)
        ]




----------------------------------------------------------------
-------- Lab A Assignment 1 Part 1 (jacknife) --------
----------------------------------------------------------------


jackknife :: ([a] -> b) -> [a] -> [b]
jackknife f = map f . resamples 500

jackknife_pmap :: NFData b => Int -> ([a] -> b) -> [a] -> [b]
jackknife_pmap 1 f = pMap f . resamples 500                           --no granularity control 
jackknife_pmap thresh f = (map_chunked thresh pMap) f . resamples 500     --granularity control

pMap :: NFData b => (a -> b) -> [a] -> [b]
pMap _ [] = []
pMap f (x:xs) = x' `par` xs' `pseq` x' : xs'
  where
    x'  = force $ f x
    xs' = pMap f xs

map_chunked _ _ _ [] = []
map_chunked chuckSize mapper f xs = foldr (++) [] l
  where
    l = mapper (map f) (chunksOf chuckSize xs)

  
jackknife_epmap :: NFData b => Int -> ([a] -> b) -> [a] -> [b]
jackknife_epmap 1 f = epMap f . resamples 500                           --no granularity control 
jackknife_epmap thresh f = (map_chunked thresh epMap) f . resamples 500     --granularity control




jackknife_strategy :: NFData b => ([a] -> b) -> [a] -> [b]
jackknife_strategy f l = (map f . resamples 500 ) l `using` parListChunk 20 rdeepseq
 


parMap :: NFData b => (a -> b) -> [a] -> Par [b]
parMap _ [] = return []
parMap f (x:xs) = do
            leftVar <- spawn $ parMap f xs 
            left <- get leftVar
            return $ (f x):left


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

crud = zipWith (\x a -> sin (x / 300)**2 + a) [0..]

chunksOf :: Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)


----------------------------------------------------------------
-------- Lab A Assignment 2 Part 1 (par merge sort) --------
----------------------------------------------------------------

-- divide and conquer framework
divConq :: NFData sol         
  => (prob -> Bool)        -- test if indivisible     
  -> (prob -> (prob,prob)) -- split into subproblems         
  -> (sol -> sol -> sol)   -- join solutions         
  -> (prob -> sol)                 
  -> (prob -> sol) 
divConq indiv split join f prob = runPar $ solve prob 
  where    
    solve prob -- solve a subproblem      
      | indiv prob = return (f prob)      
      | otherwise = do          
        let (a,b) = split prob          
        i <- spawn $ solve a          
        j <- spawn $ solve b          
        a <- get i          
        b <- get j          
        return (join a b) 

--parallelize merge sort using divide and conquer with threshold on min list length
--haskell's sort function used on irreducible subproblems
--merge used as aggregating function
parsort :: (Ord a, NFData a) => Int -> [a] -> [a] 
parsort thresh xs    = divConq indiv divide merge sort xs
  where     
    -- "merge" merges two ordered lists
    merge :: (Ord a) => [a] -> [a] -> [a]  
    merge [] ys = ys
    merge xs [] = xs
    merge (x:xs) (y:ys) = (elem) : (merge xs' ys') where
      elem = min x y
      xs' = if x <= y then xs else (x:xs)  --if heads are equal, take the elem from first list
      ys' = if x > y then ys else (y:ys)

    indiv xs = (length xs) <= thresh
    divide xs = (as, bs) 
      where 
        n = (length xs) `div` 2
        as = take n xs 
        bs = drop n xs
      

----------------------------------------------------------------
-------- Lab A Assignment 2 Part 2 (collatz conjecture) --------
----------------------------------------------------------------

--collatz conjecture
collatz :: Int -> Int
collatz x 
  | x <= 0 = 0   --wrong result
  | x == 1 = 1   --expected result
  | otherwise = if (x `mod` 2 == 0) then collatz (x `div` 2) else collatz (3 * x + 1)  --recursion

--ensure that collatz conjecture holds for each starting value in l 
--(if conjecture does not hold, then either the function never terminates or sum < lenght l)
ensurecollatz :: [Int] -> Bool
ensurecollatz l = (length l) == (sum $ map collatz l)

--parallelize using divide and conquer with threshold on min list length
--conjecture holds if holds on both subproblems (&&)
--split list in half
parensurecollatz :: Int -> [Int] -> Bool
parensurecollatz thresh = divConq indiv divide (&&) ensurecollatz
  where     
    indiv xs = (length xs) <= thresh
    divide xs = (as, bs) 
      where 
        n = (length xs) `div` 2
        as = take n xs 
        bs = drop n xs

