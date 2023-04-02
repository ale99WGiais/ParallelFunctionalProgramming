import Data.List
import System.Random
import Criterion.Main

import Control.Parallel
import Control.DeepSeq

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
jackknife_epmap f l = runEval $ (epMap f . resamples 500) l
 
pMap :: NFData b => (a -> b) -> [a] -> [b]
pMap _ [] = []
pMap f (x:xs) = x' `par` xs' `pseq` x' : xs'
  where
    x'  = force $ f x
    xs' = pMap f xs

parMap :: NFData b => (a -> b) -> [a] -> [b]
parMap _ [] = []
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

epMap :: NFData b => (a -> b) -> [a] -> Eval [b]
epMap _ [] = return []
epMap f (x:xs) = do
    x'  <- rpar $ force (f x)
    xs' <- epMap f xs
    return (x' : xs')


crud = zipWith (\x a -> sin (x / 300)**2 + a) [0..]

chunksOf :: Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

main = do
  let (xs,ys) = splitAt 1500  (take 6000
                               (randoms (mkStdGen 211570155)) :: [Float] )
  -- handy (later) to give same input different parallel functions

  let rs = crud xs ++ ys
  putStrLn $ "sample mean:    " ++ show (mean rs)

  let j = jackknife_pmap mean rs :: [Float]
  putStrLn $ "jack mean min:  " ++ show (minimum j)
  putStrLn $ "jack mean max:  " ++ show (maximum j)
  defaultMain
        [
         bench "jackknife" (nf (jackknife_pmap  mean) rs)
         ]
