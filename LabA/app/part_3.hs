import Sudoku
import Control.Exception
import System.Environment
import Control.Parallel.Strategies hiding (parMap)
import Data.Maybe

-- <<main
main :: IO ()
main = do
  [f] <- getArgs
  file <- readFile f

  let puzzles   = lines file
      --solutions = map solve puzzles
      --solutions = runEval (parMap solve puzzles)
      solutions = parMapBuffer 100 solve puzzles

  evaluate (length puzzles)
  print (length (filter isJust solutions))
-- >>

parMap :: (a -> b) -> [a] -> Eval [b]
parMap _ [] = return []
parMap f (a:as) = do
   b <- rpar (f a)
   bs <- parMap f as
   return (b:bs)

parMapBuffer :: NFData b => Int -> (a -> b) -> [a] -> [b]
parMapBuffer _ _ [] = []
--parMapBuffer n f xs = withStrategy (parBuffer n rseq) $ map f xs
--parMapBuffer n f xs = withStrategy (parBuffer n rseq) $ (map f xs `using` parListChunk 10 rdeepseq)
parMapBuffer n f xs = map f xs `using` parListChunk n rdeepseq
--parMapBuffer n f xs = map f xs `using` parList rdeepseq
