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
      solutions = runEval (parMapBuffer 100 solve puzzles)

  evaluate (length puzzles)
  print (length (filter isJust solutions))
-- >>

parMap :: (a -> b) -> [a] -> Eval [b]
parMap f [] = return []
parMap f (a:as) = do
   b <- rpar (f a)
   bs <- parMap f as
   return (b:bs)

parMapBuffer :: Int -> (a -> b) -> [a] -> Eval [b]
parMapBuffer _ _ [] = return []
parMapBuffer n f xs = withStrategy (parBuffer n rseq) $ map f xs
