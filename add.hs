--intro.hs
module Main where
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ "hello " ++ (show $ (read $ args !! 0) + (read $ args !! 1))