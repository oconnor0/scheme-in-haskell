--intro.hs
module Main where
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ "hello " ++ args !! 0 ++ " and " ++ args !! 1