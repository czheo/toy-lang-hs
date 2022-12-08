module Main where
import System.Environment
import System.IO
import Control.Monad

main :: IO ()
main = do
  args <- getArgs
  case args of
    []     -> repl
    path:_ -> interpret path

prompt :: String -> IO String
prompt s = do
  putStr s
  hFlush stdout
  getLine

repl :: IO ()
repl = do
  line <- prompt "toy> "
  -- TODO: eval line
  putStrLn line
  unless (line == ":q") repl

interpret :: String -> IO ()
interpret path = do
  handle <- openFile path ReadMode
  contents <- hGetContents handle
  -- TODO: eval file
  putStr contents
  hClose handle
