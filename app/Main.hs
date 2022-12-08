module Main where
import System.Environment
import System.IO
import Control.Monad
import qualified Parser

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
  when (line /= ":q")
    (do
       eval line
       repl)

eval :: String -> IO ()
eval text = case Parser.parse text of
              Just exp -> print exp
              Nothing -> print "Unknown expression"

interpret :: String -> IO ()
interpret path = do
  handle <- openFile path ReadMode
  contents <- hGetContents handle
  -- TODO: eval file
  putStr contents
  hClose handle
