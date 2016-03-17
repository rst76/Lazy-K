module Main where

import System.Environment
import System.IO
import LazyK.Prim

main :: IO ()
main = do
  args <- getArgs
  program <- parseExpr <$> case args of
    "-e" : str : _ -> return str
    file : _ -> readFile file
  code <- foldr (consXY . num . fromEnum) eof <$> getContents
  run $ reduce $ program :$ code
