module Main where

import Control.Applicative
import System.Environment
import System.IO
import LazyK.Prim

main :: IO ()
main = do
  args <- getArgs
  program <- parseExpr <$> case args of
    "-e" : str : _ -> return str
    file : _ -> readFile file
  code <- foldr (consXY . num . fromEnum) endOfOutput <$> getContents
  run $ reduce $ program :$ code
