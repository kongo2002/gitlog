module Gitlog where

import Control.Applicative

import System.IO          ( hGetContents )
import System.Process     ( runInteractiveProcess
                          , waitForProcess )
import System.Exit        ( ExitCode(..) )
import System.Environment ( getArgs )


getGitOutput :: FilePath -> [String] -> IO (String, ExitCode)
getGitOutput dir args = do
  (_in, out, _err, handle) <- runInteractiveProcess "git" args path Nothing
  ec <- waitForProcess handle
  output <- hGetContents out
  return (output, ec)
 where
  path = Just dir


range :: String -> String -> [String]
range f t = [f ++ ".." ++ t]


parseArgs :: [String] -> ([String], FilePath)
parseArgs [from, to, dir] = (range from to, dir)
parseArgs [from, to]      = (range from to, ".")
parseArgs [from]          = (range from "HEAD", ".")
parseArgs _               = ([], ".")


main :: IO ()
main = do
  (args, dir) <- parseArgs <$> getArgs
  (out, _ec) <- getGitOutput dir ("log" : args)
  putStr out
