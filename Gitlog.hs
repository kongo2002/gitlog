module Gitlog where

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


main :: IO ()
main = do
  [dir, from, to] <- getArgs
  let range = from ++ ".." ++ to
  (out, _ec) <- getGitOutput dir ["log", range]

  putStr out
