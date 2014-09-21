{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Prelude hiding     ( takeWhile )
import           Control.Applicative

import qualified Data.Attoparsec.ByteString.Lazy as AL
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL

import           System.IO          ( hPutStrLn, stderr )
import           System.Process     ( runInteractiveProcess, waitForProcess )
import           System.Exit        ( ExitCode(..), exitWith )
import           System.Environment ( getArgs )

import           Gitlog.Encoder
import           Gitlog.Types


parseInput :: BL.ByteString -> [GitEntry]
parseInput ls =
  case AL.parse logentry ls of
    AL.Fail {}    -> []
    AL.Done ls' l -> l : parseInput ls'


logentries :: Parser [GitEntry]
logentries =
  many logentry


logentry :: Parser GitEntry
logentry = do
  _ <- char '|'
  sha <- takeWhile (/= '@')
  _ <- char '@'
  title <- takeWhile1 $ not . iseof
  skipWhile iseof
  b <- sepBy body (takeWhile1 iseof)
  return $ GitEntry sha title b


body :: Parser GitBody
body =
  skipWhite *> (intern <|> tag <|> line)
 where
  intern = string "INTERN" >> skipWhile (not . iseof) *> return Intern
  tag    = Tag <$> (takeWhile (inClass "A-Z") <* char '-') <*> decimal
  line   = do
    c  <- satisfy (/= '|')
    cs <- takeWhile (not . iseof)
    return $ Line (c `BS.cons` cs)


skipWhite :: Parser ()
skipWhite =
  skipWhile isWhite
 where
  isWhite c = c == ' ' || c == '\t'


iseof :: Char -> Bool
iseof c = c == '\n' || c == '\r'


getGitOutput :: FilePath -> [String] -> IO (BL.ByteString, ExitCode)
getGitOutput dir args = do
  (_in, out, _err, handle) <- runInteractiveProcess "git" args path Nothing
  output <- toHtml . parseInput <$> BL.hGetContents out
  ec <- waitForProcess handle
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


exit :: String -> IO ()
exit msg =
  hPutStrLn stderr msg >> exitWith (ExitFailure 1)


main :: IO ()
main = do
  (args, dir) <- parseArgs <$> getArgs
  (out, ec)   <- getGitOutput dir (log' args)
  case ec of
    ExitSuccess -> BL.putStr out
    _           -> exit "failed to retrieve git log"
 where
  log' a = "log" : "--pretty=format:|%h@%s%n%b" : "--no-merges" : a


-- vim: set et sw=2 sts=2 tw=80:
