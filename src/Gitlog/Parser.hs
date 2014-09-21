{-# LANGUAGE OverloadedStrings #-}

module Gitlog.Parser
  ( parseInput
  ) where

import           Prelude hiding ( takeWhile )

import           Control.Applicative
import qualified Data.Attoparsec.ByteString.Lazy as AL
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL

import           Gitlog.Types


parseInput :: BL.ByteString -> [GitEntry]
parseInput ls =
  case AL.parse logentry ls of
    AL.Fail {}    -> []
    AL.Done ls' l -> l : parseInput ls'


logentry :: Parser GitEntry
logentry = do
  _ <- char '|'
  sha <- takeWhile (/= '|')
  _ <- char '|'
  author <- takeWhile (/= '|')
  _ <- char '|'
  date <- takeWhile (/= '|')
  _ <- char '|'
  title <- takeWhile1 $ not . iseof
  skipWhile iseof
  b <- sepBy body (takeWhile1 iseof)
  return $ GitEntry sha author date title b


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


-- vim: set et sw=2 sts=2 tw=80:
