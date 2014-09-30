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
  case AL.parse (logentry <* skipWhile iseol) ls of
    AL.Fail {}    -> []
    AL.Done ls' l -> l : parseInput ls'


logentry :: Parser GitEntry
logentry = do
  _      <- char '|'
  sha    <- topipe
  author <- topipe
  date   <- topipe
  title  <- takeWhile1 $ not . iseol
  skipWhile iseol
  b      <- bodies
  return $ GitEntry sha author date title b
 where
  topipe = takeWhile (/= '|') <* char '|'


bodies :: Parser [GitBody]
bodies =
  body `sepBy` takeWhile1 (inClass "\r\n\t, ")


body :: Parser GitBody
body =
  skipWhite *> (intern <|> tag <|> line)
 where
  intern = string "INTERN" >> skipWhile (not . iseol) *> return Intern
  tag    = Tag <$> (takeWhile (inClass "A-Z") <* char '-') <*> decimal <*> return Nothing
  line   = do
    c  <- satisfy (/= '|')
    cs <- takeWhile (not . iseol)
    return $ Line (c `BS.cons` cs)


skipWhite :: Parser ()
skipWhite =
  skipWhile isWhite
 where
  isWhite c = c == ' ' || c == '\t'


iseol :: Char -> Bool
iseol c = c == '\n' || c == '\r'


-- vim: set et sw=2 sts=2 tw=80:
