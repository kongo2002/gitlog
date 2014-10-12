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


------------------------------------------------------------------------------
-- | Parse a list of @GitEntry@ of the given lazy bytestring
parseInput :: BL.ByteString -> [GitEntry]
parseInput ls =
  case AL.parse (logentry <* skipWhile iseol) ls of
    AL.Fail {}    -> []
    AL.Done ls' l -> l : parseInput ls'


------------------------------------------------------------------------------
-- | Parse one commit into a @GitEntry@
logentry :: Parser GitEntry
logentry = do
  _      <- char '|'
  sha    <- topipe
  author <- topipe
  date   <- topipe
  title  <- takeWhile1 $ not . iseol
  skipWhile iseol
  b      <- bodies
  return $ GitEntry sha author date title (titleTag title b)
 where
  topipe = takeWhile (/= '|') <* char '|'

  -- check if the title starts with a tag definition
  titleTag t b =
    case parseOnly tag t of
      Right tag' -> tag' : b
      Left _     -> b


------------------------------------------------------------------------------
-- | Parse commit message
bodies :: Parser [GitBody]
bodies =
  body `sepBy` takeWhile1 (inClass "\r\n\t, ")


------------------------------------------------------------------------------
-- | Parse the commit message body. One of @Intern@, @Tag@, @Line@
body :: Parser GitBody
body =
  skipWhite *> (intern <|> tag <|> line)
 where
  intern = string "INTERN" >> skipWhile (not . iseol) *> return Intern
  line   = do
    c  <- satisfy (/= '|')
    cs <- takeWhile (not . iseol)
    return $ Line (c `BS.cons` cs)


------------------------------------------------------------------------------
-- | Try to parse a tag definition of the form 'TAG-123'
tag :: Parser GitBody
tag = Tag <$> (takeWhile (inClass "A-Z") <* char '-') <*> decimal
          <*> return Nothing


------------------------------------------------------------------------------
-- | Skip whitespace
skipWhite :: Parser ()
skipWhite =
  skipWhile isWhite
 where
  isWhite c = c == ' ' || c == '\t'


------------------------------------------------------------------------------
-- | Match the end of line
iseol :: Char -> Bool
iseol c = c == '\n' || c == '\r'


-- vim: set et sw=2 sts=2 tw=80:
