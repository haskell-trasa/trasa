{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
module Trasa.TH.Lexer
  ( ReservedChar(..)
  , ReservedSymbol(..)
  , Lexeme(..)
  , Stream(..)
  , stream ) where

import Data.Semigroup ((<>))
import qualified Data.List.NonEmpty as NE
import qualified Data.List as L
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.String as MP

data ReservedChar
  = ReservedCharNewline
  | ReservedCharColon
  | ReservedCharSlash
  | ReservedCharQuestionMark
  | ReservedCharAmpersand
  | ReservedCharEqual
  | ReservedCharOpenBracket
  | ReservedCharCloseBracket
  | ReservedCharComma
  deriving (Show,Eq,Ord)

data ReservedSymbol
  = ReservedSymbolDataType
  deriving (Show,Eq,Ord)

data Lexeme
  = LexemeSpace Word
  | LexemeChar ReservedChar
  | LexemeSymbol ReservedSymbol
  | LexemeString Word String
  deriving (Show,Eq,Ord)

instance MP.ShowToken Lexeme where
  showTokens = L.concatMap prettyToken . NE.toList
    where
      prettyToken = \case
        LexemeSpace _ -> "space(s)"
        LexemeChar resChar -> case resChar of
          ReservedCharNewline -> "newline"
          ReservedCharColon -> ":"
          ReservedCharSlash -> "/"
          ReservedCharQuestionMark -> "?"
          ReservedCharAmpersand -> "&"
          ReservedCharEqual -> "="
          ReservedCharOpenBracket -> "["
          ReservedCharCloseBracket -> "]"
          ReservedCharComma -> ","
        LexemeSymbol sym -> case sym of
          ReservedSymbolDataType -> "data-type"
        LexemeString _ _ -> "any string"

newtype Stream = Stream [Lexeme] deriving Show

instance MP.Stream Stream where
  type Token Stream = Lexeme
  uncons (Stream s) = fmap Stream <$> L.uncons s
  updatePos _ _ apos@(MP.SourcePos f l c) token = (apos,npos)
    where
      one = MP.unsafePos 1
      sourcePos = MP.SourcePos f
      npos = case token of
        LexemeSpace spaces -> sourcePos l (c <> MP.unsafePos spaces)
        LexemeChar _ -> sourcePos l (c <> one)
        LexemeSymbol sym -> case sym of
          ReservedSymbolDataType -> sourcePos l (c <> MP.unsafePos 9)
        LexemeString len _ -> sourcePos l (c <> MP.unsafePos len)

lexeme :: MP.Parser Lexeme
lexeme = MP.choice
  [ LexemeSpace . fromIntegral . length <$> MP.some (MP.char ' ')
  , LexemeChar <$> MP.choice
    [ ReservedCharNewline <$ MP.char '\n'
    , ReservedCharColon <$ MP.char ':'
    , ReservedCharSlash <$ MP.char '/'
    , ReservedCharQuestionMark <$ MP.char '?'
    , ReservedCharAmpersand <$ MP.char '&'
    , ReservedCharEqual <$ MP.char '='
    , ReservedCharOpenBracket <$ MP.char '['
    , ReservedCharCloseBracket <$ MP.char ']'
    , ReservedCharComma <$ MP.char ','
    ]
  , LexemeSymbol <$> MP.choice
    [ ReservedSymbolDataType <$ MP.string "data-type"
    ]
  , string <$> MP.some (MP.noneOf " \n:/?&=[],")
  ]
  where string str = LexemeString (fromIntegral (length str)) str

stream :: MP.Parser Stream
stream = Stream <$> MP.many lexeme
