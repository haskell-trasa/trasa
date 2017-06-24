{-# LANGUAGE LambdaCase #-}
module Trasa.TH.Parse where

import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import Data.Bifunctor (first)
import Language.Haskell.TH (Name,mkName)
import Control.Applicative ((<|>))
import Control.Monad (void)
import qualified Text.Megaparsec as MP

import Trasa.TH.Types
import Trasa.TH.Lexer

type Parser = MP.Parsec MP.Dec Stream

wrongToken :: a -> S.Set (MP.ErrorItem a)
wrongToken t = S.singleton (MP.Tokens (t NE.:| []))

space :: Parser ()
space = flip MP.token Nothing $ \case
  LexemeSpace _ -> Right ()
  other -> Left (wrongToken other,wrongToken (LexemeSpace 0),S.empty)

optionalSpace :: Parser ()
optionalSpace = void (MP.optional space)

string :: Parser String
string = flip MP.token Nothing $ \case
  LexemeString _ str -> Right str
  other -> Left (wrongToken other,wrongToken (LexemeString 0 ""),S.empty)

name :: Parser Name
name = fmap mkName string

match :: Lexeme -> Parser ()
match lexeme = flip MP.token Nothing $ \other -> case lexeme == other of
  True -> Right ()
  False -> Left (wrongToken other,wrongToken lexeme,S.empty)

matchChar :: ReservedChar -> Parser ()
matchChar = match . LexemeChar

newline :: Parser ()
newline = matchChar ReservedCharNewline

colon :: Parser ()
colon = matchChar ReservedCharColon

slash :: Parser ()
slash = matchChar ReservedCharSlash

questionMark :: Parser ()
questionMark = matchChar ReservedCharQuestionMark

ampersand :: Parser ()
ampersand = matchChar ReservedCharAmpersand

equal :: Parser ()
equal = matchChar ReservedCharEqual

bracket :: Parser a -> Parser a
bracket = MP.between (matchChar ReservedCharOpenBracket) (matchChar ReservedCharCloseBracket)

captureRep :: Parser (CaptureRep Name)
captureRep =
  fmap MatchRep string <|>
  fmap CaptureRep (colon *> name)

queries :: Parser [QueryRep Name]
queries = MP.sepBy (QueryRep <$> string <*> paramRep) ampersand
  where
    paramRep = MP.choice [ fmap OptionalRep optional, fmap ListRep list, pure FlagRep ]
    optional = MP.try (equal *> name)
    list = equal *> bracket name

-- bodiednessRep :: Parser (BodiednessRep Name)
-- bodiednessRep = bracket (MP.try (BodyRep <$> name) <|> pure BodylessRep)

routeRep :: Parser (RouteRep Name)
routeRep = do
  optionalSpace
  routeId <- string
  space
  method <- string
  space
  slash
  caps <- MP.sepBy captureRep slash
  qrys <- questionMark *> queries <|> return []
  space
  req  <- return []
  space
  res  <- undefined
  newline
  return (RouteRep routeId method caps qrys req res)

routesRep :: Parser (RoutesRep Name)
routesRep = do
  optionalSpace
  void (MP.optional newline)
  optionalSpace
  match (LexemeSymbol ReservedSymbolDataType)
  colon
  optionalSpace
  dataType <- string
  newline
  routes <- MP.many routeRep
  return (RoutesRep dataType routes)

parseRoutesRep :: String -> Either String (RoutesRep Name)
parseRoutesRep str = do
  tokens <- first MP.parseErrorPretty (MP.parse stream "" str)
  first MP.parseErrorPretty (MP.parse routesRep "" tokens)
