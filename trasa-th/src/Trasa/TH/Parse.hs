-- module Trasa.TH.Parse (parseRouteReps) where
module Trasa.TH.Parse where

import Prelude hiding (lex)
import Language.Haskell.TH (Name,mkName)
import Control.Applicative (Alternative(..))
import Control.Monad (void)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.String as MP
import qualified Text.Megaparsec.Lexer as MPL

import Trasa.TH.Types

spaceConsumer :: MP.Parser ()
spaceConsumer = MPL.space (void MP.spaceChar) (MPL.skipLineComment "#") (MPL.skipBlockComment "##" "##")

lex :: MP.Parser a -> MP.Parser a
lex = MPL.lexeme spaceConsumer

string :: MP.Parser String
string = MP.manyTill MP.anyChar MP.spaceChar

name :: MP.Parser Name
name = mkName <$> string

captureRep :: MP.Parser (CaptureRep Name)
captureRep =
  CaptureRep <$> (MP.char ':' *> (mkName <$> MP.someTill MP.anyChar (MP.oneOf " /"))) <|>
  MatchRep <$> MP.someTill MP.anyChar (MP.oneOf  " /")

routeName :: MP.Parser String
routeName = lex $ do
  void (MP.string' "data-type:")
  MP.space
  MP.manyTill MP.anyChar MP.newline

routeRep :: MP.Parser (RouteRep Name)
routeRep = do
  rName <- string
  MP.space
  method <- string
  MP.space
  void (MP.char '/')
  caps <- MP.manyTill captureRep MP.spaceChar
  qrys <- return []
  req  <- return BodylessRep
  res  <- mkName <$> MP.someTill MP.anyChar MP.newline
  pure (RouteRep rName method caps qrys req res)

parseRouteReps :: MP.Parser (RoutesRep Name)
parseRouteReps = RoutesRep
  <$> (spaceConsumer *> routeName)
  <*> many routeRep
