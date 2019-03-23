{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Trasa.TH.Lexer
  ( ReservedChar(..)
  , ReservedSymbol(..)
  , Lexeme(..)
  , Stream(..)
  , stream ) where

import Data.Proxy (Proxy(..))
import Data.Semigroup ((<>))
import Data.Void (Void)
import Data.Coerce
import qualified Data.List.NonEmpty as NE
import qualified Data.List as L
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Stream as MP

type Parser a = MP.Parsec Void String a

instance Ord a => MP.ShowErrorComponent (MP.ErrorFancy a) where
  showErrorComponent (MP.ErrorFail e) = e
  showErrorComponent (MP.ErrorCustom _) = "undefined error"

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
  | ReservedCharTab
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

newtype Stream = Stream [Lexeme] 
  deriving (Eq, Ord, Show, Monoid, Semigroup)

instance MP.Stream Stream where
  type Token Stream = Lexeme
  type Tokens Stream = [Lexeme]
  tokenToChunk Proxy = pure
  tokensToChunk Proxy = id
  chunkToTokens Proxy = id
  chunkLength Proxy = length
  chunkEmpty Proxy = null
  take1_ (Stream []) = Nothing
  take1_ (Stream (t:ts)) = Just (t, Stream ts)
  takeN_ n (Stream s)
    | n <= 0    = Just ([], Stream s)
    | null s    = Nothing
    | otherwise = Just $ coerce (splitAt n s)
  takeWhile_ = coerce . span
  -- NOTE Do not eta-reduce these (breaks inlining)
  reachOffset o pst = reachOffset' (\n (Stream l) -> coerce $ splitAt n l) L.foldl' (fmap prettyCharToken . coerce) (prettyCharToken . coerce) (coerce $ LexemeChar ReservedCharNewline, coerce $ LexemeChar ReservedCharTab) o (coerce pst)
  showTokens _ = L.concatMap prettyToken . NE.toList

prettyCharToken = \case
  LexemeSpace _ -> ' '
  LexemeChar resChar -> case resChar of
    ReservedCharNewline -> '\n'
    ReservedCharColon -> ':'
    ReservedCharSlash -> '/'
    ReservedCharQuestionMark -> '?'
    ReservedCharAmpersand -> '&'
    ReservedCharEqual -> '='
    ReservedCharOpenBracket -> '['
    ReservedCharCloseBracket -> ']'
    ReservedCharComma -> ','
    ReservedCharTab -> '\t'
  LexemeSymbol sym -> case sym of
    ReservedSymbolDataType -> 'd'
  LexemeString _ _ -> 's'

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
    ReservedCharTab -> "tab"
  LexemeSymbol sym -> case sym of
    ReservedSymbolDataType -> "data-type"
  LexemeString _ _ -> "any string"

lexeme :: Parser Lexeme
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

data St = St MP.SourcePos ShowS

reachOffset'
  :: (Int -> Stream -> (MP.Tokens Stream, Stream))
     -- ^ How to split input stream at given offset
  -> (forall b. (b -> MP.Token Stream -> b) -> b -> MP.Tokens Stream -> b)
     -- ^ How to fold over input stream
  -> (MP.Tokens Stream -> String)
     -- ^ How to convert chunk of input stream into a 'String'
  -> (MP.Token Stream -> Char)
     -- ^ How to convert a token into a 'Char'
  -> (MP.Token Stream, MP.Token Stream)
     -- ^ Newline token and tab token
  -> Int
     -- ^ Offset to reach
  -> MP.PosState Stream
     -- ^ Initial 'MP.PosState' to use
  -> (MP.SourcePos, String, MP.PosState Stream)
     -- ^ Reached 'SourcePos', line at which 'SourcePos' is located, updated
     -- 'MP.PosState'
reachOffset' splitAt'
             foldl''
             fromToks
             fromTok
             (newlineTok, tabTok)
             o
             MP.PosState {..} =
  ( spos
  , case expandTab pstateTabWidth
           . addPrefix
           . f
           . fromToks
           . fst
           $ MP.takeWhile_ (/= newlineTok) post of
      "" -> "<empty line>"
      xs -> xs
  , MP.PosState
      { MP.pstateInput = post
      , MP.pstateOffset = max pstateOffset o
      , MP.pstateSourcePos = spos
      , MP.pstateTabWidth = pstateTabWidth
      , MP.pstateLinePrefix =
          if sameLine
            -- NOTE We don't use difference lists here because it's
            -- desirable for 'MP.PosState' to be an instance of 'Eq' and
            -- 'Show'. So we just do appending here. Fortunately several
            -- parse errors on the same line should be relatively rare.
            then pstateLinePrefix ++ f ""
            else f ""
      }
  )
  where
    addPrefix xs =
      if sameLine
        then pstateLinePrefix ++ xs
        else xs
    sameLine = MP.sourceLine spos == MP.sourceLine pstateSourcePos
    (pre, post) = splitAt' (o - pstateOffset) pstateInput
    St spos f = foldl'' go (St pstateSourcePos id) pre
    go (St apos g) ch =
      let MP.SourcePos n l c = apos
          c' = MP.unPos c
          w  = MP.unPos pstateTabWidth
      in if | ch == newlineTok ->
                St (MP.SourcePos n (l <> MP.pos1) MP.pos1)
                   id
            | ch == tabTok ->
                St (MP.SourcePos n l (MP.mkPos $ c' + w - ((c' - 1) `rem` w)))
                   (g . (fromTok ch :))
            | otherwise ->
                St (MP.SourcePos n l (c <> MP.pos1))
                   (g . (fromTok ch :))
{-# INLINE reachOffset' #-}

expandTab
  :: MP.Pos
  -> String
  -> String
expandTab w' = go 0
  where
    go 0 []        = []
    go 0 ('\t':xs) = go w xs
    go 0 (x:xs)    = x : go 0 xs
    go n xs        = ' ' : go (n - 1) xs
    w              = MP.unPos w'

stream :: Parser Stream
stream = Stream <$> MP.many lexeme
