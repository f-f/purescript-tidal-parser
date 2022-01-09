module Sound.Tidal.Parser where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either)
import Data.Int as Int
import Data.List (List, foldMap)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.String as String
import Data.String.CodeUnits as Strings
import Sound.Tidal.Chords as Chords
import Text.Parsing.StringParser (Parser, try)
import Text.Parsing.StringParser as Parser
import Text.Parsing.StringParser.CodeUnits as Parse
import Text.Parsing.StringParser.Combinators as Comb

type Note = Int

data Expr
  = Rest
  | Note Note
  | Chord Note (Array Note)
  | Sample String (Maybe Int)
  | BangRep Int Expr
  | StarRep Int Expr
  | AtDuration Number Expr
  | Seq (List Expr)
  | SquareSeq (List Expr)
  | SquareSuperpos (List Expr)
  | AngleSeq (List Expr)
  | CurlyPoly (Maybe Int) (List Expr)
  | ParensEuclid Int Int (Maybe Int) Expr

derive instance eqExpr :: Eq Expr


space :: List Expr -> String
space es = String.joinWith " " (map show (List.toUnfoldable es))

comma :: List Expr -> String
comma es = String.joinWith ", " (map show (List.toUnfoldable es))

instance showExpr :: Show Expr where
  show (Rest) = "~"
  show (Note i) = show i
  show (Chord root is) = "[" <> String.joinWith "," (map (\i -> show $ root + i) is) <> "]"
  show (Sample s Nothing) = s
  show (Sample s (Just i)) = s <> ":" <> show i
  show (BangRep i e) = show e <> "!" <> show i
  show (StarRep i e) = show e <> "*" <> show i
  show (AtDuration n e) = show e <> "@" <> show n
  show (Seq es) = space es
  show (SquareSeq es) = "[" <> space es <> "]"
  show (AngleSeq es) = "<" <> space es <> ">"
  show (SquareSuperpos es) = "[" <> comma es <> "]"
  show (CurlyPoly maybeFactor es) = "{" <> comma es <> "}" <> case maybeFactor of
    Nothing -> ""
    Just i -> "%" <> show i
  show (ParensEuclid top bottom maybeShift e)
    = let shift = case maybeShift of
            Nothing -> ""
            Just s -> "," <> show s
      in show e <> "(" <> show top <> "," <> show bottom <> shift <> ")"


parse :: String -> Either Parser.ParseError Expr
parse = Parser.runParser exprParser


exprParser :: Parser Expr
exprParser = do
  let parser = Comb.fix \e -> do
        expr
          <-  try (squareSeq e)
          <|> (squareSuperpos e)
          <|> (angleSeq e)
          <|> (curlyPoly e)
          <|> atom
        try (bangRep expr)
          <|> try (starRep expr)
          <|> try (atDuration expr)
          <|> try (parensEuclid expr)
          <|> pure expr
  seq parser


atom :: Parser Expr
atom = rest <|> try noteOrChord <|> sample

rest :: Parser Expr
rest = Parse.char '~' *> Parse.skipSpaces *> pure Rest

sample :: Parser Expr
sample = do
  let label = Parse.regex "[a-zA-Z0-9_-]+"
  name <- label
  index <- Comb.optionMaybe (Parse.char ':' *> Comb.optionMaybe int)
  -- Ensure there's no more label left ahead to avoid whitespace ambiguity
  notFollowedBy label
  Parse.skipSpaces
  pure $ Sample name $ join index

bangRep :: Expr -> Parser Expr
bangRep e = do
  i <- Parse.char '!' *> int
  pure $ BangRep i e

starRep :: Expr -> Parser Expr
starRep e = do
  i <- Parse.char '*' *> int
  pure $ StarRep i e

atDuration :: Expr -> Parser Expr
atDuration e = do
  n <- Parse.char '@' *> number
  pure $ AtDuration n e

parensEuclid :: Expr -> Parser Expr
parensEuclid e = do
  Parse.char '(' *> Parse.skipSpaces
  top <- int <* Parse.skipSpaces <* Parse.char ',' <* Parse.skipSpaces
  bottom <- int <* Parse.skipSpaces
  maybeShift <- Comb.optionMaybe $ Parse.char ',' *> Parse.skipSpaces *> int
  Parse.skipSpaces <* Parse.char ')'
  pure $ ParensEuclid top bottom maybeShift e

seq :: Parser Expr -> Parser Expr
seq parser = do
  Parse.skipSpaces
  expr1 <- parser
  moreExprs <- Comb.many (try $ Parse.skipSpaces *> parser)
  Parse.skipSpaces
  if List.null moreExprs
    then pure expr1
    else pure $ Seq (List.(:) expr1 moreExprs)

squareSeq :: Parser Expr -> Parser Expr
squareSeq = map SquareSeq <<< sequence '[' ']' Parse.skipSpaces

angleSeq :: Parser Expr -> Parser Expr
angleSeq = map AngleSeq <<< sequence '<' '>' Parse.skipSpaces

squareSuperpos :: Parser Expr -> Parser Expr
squareSuperpos = map SquareSuperpos <<< sequence '[' ']' (Parse.char ',' *> Parse.skipSpaces)

curlyPoly :: Parser Expr -> Parser Expr
curlyPoly exprP = do
  content <- sequence '{' '}' (Parse.char ',' *> Parse.skipSpaces) (seq exprP)
  maybeFactor <- Comb.optionMaybe $ Parse.char '%' *> int
  pure $ CurlyPoly maybeFactor content

-- TODO: should we actually do this smart stuff in parsing?
noteOrChord :: Parser Expr
noteOrChord = do
  -- Note is parsed first..
  baseN <- noteEnum
  modifiers <- Comb.many noteModifier
  octave <- Comb.option 5 int
  let n = (List.foldr (+) baseN modifiers) + ((octave-5)*12)
  -- Ensure there's no more alphaNum ahead to avoid ambiguity with samples
  notFollowedBy Parse.alphaNum
  -- ..then we try to parse the chord
  maybeChord <- Comb.optionMaybe $ do
    void $ Parse.char '\''
    name <- alphaNumStr
    pure $ Map.lookup name Chords.chordTable
    -- TODO: add inversions to chords
  Parse.skipSpaces
  pure $ case join maybeChord of
    Just ns -> Chord n ns
    Nothing -> Note n
  where
    noteEnum :: Parser Int
    noteEnum = Comb.choice
      [ Parse.char 'c' *> pure 0
      , Parse.char 'd' *> pure 2
      , Parse.char 'e' *> pure 4
      , Parse.char 'f' *> pure 5
      , Parse.char 'g' *> pure 7
      , Parse.char 'a' *> pure 9
      , Parse.char 'b' *> pure 11
      ]
    noteModifier :: Parser Int
    noteModifier = Comb.choice
      [ Parse.char 's' *> pure 1
      , Parse.char 'f' *> pure (-1)
      , Parse.char 'n' *> pure 0
      ]





----------------------------------- Utils

-- | Match one whitespace character
whiteSpaceChar :: Parser Char
whiteSpaceChar = Parse.satisfy \ c -> c == '\n' || c == '\r' || c == ' ' || c == '\t'

alphaNumStr :: Parser String
alphaNumStr = foldMap Strings.singleton <$> Comb.many1 Parse.alphaNum

int :: Parser Int
int = do
  intStr <- Comb.choice [ Parse.regex "[-+]?[1-9]\\d*", Parse.string "0" ]
  case Int.fromString intStr of
    Just i -> pure i
    Nothing -> Parser.fail $ "Expected an integer but found: " <> intStr

number :: Parser Number
number = do
  numStr <- Parse.regex "[-+]?[0-9]*\\.?[0-9]+"
  case Number.fromString numStr of
    Just n -> pure n
    Nothing -> Parser.fail $ "Expected a number but found: " <> numStr

-- | Parse a sequence with delimiters and a separator
sequence :: forall sep a. Char -> Char -> Parser sep -> Parser a -> Parser (List a)
sequence open close separator parser = Comb.between
  (Parse.char open <* Parse.skipSpaces)
  (Parse.skipSpaces *> Parse.char close)
  (Comb.sepBy parser separator)


notFollowedBy :: forall a. Show a => Parser a -> Parser Unit
notFollowedBy p = try $ do
  next <- Comb.optionMaybe $ Comb.lookAhead p
  case next of
    Just n -> Parser.fail $ "Tried to match a separator, but got more input: " <> show n
    Nothing -> pure unit
