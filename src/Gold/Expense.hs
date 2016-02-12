{-# LANGUAGE FlexibleContexts #-}

module Gold.Expense where

import Control.Applicative
import Data.Either

import qualified Data.Time as Time

import qualified Text.Parsec as Parsec
import qualified Text.Parsec.String as Parsec



data Expense = Expense
  { expDate :: Time.Day
  , expAmount :: Int
  , expCat :: String
  , expName :: String
  }
  deriving (Eq, Show, Read)

parseExpenses :: [String] -> [Expense]
parseExpenses = snd . partitionEithers . map (Parsec.parse expense "")

expense :: Parsec.Parser Expense
expense = Expense
  <$ Parsec.char '-' <* Parsec.spaces
  <*> date <* Parsec.spaces
  <*> int <* Parsec.spaces
  <*> str <* Parsec.spaces
  <*> str <* Parsec.spaces <* Parsec.eof
  where
    str = Parsec.between quote quote (Parsec.many1 $ Parsec.noneOf "\"")
    quote = Parsec.char '"'
    int = read <$> Parsec.many1 Parsec.digit

date :: Parsec.Parser Time.Day
date = total (Time.fromGregorianValid <$> field 4 <* sep <*> field 2 <* sep <*> field 2)
  where
    field n = read <$> Parsec.count n Parsec.digit
    sep = Parsec.char '-'
    total parser = do
      mx <- parser
      case mx of
        Just x -> pure x
        Nothing -> empty
