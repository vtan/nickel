{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Nickel.Account
  ( ParsedAccount(..)
  , Income(..)
  , Expense(..)
  , parseAccounts
  ) where

import Control.Applicative
import Control.Arrow
import Control.Monad

import qualified Data.Time as Time
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.String as Parsec



data ParsedAccount
  = ParsedIncome Income
  | ParsedExpense Expense
  | InvalidLine Int -- ^ Invalid line with its number
  deriving (Eq, Show)

data Income = Income
  { incDate :: Time.Day
  , incAmount :: Int
  , incName :: String
  }
  deriving (Eq, Show)

data Expense = Expense
  { expDate :: Time.Day
  , expAmount :: Int
  , expCat :: String
  , expName :: String
  }
  deriving (Eq, Show)

parseAccounts :: [String] -> [ParsedAccount]
parseAccounts = map (handleFailure . (id *** parse)) . zip [1..]
  where
    parse = Parsec.parse account ""
    handleFailure (n, (Left _)) = InvalidLine n
    handleFailure (_, (Right acc)) = acc



account :: Parsec.Parser ParsedAccount
account = fmap ParsedIncome income <|> fmap ParsedExpense expense

income :: Parsec.Parser Income
income = do
  void $ Parsec.char '+' <* Parsec.spaces
  incDate <- date <* Parsec.spaces
  incAmount <- amount <* Parsec.spaces
  incName <- string <* Parsec.spaces <* Parsec.eof
  pure Income{..}

expense :: Parsec.Parser Expense
expense = Expense
  <$ Parsec.char '-' <* Parsec.spaces
  <*> date <* Parsec.spaces
  <*> amount <* Parsec.spaces
  <*> string <* Parsec.spaces
  <*> string <* Parsec.spaces <* Parsec.eof

date :: Parsec.Parser Time.Day
date = total $
  Time.fromGregorianValid <$> field 4 <* sep <*> field 2 <* sep <*> field 2
  where
    field n = read <$> Parsec.count n Parsec.digit
    sep = Parsec.char '-'
    total parser = do
      mx <- parser
      case mx of
        Just x -> pure x
        Nothing -> empty

amount :: Parsec.Parser Int
amount = read <$> Parsec.many1 Parsec.digit

string :: Parsec.Parser String
string = Parsec.between quote quote (Parsec.many1 $ Parsec.noneOf "\"")
  where
    quote = Parsec.char '"'
