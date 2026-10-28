module Tokens (
  tokenize,
) where

import Data.Char
import Syntax

tokenize :: [Char] -> Either Error [Token]
tokenize s = getToken 1 s

getToken :: Int -> [Char] -> Either Error [Token]
getToken n "" = Right [Token n TEOF]
getToken n ('=' : '=' : after) = getSimpleToken n after TDoubleEqual
getToken n ('=' : after) = getSimpleToken n after TSingleEqual
-- TODO:`` the rest of the exact-character tokens go here
-- Remember that whitespace should simply be skipped, and newlines should increment the line number
getToken n all@(c : _)
  | isNumber c = do
      let (numberToken, after) = getNumberToken n all
      afterTokens <- getToken n after
      Right $ numberToken : afterTokens
  -- TODO: other dynamic token guards
  | otherwise = Left $ Error n ("Unexpected character: '" ++ c : "'")

getSimpleToken :: Int -> [Char] -> TokenValue -> Either Error [Token]
getSimpleToken n s tokenValue = do
  afterTokens <- getToken n s
  Right $ Token n tokenValue : afterTokens

getNumberToken :: Int -> [Char] -> (Token, [Char])
getNumberToken n s = (Token n (TNumber (read numericChars)), after)
 where
  (numericChars, after) = span isNumber s

getStringToken :: Int -> [Char] -> (Token, [Char])
getStringToken = undefined

-- TODO: If the identifier is equal to one of the keywords, use that instead
getIdentifierToken :: Int -> [Char] -> (Token, [Char])
getIdentifierToken = undefined
