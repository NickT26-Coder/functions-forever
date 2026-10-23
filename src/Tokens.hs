module Tokens (
  tokenize,
) where

import Data.Char
import Syntax

tokenize :: [Char] -> [Token]
tokenize s = getToken 1 s

getToken :: Int -> [Char] -> [Token]
getToken _ "" = []
getToken n ('=' : after) = (Token n TSingleEqual : getToken n after)
getToken n all@(c : after) = undefined

getNumber :: Int -> [Char] -> [Token]
getNumber = undefined

getString :: Int -> [Char] -> [Token]
getString = undefined

getIdentifier :: Int -> [Char] -> [Token]
getIdentifier = undefined
