module Tokens (
  tokenize,
) where

import Data.Char
import Syntax

tokenize :: [Char] -> Either Error [Token]
tokenize = loop 1
 where
  loop :: Int -> [Char] -> Either Error [Token]
  loop n s = do
    (token, n, s) <- getToken n s
    tokensAfter <- loop n s
    Right $ token : tokensAfter

getToken :: Int -> [Char] -> Either Error (Token, Int, [Char])
getToken n "" = Right (Token n TEOF, n, "")
getToken n ('=' : '=' : after) = Right (Token n TDoubleEqual, n, after)
getToken n ('=' : after) = Right (Token n TSingleEqual, n, after)
getToken n (' ' : after) = getToken n after
-- TODO: the rest of the exact-character tokens go here
-- Remember that whitespace should simply be skipped, and newlines should increment the line number
getToken n all@(c : _)
  | isNumber c = Right $ getNumberToken n all
  | isLetter c = Right $ getIdentifierToken n all
  | otherwise = Left $ Error n ("Unexpected character: '" ++ c : "'")

getNumberToken :: Int -> [Char] -> (Token, Int, [Char])
getNumberToken n s = (Token n (TNumber (read numericChars)), n, after)
 where
  (numericChars, after) = span isNumber s

-- TODO: Encountered newlines should increment the newline
getStringToken :: Int -> [Char] -> (Token, Int, [Char])
getStringToken = undefined

-- TODO: If the identifier is equal to one of the keywords, use that instead
getIdentifierToken :: Int -> [Char] -> (Token, Int, [Char])
getIdentifierToken n s = (Token n input), n, after
where 
  isIdentifer c = isAlphaNum c || c == '_'
  (identifier, after) = span isIdentifer s
  input = case identifier of "let" -> TLet
                            "def" -> TDef
                            "if" -> TIf
                            "else" -> TElse
                            "while" -> TWhile
                            "return" -> TReturn
                            "true" -> TTrue
                            "false" -> TFalse
                            _ -> TIdentifier
  
