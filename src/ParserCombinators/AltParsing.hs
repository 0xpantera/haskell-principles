{-# LANGUAGE QuasiQuotes #-}
module ParserCombinators.AltParsing where

import Control.Applicative
import Text.RawString.QQ
import Text.Trifecta

type NumberOrString =
    Either Integer String

a = "blah"
b = "123"
c = "123blah789"

parseNos :: Parser NumberOrString
parseNos = do
    skipMany (oneOf "\n")
    v <-     (Left <$> integer)
         <|> (Right <$> some letter)
    skipMany (oneOf "\n")
    return v

eitherOr :: String
eitherOr = [r|
123
abc
456
def
|]

main :: IO ()
main = do
    let p f i = parseString f mempty i
    print $ p (some parseNos) eitherOr

