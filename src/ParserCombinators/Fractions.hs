{-# LANGUAGE OverloadedStrings #-}
module ParserCombinators.Fractions where

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta

badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
    numerator <- decimal
    char '/'
    denominator <- decimal
    return (numerator % denominator)

main :: IO ()
main = do
    let parseFraction' =
         parseString parseFraction mempty

    print $ parseFraction' shouldWork
    print $ parseFraction' shouldAlsoWork

    print $ parseFraction' alsoBad
    print $ parseFraction' badFraction

virtuousFraction :: Parser Rational
virtuousFraction = do
    numerator <- decimal
    char '/'
    denominator <- decimal
    case denominator of
        0 -> fail "Denominator cannot be zero"
        _ -> return (numerator % denominator)

testVirtuous :: IO ()
testVirtuous = do
    let parseFraction' =
         parseString virtuousFraction mempty

    print $ parseFraction' badFraction
    print $ parseFraction' alsoBad

    print $ parseFraction' shouldWork
    print $ parseFraction' shouldAlsoWork

returnInt :: Parser Integer
returnInt = do
    n <- integer
    eof
    return n
