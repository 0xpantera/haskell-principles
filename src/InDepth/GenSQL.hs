{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module InDepth.GenSQL where

import Data.Foldable
import Control.Monad.Writer
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

type SQL = T.Text

data ErrorMsg = WrongFormat Int T.Text
    deriving Show


genSQL :: T.Text -> Writer [ErrorMsg] SQL
genSQL txt = T.concat 
    <$> traverse processLine 
        (zip [1..] $ T.lines txt)

-- Never construct SQL queries like this, obviously.
genInsert :: T.Text -> T.Text -> T.Text
genInsert s1 s2 =
    "INSERT INTO items VALUES ('" <> s1 <> "','" <> s2 <> "');\n"

processLine :: (Int, T.Text) -> Writer [ErrorMsg] SQL
processLine (_, T.splitOn ":" -> [s1, s2]) = pure $ genInsert s1 s2
processLine (i, s) = tell [WrongFormat i s] >> pure ""

testData :: T.Text
testData = "Pen:Bob\nGlass:Mary:10\nPencil:Alice\nBook:Bob\nBottle"

testGenSQL :: IO ()
testGenSQL = do
    let (sql, errors) = runWriter (genSQL testData)
    TIO.putStrLn "SQL:"
    TIO.putStrLn sql
    TIO.putStrLn "Errors:"
    traverse_ print errors