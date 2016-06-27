{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib

import Graphics.Gloss
import Graphics.Gloss.Data.Color (makeColor, red)
import Data.Monoid
import Data.Word
import Data.Time
import Data.Text (Text(..), pack)
import Data.Attoparsec.Text
import Control.Applicative
import Data.Array

csvFile :: Parser [DataRow]
csvFile = many' datarow

datarow :: Parser DataRow
datarow = do
        t <- parseTOD
        char ','
        ip <- parseIP
        endOfLine <|> endOfInput
        return $ DataRow t ip

parseTOD :: Parser TimeOfDay
parseTOD = do
        decimal 
        char '-'
        decimal
        char '-'
        decimal
        char 'T'
        h <- decimal
        char ':'
        m <- decimal
        char ':'
        s <- decimal
        char 'Z'
        return $ TimeOfDay h m (fromIntegral s)

parseIP :: Parser IP
parseIP = do
        a <- decimal
        char '.'
        b <- decimal
        char '.'
        c <- decimal
        char '.'
        d <- decimal
        return $ IP a b c d

main :: IO ()
main = do
        f <- readFile "data/time_ip.csv"
        case parseOnly csvFile (pack f) of
          Left e  -> putStrLn $ "Error: " <> e
          Right d -> draw d --print $ convert d
        -- display (InWindow "Hello World" (500,500) (100,100))
        --        (makeColor 0.9 0.9 0.9 1)
        --        (f . convert $ d)

convert :: [DataRow] -> Array Int Int
convert d = array (0,23) d'
      where
              d' = [f i | i <- [0..23]]
              f i = (i,length $ filter (f' i) d)
              f' i (DataRow t _) = todHour t == i



f :: Array Int Int -> Picture
f = undefined


