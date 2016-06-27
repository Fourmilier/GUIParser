module Lib 
        ( parseCSV
        , CSV
        , Row
        , Field
        , draw
        , IP(..)
        , DataRow(..)
        )
       where

import Data.Attoparsec.Text as A
import Data.Text as T (pack, strip, Text(..), null)
import Control.Applicative
import Control.Monad (void, unless, when)
import Debug.Trace (trace)
import Graphics.Gloss
import Graphics.Gloss.Data.Color (makeColor, red)
import Data.Word
import Data.Time
import Data.List(sort,sortBy)
import Data.List.Split


-- csv-file       = { row }
-- row            = field-list, eol
-- field-list     = field, [ ",", field-list ]
-- field          = [ whitespace ], field-value, [ whitespace ]
-- field-value    = quoted-string | bare-string
-- quoted-string  = '"', quoted-content, '"'
-- quoted-content = { quoted-char }
-- quoted-char    = (any char except '"' or eol)
-- bare-string    = { bare-char }
-- bare-char      = (any char except ',' or eol without whitespace at beginning/end)
-- whitespace     = space-char, { space-char }
-- space-char     = " " | "\t"
-- eol            = "\n"

data IP = IP Word8 Word8 Word8 Word8 deriving (Show, Eq)

data DataRow = DataRow
          { t  :: TimeOfDay
          , ip :: IP
          } deriving (Show, Eq)

data Bucket = Bucket
        { stunde :: Int
        , zugriffe :: Int
        }

draw :: [DataRow] -> IO ()
draw d = display (InWindow "Hello World" (500,500) (100,100))
               (makeColor 0.9 0.9 0.9 1)
               (Pictures $ drawPic <$> buckets)
        where
                buckets = bucket d

drawPic :: Bucket -> Picture
drawPic (Bucket s z) = Translate (a' * 100) 0 $ rectangleSolid (10*a') (10*a')
  where
          a' = fromIntegral s

bucket :: [DataRow] -> [Bucket]
bucket d = []

        where 
                sorted = sortBy sortFun d
                sortFun :: DataRow -> DataRow -> Ordering
                sortFun (DataRow t ip)(DataRow t' ip') = compare t t'  


--aufrufe :: File -> Array Int Int
--aufrufe d = accumArray (\x _ -> x+1) | 0 (0,23)
--            where d' = (\(Zeile t _) -> (hour t, ())) 

--Liste von DataRow zu Liste von Tupeln, Liste von Tupeln mit accumArray bearbeiten, erhaltenes Array in Liste von Buckets umwandeln

--b :: [DataRow] -> [(TimeOfDay,IP)] 
--b 



--accumArray :: (0 -> ip -> 23) -> 0 -> (0,23) -> [(0,ip)] -> Array 24 0
--accumArray [] = []
--accumArray 


--DataRow in 24 buckets unterteilen, ips in buckets einsortieren, ips in buckets zählen, Balkendiagramm zeichnen


--accumArray :: Ix i => (e -> a -> e) -> e -> (i,i) -> [(i, a)] -> Array i e


--sortBy :: (a -> a -> Ordering) -> [a] -> [a]
--sort :: Ord a => [a] -> [a]


--sorted = sortBy sortAccess d
--sortAccess :: DataRow -> DataRow -> Ordering
--sortAccess (DataRow t ip)(DataRow t' ip') = compare ip ip' 

--toPos :: Countable a => a -> Integer
--Source

--Overloaded cToPos.

--fromPos :: Countable a => Integer -> a
--Source

--Overloaded cFromPos. 
--bucket d = [(Bucket 1 100)]
--count :: Countable a => a -> Maybe Integer
--Source

--Overloaded cCount. Doesn't attempt to reduce the dummy value given. 


type CSV = [Row]
type Row = [Field]
type Field = Text


parseCSV :: String -> Either String CSV
parseCSV s = parseOnly csvParser (pack s)

csvParser :: Parser CSV
csvParser = many rowParser

rowParser :: Parser Row
rowParser = do
             e <- atEnd
             when e $ fail "no more rows"
             f <- fieldParser
             fs <- many (char ',' *> fieldParser)
             endOfLine <|> endOfInput
             return (f:fs)

fieldParser :: Parser Field
fieldParser = do
              whitespace
              fv <- fieldValueParser
              whitespace
              return fv

whitespace :: Parser Text
whitespace = A.takeWhile pred
  where
          pred :: Char -> Bool
          pred ' '  = True
          pred '\t' = True
          pred _    = False


fieldValueParser :: Parser Text
fieldValueParser = quotedString <|> bareString
  where
          quotedString :: Parser Text
          quotedString = char '"' *> A.takeWhile quotedChar <* char '"'
          bareString :: Parser Text
          bareString = do
                         bc <- A.takeWhile bareChar
                         return $ strip bc
          quotedChar :: Char -> Bool
          quotedChar a
             | a == '"'      = False
             | isEndOfLine a = False
             | otherwise     = True
          bareChar :: Char -> Bool
          bareChar a
             | a == ','      = False
             | isEndOfLine a = False
             | otherwise     = True





