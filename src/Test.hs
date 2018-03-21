{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Data.Csv  
import GHC.Generics
import Data.Text hiding (head)
import Data.Vector as V hiding (head, sequence)
import Control.Applicative
import Data.ByteString as B hiding (putStrLn, head)
import Data.ByteString.Lazy as BL hiding (putStrLn, head)
import Data.Either
import Data.List as L
import Control.Monad (liftM5)
import Control.Lens hiding ((.=))

data Score = Score {  _title:: !Text
                    , _author :: !Text
                    , _strings :: !(Maybe Double)
                    , _neil :: !(Maybe Double)
                    , _stretch :: !(Maybe Double)
                    , _iain :: !(Maybe Double)
                    , _jethro :: !(Maybe Double)
                    , _picker :: !Text
                    , _date :: !Text
                   } deriving (Generic, Show, Eq)

makeLenses ''Score

scoreCompareBy :: (Score->Double)->Score->Score->Ordering
scoreCompareBy f s1 s2 = compare (f s1)  (f s2)

-- Adds up a stat function for a record over a vector
scoreStat :: (Score -> Maybe Double) -> B.ByteString -> Vector Score -> Maybe Double
scoreStat f rec db = fmap L.sum col
    where
        col :: Maybe [Double]
        col = sequence $ L.map f $ toList db

-- Adds up a stat function for a record over a vector
meanStat :: (Score -> Maybe Double) -> B.ByteString -> Vector Score -> Maybe Double
meanStat f rec db = case n of 
                        Nothing -> Nothing
                        Just nn -> (fmap (\c-> (L.sum c)/nn) col)
    where
        col :: Maybe [Double]
        col = sequence $ L.map f $ toList db
        n = fmap (fromIntegral . L.length) col



scoreHeader = header ["title", "author", "strings", "neil", "stretch", "iain", "jethro", "picker", "date"]
pickers = ["strings", "neil", "stretch", "iain", "jethro"]

instance DefaultOrdered Score

-- So we can read "n/a" as Nothing
instance FromNamedRecord Score where
    parseNamedRecord m = Score <$> (m .: "title") <*>  (m .: "author") <*> (get m "strings") <*> (get m "neil") <*> (get m "stretch") <*> (get m "iain") <*> (get m "jethro") <*> (m .: "picker") <*> (m .: "date")
        where
            get :: NamedRecord -> B.ByteString -> Parser (Maybe Double)
            get m s = do
                        str <- ps                        
                        if (str == "n/a") then
                            return Nothing
                        else 
                            return $ Just $ (read str)
                where
                    ps :: Parser String
                    ps = m .: s

-- So we can write Nothin to "n/a" 
instance ToNamedRecord Score where
    toNamedRecord (Score title author strings neil stretch iain jethro picker date) = namedRecord [("title".= title), ("author".= author), (put "strings" strings), (put "neil" neil), (put "stretch" stretch), (put "iain" iain), (put "jethro" jethro), ("picker".= picker), ("date".= date)]
        where
            put :: B.ByteString -> Maybe Double -> (B.ByteString, B.ByteString)           
            put s Nothing = (s, "n/a")
            put s f = (s .= f)

-- Sorts a Vector of Scores according to the column with a particular column name
sortDb :: Vector Score -> B.ByteString -> Vector Score
sortDb db r = fromList $ sortBy cc $ toList db
    where
        cc :: Score -> Score -> Ordering        
        cc b a = case r of
                    "strings" -> compare (_strings a) (_strings b)
                    "neil" -> compare (_neil a) (_neil b)
                    "stretch" -> compare (_stretch a) (_stretch b)
                    "iain" -> compare (_iain a) (_iain b)
                    "jethro" -> compare (_jethro a) (_jethro b)

getRec :: B.ByteString -> Score -> Maybe Double
getRec rec ss = case rec of
                    "strings" -> _strings ss
                    "neil" -> _neil ss
                    "stretch" -> _stretch ss
                    "iain" -> _iain ss
                    "jethro" -> _jethro ss  

-- Takes a record (as a string) and a value and returns all rows where the entry in that record equals the value
filterDb :: B.ByteString -> Double -> Vector Score -> Vector Score
filterDb rec val db = fromList $ L.filter (\ss -> getRec rec ss == Just val) $ toList db

main :: IO ()
main = do
    -- This parses the file to Either String (Header = [String], Vector Score)
    let s1 = decodeByName "title,author,strings,neil,stretch,iain,jethro,picker,date\r\nMy Favourite Wife,Tony Parsons,6,4,3,2.0,1.0,STRINGS,Jun-08\r\n" :: Either String (Header, Vector Score)
    let s2 = decodeByName "title,author,strings,neil,stretch,iain,jethro,picker,date\r\nMy Favourite Wife,Tony Parsons,6,4,3,n/a,n/a,STRINGS,Jun-08\r\n" :: Either String (Header, Vector Score)
    -- Convert the parsed csv back to a [Score] and the encode with the scoreHeader    
    let c1 = encodeByName scoreHeader $ toList $ snd $ head $ rights [s1]
    let c2 = encodeByName scoreHeader $ toList $ snd $ head $ rights [s2]
    putStrLn $ show s1
    putStrLn $ show s2
    putStrLn $ show c1
    putStrLn $ show c2
    -- sort a db according to a column
    ss <- BL.readFile "./scoreTable.csv"
    let edb = decodeByName ss :: Either String (Header, Vector Score)
    let db = snd $ head $ rights [edb]
    putStrLn $ show edb
    putStrLn "=========================================================="
    putStrLn $ show db
    putStrLn "=========================================================="
    putStrLn $ show $ sortDb db "stretch"


    
