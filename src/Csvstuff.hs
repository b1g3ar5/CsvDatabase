{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings, Arrows #-}

module Csvstuff
    (
        csvFile, dread, rownames, colnames, Col, line, isdouble, vlookup, toBool, columns, rows
    ) where

import Prelude hiding (id)
import Numeric (readFloat)
import CsvParser
import CsvDatabase

{-------------------------------------------
Some operations on a Db
--------------------------------------------}

-- takes a row and a column and returns the value
vlookup :: (Eq a, Read a) => Db a -> String -> String -> Maybe (Value a)
vlookup db id colname = select (CellName colname) r
    where
        s1 = CellName "Id" -- the selector for the house column
        v = map Char id -- the value
        r = head $ exec [(s1,v)] db -- gets just the row for the house

-- if Just 1.0, goes to TRUE otherwise FALSE
toBool :: (Eq a, Fractional a) => Maybe (Value a) -> Maybe Bool
toBool (Just (Right (Just 1.0))) = Just True
toBool _ = Nothing

-- Takes the keys as the column names
colnames :: Db a ->[String]
colnames = map fst . head

-- Takes the first column as the row names
rownames :: (Show a) => Db a ->[String]
rownames = map (show . snd . head) . tail

-- The data rows - ie tail! - no, all rows!!
rows :: Db a -> Db a
rows = tail

-- The data columns
columns :: Db a -> Db a
columns db = tail $ foldl add_on (map (\x->[x]) (head $ rows db)) (tail $ rows db)
                where
                    add_on xss ys = zipWith (\xs y -> xs++[y]) xss ys


-- Some functions to play with columns

-- Reads a strings to Maybe Double
dread :: String -> Maybe Double
dread s = case readFloat s of
            [(n,_)]-> Just n
            _ -> Nothing
