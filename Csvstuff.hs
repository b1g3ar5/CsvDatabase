{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances, OverloadedStrings, Arrows #-}

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
vlookup::Db->String->String->Maybe Value    
vlookup db housename colname = select s2 r
    where
        -- 2 filters: house = housename, fst col = colname
        s1 = CellName "Page" -- the selector for the house column
        s2 = CellName colname -- the selector for the queried column
        v = map Char housename -- the value
        f1 = (s1, v) -- the row filter
        q1 = [f1] -- the query
        r = head $ exec q1 db -- gets just the row for the house
        
-- if Just 1.0, goes to TRUE otherwise FALSE
toBool::Maybe Value->Maybe Bool    
toBool (Just (N (Just 1.0))) = Just True
toBool _ = Nothing
    
-- Takes the keys as the column names
colnames::Db->[String]
colnames = map fst . head

-- Takes the first column as the row names
rownames::Db->[String]
rownames = map (show . snd . head) . tail

-- The data rows - ie tail! - no, all rows!!
rows::Db->Db
rows = tail

-- The data columns
columns::Db->Db
columns db = tail $ foldl add_on (map (\x->[x]) (head $ rows db)) (tail $ rows db)
				where
					add_on xss ys = zipWith (\xs y -> xs++[y]) xss ys
		
					
-- Some functions to play with columns
	
-- Reads a strings to Maybe Double
dread::String->Maybe Double
dread s = case readFloat s of
			[(n,_)]-> Just n
			_ -> Nothing
	