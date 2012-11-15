{-# LANGUAGE OverlappingInstances, TypeSynonymInstances, FlexibleInstances, OverloadedStrings, Arrows #-}

module CsvDatabase
    (
		Row
        , dshow
        , popRow
        , popCell
        , string2vf
        , tailTdb
        , Db
        , Selector(..)
        , ColValue(..)
        , exec
        , Col
        , select
        , dselect
        , isdouble
        , mfoldl
        , fromDb
        , fromTdb
        , apply
        , fromCsv
        , Tdb
        , sortdb
        , CParser(..)
        , mshow
    ) where

import Data.List
-- Hide a few names that are provided by Applicative.

import Prelude hiding (id)
import Data.Monoid 
import CsvParser

-- We have 2 types, string or numbers = Maybe Double
-- So, strings are NOT NULL, NULL = Nothing in the Maybe Double
data ColValue = ColS [String] | ColN [Maybe Double] deriving (Eq, Ord)

tailColV::ColValue->ColValue
tailColV (ColS ss) =  ColS $ tail ss
tailColV (ColN ns) =  ColN $ tail ns

-- Overide the standard show
instance Show (Maybe Double) where
	show (Just v) = if (v == fromInteger (round v)) then show (round v) else show v
	show Nothing = ""
	 
mshow::Show a => Maybe a -> String
mshow (Just v) = show v
mshow Nothing = ""
     
instance Monoid ColValue where
	mappend l1 l2 = case (l1, l2) of
					(ColN ln1, ColN ln2) -> ColN (ln1 ++ ln2)
					(ColS ls1, ColS ls2) -> ColS (ls1 ++ ls2)
					(ColS ls1, ColN ln2) -> ColS (ls1 ++ (map mshow ln2))
					(ColN ln1, ColS ls2) -> ColS ((map mshow ln1) ++ ls2)
	mempty = ColN []
    
-- Shows the ColValue in a column - for the xml file for charts package
-- Shows the strings for when it's eg the names of the books
-- Shows the numbers for when it's eg the scores
instance Show ColValue where
	show (ColN ln) = concatMap (\md-> "<number>" ++ mshow md ++ "</number>" ) ln
	show (ColS ls) = concatMap (\ms-> "<string>" ++ ms ++ "</string>" ) ls	
	
type Csv = [[Value]]
type Db = [Row]
type Row = [Cell]
type Cell = (String, Value)	-- key, value
type Col = (String, ColValue)   	-- ie. all the same field - we need to make them all doubles or all strings...
type Tdb = [Col]

-- Writes a Db as a html table
instance Show Db where 
	show [] = "<table></table>"
	show db = "<table>" ++ (write_header (head db)) ++ (concatMap rshow db) ++ "</table>"

dshow:: Db-> String
dshow [] = "<table></table>"
dshow db = "<table>" ++ (write_header (head db)) ++ (concatMap rshow db) ++ "</table>"
    
    
-- Writes a Record as a html table-record
-- where each field is a <td>
instance Show Row where
	show r = "<tr>" ++ concatMap (\f -> "<td>" ++ show (snd f) ++ "</td>") r ++ "</tr>"
		
rshow:: Row->String
rshow r = "<tr>" ++ concatMap (\f -> "<td>" ++ show (snd f) ++ "</td>") r ++ "</tr>"

        
-------------------------------------------------------------------------
-- Writes a header from the strings of each field
-------------------------------------------------------------------------
write_header::Row->String
write_header r = "<tr>" ++ concatMap (\f ->"<th>" ++ fst f ++ "</th>") (r) ++ "</tr>"
	
tailCol::Col->Col
tailCol c = (fst c, tailColV $ snd c)
		
-- Add a field to a column
-- If one of the fields is a string then we get a sring
pushCell::Cell->Col->Col
pushCell fld col = case (fst fld == fst col) of
						True -> ( fst fld, lv `mappend` snd col )
								where lv = case (snd fld) of 
												N n -> ColN [n]
												S s -> ColS [s]
						False -> col -- adds nothing

popCell::Col->Cell
popCell (s, ColN []) = (s, N Nothing)
popCell (s, ColS []) = (s, N Nothing)
popCell (s, ColN (h:_)) = (s, N h)
popCell (s, ColS (h:_)) = (s, S h)
        
        
-- Add a Record to a Tdb (transposed Db)
pushRow::Row->Tdb->Tdb
pushRow = zipWith pushCell
											
popRow::Tdb->Row
popRow tdb = map popCell tdb
		
-- Turn a field into a column
cell2col::Cell->Col
cell2col (s, N n) = (s, ColN [n])
cell2col (s, S n) = (s, ColS [n])
                    
-- Turn a record into a Tdb						
row2tdb::Row->Tdb
row2tdb = map cell2col						

-- Turn a Db into a Tdb
fromDb::Db->Tdb
fromDb db = foldl (flip pushRow) (row2tdb $ head db) (tail db)
		
tailTdb::Tdb->Tdb
tailTdb tdb = map tailCol tdb
		
--Turn a Tdb into a Db
fromTdb::Tdb->Db
fromTdb tdb = fromTdb' tdb []
    where fromTdb'::Tdb->Db->Db
          fromTdb' tdb' db = case snd (head tdb') of
								ColN [] -> db
								ColS [] -> db
								_     -> fromTdb' (tailTdb tdb') ((popRow tdb'):db)

isdouble::Col->Bool
isdouble (_, ColN _) = True
isdouble (_, ColS _) = False
				

-- The headers are the first row, turned into strings, if not already
fromCsv::Csv->Db
fromCsv [] =  []
fromCsv (h:rs) =  map (\r-> zipWith tov h r) rs
                  where
                    tov (S shf) rf = (shf, rf)
                    tov (N dhf) rf = (mshow dhf, rf)

-- A list of CellName, Value pairs - ie. the WHERE bit of an SQL statement
type Query = [Filter]
-- A pair of CellName and what we want the Value of it to be
type Filter = (Selector, ValueFilter)
-- What we want the cell name/index to be
data Selector = CellName String | CellIndex Int deriving (Show)

type ValueFilter = [CParser]
data CParser = Char Char | Wildcard deriving (Show)

-- Turn a string into a ValueFilter
string2vf::String->ValueFilter
string2vf s = map Char s

cparse :: CParser -> String -> [String]
cparse (Char c) (c' : cs') | c == c' = [cs']
cparse Wildcard []                   = [[]]
cparse Wildcard cs@(_ : cs')         = cs : cparse Wildcard cs'
cparse _ _                           = []
	
-- Only implemented for S = string Values
filterValue :: ValueFilter -> Value -> Bool
filterValue ps (S cs) = any null (go ps cs)
  where
    go [] cs'       = [cs']
    go (p : ps') cs' = concatMap (go ps') (cparse p cs')
filterValue _ _ = False

-- Selects a Value from a Row for the given selector
select :: Selector -> Row -> Maybe Value
select (CellName s) r                           = lookup s r
select (CellIndex n) r | n > 0 && n <= length r = Just (snd (r !! (n - 1)))
                        | otherwise              = Nothing

-- Selects a column from a daatabase
dselect :: Selector -> Tdb -> Maybe ColValue
dselect (CellName s) tdb                           = lookup s tdb
dselect (CellIndex n) tdb | n > 0 && n <= length tdb = Just (snd (tdb !! (n - 1)))
                          | otherwise              = Nothing						
						
apply :: Filter -> Row -> Bool
apply (s, vf) r = case select s r of
  Nothing -> False
  Just v  -> filterValue vf v

-- This is a WHERE statement - getting all the columns - ie. a sub db
exec :: Query -> Db -> [Row]
exec = (flip . foldl . flip) (filter . apply)

sortdb :: Selector -> Db -> Db
sortdb s db = sortBy (\a b -> compare (select s b) (select s a)) db

-- folds a function over a LValue only if it is a Maybe Double list
mfoldl::(Double->Double->Double)->Double->ColValue->Double
mfoldl f acc (ColN ns) = 	foldl (\a x -> case x of
						Just n -> f a n
						Nothing -> a
				    ) acc ns
mfoldl _ acc (ColS _) = acc


