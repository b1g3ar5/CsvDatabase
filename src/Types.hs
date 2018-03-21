{-# LANGUAGE OverlappingInstances, TypeSynonymInstances, FlexibleInstances, OverloadedStrings, Arrows #-}

module Types (    ) where

import Data.List
-- Hide a few names that are provided by Applicative.

import Prelude hiding (id)
import Data.Monoid 
import CsvParser


data ColValue = ColS [String] | ColN [Maybe Double] deriving (Eq, Ord)

tailColV::ColValue->ColValue
tailColV = fmap tail

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
    
instance Functor ColValue where
    fmap f (ColS ss) = ColS $ f ss
    fmap f (ColN ns) = ColN $ f ss
    
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

