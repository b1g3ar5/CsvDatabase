{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings, Arrows #-}

module Types (
  ColValue(..)
  , Row
  , mshow
  , ToString(..)
) where

import Data.List
import Data.String.ToString
import Prelude hiding (id)
import Data.Monoid
import CsvParser

writeHeader::Row -> String
writeHeader r = "<tr>" ++ concatMap (\f ->"<th>" ++ fst f ++ "</th>") r ++ "</tr>"

data ColValue = ColS [String] | ColN [Maybe Double] deriving (Eq, Ord)

tailColV :: ColValue -> ColValue
tailColV (ColS ss) = ColS $ tail ss
tailColV (ColN ns) = ColN $ tail ns

mshow::Show a => Maybe a -> String
mshow (Just v) = show v
mshow Nothing = ""

instance Semigroup (ColValue) where
	l1 <> l2 = case (l1, l2) of
					(ColN ln1, ColN ln2) -> ColN (ln1 ++ ln2)
					(ColS ls1, ColS ls2) -> ColS (ls1 ++ ls2)
					(ColS ls1, ColN ln2) -> ColS (ls1 ++ (map mshow ln2))
					(ColN ln1, ColS ls2) -> ColS ((map mshow ln1) ++ ls2)

instance Monoid (ColValue) where
  mappend = (<>)
  mempty = ColN []

instance Show (ColValue) where
	show (ColN ln) = concatMap (\md-> "<number>" ++ mshow md ++ "</number>" ) ln
	show (ColS ls) = concatMap (\ms-> "<string>" ++ ms ++ "</string>" ) ls

instance ToString ColValue where
	toString (ColN ln) = concatMap (\md-> "<number>" ++ mshow md ++ "</number>" ) ln
	toString (ColS ls) = concatMap (\ms-> "<string>" ++ ms ++ "</string>" ) ls

instance ToString [String] where
  toString xs = intercalate ", " xs

instance ToString [Maybe Double] where
  toString xs = intercalate ", " $ fmap toString xs

instance ToString Double where
  toString = show

instance (ToString a) => ToString (Maybe a) where
  toString Nothing = ""
  toString (Just x) = toString x

type Db = [Row]
type Row = [Cell]
type Cell = (String, Value Double)	-- key, value
type Col = (String, ColValue)   	-- ie. all the same field - we need to make them all doubles or all strings...
type Tdb = [Col]

-- Writes a Db as a html table
{--
instance Show Db where
	show [] = "<table></table>"
	show db = "<table>" ++ (writeHeader (head db)) ++ (concatMap rshow db) ++ "</table>"
--}

dshow:: Db-> String
dshow [] = "<table></table>"
dshow db = "<table>" ++ (writeHeader (head db)) ++ (concatMap rshow db) ++ "</table>"


-- Writes a Record as a html table-record
-- where each field is a <td>
{--
instance Show Row where
	show r = "<tr>" ++ concatMap (\f -> "<td>" ++ show (snd f) ++ "</td>") r ++ "</tr>"
--}

rshow:: Row->String
rshow r = "<tr>" ++ concatMap (\f -> "<td>" ++ show (snd f) ++ "</td>") r ++ "</tr>"
