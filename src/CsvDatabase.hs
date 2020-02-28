{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module CsvDatabase
    (
        dshow
        , Db
        , Col
        , select
        , dselect
        , isdouble
        , mfoldl
        , fromDb
        , fromTdb
        , fromCsv
        , Tdb
        , sortdb
        , CParser(..)
        , Selector(..)
        , exec
        , cvShow
        , rshow
        , ColType
        , writeHeader
        , eshow
    ) where

import Data.List
import Data.String.ToString
import Prelude hiding (id)
import CsvParser
import Types hiding (Row, mshow)

-- We have 2 types, string or numbers = Maybe Double
-- So, strings are NOT NULL, NULL = Nothing in the Maybe Double
type ColType a = Either [String] [Maybe a]

-- Functor won't work because ColValue is not *->*
colMap :: (forall a. [a]->[a]) -> ColType b -> ColType b
colMap f (Left ss) =  Left $ f ss
colMap f (Right ns) =  Right $ f ns

-- Shows that it might be better to make ColValue a functor!!!
instance Show a => Monoid (ColType a) where
    mappend l1 l2 = case (l1, l2) of
                    (Right ln1, Right ln2) -> Right (ln1 ++ ln2)
                    (Left ls1, Left ls2) -> Left (ls1 ++ ls2)
                    (Left ls1, Right ln2) -> Left (ls1 ++ map mshow ln2)
                    (Right ln1, Left ls2) -> Left (map mshow ln1 ++ ls2)
    mempty = Right []


tailColV::ColType a->ColType a
tailColV = colMap tail

mshow::Show a => Maybe a -> String
mshow (Just v) = show v
mshow Nothing = ""

eshow::(ToString a, ToString b) => Either a b -> String
eshow (Left x) = toString x
eshow (Right y) = toString y

cvShow :: Show a => ColType a -> String
cvShow (Right ln) = concatMap (\md-> "<number>" ++ mshow md ++ "</number>" ) ln
cvShow (Left ls) = concatMap (\ms-> "<string>" ++ ms ++ "</string>" ) ls

type Csv a = [[Value a]]
type Db a = [Row a]
type Row a = [Cell a]
type Cell a = (String, Value a) -- key, value
type Col a = (String, ColType a) -- ie. all the same field - we need to make them all doubles or all strings...
type Tdb a = [Col a]

dshow:: (ToString a, RealFrac a) => Db a -> String
dshow [] = "<table></table>"
dshow db = "<table>" ++ writeHeader (head db) ++ concatMap rshow db ++ "</table>"

-------------------------------------------------------------------------
-- Writes a header from the strings of each field
-------------------------------------------------------------------------
writeHeader::Row a -> String
writeHeader r = "<tr>" ++ concatMap (\f ->"<th>" ++ fst f ++ "</th>") r ++ "</tr>"

rshow :: (ToString a, RealFrac a) => Row a -> String
rshow r = "<tr>" ++ concatMap (\f -> "<td>" ++ eshow (snd f) ++ "</td>") r ++ "</tr>"

tailCol::Col a -> Col a
tailCol c = (fst c, tailColV $ snd c)

-- Add a field to a column
-- If one of the fields is a string then we get a sring
pushCell :: Show a => Cell a -> Col a->Col a
pushCell fld col = case fst fld == fst col of
                        True -> ( fst fld, lv `mappend` snd col )
                                where lv = case snd fld of
                                                Right n -> Right [n]
                                                Left s -> Left [s]
                        False -> col -- adds nothing

popCell::Col a -> Cell a
popCell (s, Right []) = (s, Right Nothing)
popCell (s, Left []) = (s, Right Nothing)
popCell (s, Right (h:_)) = (s, Right h)
popCell (s, Left (h:_)) = (s, Left h)


-- Add a Record to a Tdb (transposed Db)
pushRow :: (Show a, RealFrac a) => Row a -> Tdb a -> Tdb a
pushRow = zipWith pushCell

popRow::Tdb a -> Row a
popRow = map popCell

-- Turn a field into a column
cell2col::Cell a -> Col a
cell2col (s, Right n) = (s, Right [n])
cell2col (s, Left n) = (s, Left [n])

-- Turn a record into a Tdb
row2tdb::Row a -> Tdb a
row2tdb = map cell2col

-- Turn a Db into a Tdb
fromDb :: (Show a, RealFrac a) => Db a -> Tdb a
fromDb db = foldl (flip pushRow) (row2tdb $ head db) (tail db)

tailTdb::Tdb a -> Tdb a
tailTdb = map tailCol

--Turn a Tdb into a Db
fromTdb::Tdb a -> Db a
fromTdb tdb = fromTdb' tdb []
    where fromTdb'::Tdb a -> Db a -> Db a
          fromTdb' tdb' db = case snd (head tdb') of
                                Right [] -> db
                                Left [] -> db
                                _       -> fromTdb' (tailTdb tdb') (popRow tdb':db)

isdouble::Col a -> Bool
isdouble (_, Right _) = True
isdouble (_, Left _) = False


-- The headers are the first row, turned into strings, if not already
fromCsv :: (Show a, RealFrac a) => Csv a -> Db a
fromCsv [] =  []
fromCsv (h:rs) =  map (zipWith tov h) rs
                  where
                    tov (Left shf) rf = (shf, rf)
                    tov (Right dhf) rf = (mshow dhf, rf)

-- A list of CellName, Value pairs - ie. the WHERE bit of an SQL statement
type Query = [Filter]
-- A pair of CellName and what we want the Value of it to be
type Filter = (Selector, ValueFilter)
-- What we want the cell name/index to be
data Selector = CellName String | CellIndex Int deriving (Show)

type ValueFilter = [CParser]
data CParser = Char Char | Wildcard deriving (Show)

-- Turn a string into a ValueFilter
--string2vf::String->ValueFilter
--string2vf s = map Char s

cparse :: CParser -> String -> [String]
cparse (Char c) (c' : cs') | c == c' = [cs']
cparse Wildcard []                   = [[]]
cparse Wildcard cs@(_ : cs')         = cs : cparse Wildcard cs'
cparse _ _                           = []

-- Only implemented for S = string Values
filterValue :: (Eq a, Read a) => ValueFilter -> Value a -> Bool
filterValue ps (Left cs) = any null (go ps cs)
    where
        go [] cs'       = [cs']
        go (p : ps') cs' = concatMap (go ps') (cparse p cs')
filterValue cs (Right (Just n)) = n == ((read $ map go cs))
    where
        go (Char c) = c
        go Wildcard = '0'
filterValue _ _ = False

-- Selects a Value from a Row for the given selector
select :: Selector -> Row a -> Maybe (Value a)
select (CellName s) r                           = lookup s r
select (CellIndex n) r | n > 0 && n <= length r = Just (snd (r !! (n - 1)))
                        | otherwise             = Nothing

-- Selects a column from a daatabase
dselect :: (Eq a) => Selector -> Tdb a -> Maybe (ColType a)
dselect (CellName s) tdb                              = lookup s tdb
dselect (CellIndex n) tdb | n > 0 && n <= length tdb  = Just (snd (tdb !! (n - 1)))
                            | otherwise               = Nothing

apply :: (Eq a, Read a) => Filter -> Row a -> Bool
apply (s, vf) r = case select s r of
  Nothing -> False
  Just v  -> filterValue vf v

-- This is a WHERE statement - getting all the columns - ie. a sub db
exec :: (Eq a, Read a) => Query -> Db a -> [Row a]
exec = (flip . foldl . flip) (filter . apply)

sortdb :: (Ord a, Read a) => Selector -> Db a -> Db a
sortdb s = sortBy (\a b -> compare (select s b) (select s a))

-- folds a function over a LValue only if it is a Maybe Double list
mfoldl::(b -> a -> b) -> b -> ColType a -> b
mfoldl f acc (Right ns) = foldl (\a x -> case x of
                                        Just n -> f a n
                                        Nothing -> a
                               ) acc ns
mfoldl _ acc (Left _) = acc
