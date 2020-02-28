{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}

module CsvStats
    (
        pickerBox, Box(..), Stat(..), dbBox, msum, msumsq, mcount, mmin,
        mmax, mmean, mstd, box, colBox
    ) where

import Prelude hiding (id)
import CsvDatabase (CParser(..), ColType, Tdb(..), mfoldl, dselect, isdouble, fromDb, fromTdb, exec, Selector(..))

msum :: (Fractional a) => ColType a -> a
msum = mfoldl (+) 0.0
mmax :: (Ord a, Fractional a) => ColType a -> a
mmax = mfoldl max 0.0
mmin :: (Num a, Ord a, Fractional a) => ColType a -> a
mmin = mfoldl min 1.0e100
msumsq :: (Num a, Ord a, Fractional a) => ColType a -> a
msumsq = mfoldl (\a n -> a+n*n) 0.0
mcount :: (Num a, Ord a, Fractional a) => ColType a -> a
mcount = mfoldl (\a _ -> a+1.0) 0.0
mmean :: (Num a, Ord a, Fractional a) => ColType a -> a
mmean xs = msum xs / mcount xs
mvar :: (Num a, Ord a, Fractional a, Floating a) => ColType a -> a
mvar xs = msumsq xs / mcount xs - mmean xs ** 2.0
mstd :: (Num a, Ord a, Fractional a, Floating a) => ColType a -> a
mstd xs = sqrt $ mvar xs


data Stat = Stat {smax::Double,
                  smin::Double,
                  smean::Double,
                  ssd::Double,
                  scount::Int} deriving (Show)


data Box = Box {bopen::Double,
                bhi::Double,
                blo::Double,
                bclose::Double,
                bcount::Int} deriving (Show)


stat :: ColType Double -> Stat
stat mds =  Stat {smax=mmax mds, smin=mmin mds, smean=mmean mds, ssd=mstd mds, scount=floor $ mcount mds}


box :: ColType Double -> Box
box mds =  Box {bopen=smean s + ssd s, bhi=smax s, blo=smin s, bclose=smean s - ssd s, bcount=scount s}
                where
                    s = stat mds


colBox :: Tdb Double -> String -> Box
colBox tdb name = case dselect (CellName name) tdb of
                    Nothing -> Box {bopen = 0.0, bhi=0.0, blo=0.0, bclose=0.0, bcount=0}
                    Just lv -> box lv


colStat :: Tdb Double -> String -> Stat
colStat tdb name = case dselect (CellName name) tdb of
                    Nothing -> Stat {smax = 0.0, smin=0.0, smean=0.0, ssd=0.0, scount=0}
                    Just lv -> stat lv


dbBox :: Tdb Double -> Box
dbBox tdb = Box {bopen = lmean+lsd, bhi=maximum maxs, blo=minimum mins, bclose=lmean-lsd, bcount=sum counts}
              where
                ncols = filter isdouble tdb
                names = map fst ncols
                cs = map (colStat ncols) names
                maxs = map smax cs
                mins = map smin cs
                means = map smean cs
                vars = map ((\x->x*x) . ssd) cs
                counts = map scount cs
                lmean = sum (zipWith (*) means (map fromIntegral counts)) / fromIntegral (sum counts)
                lsd = sqrt (sum (zipWith (*) vars (map fromIntegral counts)) / fromIntegral (sum counts))


pickerBox :: Tdb Double -> String -> Box
pickerBox tdb p = dbBox ftdb
              where
                vf = map Char p
                f = (CellName "Book Picker", vf)
                q = [f]
                db = fromTdb tdb
                fdb = exec q db
                ftdb = fromDb fdb
