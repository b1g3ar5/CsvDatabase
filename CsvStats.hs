{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances, OverloadedStrings, Arrows #-}

module CsvStats
    (
		pickerBox, Box(..), Stat(..), dbBox, msum, msumsq, mcount, mmin,
		mmax, mmean, mstd, box, colBox
    ) where
	
import Prelude hiding (id)
import CsvDatabase

msum::ColValue->Double
msum = mfoldl (+) 0.0					
mmax::ColValue->Double
mmax = mfoldl max 0.0
mmin::ColValue->Double
mmin = mfoldl min 1.0e100
msumsq::ColValue->Double
msumsq = mfoldl (\a n -> a+n*n) 0.0
mcount::ColValue->Double
mcount = mfoldl (\a _ -> a+1.0) 0.0
	
mmean::ColValue->Double	
mmean xs = (msum xs)/(mcount xs)
	
mvar::ColValue->Double	
mvar xs = (msumsq xs)/(mcount xs) - (mmean xs)**2.0
		
mstd::ColValue->Double	
mstd xs = (mvar xs)**0.5
		
data Stat = Stat {smax::Double,
			 smin::Double,
			 smean::Double,
			 ssd::Double,
			 scount::Int}

data Box = Box {bopen::Double,
			 bhi::Double,
			 blo::Double,
			 bclose::Double,
			 bcount::Int}

-- Works out the Box stats from am LValue
stat::ColValue->Stat
stat mds =  Stat {smax=mmax mds, smin=mmin mds, smean=mmean mds, ssd=mstd mds, scount=floor $ mcount mds}
		   
-- Works out the Box stats from am LValue
box::ColValue->Box
box mds =  Box {bopen=smean s + ssd s, bhi=smax s, blo=smin s, bclose=smean s - ssd s, bcount=scount s}
				where 
                    s = stat mds		   

-- Works out the Box for a column in a Tdb 
-- picking the column by name
colBox::Tdb->String->Box		 
colBox tdb name = case dselect (CellName name) tdb of
					Nothing -> Box {bopen = 0.0, bhi=0.0, blo=0.0, bclose=0.0, bcount=0}
					Just lv -> box lv

-- Works out the Box for a column in a Tdb 
-- picking the column by name
colStat::Tdb->String->Stat		 
colStat tdb name = case dselect (CellName name) tdb of
					Nothing -> Stat {smax = 0.0, smin=0.0, smean=0.0, ssd=0.0, scount=0}
					Just lv -> stat lv

-- Works out the Box stats for a all numerical columns in a Tdb 
dbBox::Tdb->Box		 
dbBox tdb = Box {bopen = lmean+lsd, bhi=maximum maxs, blo=minimum mins, bclose=lmean-lsd, bcount=sum counts}
              where
                ncols = filter isdouble tdb
                names = map fst ncols
                cs = map (colStat ncols) names
                maxs = map smax cs
                mins = map smin cs
                means = map smean cs
                vars = map (\x->x*x) (map ssd cs)
                counts = map scount cs
                lmean = sum (zipWith (*) means (map fromIntegral counts))/(fromIntegral $ sum counts)
                lsd = (sum (zipWith (*) vars (map fromIntegral counts))/(fromIntegral $ sum counts))**0.5

pickerBox::Tdb->String->Box
pickerBox tdb p = dbBox ftdb
              where
                vf = map (\c->Char c) p
                f = (CellName "Book Picker", vf)
                q = [f]
                db = fromTdb tdb
                fdb = exec q db
                ftdb = fromDb fdb


  