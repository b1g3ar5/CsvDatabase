{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

import Control.Monad.IO.Class  (liftIO)
import Database.Persist
--import Database.Persist.Sqlite
import Database.Persist.Postgresql
import Database.Persist.TH
import Data.Text
import Control.Monad.Logger (runStdoutLoggingT, LoggingT)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Score
    title Text
    author Text
    strings Double Maybe
    neil Double Maybe
    stretch Double Maybe
    iain Double Maybe
    jethro Double Maybe
    picker Text
    date Text
    deriving Show
|]

connStr = "host=localhost dbname=mydb user=postgres password=kippeR55 port=5432"

--filterDb :: B.ByteString -> Double -> Vector Score -> Vector Score
--filterDb rec val db = fromList $ L.filter (\ss -> getRec rec ss == Just val) $ toList db
    

getConnection :: SqlPersistM () -> IO ()
getConnection sql = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> do 
                    liftIO $ flip runSqlPersistMPool pool $ do sql

main :: IO ()
main = getConnection $ do
    oneJohnPost <- selectList [ScoreTitle ==. "My Favourite Wife"] [LimitTo 1]
    liftIO $ print (oneJohnPost :: [Entity Score])

mig :: IO ()
mig = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> do
    liftIO $ flip runSqlPersistMPool pool $ do
    runMigration migrateAll

    johnId <- insert $ Score "My Favourite Wife" "Tony Parsons" (Just 6.0) (Just 4.0) (Just 3.0) Nothing Nothing "STRINGS" "Jun-08"

    oneJohnPost <- selectList [ScoreTitle ==. "My Favourite Wife"] [LimitTo 1]
    liftIO $ print (oneJohnPost :: [Entity Score])

    john <- get johnId
    liftIO $ print (john :: Maybe Score)

    deleteWhere [ScoreTitle ==. "My Favourite Wife"]
