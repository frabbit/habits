{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Habits.Infra.Postgres.Schema where
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Control.Monad.Logger           ( runStderrLoggingT )
import qualified Data.Pool                     as P'
import           Data.Text                      ( Text )
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll'"] [persistLowerCase|
Account
    Id Text
    name Text
    email Text
    deriving Show
|]


connStr :: ConnectionString
connStr = "host=localhost dbname=test user=test password=test port=5432"

migrateAll :: (MonadIO m) => P'.Pool SqlBackend -> m ()
migrateAll pool = liftIO $ flip runSqlPersistMPool pool $ do
  runMigration migrateAll'



main :: IO ()
main = runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do
  flip runSqlPersistMPool pool $ do
    runMigration migrateAll'
    let johnId = AccountKey "john"
    let janeId = AccountKey "jane"
    insertKey johnId $ Account "John Doe" "abc@de.de"
    insertKey janeId $ Account "Jane Doe" "abc@de.de"

    john <- get johnId
    liftIO $ print (john :: Maybe Account)

    delete janeId
