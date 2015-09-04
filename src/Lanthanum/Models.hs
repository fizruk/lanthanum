{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DeriveGeneric              #-}

module Lanthanum.Models where

import Data.Aeson.TH
import Data.Text (Text)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Database.Persist.Postgresql
import Database.Persist.TH

import Lanthanum.Model.SubmitStatus

import Lanthanum.Config
import Lanthanum.Utils

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Submit
    problemId ProblemId
    status    SubmitStatus
    code      Text
    log       Text Maybe
    deriving Show

Problem
    title       Text
    description Text
    snippet     Text Maybe
    setup       Text Maybe
    examples    Text Maybe
    tests       Text Maybe
|]
deriveJSON (jsonOptions "submit")  ''Submit
deriveJSON (jsonOptions "problem") ''Problem

doMigrations :: ReaderT SqlBackend IO ()
doMigrations = runMigration migrateAll

runDb :: (MonadIO m, MonadReader Config m) => SqlPersistT IO b -> m b
runDb query = do
    pool <- asks getPool
    liftIO $ runSqlPool query pool

