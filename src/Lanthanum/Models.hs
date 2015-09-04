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
import GHC.Generics                (Generic)
import Control.Monad.Reader        (ReaderT, asks, liftIO)
import Database.Persist.Postgresql (SqlBackend(..), runMigration, 
                                    runSqlPool)
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

runDb query = do
    pool <- asks getPool
    liftIO $ runSqlPool query pool

