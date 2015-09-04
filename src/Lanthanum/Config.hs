{-# LANGUAGE OverloadedStrings #-}

module Lanthanum.Config where

import Network.Wai.Middleware.RequestLogger (logStdoutDev, logStdout)
import Network.Wai                          (Middleware)
import Control.Monad.Logger                 (runNoLoggingT, runStdoutLoggingT)

import Database.Persist.Postgresql (ConnectionPool, createPostgresqlPool, ConnectionString)

data Config = Config
    { getPool      :: ConnectionPool
    , getEnv       :: Environment
    , getClientDir :: FilePath
    }

data Environment =
    Development
  | Test
  | Production
  deriving (Eq, Show, Read)

defaultConfig :: Config
defaultConfig = Config
    { getPool      = undefined
    , getEnv       = Development
    , getClientDir = "."
    }

setLogger :: Environment -> Middleware
setLogger Test = id
setLogger Development = logStdoutDev
setLogger Production = logStdout

makePool :: Environment -> IO ConnectionPool
makePool Test = runNoLoggingT $ createPostgresqlPool (connStr Test) (envPool Test)
makePool e = runStdoutLoggingT $ createPostgresqlPool (connStr e) (envPool e)

envPool :: Environment -> Int
envPool Test = 1
envPool Development = 1
envPool Production = 8

connStr :: Environment -> ConnectionString
connStr _ = "host=localhost dbname=lanthanum user=lanthanum password=secret port=5432"
