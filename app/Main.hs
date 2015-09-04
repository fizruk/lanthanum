module Main where

import Network.Wai.Handler.Warp    (run)
import System.Environment          (lookupEnv)
import Database.Persist.Postgresql (runSqlPool)

import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)

import Lanthanum.Config
import Lanthanum.API (app)
import Lanthanum.Models (doMigrations)

import Paths_lanthanum

main :: IO ()
main = do
    env  <- lookupSetting "ENV" Development
    port <- lookupSetting "PORT" 8000
    pool <- makePool env
    clientDir <- getDataFileName "client"
    let cfg = defaultConfig { getPool = pool, getEnv = env, getClientDir = clientDir }
        logger = setLogger env
    runSqlPool doMigrations pool
    run port $ logger $ app cfg

lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
    p <- lookupEnv env
    return $ fromMaybe def (p >>= readMaybe)
