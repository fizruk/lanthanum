{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
module Lanthanum.API where

import Data.Aeson.TH
import Data.Monoid

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Control.Monad.Error.Class
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.ByteString.Lazy (ByteString)
import Network.Wai
import Network.Wai.Parse
import Servant
import Database.Persist.Postgresql

import System.Process
import System.Exit
import System.IO
import System.IO.Temp
import System.Timeout

import Lanthanum.Config
import Lanthanum.Models
import Lanthanum.Model.SubmitStatus
import Lanthanum.Utils

data Mem
data Tmp

class KnownBackend b where
  type Storage b :: *

  withBackend :: Proxy b -> (BackEnd (Storage b) -> IO r) -> IO r

instance KnownBackend Mem where
  type Storage Mem = ByteString

  withBackend Proxy f = f lbsBackEnd

instance KnownBackend Tmp where
  type Storage Tmp = FilePath

  withBackend Proxy f = runResourceT . withInternalState $ \s ->
    f (tempFileBackEnd s)

-- * Files combinator, to get all of the uploaded files

data Files b

type MultiPartData b = ([Param], [File (Storage b)]) 

instance (KnownBackend b, HasServer api) => HasServer (Files b :> api) where
  type ServerT (Files b :> api) m =
    MultiPartData b -> ServerT api m

  route Proxy subserver req respond = withBackend pb $ \b -> do
    dat <- parseRequestBody b req
    route (Proxy :: Proxy api) (subserver dat) req respond

    where pb = Proxy :: Proxy b

type FilesMem = Files Mem
type FilesTmp = Files Tmp

-- 

type AppM = ReaderT Config (EitherT ServantErr IO)

forkAppM :: AppM a -> AppM ThreadId
forkAppM m = do
  cfg <- ask
  liftIO $ forkIO $ do
    _ <- runEitherT (runReaderT m cfg)
    return ()

data Solution = Solution { solutionCode :: Text }
deriveJSON (jsonOptions "solution") ''Solution

type ProblemSubmitApi
    = "file" :> FilesTmp                 :> Post '[JSON] SubmitId
 :<|> "raw"  :> ReqBody '[JSON] Solution :> Post '[JSON] SubmitId
 :<|> Get '[JSON] [Entity Submit]

submitServer :: ProblemId -> ServerT ProblemSubmitApi AppM
submitServer problemId = fileHandler :<|> rawHandler :<|> listHandler
  where
    listHandler :: AppM [Entity Submit]
    listHandler = listSubmits problemId

    rawHandler :: Solution -> AppM SubmitId
    rawHandler Solution{..} = createSubmit problemId solutionCode

    fileHandler :: MultiPartData Tmp -> AppM SubmitId
    fileHandler ([], [(_name, fileinfo)]) = do
      code <- liftIO $ Text.readFile (fileContent fileinfo)
      createSubmit problemId code
    fileHandler _ = throwError err400 -- Bad Request

listSubmits :: ProblemId -> AppM [Entity Submit]
listSubmits problemId = runDb $ selectList [SubmitProblemId ==. problemId] [Desc SubmitId]

createSubmit :: ProblemId -> Text -> AppM SubmitId
createSubmit problemId code = do
  problem <- get404 problemId
  submitId <- runDb $ insert $ Submit problemId SubmitSubmitted code Nothing
  _ <- forkAppM $ runTestsFor problem submitId code
  return submitId

runTestsFor :: Problem -> SubmitId -> Text -> AppM ()
runTestsFor Problem{..} submitId code = do
  cfg <- ask
  lift $ lift $ runReaderT runTests cfg
  where
    timeout_ :: Int -> SubmitStatus -> ReaderT Config IO a -> ReaderT Config IO ()
    timeout_ n timeoutStatus f = do
      e <- ask
      res <- lift $ timeout n (runReaderT f e)
      case res of
        Nothing -> do
          status timeoutStatus []
          liftIO exitFailure
        _ -> return ()

    status :: SubmitStatus -> [Update Submit] -> ReaderT Config IO ()
    status s updates = runDb $ update submitId ((SubmitStatus =. s) : updates)

    stage :: FilePath -> [String] -> SubmitStatus -> SubmitStatus -> ReaderT Config IO ()
    stage cmd args processingStatus failureStatus = do
      status processingStatus []
      (exitStatus, _out, err) <- liftIO $ readProcessWithExitCode cmd args ""
      case exitStatus of
        ExitFailure _exitCode -> do
          status failureStatus [SubmitLog =. Just (Text.pack err)]
          liftIO exitFailure
        ExitSuccess -> return ()

    doctestSection :: Text -> Maybe Text -> Text
    doctestSection _ Nothing = ""
    doctestSection name (Just content) = Text.unlines $
      [ "-- $" <> name ] ++
      map ("-- " <>) (Text.lines content)

    runTests :: ReaderT Config IO ()
    runTests = do
      withTempFile "/tmp" "solutionXXX.hs" $ \tmpFile hFile -> do
        -- copy code into a temporary file
        liftIO $ do
          Text.hPutStrLn hFile "module Solution where"
          Text.hPutStrLn hFile code
          Text.hPutStrLn hFile $ doctestSection "setup"    problemSetup
          Text.hPutStrLn hFile $ doctestSection "examples" problemExamples
          Text.hPutStrLn hFile $ doctestSection "tests"    problemTests
          hClose hFile

        stage "stack" ["ghc", "--", "-c", tmpFile] SubmitCompiling SubmitCompilationError
        timeout_ (30 * 10^6) SubmitTimeLimitExceeded $ do
          stage "stack" ["exec", "doctest", tmpFile] SubmitRunningTests SubmitWrongAnswer
        status SubmitAccepted []

data ProblemInfo = ProblemInfo
  { problemTitle        :: Text
  , problemDescription  :: Text
  , problemSnippet      :: Text
  }
deriveJSON (jsonOptions "problem") ''ProblemInfo

type ProblemApi
    = "submit" :> ProblemSubmitApi
 :<|> Get '[JSON] Problem

problemServer :: ProblemId -> ServerT ProblemApi AppM
problemServer problemId = submitServer problemId :<|> infoHandler
  where
    infoHandler :: AppM Problem
    infoHandler = get404 problemId

type LanthanumApi
    = "problem" :> Capture "problem-id" ProblemId :> ProblemApi

server :: ServerT LanthanumApi AppM
server = problemServer

app :: Config -> Application
app cfg = serve api (readerServer cfg :<|> serveDirectory (getClientDir cfg))
  where
    api :: Proxy (LanthanumApi :<|> Raw)
    api = Proxy

readerServer :: Config -> Server LanthanumApi
readerServer cfg = enter (runReaderServer cfg) server

runReaderServer :: e -> ReaderT e m :~> m
runReaderServer e = Nat $ \x -> runReaderT x e
