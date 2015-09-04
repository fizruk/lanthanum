{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lanthanum.API.Files where

import Control.Monad.Trans.Resource
import Data.ByteString.Lazy (ByteString)
import Network.Wai.Parse
import Servant

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
