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

module Lanthanum.Model.SubmitStatus where

import Data.Aeson.TH
import Database.Persist.TH
import Lanthanum.Utils

data SubmitStatus
  = SubmitAccepted
  | SubmitTimeLimitExceeded
  | SubmitWrongAnswer
  | SubmitRunningTests
  | SubmitCompilationError
  | SubmitCompiling
  | SubmitSubmitted
  deriving (Eq, Show, Read)
deriveJSON (jsonOptions "submit") ''SubmitStatus
derivePersistField "SubmitStatus"

