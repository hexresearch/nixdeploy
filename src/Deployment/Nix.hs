{-# LANGUAGE RecordWildCards #-}
module Deployment.Nix(
    DeployOptions(..)
  , Command(..)
  , runDeployment
  , module R
  ) where

import Data.Monoid
import Data.Text (Text, pack, unpack)

import Deployment.Nix.Task as R
import Deployment.Nix.Task.Common as R

-- | CLI options
data DeployOptions = DeployOptions {
  optionsCommand :: Command
}

-- | Available CLI commands to perform
data Command =
    CommandDeploy {
      deployHost :: Text
    , deployUser :: Text
    }

-- | Execute program with given options
runDeployment :: DeployOptions -> Task a -> IO a
runDeployment DeployOptions{..} buildPlan = do
  case optionsCommand of
    CommandDeploy{..} -> pure ()
