{-# LANGUAGE RecordWildCards #-}
module Deployment.Nix(
    DeployOptions(..)
  , Command(..)
  , runDeployment
  , module R
  ) where

import Data.Foldable (traverse_)
import Data.Functor
import Data.Maybe
import Data.Monoid
import Data.Text (Text, pack, unpack)
import Shelly
import Transient.Base

import qualified Data.Text as T

import Deployment.Nix.Task as R
import Deployment.Nix.Task.Common as R

-- | CLI options
data DeployOptions = DeployOptions {
  optionsCommand :: Command
}

-- | Available CLI commands to perform
data Command =
    -- | Execute deployment tasks
    CommandDeploy {
      deployHost   :: Text
    , deployPort   :: Int -- ^ ssh port
    , deployUser   :: Text
    , deployDry    :: Bool -- ^ Only print wich tasks need to be deployed
    }
    -- | Revert deployment, restore machine state
    | CommandRevert {
      deployHost   :: Text
    , deployPort   :: Int -- ^ ssh port
    , deployUser   :: Text
    }

-- | Extract remote host info from options
getRemoteHost :: DeployOptions -> RemoteHost
getRemoteHost DeployOptions{..} = case optionsCommand of
  CommandDeploy{..} -> RemoteHost deployHost deployPort deployUser
  CommandRevert{..} -> RemoteHost deployHost deployPort deployUser

-- | Execute program with given options
runDeployment :: DeployOptions -> (RemoteHost -> Task a) -> IO ()
runDeployment o@DeployOptions{..} mkBuildPlan = do
  let buildPlan = mkBuildPlan $ getRemoteHost o
  void $ keep' $ case optionsCommand of
    CommandDeploy{..} -> do
      shelly . echo $ "Deploy!"
      if deployDry then do
          infos <- dryRunTask buildPlan
          shelly . echo $ T.unlines $ (\(mn, b) -> fromMaybe "unnamed" mn <> " is " <> if b then "applied" else "not applied" ) <$> infos
        else void $ executeTask buildPlan
      shelly . echo $ "Finished!"
      -- stop :: TransIO ()
    CommandRevert{..} -> do
      reverseTask buildPlan
      stop
