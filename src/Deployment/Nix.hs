{-# LANGUAGE RecordWildCards #-}
module Deployment.Nix(
    DeployOptions(..)
  , Command(..)
  , RemoteHost(..)
  , NixBuildInfo(..)
  , runDeployment
  , getNixBuildInfo
  , getRemoteHost
  , module R
  -- * CLI helpers
  , deployOptionsParser
  , makeDeploymentCLI
  ) where

import Data.Foldable (traverse_)
import Data.Functor
import Data.Maybe
import Data.Monoid
import Data.Text (Text, pack, unpack)
import Options.Applicative
import Shelly hiding (command)
import Transient.Base hiding (option)

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
      deployHost          :: Text
    , deployPort          :: Int -- ^ ssh port
    , deployUser          :: Text
    , deployDry           :: Bool -- ^ Only print wich tasks need to be deployed
    , deployNixFile       :: Text -- ^ Path to .nix file with derivations that need to be deployed
    , deployNixSshConfig  :: Maybe Text -- ^ Path to ssh-config to use for nix-build
    }
    -- | Revert deployment, restore machine state
    | CommandRevert {
      deployHost          :: Text
    , deployPort          :: Int -- ^ ssh port
    , deployUser          :: Text
    , deployNixFile       :: Text -- ^ Path to .nix file with derivations that need to be deployed
    , deployNixSshConfig  :: Maybe Text -- ^ Path to ssh-config to use for nix-build
    }

-- | Extract nix build info from options
getNixBuildInfo :: DeployOptions -> NixBuildInfo
getNixBuildInfo DeployOptions{..} = case optionsCommand of
  CommandDeploy{..} -> NixBuildInfo (fromText deployNixFile) (fromText <$> deployNixSshConfig)
  CommandRevert{..} -> NixBuildInfo (fromText deployNixFile) (fromText <$> deployNixSshConfig)

-- | Extract remote host info from options
getRemoteHost :: DeployOptions -> RemoteHost
getRemoteHost DeployOptions{..} = case optionsCommand of
  CommandDeploy{..} -> RemoteHost deployHost deployPort deployUser
  CommandRevert{..} -> RemoteHost deployHost deployPort deployUser

-- | Execute program with given options
runDeployment :: DeployOptions -> Task a -> IO ()
runDeployment o@DeployOptions{..} buildPlan = do
  void $ keep' $ case optionsCommand of
    CommandDeploy{..} -> do
      if deployDry then do
          infos <- dryRunTask buildPlan
          shelly . echo $ T.unlines $ (\(mn, b) -> fromMaybe "unnamed" mn <> " is " <> if b then "applied" else "not applied" ) <$> infos
        else void $ executeTask buildPlan
    CommandRevert{..} -> reverseTask buildPlan

-- | Helper to parse text
textArgument :: Mod ArgumentFields String -> Parser Text
textArgument = fmap pack . strArgument

-- | Helper to parse text
textOption :: Mod OptionFields String -> Parser Text
textOption = fmap pack . strOption

-- | CLI parser
deployOptionsParser :: Parser DeployOptions
deployOptionsParser = DeployOptions
  <$> cliCommand
  where
    cliCommand = subparser $
         command "deploy" (info (deployCmd <**> helper) $ progDesc "Apply deployment to remote host")
      <> command "revert" (info (revertCmd <**> helper) $ progDesc "Reverse deployment at remote host")
    deployCmd = CommandDeploy
      <$> textArgument (
          metavar "MACHINE_IP"
        )
      <*> option auto (
           long "port"
        <> short 'p'
        <> metavar "DEPLOY_PORT"
        <> showDefault
        <> value 22
        <> help "Default SSH port"
      )
      <*> textOption (
           long "user"
        <> short 'u'
        <> metavar "DEPLOY_USER"
        <> showDefault
        <> value "root"
        <> help "Which user to deploy with"
        )
      <*> switch (
           long "dry"
        <> short 'd'
        <> help "Print steps to perform only"
        )
      <*> textArgument (
           metavar "NIX_FILE"
        <> showDefault
        <> value "./default.nix"
        <> help "Which .nix file to deploy"
        )
      <*> (optional . textOption) (
           long "nix-ssh-config"
        <> metavar "NIX_SSH_CONFIG"
        <> help "Which ssh config to use with nix-build"
        )
    revertCmd = CommandRevert
      <$> textArgument (
          metavar "MACHINE_IP"
        )
      <*> option auto (
           long "port"
        <> short 'p'
        <> metavar "DEPLOY_PORT"
        <> showDefault
        <> value 22
        <> help "Default SSH port"
      )
      <*> textOption (
           long "user"
        <> short 'u'
        <> metavar "DEPLOY_USER"
        <> showDefault
        <> value "root"
        <> help "Which user to deploy with"
        )
      <*> textArgument (
           metavar "NIX_FILE"
        <> showDefault
        <> value "./default.nix"
        <> help "Which .nix file to deploy"
        )
      <*> (optional . textOption) (
           long "nix-ssh-config"
        <> metavar "NIX_SSH_CONFIG"
        <> help "Which ssh config to use with nix-build"
        )

-- | Generate CLI application for deployment with given build plan
makeDeploymentCLI :: Parser a -- ^ CLI parser
  -> (a -> DeployOptions) -- ^ How to extract deploy options from the cli parser result
  -> (a -> Task ()) -- ^ Build plan
  -> IO ()
makeDeploymentCLI parser getOpts buildPlan = do
  a <- execParser parserInfo
  runDeployment (getOpts a) $ buildPlan a
  where
    parserInfo = info (parser <**> helper)
      ( fullDesc
     <> progDesc "Deploys the project with nix on non nixos machine"
     <> header "nixdeploy - deployment tool" )
