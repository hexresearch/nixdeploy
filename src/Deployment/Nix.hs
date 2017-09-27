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
  , defaultNixPlan
  ) where

import Data.Foldable (traverse_, for_)
import Data.Functor
import Data.Maybe
import Data.Monoid
import Data.Text (Text, pack, unpack)
import Options.Applicative
import Shelly hiding (command)
import System.FilePath (takeFileName)
import Transient.Base hiding (option)

import qualified Data.Text as T

import Deployment.Nix.Task as R
import Deployment.Nix.Task.Common as R

-- | CLI options
data DeployOptions = DeployOptions {
  deployCommand       :: Command
, deployHost          :: Text
, deployNixFile       :: Text -- ^ Path to .nix file with derivations that need to be deployed
, deployNixSshConfig  :: Maybe Text -- ^ Path to ssh-config to use for nix-build
, deployKeys          :: [Text] -- ^ Which ssh keys to add to ssh-agent
, deployKeysTimeout   :: Maybe Int -- ^ Number of seconds for `deployKeys` to expire
, deployPort          :: Int -- ^ ssh port
, deployUser          :: Text
, deployServices      :: [Text] -- ^ Names of derivations in `deployNixFile` that are symlinked to /etc/systemd/system
, deployTools         :: [Text] -- ^ Names of derivations in `deployNixFile` that are symlinked to /etc/systemd/system, but not enabled.
                                -- They are usually a one-shot systemd units for administrative tools.
, deployFolders       :: [Text] -- ^ Set of folders that we need to create on remote machine
, deployPostgres      :: Maybe Text -- ^ Path to deriviation with SQL init script
}

-- | Available CLI commands to perform
data Command =
    -- | Execute deployment tasks
    CommandDeploy {
      deployDry           :: Bool -- ^ Only print wich tasks need to be deployed
    }
    -- | Revert deployment, restore machine state
    | CommandRevert

-- | Extract nix build info from options
getNixBuildInfo :: DeployOptions -> NixBuildInfo
getNixBuildInfo DeployOptions{..} = NixBuildInfo (fromText deployNixFile) (fromText <$> deployNixSshConfig)

-- | Extract remote host info from options
getRemoteHost :: DeployOptions -> RemoteHost
getRemoteHost DeployOptions{..} = RemoteHost deployHost deployPort deployUser

-- | Execute program with given options
runDeployment :: DeployOptions -> Task a -> IO ()
runDeployment o@DeployOptions{..} buildPlan =
  void $ keep' $ case deployCommand of
    CommandDeploy{..} ->
      if deployDry then do
          infos <- dryRunTask buildPlan
          liftIO $ traverse_ (\(mn, b) -> echonColor White (fromMaybe "unnamed" mn <> " is ") >> if b then echoColor Green "applied" else echoColor Red "not applied" ) infos
        else void $ executeTask buildPlan
    CommandRevert -> reverseTask buildPlan

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
  <*> textArgument (
      metavar "MACHINE_IP"
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
  <*> (many . textOption) (
       long "key"
    <> metavar "SSH_PRIVATE_KEY_PATH"
    <> help "Path to encrypted private key that will be added to ssh-agent"
    )
  <*> (optional . option auto) (
       long "keys-timeout"
    <> metavar "INT_SECONDS"
    <> help "Number of seconds the ssh keys will expired after"
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
  <*> (many . textOption) (
       long "service"
    <> metavar "SERVICE_ATR_NAME"
    <> help "Derivation in .nix file that should be symlinked as systemd service"
    )
  <*> (many . textOption) (
       long "tool"
    <> metavar "SERVICE_ATR_NAME"
    <> help "Derivation in .nix file that should be symlinked as systemd service, but not enabled"
    )
  <*> (many . textOption) (
       long "folder"
    <> metavar "FOLDER_PATH"
    <> help "Folder on remote machine that we need to create if it is missing"
    )
  <*> (optional . textOption) (
       long "postgres"
    <> metavar "SQL_DERIVATION"
    <> help "If specified, install postgres and feed the given derivation from .nix file as init SQL script"
    )
  where
    cliCommand = subparser $
         command "deploy" (info (deployCmd <**> helper) $ progDesc "Apply deployment to remote host")
      <> command "revert" (info (revertCmd <**> helper) $ progDesc "Reverse deployment at remote host")
    deployCmd = CommandDeploy
      <$> switch (
           long "dry"
        <> short 'd'
        <> help "Print steps to perform only"
        )
    revertCmd = pure CommandRevert

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

-- | Transform `/nix/store/<hash>-<service_name>` to `<service_name>`
derivToServiceName :: Text -> Text
derivToServiceName = pack . dropWhile (/= '-') . takeFileName . unpack

-- | Helper to apply function only when the first argument is 'Just'
whenJust :: Applicative f => Maybe a -> (a -> f ()) -> f ()
whenJust Nothing _ = pure ()
whenJust (Just a) f = f a

-- | Plan to build nix project and deploy it on remote host
defaultNixPlan :: DeployOptions -> Task ()
defaultNixPlan opts@DeployOptions{..} = do
  let
    nixBuildInfo = getNixBuildInfo opts
    rh = getRemoteHost opts
    loadKeys = if null deployKeys
      then sshAgent deployKeysTimeout Nothing
      else traverse_ (sshAgent deployKeysTimeout . Just . fromText) deployKeys
  bracketReverse loadKeys $ do
    aptPackages rh ["curl"]
    let deployUser = "deploy"
    addUser rh deployUser
    installNix rh deployUser
    nixCreateProfile rh deployUser
    makeNixLinks rh deployUser
    dontReverse genNixSignKeys
    copyNixSignKeys rh
    copyDeploySshKeys rh deployUser
    derivs <- nixBuild nixBuildInfo
    liftShell "Print derivs" () $ mapM_ (echo . toTextIgnore) derivs
    nixCopyClosures rh deployUser derivs
    traverse_ (\f -> ensureRemoteFolder rh f "root") deployFolders
    whenJust deployPostgres $ \derivSqlName -> do
      derivSql <- nixExtractDeriv nixBuildInfo derivSqlName
      installPostgres rh derivSql
    for_ deployTools $ \serviceName -> do
      service <- nixExtractDeriv nixBuildInfo serviceName
      nixSymlinkService rh service serviceName False
    for_ deployServices $ \serviceName -> do
      service <- nixExtractDeriv nixBuildInfo serviceName
      nixSymlinkService rh service serviceName True
      restartRemoteService rh serviceName
