{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Deployment.Nix(
    DeployOptions(..)
  , Command(..)
  , RemoteHost(..)
  , NixBuildInfo(..)
  , runDeployment
  , getRemoteHost
  , module R
  -- * CLI helpers
  , deployOptionsParser
  , makeDeploymentCLI
  , defaultNixPlan
  ) where

import Control.Arrow (second)
import Control.Exception.Base (SomeException)
import Control.Monad.IO.Class
import Data.Aeson
import Data.Foldable (traverse_, for_)
import Data.Functor
import Data.Maybe
import Data.Monoid
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding
import Deployment.Nix.Config
import Options.Applicative
import Safe (headMay)
import Shelly hiding (command, exit)
import System.Directory (getHomeDirectory)
import System.Exit
import System.FilePath (takeFileName, takeDirectory)
import Transient.Base hiding (option)

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Deployment.Nix.Task as R
import Deployment.Nix.Task.Common as R

-- | CLI options
data DeployOptions = DeployOptions {
  deployCommand       :: Command
, deployNixFile       :: Text       -- ^ Path to .nix file with full description of deployment. Config is generated from the nix expression.
, deployNixArgs       :: [Text]     -- ^ Arguments that are passed to nix deployment file
, deployNixStrArgs    :: [Text]     -- ^ Arguments that are passed to nix deployment file as Nix string values (you haven't to escape quotes by triple back slash)
, deployNixSshConfig  :: Maybe Text -- ^ Path to ssh-config to use for nix-build
, deployDry           :: Bool       -- ^ Only print wich tasks need to be deployed
, deployVerbose       :: Bool       -- ^ Verbose output from shell
, deployForce         :: Bool       -- ^ When enabled, force all checks to apply tasks
, deployDontCheckIdentity :: Bool   -- ^ Tell SSH to not check identities (for local VM deploys only, very insecure)
}

-- | Available CLI commands to perform
data Command =
    -- | Execute deployment tasks
      CommandDeploy
    -- | Revert deployment, restore machine state
    | CommandRevert
    -- | Install nix infrastructure only
    | CommandNixify

-- | Extract remote host info from options
getRemoteHost :: DeployOptions -> MachineCfg -> RemoteHost
getRemoteHost DeployOptions{..} MachineCfg{..} = RemoteHost {
    remoteAddress = machineHost
  , remotePort = fromMaybe 22 machinePort
  , remoteUser = fromMaybe "root" machineUser
  , remoteDontCheckIdentity = deployDontCheckIdentity
  }

-- | Convert input arguments to pairs of kev value and escape if needed
parseNixArgs :: Monad m => DeployOptions -> m [(Text, Text)]
parseNixArgs DeployOptions{..} = do
  args <- traverse parseSingle deployNixArgs
  strargs <- traverse parseSingle deployNixStrArgs
  pure $ args <> fmap (second $ \v -> "\\\"" <> v <> "\\\"") strargs
  where
    parseSingle v = case T.splitOn ":" v of
      k : v : _ -> pure (k, v)
      _ -> fail $ "Argument " ++ unpack v ++ " is not in <key>:<value> format!"

-- | Get some value from global deployment config with overriding from local machine config
getDeploymentSmth :: a -> (DeploymentCfg -> Maybe a) -> Config -> MachineCfg -> a
getDeploymentSmth defVal getter Config{..} MachineCfg{..} = fromMaybe defVal $ (getter =<< machineDeployment) <|> (getter =<< configDeployment)

-- | Calculate user-owner of deployment files with overriding from local machine settings
getDeploymentUser :: Config -> MachineCfg -> UserName
getDeploymentUser = getDeploymentSmth "deploy" deploymentUser

-- | Calculate user-owner of deployment files with overriding from local machine settings
getDeploymentKeys :: MonadIO m => Config -> MachineCfg -> m [KeyPath]
getDeploymentKeys Config{..} MachineCfg{..} = do
  let mval = (deploymentKeys =<< machineDeployment) <|> (deploymentKeys =<< configDeployment)
  case mval of
    Just val -> pure val
    Nothing -> do
      home <- shelly $ T.filter (/= '\n') <$> bash "echo" ["$HOME"]
      pure [ConfigPath . unpack $ home <> "/.ssh/id_rsa"]

-- | Calculate user-owner of deployment files with overriding from local machine settings
getDeploymentKeysTimeout :: Config -> MachineCfg -> Int
getDeploymentKeysTimeout = getDeploymentSmth 300 deploymentKeysTimeout

-- | Transform nix file to desired config
loadConfig :: MonadIO m => DeployOptions -> m Config
loadConfig o@DeployOptions{..} = shelly $ do
  rawArgs <- parseNixArgs o
  let args = fmap (\(k, v) -> "--arg \"" <> k <> "\" \"" <> v <> "\"") rawArgs
      sshArg = maybe [] (\p -> ["-I ssh-config-file=" <> p]) deployNixSshConfig
  json <- bash "nix-instantiate" (["--read-write-mode", "--json", "--eval", "--strict"] <> sshArg <> args <> [deployNixFile])
  cfg <- case eitherDecode' . BSL.fromStrict . encodeUtf8 $ json of
    Left er -> fail $ "Failed when parsing NIX json output: " <> unpack json <> ". Error: " <> er
    Right v -> pure v
  let cfgDir = takeDirectory $ unpack deployNixFile
  absolutize cfgDir cfg

-- | Execute program with given options
runDeployment :: DeployOptions -> (Config -> Task ()) -> IO ()
runDeployment o@DeployOptions{..} buildPlan = do
  cfg@Config{..} <- loadConfig o
  let dryRun ma = do
        infos <- dryRunTask ma
        liftIO $ traverse_ (\(mn, b) -> echonColor White (fromMaybe "unnamed" mn <> " is ") >> if b then echoColor Green "applied" else echoColor Red "not applied" ) infos

  (maybe (pure ()) (const exitFailure) =<<) $ keep' $ do
    onException $ \(e :: SomeException) -> do
        liftIO $ putStrLn $ "Error: " <> show e
        exit ()
    setShellOptions ShellOptions {
        shellVerbose = deployVerbose
      }
    let bldPlan = buildPlan cfg
    case deployCommand of
      CommandDeploy ->
        if deployDry then dryRun bldPlan else void $ executeTask deployForce bldPlan
      CommandRevert -> if deployDry then dryRun bldPlan else reverseTask bldPlan
      CommandNixify -> let plan = defaultNixPlan True o cfg in if deployDry
        then dryRun plan else executeTask deployForce plan

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
       metavar "NIX_FILE"
    <> showDefault
    <> value "./deploy.nix"
    <> help "Path to .nix file with full description of deployment."
    )
  <*> (many . textOption) (
       long "arg"
    <> metavar "NIX_KEY_VALUE"
    <> help "Multiple arguments can be specified. Nix file argument in format 'key:value'. The arguments are passed to NIX_FILE via nix-instantiate --arg option."
    )
  <*> (many . textOption) (
       long "argstr"
    <> metavar "NIX_KEY_VALUE"
    <> help "Multiple arguments can be specified. Nix file argument in format 'key:value' with value of fixed Nix string type (you haven't to escape quotes by triple back slash) The arguments are passed to NIX_FILE via nix-instantiate --arg option."
    )
  <*> (optional . textOption) (
       long "nix-ssh-config"
    <> metavar "NIX_SSH_CONFIG"
    <> help "Which ssh config to use with NIX_FILE"
    )
  <*> switch (
       long "dry"
    <> short 'd'
    <> help "Print steps to perform only"
    )
  <*> switch (
       long "verbose"
    <> short 'v'
    <> help "Verbose output including from shell commands"
    )
  <*> switch (
       long "force"
    <> short 'f'
    <> help "Force all checks to 'need to apply'"
    )
  <*> switch (
       long "dont-check-identity"
    <> help "Tell SSH to not check identities (for local VM deploys only, very insecure)"
  )
  where
    cliCommand = subparser $
         command "deploy" (info (deployCmd <**> helper) $ progDesc "Apply deployment to remote host")
      <> command "revert" (info (revertCmd <**> helper) $ progDesc "Reverse deployment at remote host")
      <> command "nixify" (info (nixifyCmd <**> helper) $ progDesc "Install nix infrastructure on remote host")
    deployCmd = pure CommandDeploy
    revertCmd = pure CommandRevert
    nixifyCmd = pure CommandNixify

-- | Generate CLI application for deployment with given build plan
makeDeploymentCLI :: Parser a -- ^ CLI parser
  -> (a -> DeployOptions) -- ^ How to extract deploy options from the cli parser result
  -> (a -> Config -> Task ()) -- ^ Build plan
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
defaultNixPlan :: Bool -> DeployOptions -> Config -> Task ()
defaultNixPlan nixifyOnly opts@DeployOptions{..} cfg@Config{..} = do
  args <- parseNixArgs opts -- important to place before any task as this is fail without reverse
  derivs <- nixConfigDerivs args (fmap fromText deployNixSshConfig) (fromText deployNixFile)
  nixRelease (fmap fromText deployNixSshConfig) derivs
  let hosts = M.elems $ M.mapWithKey (\name MachineCfg{..} -> (machineHost, name)) configMachines
  for_ (M.toList configMachines) $ \(name, mcfg@MachineCfg{..}) -> do
    let
      rh = getRemoteHost opts mcfg
      deployUser = getDeploymentUser cfg mcfg
      keysTimeout = getDeploymentKeysTimeout cfg mcfg
    setMachineName name
    keys <- liftShell "Get keys local path" [] $ getDeploymentKeys cfg mcfg
    traverse_ (sshAgent $ Just keysTimeout) keys
    unless deployDontCheckIdentity $ liftShell "Add fingerprints" () $ do
      home <- liftIO getHomeDirectory
      addFingerprints rh $ fromText (pack home) <> ".ssh/known_hosts"
    nixify rh deployUser
    unless nixifyOnly $ do
      addHosts rh hosts
      nixCopyClosures rh (headMay keys) deployUser $ machineAllDerivations mcfg
      whenJust machineDirectories $ traverse_ (ensureRemoteFolder rh)
      whenJust machinePostgres $ installPostgres rh
      whenJust machineServices $ \services -> for_ (M.toList services) $ \(serviceName, ServiceCfg{..}) -> do
        let enabled = fromMaybe True serviceEnable
        nixSymlinkService rh serviceUnit serviceName enabled
        when enabled $ restartRemoteService rh serviceName
