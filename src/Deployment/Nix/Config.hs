-- | Define JSON schema for configuration file that is generated from the nix description
-- of deployment.
{-# LANGUAGE TemplateHaskell #-}
module Deployment.Nix.Config(
  -- * Helper types
    MachineName
  , MachineHost
  , MachinePort
  , ServiceName
  , UserName
  , KeyPath
  , AbsolutePath
  , DerivationPath
  -- * Config types
  , Backend(..)
  , DeploymentCfg(..)
  , DirectoryCfg(..)
  , ServiceCfg(..)
  , MachineCfg(..)
  , machineAllDerivations
  , Config(..)
  -- * Config relative path
  , ConfigPath(..)
  , absolutize
  ) where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.TH
import Data.Data
import Data.Generics.Uniplate.Data
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import Data.String
import Data.Text (Text)
import Deployment.Nix.Aeson
import GHC.Generics
import System.Directory
import System.FilePath

-- | Name of deployment target machine
type MachineName = Text

-- | IP of hostname of target machine
type MachineHost = Text

-- | Port number of target machine
type MachinePort = Int

-- | Name of systemd unit
type ServiceName = Text

-- | Name of system user
type UserName = Text

-- | Path to SSH key relative to config file location
type KeyPath = ConfigPath

-- | Absolute path to file or directory
type AbsolutePath = Text

-- | Full path to NIX derivation in /nix/store
type DerivationPath = AbsolutePath

-- | Path that is relative to config file location. Transforms via uniplate in
-- 'absolutize' function.
newtype ConfigPath = ConfigPath { unConfigPath :: FilePath }
  deriving (Eq, Ord, Show, Read, Generic, Data, FromJSON, ToJSON)

instance IsString ConfigPath where
  fromString = ConfigPath

-- | Transform all 'SitePath' to absolute path with given prefix
absolutize :: (Uniplate a, Data a, MonadIO m) => FilePath -> a -> m a
absolutize prefix a =  liftIO $ do
  p <- canonicalizePath prefix
  transformBiM (mkAbs p) a
  where
    mkAbs :: FilePath -> ConfigPath -> IO ConfigPath
    mkAbs pref (ConfigPath p) = pure . ConfigPath $ if isAbsolute p then p else pref </> p

-- | Enumeration of supported backends
data Backend = Ubuntu | Debian
  deriving (Eq, Ord, Show, Read, Generic, Data)
deriveJSON defaultOptions { unwrapUnaryRecords = True } ''Backend

-- | Generic options that defines deployment options for each machine
data DeploymentCfg = DeploymentCfg {
  deploymentUser        :: !(Maybe UserName)  -- ^ Which user will own profile and /nix storage
, deploymentKeys        :: !(Maybe [KeyPath]) -- ^ Pathes to SSH keys to use for deployment
, deploymentKeysTimeout :: !(Maybe Int) -- ^ Number of seconds for `deploymentKeys` to expire
} deriving (Eq, Show, Read, Generic, Data)
deriveJSON dropPrefixOptions ''DeploymentCfg

-- | Defines directory that should be created (if missing) on remote machine
data DirectoryCfg = DirectoryCfg {
  directoryPath     :: !AbsolutePath      -- ^ Path to directory
, directoryOwner    :: !(Maybe UserName)  -- ^ Name of user that would be owner of the directory
, directoryReassign :: !(Maybe Bool)      -- ^ Change owner of directory recusively to the `directoryOwner` if directory is created
, directoryRecreate :: !(Maybe Bool)      -- ^ If set existing folder is erased and new one is created
} deriving (Eq, Show, Read, Generic, Data)
deriveJSON dropPrefixOptions ''DirectoryCfg

-- | Defines systemd service config
data ServiceCfg = ServiceCfg {
  serviceEnable :: !(Maybe Bool)   -- ^ Is the service enabled? True by default
, serviceUnit   :: !DerivationPath -- ^ Path to unit file that would be copied
} deriving (Eq, Show, Read, Generic, Data)
deriveJSON dropPrefixOptions ''ServiceCfg

-- | Deployment description of a single machine
data MachineCfg = MachineCfg {
  machineDeployment  :: !(Maybe DeploymentCfg)                -- ^ Overrides of shared deployment options
, machineBackend     :: !(Maybe Backend)                      -- ^ Which internal backend to use
, machineDerivations :: !(Maybe [DerivationPath])             -- ^ Additional derivations to copy
, machineDirectories :: !(Maybe [DirectoryCfg])               -- ^ Folders to create on remote machine
, machineHost        :: !MachineHost                          -- ^ Machine network address
, machinePort        :: !(Maybe MachinePort)                  -- ^ Machine SSH port
, machineUser        :: !(Maybe UserName)                     -- ^ Machine SSH user that has passwordless sudo
, machineServices    :: !(Maybe (Map ServiceName ServiceCfg)) -- ^ Systemd services that should be activated
, machinePostgres    :: !(Maybe DerivationPath)               -- ^ Install system Postgres and run the init script
} deriving (Eq, Show, Read, Generic, Data)
deriveJSON dropPrefixOptions ''MachineCfg

-- | Get list of all deriviations that should be copied on remote machine
machineAllDerivations :: MachineCfg -> [DerivationPath]
machineAllDerivations MachineCfg{..} = fromMaybe [] machineDerivations
  <> maybe [] (fmap serviceUnit . M.elems) machineServices
  <> maybe [] pure machinePostgres

-- | Config file that is generated from the nix description of deployment.
data Config = Config {
  configDeployment :: !(Maybe DeploymentCfg)
, configMachines   :: !(Map MachineName MachineCfg)
} deriving (Eq, Show, Read, Generic, Data)
deriveJSON dropPrefixOptions ''Config
