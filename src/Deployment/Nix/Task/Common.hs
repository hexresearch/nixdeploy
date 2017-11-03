{-# LANGUAGE TemplateHaskell #-}
module Deployment.Nix.Task.Common(
    RemoteHost(..)
  , NixBuildInfo(..)
  , dontReverse
  , applyReverse
  , bracketReverse
  , sshAgent
  , withSshKeys
  , remoteHostTarget
  , shellRemoteSSH
  , aptPackages
  , addUser
  , installNix
  , makeNixLinks
  , raiseNixEnv
  , genNixSignKeys
  , copyNixSignKeys
  , copyDeploySshKeys
  , nixify
  , nixBuild
  , nixConfigDerivs
  , nixRelease
  , nixCopyClosures
  , nixCreateProfile
  , nixExtractDeriv
  , nixSymlinkService
  , restartRemoteService
  , ensureRemoteFolder
  , installPostgres
  , addHosts
  ) where

import Data.FileEmbed (embedStringFile)
import Data.Foldable (traverse_)
import Data.Functor
import Data.Maybe
import Data.Monoid
import Data.Text (Text, pack, unpack)
import Deployment.Nix.Config
import Deployment.Nix.Task
import Filesystem.Path (addExtensions)
import Prelude hiding (FilePath)
import Safe
import Shelly

import qualified Data.Text as T

-- | Holds data to connect to remote machine over SSH
data RemoteHost = RemoteHost {
  remoteAddress :: Text
, remotePort    :: Int
, remoteUser    :: Text
}

-- | Construct user@host
remoteHostTarget :: RemoteHost -> Text
remoteHostTarget RemoteHost{..} = remoteUser <> "@" <> remoteAddress

-- | Compact info needed to build nix project
data NixBuildInfo = NixBuildInfo {
  nixBuildFile :: FilePath
, nixBuildSshConfig :: Maybe FilePath
}

-- | Exec shell commands (concated via &&) on remote host via SSH
shellRemoteSSH :: RemoteHost -> [(FilePath, [Text])] -> Sh Text
shellRemoteSSH rh@RemoteHost{..} = sshPairsWithOptions (remoteHostTarget rh) ["-p " <> pack (show remotePort)]

-- | Copy file from local machine to remote host
remoteScpTo :: RemoteHost -> FilePath -> FilePath -> Sh ()
remoteScpTo rh@RemoteHost{..} from to = run_ "scp" ["-P " <> pack (show remotePort), toTextIgnore from, remoteHostTarget rh <> ":" <> toTextIgnore to]

-- | Don't actually reverse actions for the tasks
dontReverse :: Task a -> Task a
dontReverse t = case t of
  AtomTask{..} -> AtomTask {
      taskName = taskName
    , taskCheck = taskCheck
    , taskApply = taskApply
    , taskReverse = pure ()
    }
  TaskApplicative fa ta -> TaskApplicative (dontReverse fa) (dontReverse ta)
  TaskMonadic ta fa -> TaskMonadic (dontReverse ta) (fmap dontReverse fa)

-- | Apply reverse action of the task in normal (not backtracking) mode. Usefull
-- for cleanup.
applyReverse :: a -- ^ Value that will be returned after reverse script
  -> Task a -- ^ Task that contains reverse script
  -> Task a -- ^ Task that apply script is equals to reverse script and always passes check
applyReverse defVal t = case t of
  AtomTask{..} -> AtomTask {
      taskName = ("Reversed " <>) <$> taskName
    , taskCheck = case taskCheck of
        Left a -> Left a
        Right chk -> Right $ do
          (_, a) <- chk
          pure (True, a)
    , taskApply = taskReverse >> pure defVal
    , taskReverse = taskReverse
    }
  TaskApplicative fa ta -> TaskApplicative (dontReverse fa) (dontReverse ta)
  TaskMonadic ta fa -> TaskMonadic (dontReverse ta) (fmap dontReverse fa)

-- | Wrap with apply and reverse action of first tast the actions of second task
--
-- TODO: use exception safe bracket
bracketReverse :: Task a -> Task b -> Task b
bracketReverse ta tb = do
  a <- ta
  b <- tb
  applyReverse a ta
  pure b

-- | Start ssh-agent and add given ('Nothing' means default) key with given timeout
--
-- Need `openssh-askpass` to be installed in system.
sshAgent :: Maybe Int -> KeyPath -> Task ()
sshAgent mseconds key = AtomTask {
  taskName = Just $ "Adding key " <> pack (unConfigPath key) <> " to ssh-agent"
, taskCheck = Right $ transShell $ errExit False $ do
    hasAgent <- isSshAgentExist
    pubkeys <- T.lines <$> bash "ssh-add" ["-L"]
    pubkey <- readfile $ addExtensions (fromText . pack . unConfigPath $ key) ["pub"]
    let getPubHash = headMay . drop 1 . T.splitOn " "
        isKeyLoaded = case getPubHash pubkey of
          Nothing -> False
          Just v -> v `elem` catMaybes (fmap getPubHash pubkeys)
    pure (not hasAgent || not isKeyLoaded, ())
, taskApply = transShell $ do
    hasAgent <- isSshAgentExist
    unless hasAgent $ bash_ "eval" ["`ssh-agent`"]
    _ <- bash "ssh-add" $
         maybe [] (\s -> ["-t", pack $ show s]) mseconds
      <> [toTextArg . unConfigPath $ key]
    pure ()
, taskReverse = transShell $ errExit False $ do
    _ <- bash "ssh-add" $
      "-d" : (toTextArg . unConfigPath $ key) : [" > /dev/null 2>&1"]
    pure ()
}
  where
    isSshAgentExist = errExit False $ do
      _ <- bash "test" ["-z", "$SSH_AUTH_SOCK"]
      err1 <- lastExitCode
      pure $ err1 /= 0

-- | Wrap task with ssh-agent invocation and termination for getting access
-- for specified ssh keys
withSshKeys :: Maybe Int -> [KeyPath] -> Task a -> Task a
withSshKeys deployKeysTimeout deployKeys = bracketReverse loadKeys
  where
    loadKeys = traverse_ (sshAgent deployKeysTimeout) deployKeys

-- | Helper for switching to root after ssh login
sudo :: (FilePath, [Text]) -> (FilePath, [Text])
sudo (prog, args) = ("sudo", toTextArg prog : args)

-- | Helper for switching via sudo to another user
sudoFrom :: Text -> (FilePath, [Text]) -> (FilePath, [Text])
sudoFrom user (prog, args) = ("sudo", ["-i", "-u " <> user] ++ toTextArg prog : args)

-- | Install needed packages
aptPackages :: RemoteHost -> [Text] -> Task ()
aptPackages rh pkgs = AtomTask {
  taskName = Just $ "Installation of " <> T.intercalate ", " pkgs <> " via apt-get"
, taskCheck = Right $ transShell $ errExit False $ do
    reses <- traverse isNotInstalled pkgs
    pure (or reses, ())
, taskApply = void $ transShell $
    shellRemoteSSH rh [sudo ("apt-get", ["update"]), sudo ("apt-get", "install":"-y":pkgs)]
, taskReverse = transShell $ errExit False $ do
    _ <- shellRemoteSSH rh [sudo ("apt-get", "remove":"-y":pkgs)]
    pure ()
}
  where
    isNotInstalled name = do
      _ <- shellRemoteSSH rh [sudo ("dpkg", ["--get-selections | grep -q \"^" <> name <> "[[:space:]]*install$\""])]
      err <- lastExitCode
      pure (err /= 0)

-- | Check that folder on host exist and create if missing
-- TODO: reassign and recreate flags
ensureRemoteFolder :: RemoteHost -> DirectoryCfg -> Task ()
ensureRemoteFolder rh DirectoryCfg{..} = AtomTask {
  taskName = Just $ "Ensure folder " <> directoryPath <> " at " <> remoteAddress rh
, taskCheck = Right $ transShell $ errExit False $ do
    _ <- shellRemoteSSH rh [sudo ("ls", [directoryPath <> " > /dev/null 2>&1"])] -- TODO: check that is directory and own user
    err <- lastExitCode
    pure (err /= 0, ())
, taskApply = void $ transShell $ shellRemoteSSH rh $
     [ sudo ("mkdir", ["-p", directoryPath])]
  <> maybe [] (\user -> [ sudo ("chown", ["-R", user, directoryPath]) ]) directoryOwner
, taskReverse = pure ()
}

-- | Add user on remote host
addUser :: RemoteHost -> Text -> Task ()
addUser rh user = AtomTask {
  taskName = Just $ "Creation of user " <> user
, taskCheck = Right $ transShell $ errExit False $ do
    _ <- shellRemoteSSH rh [sudo ("id", ["-u", user <> " > /dev/null 2>&1"])]
    err <- lastExitCode
    pure (err /= 0, ())
, taskApply = void $ transShell $
    shellRemoteSSH rh [sudo ("useradd", ["-m", "-s /bin/bash", user])]
, taskReverse = transShell $ errExit False $ do
    _ <- shellRemoteSSH rh [sudo ("userdel", ["-r", user])]
    pure ()
}

-- | Install nix on remote machine
installNix :: RemoteHost -> Text -> Task ()
installNix rh deployUser = AtomTask {
  taskName = Just "Installation of nix package manager"
, taskCheck = Right $ transShell $ errExit False $ do
    _ <- shellRemoteSSH rh [raiseNixEnv deployUser, ("which", ["nix-build > /dev/null 2>&1"])]
    err <- lastExitCode
    pure (err /= 0, ())
, taskApply = void $ transShell $
    shellRemoteSSH rh [
        sudo ("mkdir", ["-m 0755", "/nix"])
      , sudo ("chown", [deployUser, "-R", "/nix"])
      , sudoFrom deployUser ("curl", ["https://nixos.org/nix/install", "-o /tmp/install_nix.sh"])
      , sudoFrom deployUser ("sh", ["/tmp/install_nix.sh"])]
, taskReverse = transShell $ errExit False $ do
    _ <- shellRemoteSSH rh [
        sudo ("rm", ["-rf", "/nix"])
      , sudoFrom deployUser ("rm", ["-rf", "~/.nix-*"])]
    pure ()
}

-- | Make symlinks to /usr/bin to important nix binaries after installation, so
-- we can execute nic-copy-closures.
makeNixLinks :: RemoteHost -> Text -> Task ()
makeNixLinks rh deployUser = AtomTask {
  taskName = Just $ "Making global nix symlinks from user " <> deployUser
, taskCheck = Right $ transShell $ errExit False $ do
    isStore <- checkFile "/usr/bin/nix-store"
    pure (not isStore, ())
, taskApply = void $ transShell $
    shellRemoteSSH rh [sudo ("ln", ["-s", "/home/" <> deployUser <> "/.nix-profile/bin/nix-store", "/usr/bin/nix-store"])]
, taskReverse = transShell $ errExit False $ do
    _ <- shellRemoteSSH rh [sudo ("rm", ["-f", "/usr/bin/nix-store"])]
    pure ()
}
  where
    checkFile path = do
      _ <- shellRemoteSSH rh [sudo ("ls", [path <> " > /dev/null 2>&1"])]
      err <- lastExitCode
      pure $ err == 0

-- | Helper to bring nix environment in scope
raiseNixEnv :: Text -> (FilePath, [Text])
raiseNixEnv deployUser = ("source", ["/home/" <> deployUser <> "/.nix-profile/etc/profile.d/nix.sh"])

-- | Generates local signin keys. Consider usage of `dontReverse` to not remove
-- your signin keys accidentally. Requires sudo.
genNixSignKeys :: Task ()
genNixSignKeys = AtomTask {
  taskName = Just "Generate local signin keys for closures"
, taskCheck = Right $ transShell $ errExit False $ do
    privateExist <- test_f "/etc/nix/signing-key.sec"
    publicExist <- test_f "/etc/nix/signing-key.pub"
    pure (not privateExist || not publicExist, ())
, taskApply = transShell $ escaping False $ do
    bash_ "sudo mkdir -p /etc/nix || true" []
    bash_ "(umask 277 && sudo openssl genrsa -out /etc/nix/signing-key.sec 2048 && sudo chown $(whoami) /etc/nix/signing-key.sec)" []
    bash_ "sudo openssl rsa -in /etc/nix/signing-key.sec -pubout -out /etc/nix/signing-key.pub" []
, taskReverse = transShell $ errExit False $
    bash_ "sudo" ["rm", "-f", "/etc/nix/signing-key.sec", "/etc/nix/signing-key.pub"]
}

-- | Transfer nix signing keys to remote host
copyNixSignKeys :: RemoteHost -> Task ()
copyNixSignKeys rh = AtomTask {
  taskName = Just $ "Copy signing keys to " <> remoteAddress rh
, taskCheck = Right $ transShell $ errExit False $ do
    _ <- shellRemoteSSH rh [sudo ("ls", [keyPos <> " > /dev/null 2>&1"])]
    err <- lastExitCode
    pure (err /= 0, ())
, taskApply = transShell $ do
    _ <- errExit False $ shellRemoteSSH rh [sudo ("mkdir", ["-p", "/etc/nix"])]
    let tmpKeyPos = "~/signing-key.pub"
    remoteScpTo rh (fromText keyPos) (fromText tmpKeyPos)
    _ <- shellRemoteSSH rh [sudo ("mv", [tmpKeyPos, keyPos])]
    pure ()
, taskReverse = transShell $ errExit False $ do
    _ <- shellRemoteSSH rh [sudo ("rm", ["-f", keyPos])]
    pure ()
}
  where keyPos = "/etc/nix/signing-key.pub"

-- | Build .nix file with `nix-build` with optional path to ssh-config-file.
-- Returns list of deriviations that were built.
nixBuild :: NixBuildInfo -> Task [FilePath]
nixBuild NixBuildInfo{..} = AtomTask {
  taskName = Just $ "Local nix-build of " <> toTextIgnore nixBuildFile
, taskCheck = Left []
, taskApply = transShell $ do
    nixFileText <- toTextWarn nixBuildFile
    bash_ "nix-build" [maybe "" (("-I ssh-config-file=" <>) . toTextIgnore) nixBuildSshConfig, nixFileText]
    fmap fromText . T.lines <$> bash "nix-env" ["-qa", "--no-name", "--out-path", "-f " <> nixFileText]
, taskReverse = pure ()
}

-- | Build derivations with nix-store --release
nixRelease :: Maybe FilePath -> [DerivationPath] -> Task ()
nixRelease mSshConf derivs = AtomTask {
  taskName = Just "Local build of derivations from nix file"
, taskCheck = Left ()
, taskApply = transShell $ do
    mSshConf' <- traverse toTextWarn mSshConf
    bash_ "nix-store" $ "-r" : maybe "" ("-I ssh-config-file=" <>) mSshConf' : derivs
    pure ()
, taskReverse = pure ()
}

-- | Embed helper Nix expression to extract list of derivations from configuration
getDerivFunc :: Text
getDerivFunc = $(embedStringFile "data/get-derivs.nix")

-- | Extract list of derivations (`.drv` files) that can be passed into `nixRelease` to build them. Input nix file
-- should follow schema of deployment config.
--
-- Example of generated command:
-- > nix-instantiate --argstr machine1 52.28.230.199 --argstr sshUser ubuntu -E "args@{machine1,sshUser}: let deploy = import ./deploy.nix; get-derivs = import ../../data/get-derivs.nix; pkgs = import <nixpkgs> {}; in if pkgs.lib.isFunction deploy then get-derivs (deploy args) else get-derivs deploy"
nixConfigDerivs :: [(Text, Text)] -- ^ Key valud for `--arg` argument to nix-instantiate
  -> Maybe FilePath -- ^ Path to ssh-config
  -> FilePath -- ^ Path to deployment nix description
  -> Task [DerivationPath] -- ^ List of derivations to build with `nixRelease`
nixConfigDerivs args mSshConf deploy = liftShell ("Extract configuration derivations from " <> toTextIgnore deploy) [] $ do
  mSshConf' <- traverse toTextWarn mSshConf
  deploy' <- toTextWarn deploy
  let sshArg = maybe "" ("-I ssh-config-file=" <>) mSshConf'
      args' = fmap (\(k,v) -> "--arg \"" <> k <> "\" \"" <> v <> "\"") args
      expr = T.unlines [
          "-E \"args@{" <> T.intercalate "," (fmap fst args) <> "}:"
        , "let deploy = (import " <> deploy' <> "); "
        , "    get-derivs = (" <> getDerivFunc <> "); "
        , "pkgs = import <nixpkgs> {}; "
        , "in if pkgs.lib.isFunction deploy then get-derivs (deploy args) else get-derivs deploy"
        , "\"" ]

  output <- bash "nix-instantiate" $ [sshArg] <> args' <> [expr]
  pure $ T.lines output

-- | Copy ssh keys from ssh'ing user to given deploy user, so we can envoke nix-copy-closure
copyDeploySshKeys :: RemoteHost -> Text -> Task ()
copyDeploySshKeys rh deployUser = AtomTask {
  taskName = Just $ "Copy SSH keys for user " <> deployUser
, taskCheck = Left ()
, taskApply = transShell $ do
    let home user = if user == "root" then "/root" else "/home/" <> user
        sshDir = home deployUser <> "/.ssh"
    _ <- shellRemoteSSH rh [
        sudo ("cp", ["-r", home (remoteUser rh) <> "/.ssh", sshDir])
      , sudo ("chown", [deployUser, "-R", sshDir])
      , sudo ("chgrp", [deployUser, "-R", sshDir])
      , sudo ("chmod", ["o-w", home deployUser])
      , sudo ("chmod", ["700", sshDir])
      , sudo ("chmod", ["600", sshDir <> "/authorized_keys"])]
    pure ()
, taskReverse = pure ()
}

-- | Sign, copy and install nix closures on remote host, need ability to ssh to deployUser
nixCopyClosures :: RemoteHost -> Maybe KeyPath -> Text -> [DerivationPath] -> Task ()
nixCopyClosures rh mkey deployUser closures = AtomTask {
    taskName = Just $ "Copy closures to " <> remoteAddress rh
  , taskCheck = Left ()
  , taskApply = transShell $ do
      let keyArg = maybe "" (\key -> " -i \"" <> (pack . unConfigPath $ key) <> "\"") mkey
      let sshOpts = "NIX_SSHOPTS='-p " <> pack (show $ remotePort rh) <> keyArg <> "'"
      _ <- escaping False $ run_ (fromText sshOpts) $ ["nix-copy-closure", "--sign", "--gzip", "--to", remoteHostTarget rh { remoteUser = deployUser }]  ++ fmap toTextArg closures
      let installProfile closure = sudoFrom deployUser ("nix-env", ["-p /opt/deploy/profile", "-i", toTextArg closure])
      _ <- shellRemoteSSH rh $ raiseNixEnv deployUser : fmap installProfile closures
      pure ()
  , taskReverse = transShell $ errExit False $ do
      let removeProfile closure = sudoFrom deployUser ("nix-env", ["nix-env", "-p /opt/deploy/profile", "-e", toTextArg closure])
      _ <- shellRemoteSSH rh (raiseNixEnv deployUser : fmap removeProfile closures)
      pure ()
  }

-- | Create nix profile for remote user
nixCreateProfile :: RemoteHost -> Text -> Task ()
nixCreateProfile rh deployUser = AtomTask {
    taskName = Just $ "Create nix profile for " <> deployUser <> " at " <> remoteAddress rh
  , taskCheck = Right $ transShell $ errExit False $ do
      _ <- shellRemoteSSH rh [sudo ("ls", ["/opt/" <> deployUser <> " > /dev/null 2>&1"])]
      err <- lastExitCode
      pure (err /= 0, ())
  , taskApply = transShell $ do
      let profileDir = "/opt/" <> deployUser
      _ <- shellRemoteSSH rh [
          sudo ("mkdir", ["-p", profileDir])
        , sudo ("chown", ["-R", deployUser, profileDir])
        , sudo ("ln", ["-s", profileDir <> "/profile", "/nix/var/nix/gcroots/" <> deployUser])
        ]
      pure ()
  , taskReverse = transShell $ errExit False $ do
      _ <- shellRemoteSSH rh [sudo ("rm", ["-rf", "/opt/" <> deployUser, "/nix/var/nix/gcroots/" <> deployUser])]
      pure ()
  }

-- | Extract service derivation from nix file
nixExtractDeriv :: NixBuildInfo -> Text -> Task Text
nixExtractDeriv NixBuildInfo{..} name = AtomTask {
    taskName = Just $ "Get deriviation for " <> name
  , taskCheck = Right $ transShell $ do
      res <- extract
      pure (False, T.filter (\c -> c /= '\r' && c /= '\n') res)
  , taskApply = transShell extract
  , taskReverse = pure ()
  }
  where
    extract = do
      nixFileText <- toTextWarn nixBuildFile
      bash "nix-build" [maybe "" (("-I ssh-config-file=" <>) . toTextIgnore) nixBuildSshConfig, nixFileText, "-A " <> name]

-- | Symlink given derivations to systemd folder
nixSymlinkService :: RemoteHost -> Text -> Text -> Bool -> Task ()
nixSymlinkService rh deriv serviceName enable = AtomTask {
    taskName = Just $ "Symlink systemd service " <> serviceName <> " at " <> remoteAddress rh
  , taskCheck = Left ()
  , taskApply = transShell $ do
      _ <- errExit False $ shellRemoteSSH rh [sudo ("rm", ["-f", servicePath])]
      _ <- shellRemoteSSH rh $
        [ sudo ("cp", [deriv, servicePath])]
        ++ if enable then [sudo ("systemctl", ["enable", serviceName <> ".service"])] else []
        ++ [sudo ("systemctl", ["daemon-reload"])]
      pure ()
  , taskReverse = transShell $ errExit False $ do
      _ <- shellRemoteSSH rh $
           if enable then [sudo ("systemctl", ["disable", serviceName <> ".service"])] else []
        ++ [sudo ("rm", ["-f", servicePath])]
      pure ()
  }
  where
    servicePath = "/etc/systemd/system/" <> serviceName <> ".service"

-- | Restart given remote service with given name
restartRemoteService :: RemoteHost -> Text -> Task ()
restartRemoteService rh serviceName = AtomTask {
    taskName = Just $ "Restart systemd service " <> serviceName <> " at " <> remoteAddress rh
  , taskCheck = Left ()
  , taskApply = transShell $ do
      _ <- shellRemoteSSH rh [sudo ("systemctl", ["restart", serviceName])]
      pure ()
  , taskReverse = pure ()
  }

-- | Ensure that postgres is installed and init it with the given script (derivation path)
installPostgres :: RemoteHost -> Text -> Task ()
installPostgres rh derivSql = AtomTask {
  taskName = Just $ "Init postgresql at " <> remoteAddress rh
, taskCheck = Right $ transShell $ errExit False $ do
    reses <- isNotInstalled "postgresql"
    pure (reses, ())
, taskApply = void $ transShell $
    shellRemoteSSH rh [
        sudo ("apt-get", ["install", "-y", "postgresql", "postgresql-contrib"])
      , sudo ("systemctl", ["enable", "postgresql"])
      , sudo ("systemctl", ["start", "postgresql"])
      , sudoFrom "postgres" ("psql", ["-f " <> derivSql])]
, taskReverse = transShell $ errExit False $ do
    _ <- shellRemoteSSH rh [sudo ("apt-get", ["remove", "-y", "postgresql"])]
    pure ()
}
  where
    isNotInstalled name = do
      _ <- shellRemoteSSH rh [sudo ("dpkg", ["--get-selections | grep -q \"^" <> name <> "[[:space:]]*install$\""])]
      err <- lastExitCode
      pure (err /= 0)

-- | Add the entries to remote /etc/hosts file
addHosts :: RemoteHost -> [(Text, Text)] -> Task ()
addHosts rh newHosts = AtomTask {
  taskName = Just $ "Modify /etc/hosts at " <> remoteAddress rh
, taskCheck = Right $ transShell $ errExit False $ do
    reses <- traverse hasHost $ fmap snd newHosts
    pure (and . fmap not $ reses, ())
, taskApply = void $ transShell $ do
    let cnt = T.intercalate "\n" $ (\(k, v) -> k <> "  " <> v) <$> newHosts
    hosts <- shellRemoteSSH rh [("cat", ["/etc/hosts"])]
    withTmpDir $ \dir -> do
      let tmpFile = dir </> fromText "hosts"
      writefile tmpFile $ hosts <> cnt <> "\n"
      remoteScpTo rh tmpFile "hosts"
      _ <- shellRemoteSSH rh [sudo ("mv", ["hosts", "/etc/hosts"])]
      pure ()
, taskReverse = transShell $ errExit False $ traverse_ removeHost $ fmap snd newHosts
}
  where
    hasHost h = do
      _ <- shellRemoteSSH rh [("grep", ["-q", h, "/etc/hosts"])]
      err <- lastExitCode
      pure $ err == 0
    removeHost h = do
      _ <- shellRemoteSSH rh [sudo ("sed", ["-i", "/"<>h<>"/d", "/etc/hosts"])]
      pure ()

-- | Helper that holds tasks for installing nix infrastructure on remote host
--
-- This includes:
-- * User setup
-- * Sign keys
-- * Nix installation
-- * Symlinks for nix binaries
nixify :: RemoteHost -> Text -> Task ()
nixify rh deployUser = do
  aptPackages rh ["curl", "bzip2"]
  let deployUser = "deploy"
  addUser rh deployUser
  installNix rh deployUser
  nixCreateProfile rh deployUser
  makeNixLinks rh deployUser
  dontReverse genNixSignKeys
  copyNixSignKeys rh
  copyDeploySshKeys rh deployUser
