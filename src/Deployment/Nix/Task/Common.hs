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
  , nixCopyClosures
  , nixCreateProfile
  , nixExtractDeriv
  , nixSymlinkService
  , restartRemoteService
  , ensureRemoteFolder
  , installPostgres
  ) where

import Data.Foldable (traverse_)
import Data.Functor
import Data.Monoid
import Data.Text (Text, pack, unpack)
import Deployment.Nix.Task
import Prelude hiding (FilePath)
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
    , taskCheck = do
        (_, a) <- taskCheck
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
sshAgent :: Maybe Int -> Maybe FilePath -> Task ()
sshAgent mseconds mkey = AtomTask {
  taskName = Just $ "Adding key " <> maybe "id_rsa" toTextArg mkey <> " to ssh-agent"
, taskCheck = shelly $ errExit False $ do
    home <- fromText . T.filter (/= '\n') <$> bash "echo" ["$HOME"]
    hasAgent <- isSshAgentExist
    pubkeys <- T.lines <$> bash "ssh-add" ["-L"]
    pubkey <- readfile $ maybe (home <> ".ssh/id_rsa.pub") (<> ".pub") mkey
    let isKeyLoaded = pubkey `elem` pubkeys
    pure (not hasAgent || not isKeyLoaded, ())
, taskApply = shelly $ do
    hasAgent <- isSshAgentExist
    unless hasAgent $ bash_ "eval" ["`ssh-agent`"]
    _ <- bash "ssh-add" $
         maybe [] (\s -> ["-t", pack $ show s]) mseconds
      <> maybe [] (\p -> [toTextArg p]) mkey
    pure ()
, taskReverse = shelly $ errExit False $ do
    _ <- bash "ssh-add" $
      ["-d"] <> maybe [] (\p -> [toTextArg p]) mkey <> [" > /dev/null 2>&1"]
    pure ()
}
  where
    isSshAgentExist = errExit False $ do
      _ <- bash "test" ["-z", "$SSH_AUTH_SOCK"]
      err1 <- lastExitCode
      pure $ err1 == 0

-- | Wrap task with ssh-agent invocation and termination for getting access
-- for specified ssh keys
withSshKeys :: Maybe Int -> [Text] -> Task a -> Task a
withSshKeys deployKeysTimeout deployKeys = bracketReverse loadKeys
  where
    loadKeys = if null deployKeys
      then sshAgent deployKeysTimeout Nothing
      else traverse_ (sshAgent deployKeysTimeout . Just . fromText) deployKeys

-- | Install needed packages
aptPackages :: RemoteHost -> [Text] -> Task ()
aptPackages rh pkgs = AtomTask {
  taskName = Just $ "Installation of " <> T.intercalate ", " pkgs <> " via apt-get"
, taskCheck = shelly $ errExit False $ do
    reses <- traverse isNotInstalled pkgs
    pure (or reses, ())
, taskApply = void $ shelly $
    shellRemoteSSH rh [("apt-get", ["update"]), ("apt-get", "install":"-y":pkgs)]
, taskReverse = shelly $ errExit False $ do
    _ <- shellRemoteSSH rh [("apt-get", "remove":"-y":pkgs)]
    pure ()
}
  where
    isNotInstalled name = do
      _ <- shellRemoteSSH rh [("dpkg", ["--get-selections | grep -q \"^" <> name <> "[[:space:]]*install$\""])]
      err <- lastExitCode
      pure (err /= 0)

-- | Check that folder on host exist and create if missing
ensureRemoteFolder :: RemoteHost -> Text -> Text -> Task ()
ensureRemoteFolder rh folder user = AtomTask {
  taskName = Just $ "Ensure folder " <> folder <> " at " <> remoteAddress rh
, taskCheck = shelly $ errExit False $ do
    _ <- shellRemoteSSH rh [("ls", [folder <> " > /dev/null 2>&1"])] -- TODO: check that is directory and own user
    err <- lastExitCode
    pure (err /= 0, ())
, taskApply = void $ shelly $ shellRemoteSSH rh [("mkdir", ["-p", folder]), ("chown", ["-R", user, folder])]
, taskReverse = pure ()
}

-- | Add user on remote host
addUser :: RemoteHost -> Text -> Task ()
addUser rh user = AtomTask {
  taskName = Just $ "Creation of user " <> user
, taskCheck = shelly $ errExit False $ do
    _ <- shellRemoteSSH rh [("id", ["-u", user <> " > /dev/null 2>&1"])]
    err <- lastExitCode
    pure (err /= 0, ())
, taskApply = void $ shelly $
    shellRemoteSSH rh [("useradd", ["-m", "-s /bin/bash", user])]
, taskReverse = shelly $ errExit False $ do
    _ <- shellRemoteSSH rh [("userdel", ["-r", user])]
    pure ()
}

-- | Install nix on remote machine
installNix :: RemoteHost -> Text -> Task ()
installNix rh deployUser = AtomTask {
  taskName = Just "Installation of nix package manager"
, taskCheck = shelly $ errExit False $ do
    _ <- shellRemoteSSH rh [raiseNixEnv deployUser, ("which", ["nix-build > /dev/null 2>&1"])]
    err <- lastExitCode
    pure (err /= 0, ())
, taskApply = void $ shelly $
    shellRemoteSSH rh [
        ("mkdir", ["-m 0755", "/nix"])
      , ("chown", [deployUser, "/nix"])
      , ("cd", ["/home/deploy"])
      , ("sudo", ["-i", "-u " <> deployUser, "curl", "https://nixos.org/nix/install", "-o /tmp/install_nix.sh"])
      , ("sudo", ["-i", "-u " <> deployUser, "sh", "/tmp/install_nix.sh"])]
, taskReverse = shelly $ errExit False $ do
    _ <- shellRemoteSSH rh [("rm", ["-rf", "/nix", "~/.nix-*"])]
    pure ()
}

-- | Make symlinks to /usr/bin to important nix binaries after installation, so
-- we can execute nic-copy-closures.
makeNixLinks :: RemoteHost -> Text -> Task ()
makeNixLinks rh deployUser = AtomTask {
  taskName = Just $ "Making global nix symlinks from user " <> deployUser
, taskCheck = shelly $ errExit False $ do
    isStore <- checkFile "/usr/bin/nix-store"
    pure (not isStore, ())
, taskApply = void $ shelly $
    shellRemoteSSH rh [("ln", ["-s", "/home/" <> deployUser <> "/.nix-profile/bin/nix-store", "/usr/bin/nix-store"])]
, taskReverse = shelly $ errExit False $ do
    _ <- shellRemoteSSH rh [("rm", ["-f", "/usr/bin/nix-store"])]
    pure ()
}
  where
    checkFile path = do
      _ <- shellRemoteSSH rh [("ls", [path <> " > /dev/null 2>&1"])]
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
, taskCheck = shelly $ errExit False $ do
    privateExist <- test_f "/etc/nix/signing-key.sec"
    publicExist <- test_f "/etc/nix/signing-key.pub"
    pure (not privateExist || not publicExist, ())
, taskApply = shelly $ escaping False $ do
    bash_ "sudo mkdir -p /etc/nix || true" []
    bash_ "(umask 277 && sudo openssl genrsa -out /etc/nix/signing-key.sec 2048 && sudo chown $(whoami) /etc/nix/signing-key.sec)" []
    bash_ "sudo openssl rsa -in /etc/nix/signing-key.sec -pubout -out /etc/nix/signing-key.pub" []
, taskReverse = shelly $ errExit False $
    bash_ "sudo" ["rm", "-f", "/etc/nix/signing-key.sec", "/etc/nix/signing-key.pub"]
}

-- | Transfer nix signing keys to remote host
copyNixSignKeys :: RemoteHost -> Task ()
copyNixSignKeys rh = AtomTask {
  taskName = Just $ "Copy signing keys to " <> remoteAddress rh
, taskCheck = shelly $ errExit False $ do
    _ <- shellRemoteSSH rh [("ls", [keyPos <> " > /dev/null 2>&1"])]
    err <- lastExitCode
    pure (err /= 0, ())
, taskApply = shelly $ do
    _ <- errExit False $ shellRemoteSSH rh [("mkdir", ["-p", "/etc/nix"])]
    remoteScpTo rh (fromText keyPos) (fromText keyPos)
, taskReverse = shelly $ errExit False $ do
    _ <- shellRemoteSSH rh [("rm", ["-f", keyPos])]
    pure ()
}
  where keyPos = "/etc/nix/signing-key.pub"

-- | Build .nix file with `nix-build` with optional path to ssh-config-file.
-- Returns list of deriviations that were built.
nixBuild :: NixBuildInfo -> Task [FilePath]
nixBuild NixBuildInfo{..} = AtomTask {
  taskName = Just $ "Local nix-build of " <> toTextIgnore nixBuildFile
, taskCheck = pure (True, [])
, taskApply = shelly $ do
    nixFileText <- toTextWarn nixBuildFile
    bash_ "nix-build" [maybe "" (("-I ssh-config-file=" <>) . toTextIgnore) nixBuildSshConfig, nixFileText]
    fmap fromText . T.lines <$> bash "nix-env" ["-qa", "--no-name", "--out-path", "-f " <> nixFileText]
, taskReverse = pure ()
}
  where keyPos = "/etc/nix/signing-key.pub"

-- | Copy ssh keys from ssh'ing user to given deploy user, so we can envoke nix-copy-closure
copyDeploySshKeys :: RemoteHost -> Text -> Task ()
copyDeploySshKeys rh deployUser = AtomTask {
  taskName = Just $ "Copy SSH keys for user " <> deployUser
, taskCheck = pure (True, ())
, taskApply = shelly $ do
    _ <- shellRemoteSSH rh [("cp", ["-r", "~/.ssh", "/home/" <> deployUser <> "/.ssh"])
      , ("chown", [deployUser, "-R", "/home/" <> deployUser <> "/.ssh"])]
    pure ()
, taskReverse = pure ()
}

-- | Sign, copy and install nix closures on remote host, need ability to ssh to deployUser
nixCopyClosures :: RemoteHost -> Text -> [FilePath] -> Task ()
nixCopyClosures rh deployUser closures = AtomTask {
    taskName = Just $ "Copy closures to " <> remoteAddress rh
  , taskCheck = pure (True, ())
  , taskApply = shelly $ do
      run "nix-copy-closure" $ ["--sign", "--gzip", "--to", remoteHostTarget rh { remoteUser = deployUser }]  ++ fmap toTextArg closures-- TODO: pass port
      let installProfile closure = ("sudo", ["-i", "-u " <> deployUser, "nix-env", "-p /opt/deploy/profile", "-i", toTextArg closure])
      _ <- shellRemoteSSH rh $ raiseNixEnv deployUser : fmap installProfile closures
      pure ()
  , taskReverse = shelly $ errExit False $ do
    let remoteProfile closure = ("sudo", ["-i", "-u " <> deployUser, "nix-env", "-p /opt/deploy/profile", "-e", toTextArg closure])
    _ <- shellRemoteSSH rh (raiseNixEnv deployUser : fmap remoteProfile closures)
    pure ()
  }

-- | Create nix profile for remote user
nixCreateProfile :: RemoteHost -> Text -> Task ()
nixCreateProfile rh deployUser = AtomTask {
    taskName = Just $ "Create nix profile for " <> deployUser <> " at " <> remoteAddress rh
  , taskCheck = shelly $ errExit False $ do
      _ <- shellRemoteSSH rh [("ls", ["/opt/" <> deployUser <> " > /dev/null 2>&1"])]
      err <- lastExitCode
      pure (err /= 0, ())
  , taskApply = shelly $ do
      let profileDir = "/opt/" <> deployUser
      _ <- shellRemoteSSH rh [
          ("mkdir", ["-p", profileDir])
        , ("chown", ["-R", deployUser, profileDir])
        , ("ln", ["-s", profileDir <> "/profile", "/nix/var/nix/gcroots/" <> deployUser])
        ]
      pure ()
  , taskReverse = shelly $ errExit False $ do
      _ <- shellRemoteSSH rh [("rm", ["-rf", "/opt/" <> deployUser, "/nix/var/nix/gcroots/" <> deployUser])]
      pure ()
  }

-- | Extract service derivation from nix file
nixExtractDeriv :: NixBuildInfo -> Text -> Task Text
nixExtractDeriv NixBuildInfo{..} name = AtomTask {
    taskName = Just $ "Get deriviation for " <> name
  , taskCheck = shelly $ do
      res <- extract
      pure (False, T.filter (\c -> c /= '\r' && c /= '\n') res)
  , taskApply = shelly extract
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
  , taskCheck = pure (True, ())
  , taskApply = shelly $ do
      _ <- errExit False $ shellRemoteSSH rh [("rm", ["-f", servicePath])]
      _ <- shellRemoteSSH rh $
        [ ("cp", [deriv, servicePath])]
        ++ if enable then [("systemctl", ["enable", serviceName <> ".service"])] else []
        ++ [("systemctl", ["daemon-reload"])]
      pure ()
  , taskReverse = shelly $ errExit False $ do
      _ <- shellRemoteSSH rh $
           if enable then [("systemctl", ["disable", serviceName <> ".service"])] else []
        ++ [("rm", ["-f", servicePath])]
      pure ()
  }
  where
    servicePath = "/etc/systemd/system/" <> serviceName <> ".service"

-- | Restart given remote service with given name
restartRemoteService :: RemoteHost -> Text -> Task ()
restartRemoteService rh serviceName = AtomTask {
    taskName = Just $ "Restart systemd service " <> serviceName <> " at " <> remoteAddress rh
  , taskCheck = pure (True, ())
  , taskApply = shelly $ do
      _ <- shellRemoteSSH rh [("systemctl", ["restart", serviceName])]
      pure ()
  , taskReverse = pure ()
  }

-- | Ensure that postgres is installed and init it with the given script (derivation path)
installPostgres :: RemoteHost -> Text -> Task ()
installPostgres rh derivSql =  AtomTask {
  taskName = Just $ "Init postgresql at " <> remoteAddress rh
, taskCheck = shelly $ errExit False $ do
    reses <- isNotInstalled "postgresql"
    pure (reses, ())
, taskApply = void $ shelly $
    shellRemoteSSH rh [
        ("apt-get", ["install", "-y", "postgresql", "postgresql-contrib"])
      , ("systemctl", ["enable", "postgresql"])
      , ("systemctl", ["start", "postgresql"])
      , ("sudo", ["-i", "-u postgres", "psql", "-f " <> derivSql])]
, taskReverse = shelly $ errExit False $ do
    _ <- shellRemoteSSH rh [("apt-get", ["remove", "-y", "postgresql"])]
    pure ()
}
  where
    isNotInstalled name = do
      _ <- shellRemoteSSH rh [("dpkg", ["--get-selections | grep -q \"^" <> name <> "[[:space:]]*install$\""])]
      err <- lastExitCode
      pure (err /= 0)

-- | Helper that holds tasks for installing nix infrastructure on remote host
--
-- This includes:
-- * User setup
-- * Sign keys
-- * Nix installation
-- * Symlinks for nix binaries
nixify :: RemoteHost -> Text -> Task ()
nixify rh deployUser = do
  aptPackages rh ["curl"]
  let deployUser = "deploy"
  addUser rh deployUser
  installNix rh deployUser
  nixCreateProfile rh deployUser
  makeNixLinks rh deployUser
  dontReverse genNixSignKeys
  copyNixSignKeys rh
  copyDeploySshKeys rh deployUser
