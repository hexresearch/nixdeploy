module Deployment.Nix.Task.Common(
    RemoteHost(..)
  , NixBuildInfo(..)
  , remoteHostTarget
  , shellRemoteSSH
  , dontReverse
  , aptPackages
  , addUser
  , installNix
  , makeNixLinks
  , raiseNixEnv
  , genNixSignKeys
  , copyNixSignKeys
  , copyDeploySshKeys
  , nixBuild
  , nixCopyClosures
  , nixCreateProfile
  ) where

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

-- | Install needed packages
aptPackages :: RemoteHost -> [Text] -> Task ()
aptPackages rh pkgs = AtomTask {
  taskName = Just $ "Installation of " <> T.intercalate ", " pkgs <> " via apt-get"
, taskCheck = shelly $ errExit False $ do
    reses <- traverse isNotInstalled pkgs
    pure (or reses, ())
, taskApply = void $ shelly $
    shellRemoteSSH rh [("apt-get", ["update"]), ("apt-get", "install":pkgs)]
, taskReverse = shelly $ errExit False $ do
    _ <- shellRemoteSSH rh [("apt-get", "erase":pkgs)]
    pure ()
}
  where
    isNotInstalled name = do
      _ <- shellRemoteSSH rh [("dpkg", ["--get-selections | grep -q \"^" <> name <> "[[:space:]]*install$\""])]
      err <- lastExitCode
      pure (err /= 0)

-- | Add user on remote host
addUser :: RemoteHost -> Text -> Task ()
addUser rh user = AtomTask {
  taskName = Just $ "Creation of user " <> user
, taskCheck = shelly $ errExit False $ do
    _ <- shellRemoteSSH rh [("id", ["-u", user])]
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
    _ <- shellRemoteSSH rh [raiseNixEnv deployUser, ("which", ["nix-build"])]
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
    -- _ <- errExit False $ shellRemoteSSH rh [("ln", ["/home/" <> deployUser <> "/.profile", "/home/" <> deployUser <> "/.bashrc"])]
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
      _ <- shellRemoteSSH rh [raiseNixEnv deployUser, ("sudo", ["-i", "-u " <> deployUser, "nix-env", "-p /opt/deploy/profile", "--set"] ++ fmap toTextArg closures)]
      pure ()
  , taskReverse = shelly $ errExit False $ do
    _ <- shellRemoteSSH rh [raiseNixEnv deployUser, ("sudo", ["-i", "-u " <> deployUser, "nix-env", "-p /opt/deploy/profile", "-e"] ++ fmap toTextArg closures)]
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
