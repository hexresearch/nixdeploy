module Deployment.Nix.Task.Common(
    RemoteHost(..)
  , shellRemoteSSH
  , dontReverse
  , aptPackages
  , addUser
  , installNix
  , raiseNixEnv
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

-- | Exec shell commands (concated via &&) on remote host via SSH
shellRemoteSSH :: RemoteHost -> [(FilePath, [Text])] -> Sh Text
shellRemoteSSH RemoteHost{..} = sshPairsWithOptions sshConnectString ["-p" <> pack (show remotePort)]
  where
    sshConnectString = remoteUser <> "@" <> remoteAddress

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

-- | Helper to bring nix environment in scope
raiseNixEnv :: Text -> (FilePath, [Text])
raiseNixEnv deployUser = ("source", ["/home/" <> deployUser <> "/.nix-profile/etc/profile.d/nix.sh"])
