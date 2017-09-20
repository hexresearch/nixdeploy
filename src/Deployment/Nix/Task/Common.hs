module Deployment.Nix.Task.Common(
    installNix
  ) where

import Deployment.Nix.Task
import Shelly

-- | Install nix on machine
installNix :: Task ()
installNix = Task {
  taskName = Just "Installation of nix package manager"
, taskCheck = do
    _ <- run "which" ["nix-build"]
    err <- lastExitCode
    pure $ err /= 0
, taskApply = do
    run "curl" ["https://nixos.org/nix/install"] -|- run "sh" []
    pure ()
, taskReverse = do
    _ <- run "rm" ["-rf", "/nix"]
    _ <- run "rm" ["-rf", "~/.nix-*"]
    pure ()
    -- TODO: remove '. $HOME/.nix-profile/etc/profile.d/nix.sh' from .profile.
}
