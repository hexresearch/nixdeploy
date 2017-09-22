module Main where

import Data.Monoid
import Data.Text (Text, pack, unpack)
import Deployment.Nix
import Prelude hiding (FilePath)
import Shelly hiding (command)

main :: IO ()
main = makeDeploymentCLI deployOptionsParser id $ \opts ->
  nixBuildPlan (getNixBuildInfo opts) (getRemoteHost opts)

-- | Plan to build nix project and deploy it on remote host
nixBuildPlan :: NixBuildInfo -> RemoteHost -> Task ()
nixBuildPlan nixBuildInfo rh = do
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
  pure ()
