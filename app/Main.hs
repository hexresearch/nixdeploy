module Main where

import Data.Monoid
import Data.Text (Text, pack, unpack)
import Deployment.Nix
import Options.Applicative
import Prelude hiding (FilePath)
import Shelly hiding (command)

-- | Helper to parse text
textArgument :: Mod ArgumentFields String -> Parser Text
textArgument = fmap pack . strArgument

-- | Helper to parse text
textOption :: Mod OptionFields String -> Parser Text
textOption = fmap pack . strOption

-- | CLI parser
options :: Parser DeployOptions
options = DeployOptions
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

main :: IO ()
main = do
  opts <- execParser opts
  runDeployment opts $ nixBuildPlan (getNixBuildInfo opts)
  where
    opts = info (options <**> helper)
      ( fullDesc
     <> progDesc "Deploys the project with nix on non nixos machine"
     <> header "nixdeploy - deployment tool" )

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
