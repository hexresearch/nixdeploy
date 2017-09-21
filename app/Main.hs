module Main where

import Data.Monoid
import Data.Text (Text, pack, unpack)
import Deployment.Nix
import Options.Applicative

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

main :: IO ()
main = do
  opts <- execParser opts
  runDeployment opts debugPlan
  where
    opts = info (options <**> helper)
      ( fullDesc
     <> progDesc "Deploys the project with nix on non nixos machine"
     <> header "nixdeploy - deployment tool" )

debugPlan :: RemoteHost -> Task ()
debugPlan rh = do
  aptPackages rh ["curl"]
  let deployUser = "deploy"
  addUser rh deployUser
  installNix rh deployUser
  dontReverse genNixSignKeys
  copyNixSignKeys rh
