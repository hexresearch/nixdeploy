module Main where

import Data.Monoid
import Data.Text (Text, pack, unpack)
import Options.Applicative
import Deployment.Nix

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
      command "deploy" (info (deployCmd <**> helper) $ progDesc "Add a file to the repository")
    deployCmd = CommandDeploy
      <$> textArgument (
          metavar "MACHINE_IP"
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
main = runDeployment =<< execParser opts
  where
    opts = info (options <**> helper)
      ( fullDesc
     <> progDesc "Deploys the project with nix on non nixos machine"
     <> header "nixdeploy - deployment tool" )
