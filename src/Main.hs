{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.Monoid
import Options.Applicative

-- | CLI options
data Options = Options {
  optionsCommand :: Command
}

-- | Available CLI commands to perform
data Command = CommandDeploy

-- | CLI parser
options :: Parser Options
options = Options
  <$> cliCommand
  where
    cliCommand = subparser $
      command "deploy" (info (pure CommandDeploy <**> helper) $ progDesc "Add a file to the repository")

main :: IO ()
main = runOptions =<< execParser opts
  where
    opts = info (options <**> helper)
      ( fullDesc
     <> progDesc "Deploys the project with nix on non nixos machine"
     <> header "nixdeploy - deployment tool" )

-- | Execute program with given options
runOptions :: Options -> IO ()
runOptions Options{..} = do
  case optionsCommand of
    CommandDeploy -> pure ()
