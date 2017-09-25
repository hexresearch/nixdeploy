module Main where

import Data.Monoid
import Data.Text (Text, pack, unpack)
import Deployment.Nix
import Prelude hiding (FilePath)
import Shelly hiding (command)

main :: IO ()
main = makeDeploymentCLI deployOptionsParser id defaultNixPlan
