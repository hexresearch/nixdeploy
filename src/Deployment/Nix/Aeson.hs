-- | Overrides aeson TH functions to generate instances
module Deployment.Nix.Aeson(
    A.deriveJSON
  , dropPrefixOptions
  ) where

import Control.Monad
import Data.Aeson.TH (Options (..), SumEncoding (..))
import Data.Aeson.Types (camelTo2, ToJSON(..), FromJSON(..), Value(..))
import Data.Char (isUpper, toLower, isLower, isPunctuation)
import Data.List (findIndex)
import Data.Monoid
import Data.Text (unpack)

import qualified Data.Aeson.TH as A

-- | Converts first symbol to lower case
headToLower :: String -> String
headToLower [] = []
headToLower (x:xs) = toLower x : xs

-- | Drop prefix of name until first upper letter is occured
stripFieldPrefix :: String -> String
stripFieldPrefix = dropWhile (not . isUpper)

-- | Strip prefix of name that exactly matches specified prefix
stripExactPrefix :: String -- ^ Prefix
 -> String -- ^ Name
 -> String -- ^ Name without prefix
stripExactPrefix = go
   where
   go [] name = name
   go (p : ps) name@(x : xs)
       | p == x = go ps xs
       | otherwise = name
   go _ [] = []

-- | Remove from names things like ' and etc
dropPunctuation :: String -> String
dropPunctuation = filter (not . isPunctuation)

-- | Drop upper case prefixes from constructor names
--
-- Example:
-- >>> stripConstructorPrefix "ABCombo"
-- "Combo"
--
-- >>> stripConstructorPrefix "Combo"
-- "Combo"
stripConstructorPrefix :: String -> String
stripConstructorPrefix t =
   maybe t (flip drop t . decrementSafe) $ findIndex isLower t
 where
   decrementSafe 0 = 0
   decrementSafe i = i - 1

-- | Options for aeson TH generator, that generates following fields:
--
-- * without punctuation
-- * without lowercase prefix
--
-- And generates constructor tags without uppercase prefixes with
-- 'stripConstructorPrefix'.
--
-- Sums are encoded as one object with only one field corresponding the
-- constructor used.
dropPrefixOptions :: Options
dropPrefixOptions =
   A.defaultOptions
   { fieldLabelModifier = headToLower . stripFieldPrefix . dropPunctuation
   , constructorTagModifier = stripConstructorPrefix
   , sumEncoding = ObjectWithSingleField
   }
