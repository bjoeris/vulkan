module Spec.ExtensionTag
  ( ExtensionTag(..)
  , stringToExtensionTag
  ) where

import           Data.Char      (isAsciiUpper)
import           Data.Text      (Text, pack, isPrefixOf, drop)
import           Data.Text      as T

-- | A string containing only upper case ASCII characters
newtype ExtensionTag = ExtensionTag{ unExtensionTag :: Text }
  deriving (Eq, Show)

stringToExtensionTag :: String -> Maybe ExtensionTag
stringToExtensionTag s =
  if isPrefixOf p t
  then Just $ ExtensionTag $ T.drop (T.length p) t
  else if T.all isAsciiUpper t
  then Just $ ExtensionTag $ t
  else Nothing
  where
    p = pack "VK_VENDOR_ID_"
    t = pack s
