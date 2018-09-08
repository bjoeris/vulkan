{-# LANGUAGE Arrows          #-}
{-# LANGUAGE RecordWildCards #-}

module Parse.VendorID
  ( parseVendorIDs
  ) where

import           Parse.Utils
import           Spec.ExtensionTag
import           Spec.VendorID
import           Text.XML.HXT.Core

-- TODO: refactor to use allChildren
parseVendorIDs :: IOStateArrow s XmlTree [VendorID]
parseVendorIDs = extractFields "VendorIDs"
                               (hasName "enums" >>> hasAttrValue "name" (== "VkVendorId"))
                               extract
  where extract = listA (parseVendorID <<< getChildren)

parseVendorID :: IOStateArrow s XmlTree VendorID
parseVendorID = extractFields "VendorID"
                              (hasName "enum")
                              extract
  where extract = proc vendorid -> do
          viName <- required "stringToExtensionTag" stringToExtensionTag <<<
                    requiredAttrValue "name" -< vendorid
          viID <- requiredRead <<< requiredAttrValue "value" -< vendorid
          viComment <- optionalAttrValue "comment" -< vendorid
          returnA -< VendorID{..}
