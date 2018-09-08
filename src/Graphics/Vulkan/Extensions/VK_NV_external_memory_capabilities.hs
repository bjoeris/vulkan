{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_NV_external_memory_capabilities
  ( VkExternalMemoryHandleTypeFlagBitsNV(..)
  , pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV
  , pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV
  , pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV
  , pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV
  , VkExternalMemoryFeatureFlagBitsNV(..)
  , pattern VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV
  , pattern VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV
  , pattern VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV
  , pattern VK_NV_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION
  , pattern VK_NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME
  , vkGetPhysicalDeviceExternalImageFormatPropertiesNV
  , VkExternalImageFormatPropertiesNV(..)
  , VkExternalMemoryHandleTypeFlagsNV
  , VkExternalMemoryFeatureFlagsNV
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.String
  ( IsString
  )
import Foreign.Ptr
  ( Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )
import GHC.Read
  ( choose
  , expectP
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )
import Text.ParserCombinators.ReadPrec
  ( (+++)
  , prec
  , step
  )
import Text.Read
  ( Read(..)
  , parens
  )
import Text.Read.Lex
  ( Lexeme(Ident)
  )


import Graphics.Vulkan.Core10.Core
  ( VkFormat(..)
  , VkResult(..)
  , VkFlags
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkImageCreateFlagBits(..)
  , VkImageFormatProperties(..)
  , VkImageTiling(..)
  , VkImageType(..)
  , VkImageUsageFlagBits(..)
  , VkImageCreateFlags
  , VkImageUsageFlags
  , VkPhysicalDevice
  )


-- ** VkExternalMemoryHandleTypeFlagBitsNV

-- | VkExternalMemoryHandleTypeFlagBitsNV - Bitmask specifying external
-- memory handle types
--
-- = See Also
--
-- No cross-references are available
newtype VkExternalMemoryHandleTypeFlagBitsNV = VkExternalMemoryHandleTypeFlagBitsNV VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkExternalMemoryHandleTypeFlagBitsNV where
  showsPrec _ VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV = showString "VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV"
  showsPrec _ VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV = showString "VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV"
  showsPrec _ VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV = showString "VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV"
  showsPrec _ VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV = showString "VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV"
  showsPrec p (VkExternalMemoryHandleTypeFlagBitsNV x) = showParen (p >= 11) (showString "VkExternalMemoryHandleTypeFlagBitsNV " . showsPrec 11 x)

instance Read VkExternalMemoryHandleTypeFlagBitsNV where
  readPrec = parens ( choose [ ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV",     pure VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV)
                             , ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV", pure VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV)
                             , ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV",      pure VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV)
                             , ("VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV",  pure VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkExternalMemoryHandleTypeFlagBitsNV")
                        v <- step readPrec
                        pure (VkExternalMemoryHandleTypeFlagBitsNV v)
                        )
                    )

-- | @VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV@ specifies a handle
-- to memory returned by
-- 'Graphics.Vulkan.Extensions.VK_NV_external_memory_win32.vkGetMemoryWin32HandleNV',
-- or one duplicated from such a handle using @DuplicateHandle()@.
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV :: VkExternalMemoryHandleTypeFlagBitsNV
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_NV = VkExternalMemoryHandleTypeFlagBitsNV 0x00000001

-- | @VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV@ specifies a
-- handle to memory returned by
-- 'Graphics.Vulkan.Extensions.VK_NV_external_memory_win32.vkGetMemoryWin32HandleNV'.
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV :: VkExternalMemoryHandleTypeFlagBitsNV
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_NV = VkExternalMemoryHandleTypeFlagBitsNV 0x00000002

-- | @VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV@ specifies a valid NT
-- handle to memory returned by @IDXGIResource1::CreateSharedHandle()@, or
-- a handle duplicated from such a handle using @DuplicateHandle()@.
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV :: VkExternalMemoryHandleTypeFlagBitsNV
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_BIT_NV = VkExternalMemoryHandleTypeFlagBitsNV 0x00000004

-- | @VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV@ specifies a
-- handle to memory returned by @IDXGIResource::GetSharedHandle()@.
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV :: VkExternalMemoryHandleTypeFlagBitsNV
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_IMAGE_KMT_BIT_NV = VkExternalMemoryHandleTypeFlagBitsNV 0x00000008
-- ** VkExternalMemoryFeatureFlagBitsNV

-- | VkExternalMemoryFeatureFlagBitsNV - Bitmask specifying external memory
-- features
--
-- = See Also
--
-- UNKNOWN:VkExternalImageFormatPropertiesNV,
-- UNKNOWN:vkGetPhysicalDeviceExternalImageFormatPropertiesNV
newtype VkExternalMemoryFeatureFlagBitsNV = VkExternalMemoryFeatureFlagBitsNV VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkExternalMemoryFeatureFlagBitsNV where
  showsPrec _ VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV = showString "VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV"
  showsPrec _ VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV = showString "VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV"
  showsPrec _ VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV = showString "VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV"
  showsPrec p (VkExternalMemoryFeatureFlagBitsNV x) = showParen (p >= 11) (showString "VkExternalMemoryFeatureFlagBitsNV " . showsPrec 11 x)

instance Read VkExternalMemoryFeatureFlagBitsNV where
  readPrec = parens ( choose [ ("VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV", pure VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV)
                             , ("VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV",     pure VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV)
                             , ("VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV",     pure VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkExternalMemoryFeatureFlagBitsNV")
                        v <- step readPrec
                        pure (VkExternalMemoryFeatureFlagBitsNV v)
                        )
                    )

-- | @VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV@ specifies that
-- external memory of the specified type /must/ be created as a dedicated
-- allocation when used in the manner specified.
pattern VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV :: VkExternalMemoryFeatureFlagBitsNV
pattern VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_NV = VkExternalMemoryFeatureFlagBitsNV 0x00000001

-- | @VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV@ specifies that the
-- implementation supports exporting handles of the specified type.
pattern VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV :: VkExternalMemoryFeatureFlagBitsNV
pattern VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_NV = VkExternalMemoryFeatureFlagBitsNV 0x00000002

-- | @VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV@ specifies that the
-- implementation supports importing handles of the specified type.
pattern VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV :: VkExternalMemoryFeatureFlagBitsNV
pattern VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_NV = VkExternalMemoryFeatureFlagBitsNV 0x00000004
-- No documentation found for TopLevel "VK_NV_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION"
pattern VK_NV_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION :: Integral a => a
pattern VK_NV_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION = 1
-- No documentation found for TopLevel "VK_NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME"
pattern VK_NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_NV_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME = "VK_NV_external_memory_capabilities"
-- | vkGetPhysicalDeviceExternalImageFormatPropertiesNV - determine image
-- capabilities compatible with external memory handle types
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device from which to query the
--     image capabilities
--
-- -   @format@ is the image format, corresponding to
--     'Graphics.Vulkan.Core10.Image.VkImageCreateInfo'::@format@.
--
-- -   @type@ is the image type, corresponding to
--     'Graphics.Vulkan.Core10.Image.VkImageCreateInfo'::@imageType@.
--
-- -   @tiling@ is the image tiling, corresponding to
--     'Graphics.Vulkan.Core10.Image.VkImageCreateInfo'::@tiling@.
--
-- -   @usage@ is the intended usage of the image, corresponding to
--     'Graphics.Vulkan.Core10.Image.VkImageCreateInfo'::@usage@.
--
-- -   @flags@ is a bitmask describing additional parameters of the image,
--     corresponding to
--     'Graphics.Vulkan.Core10.Image.VkImageCreateInfo'::@flags@.
--
-- -   @externalHandleType@ is either one of the bits from
--     'VkExternalMemoryHandleTypeFlagBitsNV', or 0.
--
-- -   @pExternalImageFormatProperties@ points to an instance of the
--     'VkExternalImageFormatPropertiesNV' structure in which capabilities
--     are returned.
--
-- = Description
--
-- If @externalHandleType@ is 0,
-- @pExternalImageFormatProperties@::imageFormatProperties will return the
-- same values as a call to
-- 'Graphics.Vulkan.Core10.DeviceInitialization.vkGetPhysicalDeviceImageFormatProperties',
-- and the other members of @pExternalImageFormatProperties@ will all be 0.
-- Otherwise, they are filled in as described for
-- 'VkExternalImageFormatPropertiesNV'.
--
-- Unresolved directive in
-- vkGetPhysicalDeviceExternalImageFormatPropertiesNV.txt -
-- include::..\/validity\/protos\/vkGetPhysicalDeviceExternalImageFormatPropertiesNV.txt[]
--
-- = See Also
--
-- No cross-references are available
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceExternalImageFormatPropertiesNV" vkGetPhysicalDeviceExternalImageFormatPropertiesNV :: ("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("type" ::: VkImageType) -> ("tiling" ::: VkImageTiling) -> ("usage" ::: VkImageUsageFlags) -> ("flags" ::: VkImageCreateFlags) -> ("externalHandleType" ::: VkExternalMemoryHandleTypeFlagsNV) -> ("pExternalImageFormatProperties" ::: Ptr VkExternalImageFormatPropertiesNV) -> IO VkResult
-- | VkExternalImageFormatPropertiesNV - Structure specifying external image
-- format properties
--
-- = Description
--
-- Unresolved directive in VkExternalImageFormatPropertiesNV.txt -
-- include::..\/validity\/structs\/VkExternalImageFormatPropertiesNV.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkExternalImageFormatPropertiesNV = VkExternalImageFormatPropertiesNV
  { -- | @imageFormatProperties@ will be filled in as when calling
  -- 'Graphics.Vulkan.Core10.DeviceInitialization.vkGetPhysicalDeviceImageFormatProperties',
  -- but the values returned /may/ vary depending on the external handle type
  -- requested.
  vkImageFormatProperties :: VkImageFormatProperties
  , -- | @externalMemoryFeatures@ is a bitmask of
  -- 'VkExternalMemoryFeatureFlagBitsNV', indicating properties of the
  -- external memory handle type
  -- ('vkGetPhysicalDeviceExternalImageFormatPropertiesNV'::@externalHandleType@)
  -- being queried, or 0 if the external memory handle type is 0.
  vkExternalMemoryFeatures :: VkExternalMemoryFeatureFlagsNV
  , -- | @exportFromImportedHandleTypes@ is a bitmask of
  -- 'VkExternalMemoryHandleTypeFlagBitsNV' containing a bit set for every
  -- external handle type that /may/ be used to create memory from which the
  -- handles of the type specified in
  -- 'vkGetPhysicalDeviceExternalImageFormatPropertiesNV'::@externalHandleType@
  -- /can/ be exported, or 0 if the external memory handle type is 0.
  vkExportFromImportedHandleTypes :: VkExternalMemoryHandleTypeFlagsNV
  , -- | @compatibleHandleTypes@ is a bitmask of
  -- 'VkExternalMemoryHandleTypeFlagBitsNV' containing a bit set for every
  -- external handle type that /may/ be specified simultaneously with the
  -- handle type specified by
  -- 'vkGetPhysicalDeviceExternalImageFormatPropertiesNV'::@externalHandleType@
  -- when calling 'Graphics.Vulkan.Core10.Memory.vkAllocateMemory', or 0 if
  -- the external memory handle type is 0. @compatibleHandleTypes@ will
  -- always contain
  -- 'vkGetPhysicalDeviceExternalImageFormatPropertiesNV'::@externalHandleType@
  vkCompatibleHandleTypes :: VkExternalMemoryHandleTypeFlagsNV
  }
  deriving (Eq, Show)

instance Storable VkExternalImageFormatPropertiesNV where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek ptr = VkExternalImageFormatPropertiesNV <$> peek (ptr `plusPtr` 0)
                                               <*> peek (ptr `plusPtr` 32)
                                               <*> peek (ptr `plusPtr` 36)
                                               <*> peek (ptr `plusPtr` 40)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkImageFormatProperties (poked :: VkExternalImageFormatPropertiesNV))
                *> poke (ptr `plusPtr` 32) (vkExternalMemoryFeatures (poked :: VkExternalImageFormatPropertiesNV))
                *> poke (ptr `plusPtr` 36) (vkExportFromImportedHandleTypes (poked :: VkExternalImageFormatPropertiesNV))
                *> poke (ptr `plusPtr` 40) (vkCompatibleHandleTypes (poked :: VkExternalImageFormatPropertiesNV))
-- | VkExternalMemoryHandleTypeFlagsNV - Bitmask of
-- VkExternalMemoryHandleTypeFlagBitsNV
--
-- = Description
--
-- @VkExternalMemoryHandleTypeFlagsNV@ is a bitmask type for setting a mask
-- of zero or more 'VkExternalMemoryHandleTypeFlagBitsNV'.
--
-- = See Also
--
-- No cross-references are available
type VkExternalMemoryHandleTypeFlagsNV = VkExternalMemoryHandleTypeFlagBitsNV
-- | VkExternalMemoryFeatureFlagsNV - Bitmask of
-- VkExternalMemoryFeatureFlagBitsNV
--
-- = Description
--
-- @VkExternalMemoryFeatureFlagsNV@ is a bitmask type for setting a mask of
-- zero or more 'VkExternalMemoryFeatureFlagBitsNV'.
--
-- = See Also
--
-- No cross-references are available
type VkExternalMemoryFeatureFlagsNV = VkExternalMemoryFeatureFlagBitsNV
