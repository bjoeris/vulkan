{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_get_display_properties2
  ( pattern VK_STRUCTURE_TYPE_DISPLAY_PROPERTIES_2_KHR
  , pattern VK_STRUCTURE_TYPE_DISPLAY_PLANE_PROPERTIES_2_KHR
  , pattern VK_STRUCTURE_TYPE_DISPLAY_MODE_PROPERTIES_2_KHR
  , pattern VK_STRUCTURE_TYPE_DISPLAY_PLANE_INFO_2_KHR
  , pattern VK_STRUCTURE_TYPE_DISPLAY_PLANE_CAPABILITIES_2_KHR
  , pattern VK_KHR_GET_DISPLAY_PROPERTIES_2_SPEC_VERSION
  , pattern VK_KHR_GET_DISPLAY_PROPERTIES_2_EXTENSION_NAME
  , vkGetPhysicalDeviceDisplayProperties2KHR
  , vkGetPhysicalDeviceDisplayPlaneProperties2KHR
  , vkGetDisplayModeProperties2KHR
  , vkGetDisplayPlaneCapabilities2KHR
  , VkDisplayProperties2KHR(..)
  , VkDisplayPlaneProperties2KHR(..)
  , VkDisplayModeProperties2KHR(..)
  , VkDisplayPlaneInfo2KHR(..)
  , VkDisplayPlaneCapabilities2KHR(..)
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


import Graphics.Vulkan.Core10.Core
  ( VkResult(..)
  , VkStructureType(..)
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkPhysicalDevice
  )
import Graphics.Vulkan.Extensions.VK_KHR_display
  ( VkDisplayKHR(..)
  , VkDisplayModeKHR(..)
  , VkDisplayModePropertiesKHR(..)
  , VkDisplayPlaneCapabilitiesKHR(..)
  , VkDisplayPlanePropertiesKHR(..)
  , VkDisplayPropertiesKHR(..)
  )


-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DISPLAY_PROPERTIES_2_KHR"
pattern VK_STRUCTURE_TYPE_DISPLAY_PROPERTIES_2_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_DISPLAY_PROPERTIES_2_KHR = VkStructureType 1000121000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DISPLAY_PLANE_PROPERTIES_2_KHR"
pattern VK_STRUCTURE_TYPE_DISPLAY_PLANE_PROPERTIES_2_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_DISPLAY_PLANE_PROPERTIES_2_KHR = VkStructureType 1000121001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DISPLAY_MODE_PROPERTIES_2_KHR"
pattern VK_STRUCTURE_TYPE_DISPLAY_MODE_PROPERTIES_2_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_DISPLAY_MODE_PROPERTIES_2_KHR = VkStructureType 1000121002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DISPLAY_PLANE_INFO_2_KHR"
pattern VK_STRUCTURE_TYPE_DISPLAY_PLANE_INFO_2_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_DISPLAY_PLANE_INFO_2_KHR = VkStructureType 1000121003
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DISPLAY_PLANE_CAPABILITIES_2_KHR"
pattern VK_STRUCTURE_TYPE_DISPLAY_PLANE_CAPABILITIES_2_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_DISPLAY_PLANE_CAPABILITIES_2_KHR = VkStructureType 1000121004
-- No documentation found for TopLevel "VK_KHR_GET_DISPLAY_PROPERTIES_2_SPEC_VERSION"
pattern VK_KHR_GET_DISPLAY_PROPERTIES_2_SPEC_VERSION :: Integral a => a
pattern VK_KHR_GET_DISPLAY_PROPERTIES_2_SPEC_VERSION = 1
-- No documentation found for TopLevel "VK_KHR_GET_DISPLAY_PROPERTIES_2_EXTENSION_NAME"
pattern VK_KHR_GET_DISPLAY_PROPERTIES_2_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_GET_DISPLAY_PROPERTIES_2_EXTENSION_NAME = "VK_KHR_get_display_properties2"
-- | vkGetPhysicalDeviceDisplayProperties2KHR - Query information about the
-- available displays
--
-- = Parameters
--
-- -   @physicalDevice@ is a physical device.
--
-- -   @pPropertyCount@ is a pointer to an integer related to the number of
--     display devices available or queried, as described below.
--
-- -   @pProperties@ is either @NULL@ or a pointer to an array of
--     @VkDisplayProperties2KHR@ structures.
--
-- = Description
--
-- @vkGetPhysicalDeviceDisplayProperties2KHR@ behaves similarly to
-- 'Graphics.Vulkan.Extensions.VK_KHR_display.vkGetPhysicalDeviceDisplayPropertiesKHR',
-- with the ability to return extended information via chained output
-- structures.
--
-- Unresolved directive in vkGetPhysicalDeviceDisplayProperties2KHR.txt -
-- include::..\/validity\/protos\/vkGetPhysicalDeviceDisplayProperties2KHR.txt[]
--
-- = See Also
--
-- No cross-references are available
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceDisplayProperties2KHR" vkGetPhysicalDeviceDisplayProperties2KHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayProperties2KHR) -> IO VkResult
-- | vkGetPhysicalDeviceDisplayPlaneProperties2KHR - Query information about
-- the available display planes.
--
-- = Parameters
--
-- -   @physicalDevice@ is a physical device.
--
-- -   @pPropertyCount@ is a pointer to an integer related to the number of
--     display planes available or queried, as described below.
--
-- -   @pProperties@ is either @NULL@ or a pointer to an array of
--     @VkDisplayPlaneProperties2KHR@ structures.
--
-- = Description
--
-- @vkGetPhysicalDeviceDisplayPlaneProperties2KHR@ behaves similarly to
-- 'Graphics.Vulkan.Extensions.VK_KHR_display.vkGetPhysicalDeviceDisplayPlanePropertiesKHR',
-- with the ability to return extended information via chained output
-- structures.
--
-- Unresolved directive in
-- vkGetPhysicalDeviceDisplayPlaneProperties2KHR.txt -
-- include::..\/validity\/protos\/vkGetPhysicalDeviceDisplayPlaneProperties2KHR.txt[]
--
-- = See Also
--
-- No cross-references are available
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceDisplayPlaneProperties2KHR" vkGetPhysicalDeviceDisplayPlaneProperties2KHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayPlaneProperties2KHR) -> IO VkResult
-- | vkGetDisplayModeProperties2KHR - Query information about the available
-- display modes.
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device associated with @display@.
--
-- -   @display@ is the display to query.
--
-- -   @pPropertyCount@ is a pointer to an integer related to the number of
--     display modes available or queried, as described below.
--
-- -   @pProperties@ is either @NULL@ or a pointer to an array of
--     @VkDisplayModeProperties2KHR@ structures.
--
-- = Description
--
-- @vkGetDisplayModeProperties2KHR@ behaves similarly to
-- 'Graphics.Vulkan.Extensions.VK_KHR_display.vkGetDisplayModePropertiesKHR',
-- with the ability to return extended information via chained output
-- structures.
--
-- Unresolved directive in vkGetDisplayModeProperties2KHR.txt -
-- include::..\/validity\/protos\/vkGetDisplayModeProperties2KHR.txt[]
--
-- = See Also
--
-- No cross-references are available
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetDisplayModeProperties2KHR" vkGetDisplayModeProperties2KHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("display" ::: VkDisplayKHR) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayModeProperties2KHR) -> IO VkResult
-- | vkGetDisplayPlaneCapabilities2KHR - Query capabilities of a mode and
-- plane combination
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device associated with
--     @pDisplayPlaneInfo@.
--
-- -   @pDisplayPlaneInfo@ is a pointer to an instance of the
--     'VkDisplayPlaneInfo2KHR' structure describing the plane and mode.
--
-- -   @pCapabilities@ is a pointer to a 'VkDisplayPlaneCapabilities2KHR'
--     structure in which the capabilities are returned.
--
-- = Description
--
-- @vkGetDisplayPlaneCapabilities2KHR@ behaves similarly to
-- 'Graphics.Vulkan.Extensions.VK_KHR_display.vkGetDisplayPlaneCapabilitiesKHR',
-- with the ability to specify extended inputs via chained input
-- structures, and to return extended information via chained output
-- structures.
--
-- Unresolved directive in vkGetDisplayPlaneCapabilities2KHR.txt -
-- include::..\/validity\/protos\/vkGetDisplayPlaneCapabilities2KHR.txt[]
--
-- = See Also
--
-- No cross-references are available
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetDisplayPlaneCapabilities2KHR" vkGetDisplayPlaneCapabilities2KHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pDisplayPlaneInfo" ::: Ptr VkDisplayPlaneInfo2KHR) -> ("pCapabilities" ::: Ptr VkDisplayPlaneCapabilities2KHR) -> IO VkResult
-- | VkDisplayProperties2KHR - Structure describing an available display
-- device
--
-- = Description
--
-- Unresolved directive in VkDisplayProperties2KHR.txt -
-- include::..\/validity\/structs\/VkDisplayProperties2KHR.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkDisplayProperties2KHR = VkDisplayProperties2KHR
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @displayProperties@ is an instance of the
  -- 'Graphics.Vulkan.Extensions.VK_KHR_display.VkDisplayPropertiesKHR'
  -- structure.
  vkDisplayProperties :: VkDisplayPropertiesKHR
  }
  deriving (Eq, Show)

instance Storable VkDisplayProperties2KHR where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek ptr = VkDisplayProperties2KHR <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 8)
                                     <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDisplayProperties2KHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDisplayProperties2KHR))
                *> poke (ptr `plusPtr` 16) (vkDisplayProperties (poked :: VkDisplayProperties2KHR))
-- | VkDisplayPlaneProperties2KHR - Structure describing an available display
-- plane
--
-- = Description
--
-- Unresolved directive in VkDisplayPlaneProperties2KHR.txt -
-- include::..\/validity\/structs\/VkDisplayPlaneProperties2KHR.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkDisplayPlaneProperties2KHR = VkDisplayPlaneProperties2KHR
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @displayPlaneProperties@ is an instance of the
  -- 'Graphics.Vulkan.Extensions.VK_KHR_display.VkDisplayPlanePropertiesKHR'
  -- structure.
  vkDisplayPlaneProperties :: VkDisplayPlanePropertiesKHR
  }
  deriving (Eq, Show)

instance Storable VkDisplayPlaneProperties2KHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkDisplayPlaneProperties2KHR <$> peek (ptr `plusPtr` 0)
                                          <*> peek (ptr `plusPtr` 8)
                                          <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDisplayPlaneProperties2KHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDisplayPlaneProperties2KHR))
                *> poke (ptr `plusPtr` 16) (vkDisplayPlaneProperties (poked :: VkDisplayPlaneProperties2KHR))
-- | VkDisplayModeProperties2KHR - Structure describing an available display
-- mode
--
-- = Description
--
-- Unresolved directive in VkDisplayModeProperties2KHR.txt -
-- include::..\/validity\/structs\/VkDisplayModeProperties2KHR.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkDisplayModeProperties2KHR = VkDisplayModeProperties2KHR
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @displayModeProperties@ is an instance of the
  -- 'Graphics.Vulkan.Extensions.VK_KHR_display.VkDisplayModePropertiesKHR'
  -- structure.
  vkDisplayModeProperties :: VkDisplayModePropertiesKHR
  }
  deriving (Eq, Show)

instance Storable VkDisplayModeProperties2KHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkDisplayModeProperties2KHR <$> peek (ptr `plusPtr` 0)
                                         <*> peek (ptr `plusPtr` 8)
                                         <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDisplayModeProperties2KHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDisplayModeProperties2KHR))
                *> poke (ptr `plusPtr` 16) (vkDisplayModeProperties (poked :: VkDisplayModeProperties2KHR))
-- | VkDisplayPlaneInfo2KHR - Structure defining the intended configuration
-- of a display plane
--
-- = Description
--
-- __Note__
--
-- This parameter also implicitly specifies a display.
--
-- -   @planeIndex@ is the plane which the application intends to use with
--     the display.
--
-- The members of @VkDisplayPlaneInfo2KHR@ correspond to the arguments to
-- 'Graphics.Vulkan.Extensions.VK_KHR_display.vkGetDisplayPlaneCapabilitiesKHR',
-- with @sType@ and @pNext@ added for extensibility.
--
-- Unresolved directive in VkDisplayPlaneInfo2KHR.txt -
-- include::..\/validity\/structs\/VkDisplayPlaneInfo2KHR.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkDisplayPlaneInfo2KHR = VkDisplayPlaneInfo2KHR
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @mode@ is the display mode the application intends to program when using
  -- the specified plane.
  vkMode :: VkDisplayModeKHR
  , -- No documentation found for Nested "VkDisplayPlaneInfo2KHR" "planeIndex"
  vkPlaneIndex :: Word32
  }
  deriving (Eq, Show)

instance Storable VkDisplayPlaneInfo2KHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkDisplayPlaneInfo2KHR <$> peek (ptr `plusPtr` 0)
                                    <*> peek (ptr `plusPtr` 8)
                                    <*> peek (ptr `plusPtr` 16)
                                    <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDisplayPlaneInfo2KHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDisplayPlaneInfo2KHR))
                *> poke (ptr `plusPtr` 16) (vkMode (poked :: VkDisplayPlaneInfo2KHR))
                *> poke (ptr `plusPtr` 24) (vkPlaneIndex (poked :: VkDisplayPlaneInfo2KHR))
-- | VkDisplayPlaneCapabilities2KHR - Structure describing the capabilities
-- of a mode and plane combination
--
-- = Description
--
-- Unresolved directive in VkDisplayPlaneCapabilities2KHR.txt -
-- include::..\/validity\/structs\/VkDisplayPlaneCapabilities2KHR.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkDisplayPlaneCapabilities2KHR = VkDisplayPlaneCapabilities2KHR
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @capabilities@ is an instance of the
  -- 'Graphics.Vulkan.Extensions.VK_KHR_display.VkDisplayPlaneCapabilitiesKHR'
  -- structure.
  vkCapabilities :: VkDisplayPlaneCapabilitiesKHR
  }
  deriving (Eq, Show)

instance Storable VkDisplayPlaneCapabilities2KHR where
  sizeOf ~_ = 88
  alignment ~_ = 8
  peek ptr = VkDisplayPlaneCapabilities2KHR <$> peek (ptr `plusPtr` 0)
                                            <*> peek (ptr `plusPtr` 8)
                                            <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDisplayPlaneCapabilities2KHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDisplayPlaneCapabilities2KHR))
                *> poke (ptr `plusPtr` 16) (vkCapabilities (poked :: VkDisplayPlaneCapabilities2KHR))
