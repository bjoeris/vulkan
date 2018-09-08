{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_shared_presentable_image
  ( pattern VK_IMAGE_LAYOUT_SHARED_PRESENT_KHR
  , pattern VK_STRUCTURE_TYPE_SHARED_PRESENT_SURFACE_CAPABILITIES_KHR
  , pattern VK_PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR
  , pattern VK_PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR
  , pattern VK_KHR_SHARED_PRESENTABLE_IMAGE_SPEC_VERSION
  , pattern VK_KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME
  , vkGetSwapchainStatusKHR
  , VkSharedPresentSurfaceCapabilitiesKHR(..)
  ) where

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
import Graphics.Vulkan.NamedType
  ( (:::)
  )


import Graphics.Vulkan.Core10.Core
  ( VkResult(..)
  , VkStructureType(..)
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkDevice
  , VkImageUsageFlags
  )
import Graphics.Vulkan.Core10.Image
  ( VkImageLayout(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_surface
  ( VkPresentModeKHR(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_swapchain
  ( VkSwapchainKHR(..)
  )


-- No documentation found for Nested "VkImageLayout" "VK_IMAGE_LAYOUT_SHARED_PRESENT_KHR"
pattern VK_IMAGE_LAYOUT_SHARED_PRESENT_KHR :: VkImageLayout
pattern VK_IMAGE_LAYOUT_SHARED_PRESENT_KHR = VkImageLayout 1000111000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SHARED_PRESENT_SURFACE_CAPABILITIES_KHR"
pattern VK_STRUCTURE_TYPE_SHARED_PRESENT_SURFACE_CAPABILITIES_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_SHARED_PRESENT_SURFACE_CAPABILITIES_KHR = VkStructureType 1000111000
-- No documentation found for Nested "VkPresentModeKHR" "VK_PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR"
pattern VK_PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR :: VkPresentModeKHR
pattern VK_PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR = VkPresentModeKHR 1000111000
-- No documentation found for Nested "VkPresentModeKHR" "VK_PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR"
pattern VK_PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR :: VkPresentModeKHR
pattern VK_PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR = VkPresentModeKHR 1000111001
-- No documentation found for TopLevel "VK_KHR_SHARED_PRESENTABLE_IMAGE_SPEC_VERSION"
pattern VK_KHR_SHARED_PRESENTABLE_IMAGE_SPEC_VERSION :: Integral a => a
pattern VK_KHR_SHARED_PRESENTABLE_IMAGE_SPEC_VERSION = 1
-- No documentation found for TopLevel "VK_KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME"
pattern VK_KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME = "VK_KHR_shared_presentable_image"
-- | vkGetSwapchainStatusKHR - Get a swapchain’s status
--
-- = Parameters
--
-- -   @device@ is the device associated with @swapchain@.
--
-- -   @swapchain@ is the swapchain to query.
--
-- = Description
--
-- Unresolved directive in vkGetSwapchainStatusKHR.txt -
-- include::..\/validity\/protos\/vkGetSwapchainStatusKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetSwapchainStatusKHR" vkGetSwapchainStatusKHR :: ("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> IO VkResult
-- | VkSharedPresentSurfaceCapabilitiesKHR - structure describing
-- capabilities of a surface for shared presentation
--
-- = Description
--
-- Unresolved directive in VkSharedPresentSurfaceCapabilitiesKHR.txt -
-- include::..\/validity\/structs\/VkSharedPresentSurfaceCapabilitiesKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkSharedPresentSurfaceCapabilitiesKHR = VkSharedPresentSurfaceCapabilitiesKHR
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @sharedPresentSupportedUsageFlags@ is a bitmask of
  -- 'Graphics.Vulkan.Core10.DeviceInitialization.VkImageUsageFlagBits'
  -- representing the ways the application /can/ use the shared presentable
  -- image from a swapchain created with
  -- 'Graphics.Vulkan.Extensions.VK_KHR_surface.VkPresentModeKHR' set to
  -- @VK_PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR@ or
  -- @VK_PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR@ for the surface on the
  -- specified device. @VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT@ /must/ be
  -- included in the set but implementations /may/ support additional usages.
  vkSharedPresentSupportedUsageFlags :: VkImageUsageFlags
  }
  deriving (Eq, Show)

instance Storable VkSharedPresentSurfaceCapabilitiesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkSharedPresentSurfaceCapabilitiesKHR <$> peek (ptr `plusPtr` 0)
                                                   <*> peek (ptr `plusPtr` 8)
                                                   <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSharedPresentSurfaceCapabilitiesKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSharedPresentSurfaceCapabilitiesKHR))
                *> poke (ptr `plusPtr` 16) (vkSharedPresentSupportedUsageFlags (poked :: VkSharedPresentSurfaceCapabilitiesKHR))
