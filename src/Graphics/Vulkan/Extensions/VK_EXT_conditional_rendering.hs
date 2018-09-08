{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_conditional_rendering
  ( VkConditionalRenderingFlagBitsEXT(..)
  , pattern VK_CONDITIONAL_RENDERING_INVERTED_BIT_EXT
  , pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_CONDITIONAL_RENDERING_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONDITIONAL_RENDERING_FEATURES_EXT
  , pattern VK_STRUCTURE_TYPE_CONDITIONAL_RENDERING_BEGIN_INFO_EXT
  , pattern VK_ACCESS_CONDITIONAL_RENDERING_READ_BIT_EXT
  , pattern VK_BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT
  , pattern VK_PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT
  , pattern VK_EXT_CONDITIONAL_RENDERING_SPEC_VERSION
  , pattern VK_EXT_CONDITIONAL_RENDERING_EXTENSION_NAME
  , vkCmdBeginConditionalRenderingEXT
  , vkCmdEndConditionalRenderingEXT
  , VkConditionalRenderingBeginInfoEXT(..)
  , VkCommandBufferInheritanceConditionalRenderingInfoEXT(..)
  , VkPhysicalDeviceConditionalRenderingFeaturesEXT(..)
  , VkConditionalRenderingFlagsEXT
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


import Graphics.Vulkan.Core10.Buffer
  ( VkBufferUsageFlagBits(..)
  )
import Graphics.Vulkan.Core10.Core
  ( VkBool32(..)
  , VkStructureType(..)
  , VkFlags
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkDeviceSize
  )
import Graphics.Vulkan.Core10.MemoryManagement
  ( VkBuffer(..)
  )
import Graphics.Vulkan.Core10.Pass
  ( VkAccessFlagBits(..)
  )
import Graphics.Vulkan.Core10.Queue
  ( VkPipelineStageFlagBits(..)
  , VkCommandBuffer
  )


-- ** VkConditionalRenderingFlagBitsEXT

-- | VkConditionalRenderingFlagBitsEXT - Specify the behavior of conditional
-- rendering
--
-- = See Also
--
-- No cross-references are available
newtype VkConditionalRenderingFlagBitsEXT = VkConditionalRenderingFlagBitsEXT VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkConditionalRenderingFlagBitsEXT where
  showsPrec _ VK_CONDITIONAL_RENDERING_INVERTED_BIT_EXT = showString "VK_CONDITIONAL_RENDERING_INVERTED_BIT_EXT"
  showsPrec p (VkConditionalRenderingFlagBitsEXT x) = showParen (p >= 11) (showString "VkConditionalRenderingFlagBitsEXT " . showsPrec 11 x)

instance Read VkConditionalRenderingFlagBitsEXT where
  readPrec = parens ( choose [ ("VK_CONDITIONAL_RENDERING_INVERTED_BIT_EXT", pure VK_CONDITIONAL_RENDERING_INVERTED_BIT_EXT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkConditionalRenderingFlagBitsEXT")
                        v <- step readPrec
                        pure (VkConditionalRenderingFlagBitsEXT v)
                        )
                    )

-- | @VK_CONDITIONAL_RENDERING_INVERTED_BIT_EXT@ specifies the condition used
-- to determine whether to discard rendering commands or not. That is, if
-- the 32-bit predicate read from @buffer@ memory at @offset@ is zero, the
-- rendering commands are not discarded, and if non zero, then they are
-- discarded.
pattern VK_CONDITIONAL_RENDERING_INVERTED_BIT_EXT :: VkConditionalRenderingFlagBitsEXT
pattern VK_CONDITIONAL_RENDERING_INVERTED_BIT_EXT = VkConditionalRenderingFlagBitsEXT 0x00000001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_CONDITIONAL_RENDERING_INFO_EXT"
pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_CONDITIONAL_RENDERING_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_CONDITIONAL_RENDERING_INFO_EXT = VkStructureType 1000081000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONDITIONAL_RENDERING_FEATURES_EXT"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONDITIONAL_RENDERING_FEATURES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONDITIONAL_RENDERING_FEATURES_EXT = VkStructureType 1000081001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_CONDITIONAL_RENDERING_BEGIN_INFO_EXT"
pattern VK_STRUCTURE_TYPE_CONDITIONAL_RENDERING_BEGIN_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_CONDITIONAL_RENDERING_BEGIN_INFO_EXT = VkStructureType 1000081002
-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_CONDITIONAL_RENDERING_READ_BIT_EXT"
pattern VK_ACCESS_CONDITIONAL_RENDERING_READ_BIT_EXT :: VkAccessFlagBits
pattern VK_ACCESS_CONDITIONAL_RENDERING_READ_BIT_EXT = VkAccessFlagBits 0x00100000
-- | @VK_BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT@ specifies that the
-- buffer is suitable for passing as the @buffer@ parameter to
-- 'vkCmdBeginConditionalRenderingEXT'.
pattern VK_BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT :: VkBufferUsageFlagBits
pattern VK_BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT = VkBufferUsageFlagBits 0x00000200
-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT"
pattern VK_PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT :: VkPipelineStageFlagBits
pattern VK_PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT = VkPipelineStageFlagBits 0x00040000
-- No documentation found for TopLevel "VK_EXT_CONDITIONAL_RENDERING_SPEC_VERSION"
pattern VK_EXT_CONDITIONAL_RENDERING_SPEC_VERSION :: Integral a => a
pattern VK_EXT_CONDITIONAL_RENDERING_SPEC_VERSION = 1
-- No documentation found for TopLevel "VK_EXT_CONDITIONAL_RENDERING_EXTENSION_NAME"
pattern VK_EXT_CONDITIONAL_RENDERING_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_EXT_CONDITIONAL_RENDERING_EXTENSION_NAME = "VK_EXT_conditional_rendering"
-- | vkCmdBeginConditionalRenderingEXT - Define the beginning of a
-- conditional rendering block
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which this command will
--     be recorded.
--
-- -   @pConditionalRenderingBegin@ is a pointer to an instance of the
--     'VkConditionalRenderingBeginInfoEXT' structure specifying the
--     parameters of conditional rendering.
--
-- == Valid Usage
--
-- -   Conditional rendering /must/ not already be
--     [active](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#active-conditional-rendering)
--
-- Unresolved directive in vkCmdBeginConditionalRenderingEXT.txt -
-- include::..\/validity\/protos\/vkCmdBeginConditionalRenderingEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdBeginConditionalRenderingEXT" vkCmdBeginConditionalRenderingEXT :: ("commandBuffer" ::: VkCommandBuffer) -> ("pConditionalRenderingBegin" ::: Ptr VkConditionalRenderingBeginInfoEXT) -> IO ()
-- | vkCmdEndConditionalRenderingEXT - Define the end of a conditional
-- rendering block
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which this command will
--     be recorded.
--
-- = Description
--
-- Once ended, conditional rendering becomes inactive.
--
-- == Valid Usage
--
-- -   Conditional rendering /must/ be
--     [active](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#active-conditional-rendering)
--
-- -   If conditional rendering was made
--     [active](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#active-conditional-rendering)
--     outside of a render pass instance, it must not be ended inside a
--     render pass instance
--
-- -   If conditional rendering was made
--     [active](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#active-conditional-rendering)
--     within a subpass it must be ended in the same subpass
--
-- Unresolved directive in vkCmdEndConditionalRenderingEXT.txt -
-- include::..\/validity\/protos\/vkCmdEndConditionalRenderingEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdEndConditionalRenderingEXT" vkCmdEndConditionalRenderingEXT :: ("commandBuffer" ::: VkCommandBuffer) -> IO ()
-- No documentation found for TopLevel "VkConditionalRenderingBeginInfoEXT"
data VkConditionalRenderingBeginInfoEXT = VkConditionalRenderingBeginInfoEXT
  { -- No documentation found for Nested "VkConditionalRenderingBeginInfoEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkConditionalRenderingBeginInfoEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkConditionalRenderingBeginInfoEXT" "buffer"
  vkBuffer :: VkBuffer
  , -- No documentation found for Nested "VkConditionalRenderingBeginInfoEXT" "offset"
  vkOffset :: VkDeviceSize
  , -- No documentation found for Nested "VkConditionalRenderingBeginInfoEXT" "flags"
  vkFlags :: VkConditionalRenderingFlagsEXT
  }
  deriving (Eq, Show)

instance Storable VkConditionalRenderingBeginInfoEXT where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkConditionalRenderingBeginInfoEXT <$> peek (ptr `plusPtr` 0)
                                                <*> peek (ptr `plusPtr` 8)
                                                <*> peek (ptr `plusPtr` 16)
                                                <*> peek (ptr `plusPtr` 24)
                                                <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkConditionalRenderingBeginInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkConditionalRenderingBeginInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkBuffer (poked :: VkConditionalRenderingBeginInfoEXT))
                *> poke (ptr `plusPtr` 24) (vkOffset (poked :: VkConditionalRenderingBeginInfoEXT))
                *> poke (ptr `plusPtr` 32) (vkFlags (poked :: VkConditionalRenderingBeginInfoEXT))
-- | VkCommandBufferInheritanceConditionalRenderingInfoEXT - Structure
-- specifying command buffer inheritance info
--
-- = Description
--
-- If this structure is not present, the behavior is as if
-- @conditionalRenderingEnable@ is @VK_FALSE@.
--
-- == Valid Usage
--
-- -   If the [inherited conditional
--     rendering](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-inheritedConditionalRendering)
--     feature is not enabled, @conditionalRenderingEnable@ /must/ be
--     @VK_FALSE@
--
-- Unresolved directive in
-- VkCommandBufferInheritanceConditionalRenderingInfoEXT.txt -
-- include::..\/validity\/structs\/VkCommandBufferInheritanceConditionalRenderingInfoEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkCommandBufferInheritanceConditionalRenderingInfoEXT = VkCommandBufferInheritanceConditionalRenderingInfoEXT
  { -- | @sType@ is the type of this structure
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure
  vkPNext :: Ptr ()
  , -- | @conditionalRenderingEnable@ specifies whether the command buffer /can/
  -- be executed while conditional rendering is active in the primary command
  -- buffer. If this is @VK_TRUE@, then this command buffer /can/ be executed
  -- whether the primary command buffer has active conditional rendering or
  -- not. If this is @VK_FALSE@, then the primary command buffer /must/ not
  -- have conditional rendering active.
  vkConditionalRenderingEnable :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkCommandBufferInheritanceConditionalRenderingInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkCommandBufferInheritanceConditionalRenderingInfoEXT <$> peek (ptr `plusPtr` 0)
                                                                   <*> peek (ptr `plusPtr` 8)
                                                                   <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkCommandBufferInheritanceConditionalRenderingInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkCommandBufferInheritanceConditionalRenderingInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkConditionalRenderingEnable (poked :: VkCommandBufferInheritanceConditionalRenderingInfoEXT))
-- | VkPhysicalDeviceConditionalRenderingFeaturesEXT - Structure describing
-- if a secondary command buffer can be executed if conditional rendering
-- is active in the primary command buffer
--
-- = Description
--
-- If the @VkPhysicalDeviceConditionalRenderingFeaturesEXT@ structure is
-- included in the @pNext@ chain of
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2',
-- it is filled with values indicating the implementation-dependent
-- behavior. @VkPhysicalDeviceConditionalRenderingFeaturesEXT@ /can/ also
-- be used in @pNext@ chain of
-- 'Graphics.Vulkan.Core10.Device.VkDeviceCreateInfo' to enable the
-- features.
--
-- Unresolved directive in
-- VkPhysicalDeviceConditionalRenderingFeaturesEXT.txt -
-- include::..\/validity\/structs\/VkPhysicalDeviceConditionalRenderingFeaturesEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkPhysicalDeviceConditionalRenderingFeaturesEXT = VkPhysicalDeviceConditionalRenderingFeaturesEXT
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @conditionalRendering@ specifies whether conditional rendering is
  -- supported.
  vkConditionalRendering :: VkBool32
  , -- | @inheritedConditionalRendering@ specifies whether a secondary command
  -- buffer /can/ be executed while conditional rendering is active in the
  -- primary command buffer.
  vkInheritedConditionalRendering :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceConditionalRenderingFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceConditionalRenderingFeaturesEXT <$> peek (ptr `plusPtr` 0)
                                                             <*> peek (ptr `plusPtr` 8)
                                                             <*> peek (ptr `plusPtr` 16)
                                                             <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceConditionalRenderingFeaturesEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceConditionalRenderingFeaturesEXT))
                *> poke (ptr `plusPtr` 16) (vkConditionalRendering (poked :: VkPhysicalDeviceConditionalRenderingFeaturesEXT))
                *> poke (ptr `plusPtr` 20) (vkInheritedConditionalRendering (poked :: VkPhysicalDeviceConditionalRenderingFeaturesEXT))
-- | VkConditionalRenderingFlagsEXT - Bitmask of
-- VkConditionalRenderingFlagBitsEXT
--
-- = Description
--
-- @VkConditionalRenderingFlagsEXT@ is a mask of zero or more
-- 'VkConditionalRenderingFlagBitsEXT'. It is used as a member and\/or
-- parameter of the structures and commands in the See Also section below.
--
-- = See Also
--
-- No cross-references are available
type VkConditionalRenderingFlagsEXT = VkConditionalRenderingFlagBitsEXT
