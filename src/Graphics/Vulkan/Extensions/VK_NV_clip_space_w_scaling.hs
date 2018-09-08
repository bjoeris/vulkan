{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_NV_clip_space_w_scaling
  ( pattern VK_DYNAMIC_STATE_VIEWPORT_W_SCALING_NV
  , pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV
  , pattern VK_NV_CLIP_SPACE_W_SCALING_SPEC_VERSION
  , pattern VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME
  , vkCmdSetViewportWScalingNV
  , VkViewportWScalingNV(..)
  , VkPipelineViewportWScalingStateCreateInfoNV(..)
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  )
import Foreign.C.Types
  ( CFloat(..)
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
  ( VkBool32(..)
  , VkStructureType(..)
  )
import Graphics.Vulkan.Core10.Pipeline
  ( VkDynamicState(..)
  )
import Graphics.Vulkan.Core10.Queue
  ( VkCommandBuffer
  )


-- | @VK_DYNAMIC_STATE_VIEWPORT_W_SCALING_NV@ specifies that the
-- @pViewportScalings@ state in
-- @VkPipelineViewportWScalingStateCreateInfoNV@ will be ignored and /must/
-- be set dynamically with 'vkCmdSetViewportWScalingNV' before any draws
-- are performed with a pipeline state with
-- @VkPipelineViewportWScalingStateCreateInfo@ member
-- @viewportScalingEnable@ set to @VK_TRUE@
pattern VK_DYNAMIC_STATE_VIEWPORT_W_SCALING_NV :: VkDynamicState
pattern VK_DYNAMIC_STATE_VIEWPORT_W_SCALING_NV = VkDynamicState 1000087000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV"
pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV = VkStructureType 1000087000
-- No documentation found for TopLevel "VK_NV_CLIP_SPACE_W_SCALING_SPEC_VERSION"
pattern VK_NV_CLIP_SPACE_W_SCALING_SPEC_VERSION :: Integral a => a
pattern VK_NV_CLIP_SPACE_W_SCALING_SPEC_VERSION = 1
-- No documentation found for TopLevel "VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME"
pattern VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME = "VK_NV_clip_space_w_scaling"
-- | vkCmdSetViewportWScalingNV - Set the viewport W scaling on a command
-- buffer
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @firstViewport@ is the index of the first viewport whose parameters
--     are updated by the command.
--
-- -   @viewportCount@ is the number of viewports whose parameters are
--     updated by the command.
--
-- -   @pViewportWScalings@ is a pointer to an array of
--     'VkViewportWScalingNV' structures specifying viewport parameters.
--
-- = Description
--
-- The viewport parameters taken from element i of @pViewportWScalings@
-- replace the current state for the viewport index @firstViewport@ + i,
-- for i in [0, @viewportCount@).
--
-- == Valid Usage
--
-- -   The bound graphics pipeline /must/ have been created with the
--     @VK_DYNAMIC_STATE_VIEWPORT_W_SCALING_NV@ dynamic state enabled
--
-- -   @firstViewport@ /must/ be less than
--     'Graphics.Vulkan.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxViewports@
--
-- -   The sum of @firstViewport@ and @viewportCount@ /must/ be between @1@
--     and
--     'Graphics.Vulkan.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxViewports@,
--     inclusive
--
-- Unresolved directive in vkCmdSetViewportWScalingNV.txt -
-- include::..\/validity\/protos\/vkCmdSetViewportWScalingNV.txt[]
--
-- = See Also
--
-- No cross-references are available
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdSetViewportWScalingNV" vkCmdSetViewportWScalingNV :: ("commandBuffer" ::: VkCommandBuffer) -> ("firstViewport" ::: Word32) -> ("viewportCount" ::: Word32) -> ("pViewportWScalings" ::: Ptr VkViewportWScalingNV) -> IO ()
-- | VkViewportWScalingNV - Structure specifying a viewport
--
-- = Description
--
-- Unresolved directive in VkViewportWScalingNV.txt -
-- include::..\/validity\/structs\/VkViewportWScalingNV.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkViewportWScalingNV = VkViewportWScalingNV
  { -- | @xcoeff@ and @ycoeff@ are the viewport’s W scaling factor for x and y
  -- respectively.
  vkXcoeff :: CFloat
  , -- No documentation found for Nested "VkViewportWScalingNV" "ycoeff"
  vkYcoeff :: CFloat
  }
  deriving (Eq, Show)

instance Storable VkViewportWScalingNV where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek ptr = VkViewportWScalingNV <$> peek (ptr `plusPtr` 0)
                                  <*> peek (ptr `plusPtr` 4)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkXcoeff (poked :: VkViewportWScalingNV))
                *> poke (ptr `plusPtr` 4) (vkYcoeff (poked :: VkViewportWScalingNV))
-- | VkPipelineViewportWScalingStateCreateInfoNV - Structure specifying
-- parameters of a newly created pipeline viewport W scaling state
--
-- = Description
--
-- Unresolved directive in VkPipelineViewportWScalingStateCreateInfoNV.txt
-- -
-- include::..\/validity\/structs\/VkPipelineViewportWScalingStateCreateInfoNV.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkPipelineViewportWScalingStateCreateInfoNV = VkPipelineViewportWScalingStateCreateInfoNV
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @viewportWScalingEnable@ controls whether viewport __W__ scaling is
  -- enabled.
  vkViewportWScalingEnable :: VkBool32
  , -- | @viewportCount@ is the number of viewports used by __W__ scaling, and
  -- /must/ match the number of viewports in the pipeline if viewport __W__
  -- scaling is enabled.
  vkViewportCount :: Word32
  , -- | @pViewportWScalings@ is a pointer to an array of @VkViewportWScalingNV@
  -- structures, which define the __W__ scaling parameters for the
  -- corresponding viewport. If the viewport __W__ scaling state is dynamic,
  -- this member is ignored.
  vkPViewportWScalings :: Ptr VkViewportWScalingNV
  }
  deriving (Eq, Show)

instance Storable VkPipelineViewportWScalingStateCreateInfoNV where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkPipelineViewportWScalingStateCreateInfoNV <$> peek (ptr `plusPtr` 0)
                                                         <*> peek (ptr `plusPtr` 8)
                                                         <*> peek (ptr `plusPtr` 16)
                                                         <*> peek (ptr `plusPtr` 20)
                                                         <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineViewportWScalingStateCreateInfoNV))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineViewportWScalingStateCreateInfoNV))
                *> poke (ptr `plusPtr` 16) (vkViewportWScalingEnable (poked :: VkPipelineViewportWScalingStateCreateInfoNV))
                *> poke (ptr `plusPtr` 20) (vkViewportCount (poked :: VkPipelineViewportWScalingStateCreateInfoNV))
                *> poke (ptr `plusPtr` 24) (vkPViewportWScalings (poked :: VkPipelineViewportWScalingStateCreateInfoNV))
