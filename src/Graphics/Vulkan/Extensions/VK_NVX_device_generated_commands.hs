{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands
  ( VkIndirectCommandsTokenTypeNVX(..)
  , pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX
  , pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DESCRIPTOR_SET_NVX
  , pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NVX
  , pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NVX
  , pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NVX
  , pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NVX
  , pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX
  , pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX
  , VkObjectEntryTypeNVX(..)
  , pattern VK_OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX
  , pattern VK_OBJECT_ENTRY_TYPE_PIPELINE_NVX
  , pattern VK_OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX
  , pattern VK_OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX
  , pattern VK_OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX
  , VkIndirectCommandsLayoutUsageFlagBitsNVX(..)
  , pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX
  , pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX
  , pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX
  , pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX
  , VkObjectEntryUsageFlagBitsNVX(..)
  , pattern VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX
  , pattern VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX
  , pattern VK_STRUCTURE_TYPE_OBJECT_TABLE_CREATE_INFO_NVX
  , pattern VK_STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NVX
  , pattern VK_STRUCTURE_TYPE_CMD_PROCESS_COMMANDS_INFO_NVX
  , pattern VK_STRUCTURE_TYPE_CMD_RESERVE_SPACE_FOR_COMMANDS_INFO_NVX
  , pattern VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_LIMITS_NVX
  , pattern VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_FEATURES_NVX
  , pattern VK_OBJECT_TYPE_OBJECT_TABLE_NVX
  , pattern VK_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NVX
  , pattern VK_ACCESS_COMMAND_PROCESS_READ_BIT_NVX
  , pattern VK_ACCESS_COMMAND_PROCESS_WRITE_BIT_NVX
  , pattern VK_PIPELINE_STAGE_COMMAND_PROCESS_BIT_NVX
  , pattern VK_NVX_DEVICE_GENERATED_COMMANDS_SPEC_VERSION
  , pattern VK_NVX_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME
  , VkObjectTableNVX(..)
  , VkIndirectCommandsLayoutNVX(..)
  , vkCmdProcessCommandsNVX
  , vkCmdReserveSpaceForCommandsNVX
  , vkCreateIndirectCommandsLayoutNVX
  , vkDestroyIndirectCommandsLayoutNVX
  , vkCreateObjectTableNVX
  , vkDestroyObjectTableNVX
  , vkRegisterObjectsNVX
  , vkUnregisterObjectsNVX
  , vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX
  , VkDeviceGeneratedCommandsFeaturesNVX(..)
  , VkDeviceGeneratedCommandsLimitsNVX(..)
  , VkIndirectCommandsTokenNVX(..)
  , VkIndirectCommandsLayoutTokenNVX(..)
  , VkIndirectCommandsLayoutCreateInfoNVX(..)
  , VkCmdProcessCommandsInfoNVX(..)
  , VkCmdReserveSpaceForCommandsInfoNVX(..)
  , VkObjectTableCreateInfoNVX(..)
  , VkObjectTableEntryNVX(..)
  , VkObjectTablePipelineEntryNVX(..)
  , VkObjectTableDescriptorSetEntryNVX(..)
  , VkObjectTableVertexBufferEntryNVX(..)
  , VkObjectTableIndexBufferEntryNVX(..)
  , VkObjectTablePushConstantEntryNVX(..)
  , VkIndirectCommandsLayoutUsageFlagsNVX
  , VkObjectEntryUsageFlagsNVX
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.Int
  ( Int32
  )
import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  , Word64
  )
import Foreign.Ptr
  ( Ptr
  , castPtr
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


import Graphics.Vulkan.Core10.CommandBufferBuilding
  ( VkIndexType(..)
  )
import Graphics.Vulkan.Core10.Core
  ( VkBool32(..)
  , VkObjectType(..)
  , VkResult(..)
  , VkStructureType(..)
  , VkFlags
  )
import Graphics.Vulkan.Core10.DescriptorSet
  ( VkDescriptorSet(..)
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkDevice
  , VkDeviceSize
  , VkPhysicalDevice
  )
import Graphics.Vulkan.Core10.MemoryManagement
  ( VkBuffer(..)
  )
import Graphics.Vulkan.Core10.Pass
  ( VkAccessFlagBits(..)
  , VkPipelineBindPoint(..)
  )
import Graphics.Vulkan.Core10.Pipeline
  ( VkPipeline(..)
  , VkPipelineLayout(..)
  )
import Graphics.Vulkan.Core10.PipelineLayout
  ( VkShaderStageFlags
  )
import Graphics.Vulkan.Core10.Queue
  ( VkPipelineStageFlagBits(..)
  , VkCommandBuffer
  )


-- ** VkIndirectCommandsTokenTypeNVX

-- | VkIndirectCommandsTokenTypeNVX - Enum specifying
--
-- = Description
--
-- \'
--
-- +------------------------------------------------------+----------------------------+
-- | Token type                                           | Equivalent command         |
-- +======================================================+============================+
-- | @VK_INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX@       | @vkCmdBindPipeline@        |
-- +------------------------------------------------------+----------------------------+
-- | @VK_INDIRECT_COMMANDS_TOKEN_TYPE_DESCRIPTOR_SET_NVX@ | @vkCmdBindDescriptorSets@  |
-- +------------------------------------------------------+----------------------------+
-- | @VK_INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NVX@   | @vkCmdBindIndexBuffer@     |
-- +------------------------------------------------------+----------------------------+
-- | @VK_INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NVX@  | @vkCmdBindVertexBuffers@   |
-- +------------------------------------------------------+----------------------------+
-- | @VK_INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NVX@  | @vkCmdPushConstants@       |
-- +------------------------------------------------------+----------------------------+
-- | @VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NVX@   | @vkCmdDrawIndexedIndirect@ |
-- +------------------------------------------------------+----------------------------+
-- | @VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX@           | @vkCmdDrawIndirect@        |
-- +------------------------------------------------------+----------------------------+
-- | @VK_INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX@       | @vkCmdDispatchIndirect@    |
-- +------------------------------------------------------+----------------------------+
--
-- Supported indirect command tokens
--
-- = See Also
--
-- No cross-references are available
newtype VkIndirectCommandsTokenTypeNVX = VkIndirectCommandsTokenTypeNVX Int32
  deriving (Eq, Ord, Storable)

instance Show VkIndirectCommandsTokenTypeNVX where
  showsPrec _ VK_INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX = showString "VK_INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX"
  showsPrec _ VK_INDIRECT_COMMANDS_TOKEN_TYPE_DESCRIPTOR_SET_NVX = showString "VK_INDIRECT_COMMANDS_TOKEN_TYPE_DESCRIPTOR_SET_NVX"
  showsPrec _ VK_INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NVX = showString "VK_INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NVX"
  showsPrec _ VK_INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NVX = showString "VK_INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NVX"
  showsPrec _ VK_INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NVX = showString "VK_INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NVX"
  showsPrec _ VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NVX = showString "VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NVX"
  showsPrec _ VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX = showString "VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX"
  showsPrec _ VK_INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX = showString "VK_INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX"
  showsPrec p (VkIndirectCommandsTokenTypeNVX x) = showParen (p >= 11) (showString "VkIndirectCommandsTokenTypeNVX " . showsPrec 11 x)

instance Read VkIndirectCommandsTokenTypeNVX where
  readPrec = parens ( choose [ ("VK_INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX",       pure VK_INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX)
                             , ("VK_INDIRECT_COMMANDS_TOKEN_TYPE_DESCRIPTOR_SET_NVX", pure VK_INDIRECT_COMMANDS_TOKEN_TYPE_DESCRIPTOR_SET_NVX)
                             , ("VK_INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NVX",   pure VK_INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NVX)
                             , ("VK_INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NVX",  pure VK_INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NVX)
                             , ("VK_INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NVX",  pure VK_INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NVX)
                             , ("VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NVX",   pure VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NVX)
                             , ("VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX",           pure VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX)
                             , ("VK_INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX",       pure VK_INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkIndirectCommandsTokenTypeNVX")
                        v <- step readPrec
                        pure (VkIndirectCommandsTokenTypeNVX v)
                        )
                    )

-- No documentation found for Nested "VkIndirectCommandsTokenTypeNVX" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX"
pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX :: VkIndirectCommandsTokenTypeNVX
pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX = VkIndirectCommandsTokenTypeNVX 0

-- No documentation found for Nested "VkIndirectCommandsTokenTypeNVX" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_DESCRIPTOR_SET_NVX"
pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DESCRIPTOR_SET_NVX :: VkIndirectCommandsTokenTypeNVX
pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DESCRIPTOR_SET_NVX = VkIndirectCommandsTokenTypeNVX 1

-- No documentation found for Nested "VkIndirectCommandsTokenTypeNVX" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NVX"
pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NVX :: VkIndirectCommandsTokenTypeNVX
pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NVX = VkIndirectCommandsTokenTypeNVX 2

-- No documentation found for Nested "VkIndirectCommandsTokenTypeNVX" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NVX"
pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NVX :: VkIndirectCommandsTokenTypeNVX
pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NVX = VkIndirectCommandsTokenTypeNVX 3

-- No documentation found for Nested "VkIndirectCommandsTokenTypeNVX" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NVX"
pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NVX :: VkIndirectCommandsTokenTypeNVX
pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NVX = VkIndirectCommandsTokenTypeNVX 4

-- No documentation found for Nested "VkIndirectCommandsTokenTypeNVX" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NVX"
pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NVX :: VkIndirectCommandsTokenTypeNVX
pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NVX = VkIndirectCommandsTokenTypeNVX 5

-- No documentation found for Nested "VkIndirectCommandsTokenTypeNVX" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX"
pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX :: VkIndirectCommandsTokenTypeNVX
pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX = VkIndirectCommandsTokenTypeNVX 6

-- No documentation found for Nested "VkIndirectCommandsTokenTypeNVX" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX"
pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX :: VkIndirectCommandsTokenTypeNVX
pattern VK_INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX = VkIndirectCommandsTokenTypeNVX 7
-- ** VkObjectEntryTypeNVX

-- | VkObjectEntryTypeNVX - Enum specifying object table entry type
--
-- = See Also
--
-- No cross-references are available
newtype VkObjectEntryTypeNVX = VkObjectEntryTypeNVX Int32
  deriving (Eq, Ord, Storable)

instance Show VkObjectEntryTypeNVX where
  showsPrec _ VK_OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX = showString "VK_OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX"
  showsPrec _ VK_OBJECT_ENTRY_TYPE_PIPELINE_NVX = showString "VK_OBJECT_ENTRY_TYPE_PIPELINE_NVX"
  showsPrec _ VK_OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX = showString "VK_OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX"
  showsPrec _ VK_OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX = showString "VK_OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX"
  showsPrec _ VK_OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX = showString "VK_OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX"
  showsPrec p (VkObjectEntryTypeNVX x) = showParen (p >= 11) (showString "VkObjectEntryTypeNVX " . showsPrec 11 x)

instance Read VkObjectEntryTypeNVX where
  readPrec = parens ( choose [ ("VK_OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX", pure VK_OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX)
                             , ("VK_OBJECT_ENTRY_TYPE_PIPELINE_NVX",       pure VK_OBJECT_ENTRY_TYPE_PIPELINE_NVX)
                             , ("VK_OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX",   pure VK_OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX)
                             , ("VK_OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX",  pure VK_OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX)
                             , ("VK_OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX",  pure VK_OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkObjectEntryTypeNVX")
                        v <- step readPrec
                        pure (VkObjectEntryTypeNVX v)
                        )
                    )

-- | @VK_OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX@ specifies a @VkDescriptorSet@
-- resource entry that is registered via
-- @VkObjectTableDescriptorSetEntryNVX@.
pattern VK_OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX :: VkObjectEntryTypeNVX
pattern VK_OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX = VkObjectEntryTypeNVX 0

-- | @VK_OBJECT_ENTRY_TYPE_PIPELINE_NVX@ specifies a @VkPipeline@ resource
-- entry that is registered via @VkObjectTablePipelineEntryNVX@.
pattern VK_OBJECT_ENTRY_TYPE_PIPELINE_NVX :: VkObjectEntryTypeNVX
pattern VK_OBJECT_ENTRY_TYPE_PIPELINE_NVX = VkObjectEntryTypeNVX 1

-- | @VK_OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX@ specifies a @VkBuffer@ resource
-- entry that is registered via @VkObjectTableIndexBufferEntryNVX@.
pattern VK_OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX :: VkObjectEntryTypeNVX
pattern VK_OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX = VkObjectEntryTypeNVX 2

-- | @VK_OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX@ specifies a @VkBuffer@ resource
-- entry that is registered via @VkObjectTableVertexBufferEntryNVX@.
pattern VK_OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX :: VkObjectEntryTypeNVX
pattern VK_OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX = VkObjectEntryTypeNVX 3

-- | @VK_OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX@ specifies the resource entry is
-- registered via @VkObjectTablePushConstantEntryNVX@.
pattern VK_OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX :: VkObjectEntryTypeNVX
pattern VK_OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX = VkObjectEntryTypeNVX 4
-- ** VkIndirectCommandsLayoutUsageFlagBitsNVX

-- | VkIndirectCommandsLayoutUsageFlagBitsNVX - Bitmask specifying allowed
-- usage of an indirect commands layout
--
-- = See Also
--
-- No cross-references are available
newtype VkIndirectCommandsLayoutUsageFlagBitsNVX = VkIndirectCommandsLayoutUsageFlagBitsNVX VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkIndirectCommandsLayoutUsageFlagBitsNVX where
  showsPrec _ VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX = showString "VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX"
  showsPrec _ VK_INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX = showString "VK_INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX"
  showsPrec _ VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX = showString "VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX"
  showsPrec _ VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX = showString "VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX"
  showsPrec p (VkIndirectCommandsLayoutUsageFlagBitsNVX x) = showParen (p >= 11) (showString "VkIndirectCommandsLayoutUsageFlagBitsNVX " . showsPrec 11 x)

instance Read VkIndirectCommandsLayoutUsageFlagBitsNVX where
  readPrec = parens ( choose [ ("VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX", pure VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX)
                             , ("VK_INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX",    pure VK_INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX)
                             , ("VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX",    pure VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX)
                             , ("VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX",   pure VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkIndirectCommandsLayoutUsageFlagBitsNVX")
                        v <- step readPrec
                        pure (VkIndirectCommandsLayoutUsageFlagBitsNVX v)
                        )
                    )

-- | @VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX@
-- specifies that the processing of sequences /can/ happen at an
-- implementation-dependent order, which is not guaranteed to be coherent
-- across multiple invocations.
pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX :: VkIndirectCommandsLayoutUsageFlagBitsNVX
pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX = VkIndirectCommandsLayoutUsageFlagBitsNVX 0x00000001

-- | @VK_INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX@ specifies
-- that there is likely a high difference between allocated number of
-- sequences and actually used.
pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX :: VkIndirectCommandsLayoutUsageFlagBitsNVX
pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_SPARSE_SEQUENCES_BIT_NVX = VkIndirectCommandsLayoutUsageFlagBitsNVX 0x00000002

-- | @VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX@ specifies
-- that there are likely many draw or dispatch calls that are zero-sized
-- (zero grid dimension, no primitives to render).
pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX :: VkIndirectCommandsLayoutUsageFlagBitsNVX
pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EMPTY_EXECUTIONS_BIT_NVX = VkIndirectCommandsLayoutUsageFlagBitsNVX 0x00000004

-- | @VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX@ specifies
-- that the input data for the sequences is not implicitly indexed from
-- 0..sequencesUsed but a user provided @VkBuffer@ encoding the index is
-- provided.
pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX :: VkIndirectCommandsLayoutUsageFlagBitsNVX
pattern VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX = VkIndirectCommandsLayoutUsageFlagBitsNVX 0x00000008
-- ** VkObjectEntryUsageFlagBitsNVX

-- | VkObjectEntryUsageFlagBitsNVX - Bitmask specifying allowed usage of an
-- object entry
--
-- = See Also
--
-- No cross-references are available
newtype VkObjectEntryUsageFlagBitsNVX = VkObjectEntryUsageFlagBitsNVX VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkObjectEntryUsageFlagBitsNVX where
  showsPrec _ VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX = showString "VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX"
  showsPrec _ VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX = showString "VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX"
  showsPrec p (VkObjectEntryUsageFlagBitsNVX x) = showParen (p >= 11) (showString "VkObjectEntryUsageFlagBitsNVX " . showsPrec 11 x)

instance Read VkObjectEntryUsageFlagBitsNVX where
  readPrec = parens ( choose [ ("VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX", pure VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX)
                             , ("VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX",  pure VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkObjectEntryUsageFlagBitsNVX")
                        v <- step readPrec
                        pure (VkObjectEntryUsageFlagBitsNVX v)
                        )
                    )

-- | @VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX@ specifies that the resource is
-- bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@
pattern VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX :: VkObjectEntryUsageFlagBitsNVX
pattern VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX = VkObjectEntryUsageFlagBitsNVX 0x00000001

-- | @VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX@ specifies that the resource is
-- bound to @VK_PIPELINE_BIND_POINT_COMPUTE@
pattern VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX :: VkObjectEntryUsageFlagBitsNVX
pattern VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX = VkObjectEntryUsageFlagBitsNVX 0x00000002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_OBJECT_TABLE_CREATE_INFO_NVX"
pattern VK_STRUCTURE_TYPE_OBJECT_TABLE_CREATE_INFO_NVX :: VkStructureType
pattern VK_STRUCTURE_TYPE_OBJECT_TABLE_CREATE_INFO_NVX = VkStructureType 1000086000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NVX"
pattern VK_STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NVX :: VkStructureType
pattern VK_STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NVX = VkStructureType 1000086001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_CMD_PROCESS_COMMANDS_INFO_NVX"
pattern VK_STRUCTURE_TYPE_CMD_PROCESS_COMMANDS_INFO_NVX :: VkStructureType
pattern VK_STRUCTURE_TYPE_CMD_PROCESS_COMMANDS_INFO_NVX = VkStructureType 1000086002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_CMD_RESERVE_SPACE_FOR_COMMANDS_INFO_NVX"
pattern VK_STRUCTURE_TYPE_CMD_RESERVE_SPACE_FOR_COMMANDS_INFO_NVX :: VkStructureType
pattern VK_STRUCTURE_TYPE_CMD_RESERVE_SPACE_FOR_COMMANDS_INFO_NVX = VkStructureType 1000086003
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_LIMITS_NVX"
pattern VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_LIMITS_NVX :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_LIMITS_NVX = VkStructureType 1000086004
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_FEATURES_NVX"
pattern VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_FEATURES_NVX :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_FEATURES_NVX = VkStructureType 1000086005
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_OBJECT_TABLE_NVX"
pattern VK_OBJECT_TYPE_OBJECT_TABLE_NVX :: VkObjectType
pattern VK_OBJECT_TYPE_OBJECT_TABLE_NVX = VkObjectType 1000086000
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NVX"
pattern VK_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NVX :: VkObjectType
pattern VK_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NVX = VkObjectType 1000086001
-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_COMMAND_PROCESS_READ_BIT_NVX"
pattern VK_ACCESS_COMMAND_PROCESS_READ_BIT_NVX :: VkAccessFlagBits
pattern VK_ACCESS_COMMAND_PROCESS_READ_BIT_NVX = VkAccessFlagBits 0x00020000
-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_COMMAND_PROCESS_WRITE_BIT_NVX"
pattern VK_ACCESS_COMMAND_PROCESS_WRITE_BIT_NVX :: VkAccessFlagBits
pattern VK_ACCESS_COMMAND_PROCESS_WRITE_BIT_NVX = VkAccessFlagBits 0x00040000
-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_COMMAND_PROCESS_BIT_NVX"
pattern VK_PIPELINE_STAGE_COMMAND_PROCESS_BIT_NVX :: VkPipelineStageFlagBits
pattern VK_PIPELINE_STAGE_COMMAND_PROCESS_BIT_NVX = VkPipelineStageFlagBits 0x00020000
-- No documentation found for TopLevel "VK_NVX_DEVICE_GENERATED_COMMANDS_SPEC_VERSION"
pattern VK_NVX_DEVICE_GENERATED_COMMANDS_SPEC_VERSION :: Integral a => a
pattern VK_NVX_DEVICE_GENERATED_COMMANDS_SPEC_VERSION = 3
-- No documentation found for TopLevel "VK_NVX_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME"
pattern VK_NVX_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_NVX_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME = "VK_NVX_device_generated_commands"
-- | VkObjectTableNVX - Opaque handle to an object table
--
-- = See Also
--
-- No cross-references are available
newtype VkObjectTableNVX = VkObjectTableNVX Word64
  deriving (Eq, Show)

instance Storable VkObjectTableNVX where
  sizeOf (VkObjectTableNVX w) = sizeOf w
  alignment (VkObjectTableNVX w) = alignment w
  peek ptr = VkObjectTableNVX <$> peek (castPtr ptr)
  poke ptr (VkObjectTableNVX w) = poke (castPtr ptr) w
-- | VkIndirectCommandsLayoutNVX - Opaque handle to an indirect commands
-- layout object
--
-- = See Also
--
-- No cross-references are available
newtype VkIndirectCommandsLayoutNVX = VkIndirectCommandsLayoutNVX Word64
  deriving (Eq, Show)

instance Storable VkIndirectCommandsLayoutNVX where
  sizeOf (VkIndirectCommandsLayoutNVX w) = sizeOf w
  alignment (VkIndirectCommandsLayoutNVX w) = alignment w
  peek ptr = VkIndirectCommandsLayoutNVX <$> peek (castPtr ptr)
  poke ptr (VkIndirectCommandsLayoutNVX w) = poke (castPtr ptr) w
-- | vkCmdProcessCommandsNVX - Performs the generation of commands on the
-- device
--
-- = Parameters
--
-- -   @commandBuffer@ is the primary command buffer in which the
--     generation process takes space.
--
-- -   @pProcessCommandsInfo@ is a pointer to an instance of the
--     'VkCmdProcessCommandsInfoNVX' structure containing parameters
--     affecting the processing of commands.
--
-- = Description
--
-- Unresolved directive in vkCmdProcessCommandsNVX.txt -
-- include::..\/validity\/protos\/vkCmdProcessCommandsNVX.txt[]
--
-- = See Also
--
-- No cross-references are available
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdProcessCommandsNVX" vkCmdProcessCommandsNVX :: ("commandBuffer" ::: VkCommandBuffer) -> ("pProcessCommandsInfo" ::: Ptr VkCmdProcessCommandsInfoNVX) -> IO ()
-- | vkCmdReserveSpaceForCommandsNVX - Perform a reservation of command
-- buffer space
--
-- = Parameters
--
-- -   @commandBuffer@ is the secondary command buffer in which the space
--     for device-generated commands is reserved.
--
-- -   @pProcessCommandsInfo@ is a pointer to an instance of the
--     'vkCmdReserveSpaceForCommandsNVX' structure containing parameters
--     affecting the reservation of command buffer space.
--
-- == Valid Usage
--
-- -   The provided @commandBuffer@ /must/ not have had a prior space
--     reservation since its creation or the last reset.
--
-- -   The state of the @commandBuffer@ /must/ be legal to execute all
--     commands within the sequence provided by the
--     @indirectCommandsLayout@ member of @pProcessCommandsInfo@.
--
-- Unresolved directive in vkCmdReserveSpaceForCommandsNVX.txt -
-- include::..\/validity\/protos\/vkCmdReserveSpaceForCommandsNVX.txt[]
--
-- = See Also
--
-- No cross-references are available
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdReserveSpaceForCommandsNVX" vkCmdReserveSpaceForCommandsNVX :: ("commandBuffer" ::: VkCommandBuffer) -> ("pReserveSpaceInfo" ::: Ptr VkCmdReserveSpaceForCommandsInfoNVX) -> IO ()
-- | vkCreateIndirectCommandsLayoutNVX - Create an indirect command layout
-- object
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the indirect command
--     layout.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     @VkIndirectCommandsLayoutCreateInfoNVX@ structure containing
--     parameters affecting creation of the indirect command layout.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     [Memory
--     Allocation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-allocation)
--     chapter.
--
-- -   @pIndirectCommandsLayout@ points to a @VkIndirectCommandsLayoutNVX@
--     handle in which the resulting indirect command layout is returned.
--
-- = Description
--
-- Unresolved directive in vkCreateIndirectCommandsLayoutNVX.txt -
-- include::..\/validity\/protos\/vkCreateIndirectCommandsLayoutNVX.txt[]
--
-- = See Also
--
-- No cross-references are available
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateIndirectCommandsLayoutNVX" vkCreateIndirectCommandsLayoutNVX :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkIndirectCommandsLayoutCreateInfoNVX) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pIndirectCommandsLayout" ::: Ptr VkIndirectCommandsLayoutNVX) -> IO VkResult
-- | vkDestroyIndirectCommandsLayoutNVX - Destroy an object table
--
-- = Parameters
--
-- -   @device@ is the logical device that destroys the layout.
--
-- -   @indirectCommandsLayout@ is the table to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     [Memory
--     Allocation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-allocation)
--     chapter.
--
-- == Valid Usage
--
-- -   All submitted commands that refer to @indirectCommandsLayout@ /must/
--     have completed execution
--
-- -   If @VkAllocationCallbacks@ were provided when @objectTable@ was
--     created, a compatible set of callbacks /must/ be provided here
--
-- -   If no @VkAllocationCallbacks@ were provided when @objectTable@ was
--     created, @pAllocator@ /must/ be @NULL@
--
-- Unresolved directive in vkDestroyIndirectCommandsLayoutNVX.txt -
-- include::..\/validity\/protos\/vkDestroyIndirectCommandsLayoutNVX.txt[]
--
-- = See Also
--
-- No cross-references are available
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroyIndirectCommandsLayoutNVX" vkDestroyIndirectCommandsLayoutNVX :: ("device" ::: VkDevice) -> ("indirectCommandsLayout" ::: VkIndirectCommandsLayoutNVX) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
-- | vkCreateObjectTableNVX - Create an object table
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the object table.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     @VkObjectTableCreateInfoNVX@ structure containing parameters
--     affecting creation of the table.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     [Memory
--     Allocation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-allocation)
--     chapter.
--
-- -   @pObjectTable@ points to a 'VkObjectTableNVX' handle in which the
--     resulting object table is returned.
--
-- = Description
--
-- Unresolved directive in vkCreateObjectTableNVX.txt -
-- include::..\/validity\/protos\/vkCreateObjectTableNVX.txt[]
--
-- = See Also
--
-- No cross-references are available
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateObjectTableNVX" vkCreateObjectTableNVX :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkObjectTableCreateInfoNVX) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pObjectTable" ::: Ptr VkObjectTableNVX) -> IO VkResult
-- | vkDestroyObjectTableNVX - Destroy an object table
--
-- = Parameters
--
-- -   @device@ is the logical device that destroys the table.
--
-- -   @objectTable@ is the table to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     [Memory
--     Allocation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-allocation)
--     chapter.
--
-- == Valid Usage
--
-- -   All submitted commands that refer to @objectTable@ /must/ have
--     completed execution.
--
-- -   If @VkAllocationCallbacks@ were provided when @objectTable@ was
--     created, a compatible set of callbacks /must/ be provided here.
--
-- -   If no @VkAllocationCallbacks@ were provided when @objectTable@ was
--     created, @pAllocator@ /must/ be @NULL@.
--
-- Unresolved directive in vkDestroyObjectTableNVX.txt -
-- include::..\/validity\/protos\/vkDestroyObjectTableNVX.txt[]
--
-- = See Also
--
-- No cross-references are available
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroyObjectTableNVX" vkDestroyObjectTableNVX :: ("device" ::: VkDevice) -> ("objectTable" ::: VkObjectTableNVX) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
-- | vkRegisterObjectsNVX - Register resource bindings in an object table
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the object table.
--
-- -   @objectTable@ is the table for which the resources are registered.
--
-- -   @objectCount@ is the number of resources to register.
--
-- -   @ppObjectTableEntries@ provides an array for detailed binding
--     informations, each array element is a pointer to a struct of type
--     @VkObjectTablePipelineEntryNVX@,
--     @VkObjectTableDescriptorSetEntryNVX@,
--     @VkObjectTableVertexBufferEntryNVX@,
--     @VkObjectTableIndexBufferEntryNVX@ or
--     @VkObjectTablePushConstantEntryNVX@ (see below for details).
--
-- -   @pObjectIndices@ are the indices at which each resource is
--     registered.
--
-- == Valid Usage
--
-- -   The contents of @pObjectTableEntry@ /must/ yield plausible bindings
--     supported by the device.
--
-- -   At any @pObjectIndices@ there /must/ not be a registered resource
--     already.
--
-- -   Any value inside @pObjectIndices@ /must/ be below the appropriate
--     @VkObjectTableCreateInfoNVX@::@pObjectEntryCounts@ limits provided
--     at @objectTable@ creation time.
--
-- Unresolved directive in vkRegisterObjectsNVX.txt -
-- include::..\/validity\/protos\/vkRegisterObjectsNVX.txt[]
--
-- = See Also
--
-- No cross-references are available
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkRegisterObjectsNVX" vkRegisterObjectsNVX :: ("device" ::: VkDevice) -> ("objectTable" ::: VkObjectTableNVX) -> ("objectCount" ::: Word32) -> ("ppObjectTableEntries" ::: Ptr (Ptr VkObjectTableEntryNVX)) -> ("pObjectIndices" ::: Ptr Word32) -> IO VkResult
-- | vkUnregisterObjectsNVX - Unregister resource bindings in an object table
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the object table.
--
-- -   @objectTable@ is the table from which the resources are
--     unregistered.
--
-- -   @objectCount@ is the number of resources being removed from the
--     object table.
--
-- -   @pObjectEntryType@ provides an array of 'VkObjectEntryTypeNVX' for
--     the resources being removed.
--
-- -   @pObjectIndices@ provides the array of object indices to be removed.
--
-- == Valid Usage
--
-- -   At any @pObjectIndices@ there /must/ be a registered resource
--     already.
--
-- -   The @pObjectEntryTypes@ of the resource at @pObjectIndices@ /must/
--     match.
--
-- -   All operations on the device using the registered resource /must/
--     have been completed.
--
-- Unresolved directive in vkUnregisterObjectsNVX.txt -
-- include::..\/validity\/protos\/vkUnregisterObjectsNVX.txt[]
--
-- = See Also
--
-- No cross-references are available
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkUnregisterObjectsNVX" vkUnregisterObjectsNVX :: ("device" ::: VkDevice) -> ("objectTable" ::: VkObjectTableNVX) -> ("objectCount" ::: Word32) -> ("pObjectEntryTypes" ::: Ptr VkObjectEntryTypeNVX) -> ("pObjectIndices" ::: Ptr Word32) -> IO VkResult
-- | vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX - Returns
-- device-generated commands related properties of a physical device
--
-- = Parameters
--
-- -   @physicalDevice@ is the handle to the physical device whose
--     properties will be queried.
--
-- -   @pFeatures@ points to an instance of the
--     'VkDeviceGeneratedCommandsFeaturesNVX' structure, that will be
--     filled with returned information.
--
-- -   @pLimits@ points to an instance of the
--     'VkDeviceGeneratedCommandsLimitsNVX' structure, that will be filled
--     with returned information.
--
-- = Description
--
-- Unresolved directive in
-- vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX.txt -
-- include::..\/validity\/protos\/vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX.txt[]
--
-- = See Also
--
-- No cross-references are available
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX" vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pFeatures" ::: Ptr VkDeviceGeneratedCommandsFeaturesNVX) -> ("pLimits" ::: Ptr VkDeviceGeneratedCommandsLimitsNVX) -> IO ()
-- | VkDeviceGeneratedCommandsFeaturesNVX - Structure specifying physical
-- device support
--
-- = Description
--
-- Unresolved directive in VkDeviceGeneratedCommandsFeaturesNVX.txt -
-- include::..\/validity\/structs\/VkDeviceGeneratedCommandsFeaturesNVX.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkDeviceGeneratedCommandsFeaturesNVX = VkDeviceGeneratedCommandsFeaturesNVX
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @computeBindingPointSupport@ specifies whether the @VkObjectTableNVX@
  -- supports entries with @VK_OBJECT_ENTRY_USAGE_GRAPHICS_BIT_NVX@ bit set
  -- and @VkIndirectCommandsLayoutNVX@ supports
  -- @VK_PIPELINE_BIND_POINT_COMPUTE@.
  vkComputeBindingPointSupport :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkDeviceGeneratedCommandsFeaturesNVX where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkDeviceGeneratedCommandsFeaturesNVX <$> peek (ptr `plusPtr` 0)
                                                  <*> peek (ptr `plusPtr` 8)
                                                  <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDeviceGeneratedCommandsFeaturesNVX))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDeviceGeneratedCommandsFeaturesNVX))
                *> poke (ptr `plusPtr` 16) (vkComputeBindingPointSupport (poked :: VkDeviceGeneratedCommandsFeaturesNVX))
-- | VkDeviceGeneratedCommandsLimitsNVX - Structure specifying physical
-- device limits
--
-- = Description
--
-- Unresolved directive in VkDeviceGeneratedCommandsLimitsNVX.txt -
-- include::..\/validity\/structs\/VkDeviceGeneratedCommandsLimitsNVX.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkDeviceGeneratedCommandsLimitsNVX = VkDeviceGeneratedCommandsLimitsNVX
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @maxIndirectCommandsLayoutTokenCount@ the maximum number of tokens in
  -- @VkIndirectCommandsLayoutNVX@.
  vkMaxIndirectCommandsLayoutTokenCount :: Word32
  , -- | @maxObjectEntryCounts@ the maximum number of entries per resource type
  -- in @VkObjectTableNVX@.
  vkMaxObjectEntryCounts :: Word32
  , -- | @minSequenceCountBufferOffsetAlignment@ the minimum alignment for memory
  -- addresses optionally used in @vkCmdProcessCommandsNVX@.
  vkMinSequenceCountBufferOffsetAlignment :: Word32
  , -- | @minSequenceIndexBufferOffsetAlignment@ the minimum alignment for memory
  -- addresses optionally used in @vkCmdProcessCommandsNVX@.
  vkMinSequenceIndexBufferOffsetAlignment :: Word32
  , -- | @minCommandsTokenBufferOffsetAlignment@ the minimum alignment for memory
  -- addresses optionally used in @vkCmdProcessCommandsNVX@.
  vkMinCommandsTokenBufferOffsetAlignment :: Word32
  }
  deriving (Eq, Show)

instance Storable VkDeviceGeneratedCommandsLimitsNVX where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkDeviceGeneratedCommandsLimitsNVX <$> peek (ptr `plusPtr` 0)
                                                <*> peek (ptr `plusPtr` 8)
                                                <*> peek (ptr `plusPtr` 16)
                                                <*> peek (ptr `plusPtr` 20)
                                                <*> peek (ptr `plusPtr` 24)
                                                <*> peek (ptr `plusPtr` 28)
                                                <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDeviceGeneratedCommandsLimitsNVX))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDeviceGeneratedCommandsLimitsNVX))
                *> poke (ptr `plusPtr` 16) (vkMaxIndirectCommandsLayoutTokenCount (poked :: VkDeviceGeneratedCommandsLimitsNVX))
                *> poke (ptr `plusPtr` 20) (vkMaxObjectEntryCounts (poked :: VkDeviceGeneratedCommandsLimitsNVX))
                *> poke (ptr `plusPtr` 24) (vkMinSequenceCountBufferOffsetAlignment (poked :: VkDeviceGeneratedCommandsLimitsNVX))
                *> poke (ptr `plusPtr` 28) (vkMinSequenceIndexBufferOffsetAlignment (poked :: VkDeviceGeneratedCommandsLimitsNVX))
                *> poke (ptr `plusPtr` 32) (vkMinCommandsTokenBufferOffsetAlignment (poked :: VkDeviceGeneratedCommandsLimitsNVX))
-- | VkIndirectCommandsTokenNVX - Structure specifying parameters for the
-- reservation of command buffer space
--
-- == Valid Usage
--
-- -   The @buffer@’s usage flag /must/ have the
--     @VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT@ bit set.
--
-- -   The @offset@ /must/ be aligned to
--     @VkDeviceGeneratedCommandsLimitsNVX@::@minCommandsTokenBufferOffsetAlignment@.
--
-- Unresolved directive in VkIndirectCommandsTokenNVX.txt -
-- include::..\/validity\/structs\/VkIndirectCommandsTokenNVX.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkIndirectCommandsTokenNVX = VkIndirectCommandsTokenNVX
  { -- | @tokenType@ specifies the token command type.
  vkTokenType :: VkIndirectCommandsTokenTypeNVX
  , -- | @buffer@ specifies the
  -- 'Graphics.Vulkan.Core10.MemoryManagement.VkBuffer' storing the
  -- functional arguments for each squence. These argumetns can be written by
  -- the device.
  vkBuffer :: VkBuffer
  , -- | @offset@ specified an offset into @buffer@ where the arguments start.
  vkOffset :: VkDeviceSize
  }
  deriving (Eq, Show)

instance Storable VkIndirectCommandsTokenNVX where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkIndirectCommandsTokenNVX <$> peek (ptr `plusPtr` 0)
                                        <*> peek (ptr `plusPtr` 8)
                                        <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkTokenType (poked :: VkIndirectCommandsTokenNVX))
                *> poke (ptr `plusPtr` 8) (vkBuffer (poked :: VkIndirectCommandsTokenNVX))
                *> poke (ptr `plusPtr` 16) (vkOffset (poked :: VkIndirectCommandsTokenNVX))
-- | VkIndirectCommandsLayoutTokenNVX - Struct specifying the details of an
-- indirect command layout token
--
-- == Valid Usage
--
-- -   @bindingUnit@ /must/ stay within device supported limits for the
--     appropriate commands.
--
-- -   @dynamicCount@ /must/ stay within device supported limits for the
--     appropriate commands.
--
-- -   @divisor@ /must/ be greater than @0@ and a power of two.
--
-- Unresolved directive in VkIndirectCommandsLayoutTokenNVX.txt -
-- include::..\/validity\/structs\/VkIndirectCommandsLayoutTokenNVX.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkIndirectCommandsLayoutTokenNVX = VkIndirectCommandsLayoutTokenNVX
  { -- No documentation found for Nested "VkIndirectCommandsLayoutTokenNVX" "tokenType"
  vkTokenType :: VkIndirectCommandsTokenTypeNVX
  , -- | @bindingUnit@ has a different meaning depending on the type, please
  -- refer pseudo code further down for details.
  vkBindingUnit :: Word32
  , -- | @dynamicCount@ has a different meaning depending on the type, please
  -- refer pseudo code further down for details.
  vkDynamicCount :: Word32
  , -- | @divisor@ defines the rate at which the input data buffers are accessed.
  vkDivisor :: Word32
  }
  deriving (Eq, Show)

instance Storable VkIndirectCommandsLayoutTokenNVX where
  sizeOf ~_ = 16
  alignment ~_ = 4
  peek ptr = VkIndirectCommandsLayoutTokenNVX <$> peek (ptr `plusPtr` 0)
                                              <*> peek (ptr `plusPtr` 4)
                                              <*> peek (ptr `plusPtr` 8)
                                              <*> peek (ptr `plusPtr` 12)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkTokenType (poked :: VkIndirectCommandsLayoutTokenNVX))
                *> poke (ptr `plusPtr` 4) (vkBindingUnit (poked :: VkIndirectCommandsLayoutTokenNVX))
                *> poke (ptr `plusPtr` 8) (vkDynamicCount (poked :: VkIndirectCommandsLayoutTokenNVX))
                *> poke (ptr `plusPtr` 12) (vkDivisor (poked :: VkIndirectCommandsLayoutTokenNVX))
-- | VkIndirectCommandsLayoutCreateInfoNVX - Structure specifying the
-- parameters of a newly created indirect commands layout object
--
-- = Description
--
-- The following code illustrates some of the key flags:
--
-- > void cmdProcessAllSequences(cmd, objectTable, indirectCommandsLayout, pIndirectCommandsTokens, sequencesCount, indexbuffer, indexbufferoffset)
-- > {
-- >   for (s = 0; s < sequencesCount; s++)
-- >   {
-- >     sequence = s;
-- >
-- >     if (indirectCommandsLayout.flags & VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NVX) {
-- >       sequence = incoherent_implementation_dependent_permutation[ sequence ];
-- >     }
-- >     if (indirectCommandsLayout.flags & VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NVX) {
-- >       sequence = indexbuffer.load_uint32( sequence * sizeof(uint32_t) + indexbufferoffset);
-- >     }
-- >
-- >     cmdProcessSequence( cmd, objectTable, indirectCommandsLayout, pIndirectCommandsTokens, sequence );
-- >   }
-- > }
--
-- == Valid Usage
--
-- -   @tokenCount@ /must/ be greater than @0@ and below
--     @VkDeviceGeneratedCommandsLimitsNVX@::@maxIndirectCommandsLayoutTokenCount@
--
-- -   If the
--     @VkDeviceGeneratedCommandsFeaturesNVX@::@computeBindingPointSupport@
--     feature is not enabled, then @pipelineBindPoint@ /must/ not be
--     @VK_PIPELINE_BIND_POINT_COMPUTE@
--
-- -   If @pTokens@ contains an entry of
--     @VK_INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NVX@ it /must/ be the
--     first element of the array and there /must/ be only a single element
--     of such token type.
--
-- -   All state binding tokens in @pTokens@ /must/ occur prior work
--     provoking tokens (@VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NVX@,
--     @VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NVX@,
--     @VK_INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NVX@).
--
-- -   The content of @pTokens@ /must/ include one single work provoking
--     token that is compatible with the @pipelineBindPoint@.
--
-- Unresolved directive in VkIndirectCommandsLayoutCreateInfoNVX.txt -
-- include::..\/validity\/structs\/VkIndirectCommandsLayoutCreateInfoNVX.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkIndirectCommandsLayoutCreateInfoNVX = VkIndirectCommandsLayoutCreateInfoNVX
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @pipelineBindPoint@ is the
  -- 'Graphics.Vulkan.Core10.Pass.VkPipelineBindPoint' that this layout
  -- targets.
  vkPipelineBindPoint :: VkPipelineBindPoint
  , -- | @flags@ is a bitmask of 'VkIndirectCommandsLayoutUsageFlagBitsNVX'
  -- specifying usage hints of this layout.
  vkFlags :: VkIndirectCommandsLayoutUsageFlagsNVX
  , -- | @tokenCount@ is the length of the individual command sequnce.
  vkTokenCount :: Word32
  , -- | @pTokens@ is an array describing each command token in detail. See
  -- 'VkIndirectCommandsTokenTypeNVX' and 'VkIndirectCommandsLayoutTokenNVX'
  -- below for details.
  vkPTokens :: Ptr VkIndirectCommandsLayoutTokenNVX
  }
  deriving (Eq, Show)

instance Storable VkIndirectCommandsLayoutCreateInfoNVX where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkIndirectCommandsLayoutCreateInfoNVX <$> peek (ptr `plusPtr` 0)
                                                   <*> peek (ptr `plusPtr` 8)
                                                   <*> peek (ptr `plusPtr` 16)
                                                   <*> peek (ptr `plusPtr` 20)
                                                   <*> peek (ptr `plusPtr` 24)
                                                   <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkIndirectCommandsLayoutCreateInfoNVX))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkIndirectCommandsLayoutCreateInfoNVX))
                *> poke (ptr `plusPtr` 16) (vkPipelineBindPoint (poked :: VkIndirectCommandsLayoutCreateInfoNVX))
                *> poke (ptr `plusPtr` 20) (vkFlags (poked :: VkIndirectCommandsLayoutCreateInfoNVX))
                *> poke (ptr `plusPtr` 24) (vkTokenCount (poked :: VkIndirectCommandsLayoutCreateInfoNVX))
                *> poke (ptr `plusPtr` 32) (vkPTokens (poked :: VkIndirectCommandsLayoutCreateInfoNVX))
-- | VkCmdProcessCommandsInfoNVX - Structure specifying parameters for the
-- generation of commands
--
-- == Valid Usage
--
-- -   The provided @objectTable@ /must/ include all objects referenced by
--     the generation process.
--
-- -   @indirectCommandsTokenCount@ /must/ match the
--     @indirectCommandsLayout@’s @tokenCount@.
--
-- -   The @tokenType@ member of each entry in the
--     @pIndirectCommandsTokens@ array /must/ match the values used at
--     creation time of @indirectCommandsLayout@
--
-- -   If @targetCommandBuffer@ is provided, it /must/ have reserved
--     command space.
--
-- -   If @targetCommandBuffer@ is provided, the @objectTable@ /must/ match
--     the reservation’s objectTable and /must/ have had all referenced
--     objects registered at reservation time.
--
-- -   If @targetCommandBuffer@ is provided, the @indirectCommandsLayout@
--     /must/ match the reservation’s indirectCommandsLayout.
--
-- -   If @targetCommandBuffer@ is provided, the @maxSequencesCount@ /must/
--     not exceed the reservation’s maxSequencesCount.
--
-- -   If @sequencesCountBuffer@ is used, its usage flag /must/ have
--     @VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT@ bit set.
--
-- -   If @sequencesCountBuffer@ is used, @sequencesCountOffset@ /must/ be
--     aligned to
--     @VkDeviceGeneratedCommandsLimitsNVX@::@minSequenceCountBufferOffsetAlignment@.
--
-- -   If @sequencesIndexBuffer@ is used, its usage flag /must/ have
--     @VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT@ bit set.
--
-- -   If @sequencesIndexBuffer@ is used, @sequencesIndexOffset@ /must/ be
--     aligned to
--     @VkDeviceGeneratedCommandsLimitsNVX@::@minSequenceIndexBufferOffsetAlignment@.
--
-- Unresolved directive in VkCmdProcessCommandsInfoNVX.txt -
-- include::..\/validity\/structs\/VkCmdProcessCommandsInfoNVX.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkCmdProcessCommandsInfoNVX = VkCmdProcessCommandsInfoNVX
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @objectTable@ is the 'VkObjectTableNVX' to be used for the generation
  -- process. Only registered objects at the time
  -- 'vkCmdReserveSpaceForCommandsNVX' is called, will be taken into account
  -- for the reservation.
  vkObjectTable :: VkObjectTableNVX
  , -- | @indirectCommandsLayout@ is the 'VkIndirectCommandsLayoutNVX' that
  -- provides the command sequence to generate.
  vkIndirectCommandsLayout :: VkIndirectCommandsLayoutNVX
  , -- | @indirectCommandsTokenCount@ defines the number of input tokens used.
  vkIndirectCommandsTokenCount :: Word32
  , -- | @pIndirectCommandsTokens@ provides an array of
  -- 'VkIndirectCommandsTokenNVX' that reference the input data for each
  -- token command.
  vkPIndirectCommandsTokens :: Ptr VkIndirectCommandsTokenNVX
  , -- | @maxSequencesCount@ is the maximum number of sequences for which command
  -- buffer space will be reserved. If @sequencesCountBuffer@ is
  -- 'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE', this is also the
  -- actual number of sequences generated.
  vkMaxSequencesCount :: Word32
  , -- | @targetCommandBuffer@ /can/ be the secondary
  -- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer' in which the commands
  -- should be recorded. If @targetCommandBuffer@ is @NULL@ an implicit
  -- reservation as well as execution takes place on the processing
  -- @VkCommandBuffer@.
  vkTargetCommandBuffer :: VkCommandBuffer
  , -- | @sequencesCountBuffer@ /can/ be
  -- 'Graphics.Vulkan.Core10.MemoryManagement.VkBuffer' from which the actual
  -- amount of sequences is sourced from as @uint32_t@ value.
  vkSequencesCountBuffer :: VkBuffer
  , -- | @sequencesCountOffset@ is the byte offset into @sequencesCountBuffer@
  -- where the count value is stored.
  vkSequencesCountOffset :: VkDeviceSize
  , -- | @sequencesIndexBuffer@ /must/ be set if @indirectCommandsLayout@’s
  -- @VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT@ is set and
  -- provides the used sequence indices as @uint32_t@ array. Otherwise it
  -- /must/ be 'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE'.
  vkSequencesIndexBuffer :: VkBuffer
  , -- | @sequencesIndexOffset@ is the byte offset into @sequencesIndexBuffer@
  -- where the index values start.
  vkSequencesIndexOffset :: VkDeviceSize
  }
  deriving (Eq, Show)

instance Storable VkCmdProcessCommandsInfoNVX where
  sizeOf ~_ = 96
  alignment ~_ = 8
  peek ptr = VkCmdProcessCommandsInfoNVX <$> peek (ptr `plusPtr` 0)
                                         <*> peek (ptr `plusPtr` 8)
                                         <*> peek (ptr `plusPtr` 16)
                                         <*> peek (ptr `plusPtr` 24)
                                         <*> peek (ptr `plusPtr` 32)
                                         <*> peek (ptr `plusPtr` 40)
                                         <*> peek (ptr `plusPtr` 48)
                                         <*> peek (ptr `plusPtr` 56)
                                         <*> peek (ptr `plusPtr` 64)
                                         <*> peek (ptr `plusPtr` 72)
                                         <*> peek (ptr `plusPtr` 80)
                                         <*> peek (ptr `plusPtr` 88)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkCmdProcessCommandsInfoNVX))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkCmdProcessCommandsInfoNVX))
                *> poke (ptr `plusPtr` 16) (vkObjectTable (poked :: VkCmdProcessCommandsInfoNVX))
                *> poke (ptr `plusPtr` 24) (vkIndirectCommandsLayout (poked :: VkCmdProcessCommandsInfoNVX))
                *> poke (ptr `plusPtr` 32) (vkIndirectCommandsTokenCount (poked :: VkCmdProcessCommandsInfoNVX))
                *> poke (ptr `plusPtr` 40) (vkPIndirectCommandsTokens (poked :: VkCmdProcessCommandsInfoNVX))
                *> poke (ptr `plusPtr` 48) (vkMaxSequencesCount (poked :: VkCmdProcessCommandsInfoNVX))
                *> poke (ptr `plusPtr` 56) (vkTargetCommandBuffer (poked :: VkCmdProcessCommandsInfoNVX))
                *> poke (ptr `plusPtr` 64) (vkSequencesCountBuffer (poked :: VkCmdProcessCommandsInfoNVX))
                *> poke (ptr `plusPtr` 72) (vkSequencesCountOffset (poked :: VkCmdProcessCommandsInfoNVX))
                *> poke (ptr `plusPtr` 80) (vkSequencesIndexBuffer (poked :: VkCmdProcessCommandsInfoNVX))
                *> poke (ptr `plusPtr` 88) (vkSequencesIndexOffset (poked :: VkCmdProcessCommandsInfoNVX))
-- | VkCmdReserveSpaceForCommandsInfoNVX - Structure specifying parameters
-- for the reservation of command buffer space
--
-- = Description
--
-- Unresolved directive in VkCmdReserveSpaceForCommandsInfoNVX.txt -
-- include::..\/validity\/structs\/VkCmdReserveSpaceForCommandsInfoNVX.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkCmdReserveSpaceForCommandsInfoNVX = VkCmdReserveSpaceForCommandsInfoNVX
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @objectTable@ is the 'VkObjectTableNVX' to be used for the generation
  -- process. Only registered objects at the time
  -- 'vkCmdReserveSpaceForCommandsNVX' is called, will be taken into account
  -- for the reservation.
  vkObjectTable :: VkObjectTableNVX
  , -- | @indirectCommandsLayout@ is the 'VkIndirectCommandsLayoutNVX' that
  -- /must/ also be used at generation time.
  vkIndirectCommandsLayout :: VkIndirectCommandsLayoutNVX
  , -- | @maxSequencesCount@ is the maximum number of sequences for which command
  -- buffer space will be reserved.
  vkMaxSequencesCount :: Word32
  }
  deriving (Eq, Show)

instance Storable VkCmdReserveSpaceForCommandsInfoNVX where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkCmdReserveSpaceForCommandsInfoNVX <$> peek (ptr `plusPtr` 0)
                                                 <*> peek (ptr `plusPtr` 8)
                                                 <*> peek (ptr `plusPtr` 16)
                                                 <*> peek (ptr `plusPtr` 24)
                                                 <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkCmdReserveSpaceForCommandsInfoNVX))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkCmdReserveSpaceForCommandsInfoNVX))
                *> poke (ptr `plusPtr` 16) (vkObjectTable (poked :: VkCmdReserveSpaceForCommandsInfoNVX))
                *> poke (ptr `plusPtr` 24) (vkIndirectCommandsLayout (poked :: VkCmdReserveSpaceForCommandsInfoNVX))
                *> poke (ptr `plusPtr` 32) (vkMaxSequencesCount (poked :: VkCmdReserveSpaceForCommandsInfoNVX))
-- | VkObjectTableCreateInfoNVX - Structure specifying the parameters of a
-- newly created object table
--
-- == Valid Usage
--
-- -   If the
--     @VkDeviceGeneratedCommandsFeaturesNVX@::@computeBindingPointSupport@
--     feature is not enabled, @pObjectEntryUsageFlags@ /must/ not contain
--     @VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX@
--
-- -   Any value within @pObjectEntryCounts@ /must/ not exceed
--     @VkDeviceGeneratedCommandsLimitsNVX@::@maxObjectEntryCounts@
--
-- -   @maxUniformBuffersPerDescriptor@ /must/ be within the limits
--     supported by the device.
--
-- -   @maxStorageBuffersPerDescriptor@ /must/ be within the limits
--     supported by the device.
--
-- -   @maxStorageImagesPerDescriptor@ /must/ be within the limits
--     supported by the device.
--
-- -   @maxSampledImagesPerDescriptor@ /must/ be within the limits
--     supported by the device.
--
-- Unresolved directive in VkObjectTableCreateInfoNVX.txt -
-- include::..\/validity\/structs\/VkObjectTableCreateInfoNVX.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkObjectTableCreateInfoNVX = VkObjectTableCreateInfoNVX
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @objectCount@ is the number of entry configurations that the object
  -- table supports.
  vkObjectCount :: Word32
  , -- | @pObjectEntryTypes@ is an array of 'VkObjectEntryTypeNVX' values
  -- providing the entry type of a given configuration.
  vkPObjectEntryTypes :: Ptr VkObjectEntryTypeNVX
  , -- | @pObjectEntryCounts@ is an array of counts of how many objects can be
  -- registered in the table.
  vkPObjectEntryCounts :: Ptr Word32
  , -- | @pObjectEntryUsageFlags@ is an array of bitmasks of
  -- 'VkObjectEntryUsageFlagBitsNVX' specifying the binding usage of the
  -- entry.
  vkPObjectEntryUsageFlags :: Ptr VkObjectEntryUsageFlagsNVX
  , -- | @maxUniformBuffersPerDescriptor@ is the maximum number of
  -- @VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER@ or
  -- @VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC@ used by any single
  -- registered @VkDescriptorSet@ in this table.
  vkMaxUniformBuffersPerDescriptor :: Word32
  , -- | @maxStorageBuffersPerDescriptor@ is the maximum number of
  -- @VK_DESCRIPTOR_TYPE_STORAGE_BUFFER@ or
  -- @VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC@ used by any single
  -- registered @VkDescriptorSet@ in this table.
  vkMaxStorageBuffersPerDescriptor :: Word32
  , -- | @maxStorageImagesPerDescriptor@ is the maximum number of
  -- @VK_DESCRIPTOR_TYPE_STORAGE_IMAGE@ or
  -- @VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER@ used by any single registered
  -- @VkDescriptorSet@ in this table.
  vkMaxStorageImagesPerDescriptor :: Word32
  , -- | @maxSampledImagesPerDescriptor@ is the maximum number of
  -- @VK_DESCRIPTOR_TYPE_SAMPLER@,
  -- @VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER@,
  -- @VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER@ or
  -- @VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT@ used by any single registered
  -- @VkDescriptorSet@ in this table.
  vkMaxSampledImagesPerDescriptor :: Word32
  , -- | @maxPipelineLayouts@ is the maximum number of unique @VkPipelineLayout@
  -- used by any registered @VkDescriptorSet@ or @VkPipeline@ in this table.
  vkMaxPipelineLayouts :: Word32
  }
  deriving (Eq, Show)

instance Storable VkObjectTableCreateInfoNVX where
  sizeOf ~_ = 72
  alignment ~_ = 8
  peek ptr = VkObjectTableCreateInfoNVX <$> peek (ptr `plusPtr` 0)
                                        <*> peek (ptr `plusPtr` 8)
                                        <*> peek (ptr `plusPtr` 16)
                                        <*> peek (ptr `plusPtr` 24)
                                        <*> peek (ptr `plusPtr` 32)
                                        <*> peek (ptr `plusPtr` 40)
                                        <*> peek (ptr `plusPtr` 48)
                                        <*> peek (ptr `plusPtr` 52)
                                        <*> peek (ptr `plusPtr` 56)
                                        <*> peek (ptr `plusPtr` 60)
                                        <*> peek (ptr `plusPtr` 64)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkObjectTableCreateInfoNVX))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkObjectTableCreateInfoNVX))
                *> poke (ptr `plusPtr` 16) (vkObjectCount (poked :: VkObjectTableCreateInfoNVX))
                *> poke (ptr `plusPtr` 24) (vkPObjectEntryTypes (poked :: VkObjectTableCreateInfoNVX))
                *> poke (ptr `plusPtr` 32) (vkPObjectEntryCounts (poked :: VkObjectTableCreateInfoNVX))
                *> poke (ptr `plusPtr` 40) (vkPObjectEntryUsageFlags (poked :: VkObjectTableCreateInfoNVX))
                *> poke (ptr `plusPtr` 48) (vkMaxUniformBuffersPerDescriptor (poked :: VkObjectTableCreateInfoNVX))
                *> poke (ptr `plusPtr` 52) (vkMaxStorageBuffersPerDescriptor (poked :: VkObjectTableCreateInfoNVX))
                *> poke (ptr `plusPtr` 56) (vkMaxStorageImagesPerDescriptor (poked :: VkObjectTableCreateInfoNVX))
                *> poke (ptr `plusPtr` 60) (vkMaxSampledImagesPerDescriptor (poked :: VkObjectTableCreateInfoNVX))
                *> poke (ptr `plusPtr` 64) (vkMaxPipelineLayouts (poked :: VkObjectTableCreateInfoNVX))
-- | VkObjectTableEntryNVX - Common parameters of an object table resource
-- entry
--
-- == Valid Usage
--
-- -   If the
--     @VkDeviceGeneratedCommandsFeaturesNVX@::@computeBindingPointSupport@
--     feature is not enabled, @flags@ /must/ not contain
--     @VK_OBJECT_ENTRY_USAGE_COMPUTE_BIT_NVX@
--
-- Unresolved directive in VkObjectTableEntryNVX.txt -
-- include::..\/validity\/structs\/VkObjectTableEntryNVX.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkObjectTableEntryNVX = VkObjectTableEntryNVX
  { -- | @type@ defines the entry type
  vkType :: VkObjectEntryTypeNVX
  , -- | @flags@ defines which 'Graphics.Vulkan.Core10.Pass.VkPipelineBindPoint'
  -- the resource can be used with. Some entry types allow only a single flag
  -- to be set.
  vkFlags :: VkObjectEntryUsageFlagsNVX
  }
  deriving (Eq, Show)

instance Storable VkObjectTableEntryNVX where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek ptr = VkObjectTableEntryNVX <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 4)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkType (poked :: VkObjectTableEntryNVX))
                *> poke (ptr `plusPtr` 4) (vkFlags (poked :: VkObjectTableEntryNVX))
-- | VkObjectTablePipelineEntryNVX - Parameters of an object table pipeline
-- entry
--
-- == Valid Usage
--
-- -   @type@ /must/ be @VK_OBJECT_ENTRY_TYPE_PIPELINE_NVX@
--
-- Unresolved directive in VkObjectTablePipelineEntryNVX.txt -
-- include::..\/validity\/structs\/VkObjectTablePipelineEntryNVX.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkObjectTablePipelineEntryNVX = VkObjectTablePipelineEntryNVX
  { -- No documentation found for Nested "VkObjectTablePipelineEntryNVX" "type"
  vkType :: VkObjectEntryTypeNVX
  , -- No documentation found for Nested "VkObjectTablePipelineEntryNVX" "flags"
  vkFlags :: VkObjectEntryUsageFlagsNVX
  , -- | @pipeline@ specifies the 'Graphics.Vulkan.Core10.Pipeline.VkPipeline'
  -- that this resource entry references.
  vkPipeline :: VkPipeline
  }
  deriving (Eq, Show)

instance Storable VkObjectTablePipelineEntryNVX where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek ptr = VkObjectTablePipelineEntryNVX <$> peek (ptr `plusPtr` 0)
                                           <*> peek (ptr `plusPtr` 4)
                                           <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkType (poked :: VkObjectTablePipelineEntryNVX))
                *> poke (ptr `plusPtr` 4) (vkFlags (poked :: VkObjectTablePipelineEntryNVX))
                *> poke (ptr `plusPtr` 8) (vkPipeline (poked :: VkObjectTablePipelineEntryNVX))
-- | VkObjectTableDescriptorSetEntryNVX - Parameters of an object table
-- descriptor set entry
--
-- == Valid Usage
--
-- -   @type@ /must/ be @VK_OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX@
--
-- Unresolved directive in VkObjectTableDescriptorSetEntryNVX.txt -
-- include::..\/validity\/structs\/VkObjectTableDescriptorSetEntryNVX.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkObjectTableDescriptorSetEntryNVX = VkObjectTableDescriptorSetEntryNVX
  { -- No documentation found for Nested "VkObjectTableDescriptorSetEntryNVX" "type"
  vkType :: VkObjectEntryTypeNVX
  , -- No documentation found for Nested "VkObjectTableDescriptorSetEntryNVX" "flags"
  vkFlags :: VkObjectEntryUsageFlagsNVX
  , -- | @pipelineLayout@ specifies the
  -- 'Graphics.Vulkan.Core10.Pipeline.VkPipelineLayout' that the
  -- @descriptorSet@ is used with.
  vkPipelineLayout :: VkPipelineLayout
  , -- | @descriptorSet@ specifies the
  -- 'Graphics.Vulkan.Core10.DescriptorSet.VkDescriptorSet' that can be bound
  -- with this entry.
  vkDescriptorSet :: VkDescriptorSet
  }
  deriving (Eq, Show)

instance Storable VkObjectTableDescriptorSetEntryNVX where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkObjectTableDescriptorSetEntryNVX <$> peek (ptr `plusPtr` 0)
                                                <*> peek (ptr `plusPtr` 4)
                                                <*> peek (ptr `plusPtr` 8)
                                                <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkType (poked :: VkObjectTableDescriptorSetEntryNVX))
                *> poke (ptr `plusPtr` 4) (vkFlags (poked :: VkObjectTableDescriptorSetEntryNVX))
                *> poke (ptr `plusPtr` 8) (vkPipelineLayout (poked :: VkObjectTableDescriptorSetEntryNVX))
                *> poke (ptr `plusPtr` 16) (vkDescriptorSet (poked :: VkObjectTableDescriptorSetEntryNVX))
-- | VkObjectTableVertexBufferEntryNVX - Parameters of an object table vertex
-- buffer entry
--
-- == Valid Usage
--
-- -   @type@ /must/ be @VK_OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX@
--
-- Unresolved directive in VkObjectTableVertexBufferEntryNVX.txt -
-- include::..\/validity\/structs\/VkObjectTableVertexBufferEntryNVX.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkObjectTableVertexBufferEntryNVX = VkObjectTableVertexBufferEntryNVX
  { -- No documentation found for Nested "VkObjectTableVertexBufferEntryNVX" "type"
  vkType :: VkObjectEntryTypeNVX
  , -- No documentation found for Nested "VkObjectTableVertexBufferEntryNVX" "flags"
  vkFlags :: VkObjectEntryUsageFlagsNVX
  , -- | @buffer@ specifies the
  -- 'Graphics.Vulkan.Core10.MemoryManagement.VkBuffer' that can be bound as
  -- vertex bufer
  vkBuffer :: VkBuffer
  }
  deriving (Eq, Show)

instance Storable VkObjectTableVertexBufferEntryNVX where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek ptr = VkObjectTableVertexBufferEntryNVX <$> peek (ptr `plusPtr` 0)
                                               <*> peek (ptr `plusPtr` 4)
                                               <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkType (poked :: VkObjectTableVertexBufferEntryNVX))
                *> poke (ptr `plusPtr` 4) (vkFlags (poked :: VkObjectTableVertexBufferEntryNVX))
                *> poke (ptr `plusPtr` 8) (vkBuffer (poked :: VkObjectTableVertexBufferEntryNVX))
-- | VkObjectTableIndexBufferEntryNVX - Parameters of an object table index
-- buffer entry
--
-- == Valid Usage
--
-- -   @type@ /must/ be @VK_OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX@
--
-- Unresolved directive in VkObjectTableIndexBufferEntryNVX.txt -
-- include::..\/validity\/structs\/VkObjectTableIndexBufferEntryNVX.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkObjectTableIndexBufferEntryNVX = VkObjectTableIndexBufferEntryNVX
  { -- No documentation found for Nested "VkObjectTableIndexBufferEntryNVX" "type"
  vkType :: VkObjectEntryTypeNVX
  , -- No documentation found for Nested "VkObjectTableIndexBufferEntryNVX" "flags"
  vkFlags :: VkObjectEntryUsageFlagsNVX
  , -- | @buffer@ specifies the
  -- 'Graphics.Vulkan.Core10.MemoryManagement.VkBuffer' that can be bound as
  -- index buffer
  vkBuffer :: VkBuffer
  , -- | @indexType@ specifies the
  -- 'Graphics.Vulkan.Core10.CommandBufferBuilding.VkIndexType' used with
  -- this index buffer
  vkIndexType :: VkIndexType
  }
  deriving (Eq, Show)

instance Storable VkObjectTableIndexBufferEntryNVX where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkObjectTableIndexBufferEntryNVX <$> peek (ptr `plusPtr` 0)
                                              <*> peek (ptr `plusPtr` 4)
                                              <*> peek (ptr `plusPtr` 8)
                                              <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkType (poked :: VkObjectTableIndexBufferEntryNVX))
                *> poke (ptr `plusPtr` 4) (vkFlags (poked :: VkObjectTableIndexBufferEntryNVX))
                *> poke (ptr `plusPtr` 8) (vkBuffer (poked :: VkObjectTableIndexBufferEntryNVX))
                *> poke (ptr `plusPtr` 16) (vkIndexType (poked :: VkObjectTableIndexBufferEntryNVX))
-- | VkObjectTablePushConstantEntryNVX - Parameters of an object table push
-- constant entry
--
-- == Valid Usage
--
-- -   @type@ /must/ be @VK_OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX@
--
-- Unresolved directive in VkObjectTablePushConstantEntryNVX.txt -
-- include::..\/validity\/structs\/VkObjectTablePushConstantEntryNVX.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkObjectTablePushConstantEntryNVX = VkObjectTablePushConstantEntryNVX
  { -- No documentation found for Nested "VkObjectTablePushConstantEntryNVX" "type"
  vkType :: VkObjectEntryTypeNVX
  , -- No documentation found for Nested "VkObjectTablePushConstantEntryNVX" "flags"
  vkFlags :: VkObjectEntryUsageFlagsNVX
  , -- | @pipelineLayout@ specifies the
  -- 'Graphics.Vulkan.Core10.Pipeline.VkPipelineLayout' that the
  -- pushconstants are used with
  vkPipelineLayout :: VkPipelineLayout
  , -- | @stageFlags@ specifies the
  -- 'Graphics.Vulkan.Core10.PipelineLayout.VkShaderStageFlags' that the
  -- pushconstants are used with
  vkStageFlags :: VkShaderStageFlags
  }
  deriving (Eq, Show)

instance Storable VkObjectTablePushConstantEntryNVX where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkObjectTablePushConstantEntryNVX <$> peek (ptr `plusPtr` 0)
                                               <*> peek (ptr `plusPtr` 4)
                                               <*> peek (ptr `plusPtr` 8)
                                               <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkType (poked :: VkObjectTablePushConstantEntryNVX))
                *> poke (ptr `plusPtr` 4) (vkFlags (poked :: VkObjectTablePushConstantEntryNVX))
                *> poke (ptr `plusPtr` 8) (vkPipelineLayout (poked :: VkObjectTablePushConstantEntryNVX))
                *> poke (ptr `plusPtr` 16) (vkStageFlags (poked :: VkObjectTablePushConstantEntryNVX))
-- | VkIndirectCommandsLayoutUsageFlagsNVX - Bitmask of
-- VkIndirectCommandsLayoutUsageFlagBitsNVX
--
-- = Description
--
-- @VkIndirectCommandsLayoutUsageFlagsNVX@ is a bitmask type for setting a
-- mask of zero or more 'VkIndirectCommandsLayoutUsageFlagBitsNVX'.
--
-- = See Also
--
-- No cross-references are available
type VkIndirectCommandsLayoutUsageFlagsNVX = VkIndirectCommandsLayoutUsageFlagBitsNVX
-- | VkObjectEntryUsageFlagsNVX - Bitmask of VkObjectEntryUsageFlagBitsNVX
--
-- = Description
--
-- @VkObjectEntryUsageFlagsNVX@ is a bitmask type for setting a mask of
-- zero or more 'VkObjectEntryUsageFlagBitsNVX'.
--
-- = See Also
--
-- No cross-references are available
type VkObjectEntryUsageFlagsNVX = VkObjectEntryUsageFlagBitsNVX
