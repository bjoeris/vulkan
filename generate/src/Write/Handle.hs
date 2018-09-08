{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Write.Handle
  ( writeHandle
  ) where

import           Data.Text.Prettyprint.Doc
import           Prelude                                  hiding (Enum)
import           Text.InterpolatedString.Perl6.Unindented

import           Spec.Savvy.Error
import           Spec.Savvy.Handle
import           Spec.Savvy.Type

import           Write.Element                            hiding (TypeName)
import           Write.Util

writeHandle :: Handle -> Either [SpecError] WriteElement
writeHandle h@Handle {..} =
  if hDispatchable
  then writeDispatchableHandle h
  else writeNonDispatchableHandle h

writeDispatchableHandle :: Handle -> Either [SpecError] WriteElement
writeDispatchableHandle h@Handle {..} = do
  weDoc <- hDocDispatchable h
  let weName       = "Handle: " <> hName
      weExtensions = []
      weImports    = [Import "Foreign.Ptr" ["Ptr"]]
      weProvides   = [Unguarded $ TypeAlias hName]
      weDepends    = []
  pure WriteElement {..}

hDocDispatchable :: Handle -> Either [SpecError] (DocMap -> Doc ())
hDocDispatchable Handle{..} = do
  p <- case hType of
    Ptr (TypeName p) -> pure p
    _                -> Left [HandleToNonPointerType hName]
  pure (\getDoc -> [qci|
    -- | Dummy data to tag the 'Ptr' with
    data {p}
    {document getDoc (TopLevel hName)}
    type {hName} = Ptr {p}
  |])

writeNonDispatchableHandle :: Handle -> Either [SpecError] WriteElement
writeNonDispatchableHandle h@Handle {..} = do
  weDoc <- hDocNonDispatchable h
  let weName       = "Handle: " <> hName
      weExtensions = []
      weImports    = [Import "Data.Word" ["Word64"], Import "Foreign.Ptr" ["castPtr"]]
      weProvides   = [Unguarded $ TypeConstructor hName]
      weDepends    = []
  pure WriteElement {..}

hDocNonDispatchable :: Handle -> Either [SpecError] (DocMap -> Doc ())
hDocNonDispatchable Handle{..} =
  pure (\getDoc -> [qci|
    {document getDoc (TopLevel hName)}
    newtype {hName} = {hName} Word64
      deriving (Eq, Show)

    instance Storable {hName} where
      sizeOf ({hName} w) = sizeOf w
      alignment ({hName} w) = alignment w
      peek ptr = {hName} <$> peek (castPtr ptr)
      poke ptr ({hName} w) = poke (castPtr ptr) w
  |])
