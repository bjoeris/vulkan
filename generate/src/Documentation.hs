{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Documentation
  ( main
  ) where

import           Control.Monad
import           Data.Bifunctor
import           Data.Default
import           Data.Either.Validation
import           Data.Foldable
import           Data.List
import           Data.Maybe
import           Data.Semigroup
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as T
import           Documentation.RunAsciiDoctor
import           Prelude                      hiding (rem)
import           Say
import           System.Environment
import           Text.Pandoc
import           Text.Pandoc.Generic
import           Text.Pandoc.Class
import           Text.Pandoc.Readers

data Documentation = Documentation
  { dDocumentee    :: Text
    -- ^ The name of the thing being documented
  , dDocumentation :: Pandoc
    -- ^ The documentation itself
  }
  deriving (Show)

documentationToHaddock :: Documentation -> Either PandocError Text
documentationToHaddock Documentation{..} =
  let writerOptions = def
  in runPure (writeHaddock writerOptions dDocumentation)

docBookToDocumentation
  :: Text
  -- ^ The object being documented
  -> Text
  -- ^ The docbook string
  -> Either PandocError Documentation
docBookToDocumentation name db = do
  let readerOptions = def
      dDocumentee = name
  dDocumentation <- runPure (readDocBook readerOptions db)
  pure Documentation{..}

-- | If the description is a bullet list of "enames" then remove those from the
-- original documentation and return them separately.
splitDocumentation :: Documentation -> Either Text [Documentation]
splitDocumentation Documentation {..} = case dDocumentation of
  Pandoc meta bs -> do
    (es, bs') <- extractMatchesM (splitPrefix meta) bs
    pure $ Documentation dDocumentee (Pandoc meta bs') : join (catMaybes es)
  where
    splitPrefix m = \case
      -- Remove the "Document Notes" section
      Section "_document_notes"  _  rem -> pure (Nothing, rem)

      -- Remove the "C Specification" section
      Section "_c_specification" _  rem -> pure (Nothing, rem)

      -- Remove the "Name" header
      Section "_name"            bs rem -> pure (Nothing, bs ++ rem)

      -- If the description section is a list of documentation for enumeration
      -- values, split them into separate documentation elements
      xs@(Section sectionTag bs rem)
        | sectionTag `elem` ["_description", "_members"] -> case
            memberDocs m bs
          of
            Left  err -> pure (Nothing, xs)
            Right ds  -> pure (Just ds, rem)

      -- Leave everything else alone
      xs -> pure (Nothing, xs)

--
fixupDocumentation :: Documentation -> Documentation
fixupDocumentation (Documentation dDocumentee p) = Documentation
  { dDocumentation = fixup p
  , ..
  }
  where
    fixup :: Pandoc -> Pandoc
    fixup = topDown fixupBlock . topDown fixupInline

    fixupBlock :: Block -> Block
    fixupBlock = \case
      -- Remove idents from headers
      Header n (_, cs, kvs) is -> Header n ("", cs, kvs) is
      DefinitionList ds | all (null . fst) ds, all ((== 1) . length . snd) ds ->
        BulletList (head . snd <$> ds)
      b -> b
    fixupInline :: Inline -> Inline
    fixupInline = \case
      Link ("", [], []) [Str name] (tag, "") | tag == "#" <> name ->
        RawInline "haddock" ("'" <> name <> "'")
      i -> i

dropPrefix :: String -> String -> Maybe String
dropPrefix prefix s = if prefix `isPrefixOf` s
                        then Just (drop (length prefix) s)
                        else Nothing

dropSuffix :: String -> String -> Maybe String
dropSuffix suffix s = if suffix `isSuffixOf` s
                        then Just (take (length s - length suffix) s)
                        else Nothing

pattern Section :: String -> [Block] -> [Block] -> [Block]
pattern Section ref blocks remainder
  <- Header headerLevel (ref, _, _) _
   : (break (isHeaderLE headerLevel) -> (blocks, remainder))

isHeaderLE :: Int -> Block -> Bool
isHeaderLE n = \case
  Header n' _ _ -> n' <= n
  _             -> False

-- Handle struct members, enum docs and function parameter documentation
memberDocs :: Meta -> [Block] -> Either Text [Documentation]
memberDocs m = \case
  [BulletList bs] ->
    let enumDoc :: [Block] -> Either Text Documentation
        enumDoc = \case
          [p@(Para (Code ("",[],[]) memberName:_))] ->
            pure Documentation {dDocumentee = T.pack memberName, dDocumentation = Pandoc m [p]}
          _ -> Left "Unhandled member documentation declaration"
    in  traverse enumDoc bs
  _ -> Left "Trying to extract member documentation from an unhandled desscription"

--
-- runhaskell -isrc src/Documentation.hs \
--   <(asciidoctor -r ~/src/Vulkan-Docs/config/tilde_open_block.rb \
--                 -r asciidoctor-mathematical \
--                 -r ~/src/Vulkan-Docs/config/vulkan-macros.rb \
--                 --backend docbook5 \
--                 ~/src/Vulkan-Docs/man/VkSparseMemoryBind.txt \
--                 --out-file -
--    | sed 's/sidebar/section/'
--    | sed 's/<strong /<emphasis role="strong"/g'
--    | sed 's/<\/strong/<\/emphasis/g')



main :: IO ()
main = do
  [d, m] <- getArgs
  manTxtToDocbook d m >>= \case
    Left  e -> sayErr e
    Right d -> case docBookToDocumentation (T.pack m) d of
      Left  e -> sayErrShow e
      Right d -> case splitDocumentation d of
        Left  e  -> sayErr e
        Right ds -> do
          for_ ds sayShow
          for_ ds $ \d -> case documentationToHaddock (fixupDocumentation d) of
            Right t -> do
              sayShow ()
              -- say t
              -- say
              --   "\n\n--------------------------------------------------------------------------------\n\n"
            Left e -> sayErrShow e

-- extractMatches
--   :: ([a] -> Int)
--   -- ^ A function which takes a list and determines the length of a prefix to
--   -- extract.
--   -> [a]
--   -- ^ A list to extract parts from
--   -> ([[a]], [a])
--   -- ^ (The list of extracted prefixes, the list without those prefixes)
-- extractMatches _ [] = ([], [])
-- extractMatches split (x:xs) =
--   let (is, xs') = extractMatches split xs
--   in case split (x:xs') of
--         0 ->(is, x:xs')
--         i ->(take i (x:xs') : is, drop i (x:xs'))


extractMatches'
  :: forall a b
   . ([a] -> (b, [a]))
  -- ^ A function which takes a list transforming it and returning something
  -> [a]
  -- ^ A list to extract parts from
  -> ([b], [a])
  -- ^ (The list of (non-empty) extracted prefixes, the list without those
  -- prefixes)
extractMatches' split = foldr go ([], [])
  where
    go :: a -> ([b], [a]) -> ([b], [a])
    go x (ss, xs) = first (: ss) $ split (x : xs)

extractMatchesM
  :: forall m a b
   . Monad m
  => ([a] -> m (b, [a]))
  -- ^ A function which takes a list transforming it and returning something
  -> [a]
  -- ^ A list to extract parts from
  -> m ([b], [a])
  -- ^ (The list of (non-empty) extracted prefixes, the list without those
  -- prefixes)
extractMatchesM split = foldrM go ([], [])
  where
    go :: a -> ([b], [a]) -> m ([b], [a])
    go x (ss, xs) = first (: ss) <$> split (x : xs)

extractMatches
  :: forall a
   . ([a] -> Int)
  -- ^ A function which takes a list and determines the length of a prefix to
  -- extract.
  -> [a]
  -- ^ A list to extract parts from
  -> ([[a]], [a])
  -- ^ (The list of (non-empty) extracted prefixes, the list without those
  -- prefixes)
extractMatches split = foldr go ([], [])
  where
    go :: a -> ([[a]], [a]) -> ([[a]], [a])
    go x (ss, xs) =
      let xs' = x : xs
      in  case split xs' of
            0 -> (ss, xs')
            n -> (take n xs' : ss, drop n xs')
