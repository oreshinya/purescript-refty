module Refty
  ( Key
  , Identifier
  , Format
  , Entity
  , Reference
  , Params
  , Refty
  , entity
  , hasOneRef
  , hasManyRef
  , selfRef
  , params
  , individual
  , collection
  , concat
  , noBody
  , response
  , failure
  ) where

import Prelude

import Data.Foreign (Foreign, toForeign)
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap, empty, fromFoldableWith, singleton, unions)
import Data.Tuple (Tuple(..))
import Simple.JSON (class WriteForeign, writeImpl)



type Key = String

type Identifier a = a -> String

type Format =
  { entities :: StrMap (StrMap Foreign)
  , references :: StrMap Foreign
  }

data Entity a
  = Entity Key (Identifier a)

data Reference a
  = HasOne Key (Identifier a)
  | HasMany Key (Identifier a)
  | Self Key

data Params a
  = Params (Entity a) (Maybe (Reference a))

data Refty
  = Success Format
  | Failure (Array String)
  | NoBody



entity :: forall a. Key -> Identifier a -> Entity a
entity = Entity



hasOneRef :: forall a. Key -> Identifier a -> Maybe (Reference a)
hasOneRef k i = Just $ HasOne k i



hasManyRef :: forall a. Key -> Identifier a -> Maybe (Reference a)
hasManyRef k i = Just $ HasMany k i



selfRef :: forall a. Key -> Maybe (Reference a)
selfRef k = Just $ Self k



params :: forall a. Entity a -> Maybe (Reference a) -> Params a
params = Params



noBody :: Refty
noBody = NoBody



response :: Format -> Refty
response = Success



failure :: Array String -> Refty
failure = Failure



individual :: forall a. WriteForeign a => a -> Params a -> Format
individual x (Params (Entity k i) r) =
  let
    entities = singleton k $ formatEntity i x
  in
    case r of
      Nothing ->
        { entities, references: empty }
      Just (Self k') ->
        { entities
        , references: singleton k' $ toForeign $ i x
        }
      Just (HasOne k' i') ->
        { entities
        , references: singleton k' $ toForeign $ formatHasOne i i' x
        }
      Just (HasMany k' i') ->
        { entities
        , references: singleton k' $ toForeign $ singleton (i' x) [ i x ]
        }



collection :: forall a. WriteForeign a => Array a -> Params a -> Format
collection xs (Params (Entity k i) r) =
  let
    entities = singleton k $ unions $ map (formatEntity i) xs
  in
    case r of
      Nothing ->
        { entities, references: empty }
      Just (Self k') ->
        { entities
        , references: singleton k' $ toForeign $ map i xs
        }
      Just (HasOne k' i') ->
        { entities
        , references: singleton k' $ toForeign $
                       unions $ flip map xs $ formatHasOne i i'
        }
      Just (HasMany k' i') ->
        { entities
        , references: singleton k' $ toForeign $
                       fromFoldableWith (flip append) $ map (\x -> Tuple (i' x) [ i x ]) xs
        }



concat :: Array Format -> Format
concat fs =
  let entities = unions $ flip map fs \f -> f.entities
      references = unions $ flip map fs \f -> f.references
   in { entities, references  }



formatEntity :: forall a. WriteForeign a => Identifier a -> a -> StrMap Foreign
formatEntity i x = singleton (i x) $ writeImpl x



formatHasOne :: forall a. Identifier a -> Identifier a -> a -> StrMap String
formatHasOne i i' x = singleton (i' x) (i x)



instance writeForeignRefty :: WriteForeign Refty where
  writeImpl (Success f) = writeImpl f
  writeImpl (Failure messages) = writeImpl messages
  writeImpl NoBody = writeImpl ""
