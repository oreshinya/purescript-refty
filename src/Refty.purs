module Refty where

import Prelude
import Data.Foreign (Foreign, toForeign)
import Data.Foreign.Class (class Encode, encode)
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap, empty, fromFoldableWith, singleton, unions)
import Data.Tuple (Tuple(..))



type Key = String

type Identifier a = a -> String

type Format =
  { entities :: StrMap (StrMap Foreign)
  , references :: StrMap Foreign
  }

data Entity a
  = Entity Key (Identifier a)

infix 2 Entity as ~

data Reference a
  = HasOne Key (Identifier a)
  | HasMany Key (Identifier a)
  | Self Key

data Params a
  = Params (Entity a) (Maybe (Reference a))

infix 1 Params as ^

newtype Refty = Refty Format



hasOne :: forall a. Key -> Identifier a -> Maybe (Reference a)
hasOne k i = Just $ HasOne k i

infix 2 hasOne as |-|



hasMany :: forall a. Key -> Identifier a -> Maybe (Reference a)
hasMany k i = Just $ HasMany k i

infix 2 hasMany as |<|



self :: forall a. Key -> Maybe (Reference a)
self k = Just $ Self k



formatEntity :: forall a. Encode a => Identifier a -> a -> StrMap Foreign
formatEntity i x = singleton (i x) $ encode x



formatHasOne :: forall a. Identifier a -> Identifier a -> a -> StrMap String
formatHasOne i i' x = singleton (i' x) (i x)



individual :: forall a. Encode a => a -> Params a -> Format
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



collection :: forall a. Encode a => Array a -> Params a -> Format
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



response :: Format -> Refty
response = Refty



instance encodeRefty :: Encode Refty where
  encode (Refty f) = toForeign f
