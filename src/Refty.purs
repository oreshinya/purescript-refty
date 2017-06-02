module Refty where

import Prelude
import Data.Exists (Exists)
import Data.Foreign (Foreign, toForeign)
import Data.Foreign.Class (class Encode, encode)
import Data.Maybe (Maybe)
import Data.StrMap (StrMap, singleton, fromFoldable)
import Data.Tuple (Tuple(..))



type Key = String

type Identifier a = a -> String

data Entity a
  = Individual Key (Identifier a) a
  | Collection Key (Identifier a) (Array a)

data Reference a
  = HasOne Key (Identifier a)
  | HasMany Key (Identifier a)

data AssociationF a = AssociationF (Entity a) (Maybe (Reference a))

type Association = Exists AssociationF

data Encoder a = Encoder (Entity a) (Array Association)


mapify :: forall a. Encode a => Entity a -> StrMap (StrMap Foreign)
mapify (Individual k i a) = singleton k $ singleton (i a) $ encode a
mapify (Collection k i as) = singleton k $ fromFoldable $ map (\a -> Tuple (i a) (encode a)) as

-- TODO toEntities関数

-- TODO toReferences関数

instance encoderEncode :: Encode a => Encode (Encoder a) where
  encode encoder = toForeign
    { entities: [] -- toEntities関数にencoderを渡す
    , references: [] -- toReferences関数にencoderを渡す
    }
