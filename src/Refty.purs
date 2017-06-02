module Refty where

import Data.Exists (Exists)
import Data.Maybe (Maybe)



type Key = String

type Identifier i a = a -> i

data Entity i a
  = Individual Key (Identifier i a) a
  | Collection Key (Identifier i a) (Array a)

data Reference i a
  = HasOne Key (Identifier i a)
  | HasMany Key (Identifier i a)

data AssociationF i a = AssociationF (Entity i a) (Maybe (Reference i a))

type Association i = Exists (AssociationF i)

data Formatter i a = Formatter (Entity i a) (Array (Association i))
