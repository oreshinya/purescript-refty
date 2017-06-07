module Main where

import Prelude
import Data.Foreign.Class (class Encode)
import Data.Foreign.Generic (defaultOptions, genericEncode, encodeJSON)
import Data.Generic.Rep (class Generic)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Refty (Format, self, concat, collection, response, (~), (^), (|<|))


newtype User = User { id :: String, name :: String }

derive instance genericUser :: Generic User _

instance encodeUser :: Encode User where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }

newtype Comment = Comment { id :: String, userId :: String, content :: String }

derive instance genericComment :: Generic Comment _

instance encodeComment :: Encode Comment where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }

users :: Array User
users =
  [ User { id: "3", name: "dongri" }
  , User { id: "1", name: "oreshinya" }
  , User { id: "2", name: "mdaisuke" }
  ]

comments :: Array Comment
comments =
  [ Comment { id : "3", userId: "1", content: "comment 1" }
  , Comment { id : "1", userId: "3", content: "comment 3" }
  , Comment { id : "2", userId: "1", content: "comment 2" }
  ]

formatter :: Format
formatter = concat
  [ collection users $ "users" ~ (\(User u) -> u.id) ^ self "self"
  , collection comments $ "comments" ~ (\(Comment c) -> c.id) ^ "userComments" |<| (\(Comment c) -> c.userId)
  ]

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = log $ encodeJSON $ response formatter
