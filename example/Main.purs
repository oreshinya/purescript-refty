module Main where

import Prelude
import Data.Foreign.Class (class Encode)
import Data.Foreign.Generic (defaultOptions, genericEncode, encodeJSON)
import Data.Generic.Rep (class Generic)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Refty (Format, Params, self, concat, individual, collection, response, (~), (^), (|-|), (|<|))


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


user :: User
user = User { id: "3", name: "dongri" }


comments :: Array Comment
comments =
  [ Comment { id : "3", userId: "1", content: "comment 1" }
  , Comment { id : "1", userId: "3", content: "comment 3" }
  , Comment { id : "2", userId: "1", content: "comment 2" }
  ]

userParams :: Params User
userParams = "users" ~ (\(User u) -> u.id) ^ self "self"

commentParams :: Params Comment
commentParams = "comments" ~ (\(Comment c) -> c.id) ^ "userComments" |<| (\(Comment c) -> c.userId)

commentParams2 :: Params Comment
commentParams2 = "comments" ~ (\(Comment c) -> c.id) ^ "userComment" |-| (\(Comment c) -> c.userId)

formatter :: Format
formatter = concat
  [ collection users userParams
  , collection comments commentParams
  ]

formatter2 :: Format
formatter2 = concat
  [ individual user userParams
  , collection comments commentParams2
  ]

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log $ "Collection: " <> (encodeJSON $ response formatter)
  log $ "Individual: " <> (encodeJSON $ response formatter2)

