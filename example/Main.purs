module Main where

import Prelude
import Data.Foreign.Class (class Encode)
import Data.Foreign.Generic (defaultOptions, genericEncode, encodeJSON)
import Data.Generic.Rep (class Generic)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Refty as R


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
  [ User { id: "3", name: "keisukehonda" }
  , User { id: "1", name: "oreshinya" }
  , User { id: "2", name: "shinjiokazaki" }
  ]


user :: User
user = User { id: "3", name: "dongri" }


comments :: Array Comment
comments =
  [ Comment { id : "3", userId: "1", content: "comment 1" }
  , Comment { id : "1", userId: "3", content: "comment 3" }
  , Comment { id : "2", userId: "1", content: "comment 2" }
  ]

userId :: User -> String
userId (User u) = u.id

commentId :: Comment -> String
commentId (Comment c) = c.id

commentUserId :: Comment -> String
commentUserId (Comment c) = c.userId

userParams :: R.Params User
userParams = R.params (R.entity "users" userId) $ R.selfRef "self"

commentParams :: R.Params Comment
commentParams = R.params (R.entity "comments" commentId) $ R.hasManyRef "userComments" commentUserId

commentParams2 :: R.Params Comment
commentParams2 = R.params (R.entity "comments" commentId) $ R.hasOneRef "userComments" commentUserId

formatter :: R.Format
formatter = R.concat
  [ R.collection users userParams
  , R.collection comments commentParams
  ]

formatter2 :: R.Format
formatter2 = R.concat
  [ R.individual user userParams
  , R.collection comments commentParams2
  ]

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log $ "Collection: " <> (encodeJSON $ R.response formatter)
  log $ "Individual: " <> (encodeJSON $ R.response formatter2)

