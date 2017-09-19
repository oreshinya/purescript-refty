module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Foreign (F, unsafeFromForeign)
import Data.Foreign.Class (class Encode, class Decode, encode, decode)
import Data.Foreign.Generic (defaultOptions, genericEncode, genericDecode)
import Data.Generic.Rep (class Generic)
import Data.StrMap (StrMap)
import Global.Unsafe (unsafeStringify)
import Refty (reftyify)
import Refty as R


newtype User = User { id :: String, name :: String }

derive instance genericUser :: Generic User _

instance encodeUser :: Encode User where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }

instance reftyableUser :: R.Reftyable User where
  reftyify = encode

instance decodeUser :: Decode User where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }

newtype Comment = Comment { id :: String, userId :: String, content :: String }

derive instance genericComment :: Generic Comment _

instance encodeComment :: Encode Comment where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }

instance reftyableComment :: R.Reftyable Comment where
  reftyify = encode

instance decodeComment :: Decode Comment where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }

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

newtype Response =
  Response
    { entities :: { users :: StrMap User, comments :: StrMap Comment }
    , references :: { self :: Array String, userComments :: StrMap String }
    }

instance decodeResponse :: Decode Response where
  decode value = pure $ unsafeFromForeign value

dc :: F Response
dc = decode $ R.reftyify $ R.response formatter2

foreign import unsafeLog :: forall a e. a -> Eff e Unit

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log $ "Collection: " <> (unsafeStringify $ reftyify $ R.response formatter)
  log $ "Individual: " <> (unsafeStringify $ reftyify $ R.response formatter2)
  log $ "Failure: " <> (unsafeStringify $ reftyify $ R.failure [ "Error Message 1", "Error Message 2", "Error Message 3" ])
  decoded
    where
      decoded :: Eff (console :: CONSOLE | e) Unit
      decoded = case (runExcept dc) of
                  Left _ -> log "failure"
                  Right (Response r) -> unsafeLog r.entities.comments
