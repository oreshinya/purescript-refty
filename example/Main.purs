module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Foreign (F)
import Data.StrMap (StrMap)
import Global.Unsafe (unsafeStringify)
import Refty as R
import Simple.JSON (read, write)



type User = { id :: String, name :: String }

type Comment = { id :: String, userId :: String, content :: String }



users :: Array User
users =
  [ { id: "3", name: "keisukehonda" }
  , { id: "1", name: "oreshinya" }
  , { id: "2", name: "shinjiokazaki" }
  ]



user :: User
user = { id: "3", name: "dongri" }



comments :: Array Comment
comments =
  [ { id : "3", userId: "1", content: "comment 1" }
  , { id : "1", userId: "3", content: "comment 3" }
  , { id : "2", userId: "1", content: "comment 2" }
  ]



userParams :: R.Params User
userParams = R.params (R.entity "users" _.id) $ R.selfRef "self"



commentParams :: R.Params Comment
commentParams = R.params (R.entity "comments" _.id) $ R.hasManyRef "userComments" _.userId



commentParams2 :: R.Params Comment
commentParams2 = R.params (R.entity "comments" _.id) $ R.hasOneRef "userComments" _.userId



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



type Response =
  { entities :: { users :: StrMap User, comments :: StrMap Comment }
  , references :: { self :: String, userComments :: StrMap String }
  }



dc :: F Response
dc = read $ write $ R.response formatter2



foreign import unsafeLog :: forall a e. a -> Eff e Unit



main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log $ "Collection: " <> (unsafeStringify $ write $ R.response formatter)
  log $ "Individual: " <> (unsafeStringify $ write $ R.response formatter2)
  log $ "Failure: " <> (unsafeStringify $ write $ R.failure [ "Error Message 1", "Error Message 2", "Error Message 3" ])
  log $ "NoBody: " <> (unsafeStringify $ write R.noBody)
  decoded
    where
      decoded :: Eff (console :: CONSOLE | e) Unit
      decoded = case (runExcept dc) of
                  Left e -> unsafeLog e
                  Right r -> do
                    unsafeLog r
                    unsafeLog r.references
                    unsafeLog r.entities
