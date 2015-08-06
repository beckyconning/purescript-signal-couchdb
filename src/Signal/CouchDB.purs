module Signal.CouchDB where

import Prelude
import Signal
import Signal.Channel
import Data.Identity
import Signal.Loop
import Data.Either
import Data.Foreign
import Data.Foreign.Class
import Data.Foreign.Undefined
import Data.Maybe
import Control.Monad.Eff

newtype Change = Change { rev :: String }

newtype Result = Result { seq :: Int, id :: String, deleted :: Maybe Boolean, changes :: Array Change }

newtype Notification = Notification { last_seq :: Int, results :: Array Result }

exampleNotification = Notification { last_seq: 1, results: [ Result  { seq: 1
                                                                     , id: "hi"
                                                                     , deleted: Just true
                                                                     , changes: [ Change { rev: "bye" } ] } ] }

instance eqChange :: Eq Change where
  eq (Change o1) (Change o2) = (o1.rev `eq` o2.rev)

instance eqResult :: Eq Result where
  eq (Result o1) (Result o2) = (o1.seq `eq` o2.seq) `conj` (o1.id `eq` o2.id)
                                                    `conj` (o1.deleted `eq` o2.deleted)
                                                    `conj` (o1.changes `eq` o2.changes)

instance eqNotification :: Eq Notification where
  eq (Notification o1) (Notification o2) = (o1.last_seq `eq` o2.last_seq) `conj` (o1.results `eq` o2.results)

instance showChange :: Show Change where
  show (Change o) = "Change " ++ "{ rev: " ++ o.rev ++ " }"

instance showResult :: Show Result where
  show (Result o) = "Result " ++ "{ seq: " ++ show o.seq ++ ", id: " ++ show o.id ++ ", deleted: " ++ show o.deleted ++ ", changes: " ++ show o.changes ++ " }"

instance showNotification :: Show Notification where
  show (Notification o) = "Notification " ++ "{ last_seq: " ++ show o.last_seq ++ ", results: " ++ show o.results ++ " }"

changesUrl :: String -> String -> Int -> String
changesUrl couchDBURL dBName sinceSeq = couchDBURL ++ "/" ++ dBName ++ query
  where
  query = "/_changes?feed=longpoll&since=" ++ show sinceSeq

getNotifications :: forall a b e. (Int -> Eff (chan :: Chan | e) (Either a (Either b Notification)))
                                  -> Int
                                  -> Eff (chan :: Chan | e) (Signal (Either a (Either b Notification)))
getNotifications get seq = get seq >>= (flip runLoop) getNext
  where
  getNext :: Loop e (Either a (Either b Notification))
  getNext notificationSignal = getEmitter `map` notificationSignal
    where
    getEmitter :: Either a (Either b Notification) -> Emitter (chan :: Chan | e) (Either a (Either b Notification))
    getEmitter (Left _)                         c = return unit
    getEmitter (Right (Left _))                 c = return unit
    getEmitter (Right (Right (Notification o))) c = void $ get o.last_seq >>= send c
