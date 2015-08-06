module Test.Main where

import Prelude
import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console
import Control.Monad.ST
import Data.Array hiding (filter)
import Data.Array.ST
import Data.Tuple
import Data.Either
import Data.Maybe
import Signal
import Test.Unit
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.Signal

import Signal.CouchDB

instance arbChange :: Arbitrary Change where
  arbitrary = Change `map` ({ rev: _ } `map` arbitrary)

instance arbResult :: Arbitrary Result where
  arbitrary = Result `map` ({ seq: _, id: _, deleted: _, changes: _ } `map` arbitrary
                                                                      `apply` arbitrary
                                                                      `apply` arbitrary
                                                                      `apply` arbitrary)

instance arbNotification :: Arbitrary Notification where
  arbitrary = Notification `map` ({ last_seq: _, results: _ } `map` arbitrary `apply` arbitrary)

popSTArray :: forall a h eff. STArray h a -> Eff (st :: ST h | eff) (Maybe a)
popSTArray stArr = do
  arr <- freeze stArr
  spliceSTArray stArr 0 1 []
  return $ head arr

signalToArray :: forall a. Int -> Signal a -> Aff _ (Array a)
signalToArray i s = makeAff \_ success -> do
  st <- emptySTArray
  let f x = do
        pushSTArray st x
        a <- freeze st
        if (length a == i) then success a else return unit
  runSignal $ s ~> f

getStubGet :: forall a. (Show a) => Array (Either String (Either String a)) -> Eff _ (Int -> Eff _ (Either String (Either String a)))
getStubGet results = do
  mutableResults <- thaw results
  return $ \_ -> do
    currentResult <- popSTArray mutableResults
    --case currentResult of
    --  Just (Right (Right a)) -> log (show a)
    --  a -> log (show a)
    return $ maybe (Left "Error") id currentResult

main = do
  --randomSample' 100 arbitrary >>= (flip foreachE) \(Tuple notification notifications) -> runTest do
    --test "getNotification" do
    --timeout 10000 $ assertFn "should get notifications sequentially" \done -> do
  let nonEmptyNotifications :: Array (Either String (Either String Notification))
      nonEmptyNotifications = [Right (Right exampleNotification)]
  stubGet <- getStubGet nonEmptyNotifications
  signal <- getNotifications stubGet 0
  runSignal $ signal ~> log <<< show
  --launchAff $ do
  --  values <- signalToArray (length nonEmptyNotifications) signal
  --  liftEff $ log (show values)
    --liftEff $ case head values of
    --  Just (Right (Right (Notification value))) -> log $ show value.last_seq
    --  Just (Left s) -> log s
    --  Just _ -> log "Something"
    --  _ -> log "Nothing"
