module MMZ (MMZ, fold, runMMZ, mmzToEvent) where

import Prelude

import Control.Alt (class Alt)
import Control.Plus (class Plus)
import Data.Either (Either(..), either)
import Data.Filterable (class Compactable, class Filterable, filterMap, partitionMap)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import FRP.Event (Event, makeEvent, subscribe)

data MMZ (r :: Type) (a :: Type)

foreign import mmzMap :: forall r a b. (a -> b) -> MMZ r a -> MMZ r b

instance Functor (MMZ r) where
  map = mmzMap

foreign import mmzApply :: forall r a b. MMZ r (a -> b) -> MMZ r a -> MMZ r b

instance Apply (MMZ r) where
  apply = mmzApply

foreign import mmzEmpty :: forall r a. MMZ r a

instance Plus (MMZ r) where
  empty = mmzEmpty

foreign import mmzAlt :: forall r a. MMZ r a -> MMZ r a -> MMZ r a

instance Alt (MMZ r) where
  alt = mmzAlt

foreign import mmzPartitionMap
  :: forall x a l r
   . (a -> Either l r)
  -> MMZ x a
  -> { left :: MMZ x l, right :: MMZ x r }

instance Compactable (MMZ r) where
  compact = filterMap identity
  separate = partitionMap identity

instance Filterable (MMZ r) where
  partitionMap = mmzPartitionMap
  partition f x =
    let
      { left, right } = partitionMap (\v -> if f v then Left v else Right v) x
    in
      { yes: right, no: left }
  filterMap f = map _.right
    ( partitionMap
        ( \v -> case f v of
            Just x -> Right x
            Nothing -> Left unit
        )
    )
  filter f = filterMap (\v -> if f v then Just v else Nothing)

foreign import mmzFold :: forall r a b. (a -> b -> b) -> MMZ r a -> b -> MMZ r b

fold :: forall r a b. (a -> b -> b) -> MMZ r a -> b -> MMZ r b
fold = mmzFold

foreign import runMMZ_
  :: forall r a
   . { either :: forall f g m. (f -> m) -> (g -> m) -> Either f g -> m }
  -> r
  -> MMZ r a
  -> Effect Unit

foreign import addSubscription_
  :: forall r a. (a -> Effect Unit) -> MMZ r a -> Effect Unit

addSubscription :: forall r a. (a -> Effect Unit) -> MMZ r a -> Effect Unit
addSubscription = addSubscription_

foreign import removeSubscription_
  :: forall r a. (a -> Effect Unit) -> MMZ r a -> Effect Unit

removeSubscription :: forall r a. (a -> Effect Unit) -> MMZ r a -> Effect Unit
removeSubscription = removeSubscription_

foreign import mmzStart_ :: forall r a. Event a -> MMZ r a

runMMZ :: forall a o. Event a -> (forall r. MMZ r a -> o) -> o
runMMZ e f =
  let
    o = mmzStart_ $ makeEvent \_ -> do
      subscribe e \v -> do
        runMMZInternal v o
  in
    f o

foreign import actualizeMMZ_ :: forall r a. MMZ r a -> Effect Unit

mmzToEvent :: forall r a. MMZ r a -> Event a
mmzToEvent mmz = makeEvent \k -> do
  actualizeMMZ_ mmz
  addSubscription k mmz
  pure $ removeSubscription k mmz

runMMZInternal :: forall r a. r -> MMZ r a -> Effect Unit
runMMZInternal = runMMZ_ { either }