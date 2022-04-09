module MMZ(MMZ, runMMZ, fold, addSubscription, encapsulateMMZ) where

import Prelude

import Control.Alt (class Alt)
import Control.Plus (class Plus)
import Data.Either (Either(..), either)
import Data.Filterable (class Compactable, class Filterable, filterMap, partitionMap)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Unsafe.Coerce (unsafeCoerce)

data MMZ (p :: Type) (r :: Type) (a :: Type)

foreign import mmzMap :: forall p r a b. (a -> b) -> MMZ p r a -> MMZ p r b
instance Functor (MMZ p r) where
  map = mmzMap
foreign import mmzApply :: forall p r a b. MMZ p r (a -> b) -> MMZ p r a -> MMZ p r b
instance Apply (MMZ p r) where
  apply = mmzApply
foreign import mmzEmpty :: forall p r a. MMZ p r a
instance Plus (MMZ p r) where
  empty = mmzEmpty
foreign import mmzAlt :: forall p r a. MMZ p r a -> MMZ p r a -> MMZ p r a
instance Alt (MMZ p r) where
  alt = mmzAlt
foreign import mmzPartitionMap :: forall p x a l r. (a -> Either l r) -> MMZ p x a -> { left :: MMZ p x l, right :: MMZ p x r }
instance Compactable (MMZ p r) where
  compact = filterMap identity
  separate = partitionMap identity
instance Filterable (MMZ p r) where
  partitionMap = mmzPartitionMap
  partition f x = let {left,right} = partitionMap (\v -> if f v then Left v else Right v) x in {yes:right, no:left}
  filterMap f = map _.right (partitionMap (\v -> case f v of
     Just x -> Right x
     Nothing -> Left unit))
  filter f = filterMap (\v -> if f v then Just v else Nothing)

foreign import mmzFold :: forall p r a b. (a -> b -> b) -> MMZ p r a -> b -> MMZ p r b
fold :: forall p r a b. (a -> b -> b) -> MMZ p r a -> b -> MMZ p r b
fold = mmzFold

foreign import runMMZ_ :: forall p r a. {either :: forall f g m. (f -> m) -> (g -> m) -> Either f g -> m} -> r -> MMZ p r a -> Effect Unit

foreign import addSubscription_ :: forall p r a. (a -> Effect Unit) -> MMZ p r a -> Effect Unit

addSubscription :: forall p r a. (a -> Effect Unit) -> MMZ p r a -> Effect Unit
addSubscription = addSubscription_

foreign import mmzUnsafeStart :: forall p r. r -> MMZ p r r

encapsulateMMZ :: forall r o. (forall p. p -> MMZ p r r -> o) -> o
encapsulateMMZ f = f (unsafeCoerce unit) (mmzUnsafeStart (unsafeCoerce unit))

runMMZ :: forall p r a. p -> r -> MMZ p r a -> Effect Unit
runMMZ _ = runMMZ_ {either}