module Bolson.Core where

import Prelude

import Control.Monad.ST (ST)
import Control.Monad.ST.Global as ST
import Data.List as List
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import FRP.Poll (Poll)

type HeadElement' interpreter payload = interpreter -> Poll payload

type Element' interpreter r payload =
  PSR r -> HeadElement' interpreter payload

newtype Element interpreter r payload = Element (Element' interpreter r payload)

data Child (logic :: Type)
  = Remove
  | Logic logic

newtype DynamicChildren logic obj = DynamicChildren
      (Poll (Tuple (Poll (Child logic)) (Entity logic obj)))

newtype FixedChildren logic obj = FixedChildren
  (Array (Entity logic obj))

data Scope = Local String | Global

derive instance Eq Scope
derive instance Ord Scope

type PSR r =
  { parent :: Maybe String
  , scope :: Scope
  , raiseId :: String -> ST ST.Global Unit
  , deferralPath :: List.List Int
  | r
  }

data Entity logic obj
  = DynamicChildren' (DynamicChildren logic obj)
  | FixedChildren' (FixedChildren logic obj)
  | Element' obj

instance Functor (Entity logic) where
  map f = case _ of
    DynamicChildren' (DynamicChildren a) ->
      DynamicChildren' (DynamicChildren $ map (map (map f)) a)
    FixedChildren' (FixedChildren a) ->
      FixedChildren' (FixedChildren (map (map f) a))
    Element' a -> Element' (f a)

fixed
  :: forall logic obj
   . Array (Entity logic obj)
  -> Entity logic obj
fixed a = FixedChildren' (FixedChildren a)

dyn
  :: forall logic obj
   . (Poll (Tuple (Poll (Child logic)) (Entity logic obj)))
  -> Entity logic obj
dyn a = DynamicChildren' (DynamicChildren a)
