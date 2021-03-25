module Main4 where

import Prelude
import Data.List ((:))
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Prim.Row (class Cons, class Lacks, class Union)
import Prim.RowList (class RowToList, Cons, Nil)
import Record (insert, union)
import Type.RowList (class ListToRow, RLProxy(..))

class WithNothingDefaults rin rout | rin -> rout where
  withNothingDefaults :: RLProxy rin -> { | rout }

instance witNothingDefaultsComplete :: WithNothingDefaults Nil () where
  withNothingDefaults _ = {}
else instance witNothingDefaultsNeeding :: (Lacks k prev, IsSymbol k, WithNothingDefaults rest prev, Cons k (Maybe v) prev rout) => WithNothingDefaults (Cons k (Maybe v) rest) rout where
  withNothingDefaults _ = insert (SProxy :: SProxy k) (Nothing :: Maybe v) $ withNothingDefaults (RLProxy :: RLProxy rest)
else instance witNothingDefaultsSkipping :: (WithNothingDefaults rest rout) => WithNothingDefaults (Cons k v rest) rout where
  withNothingDefaults _ = withNothingDefaults (RLProxy :: RLProxy rest)

type MyJSON
  = ( a :: Maybe Int
    , b :: Maybe String
    , c :: Int
    , d :: Maybe Boolean
    , e :: String
    , f :: Maybe (Array Number)
    )

defaultize :: forall rin rout rl def. RowToList MyJSON rl => WithNothingDefaults rl def => Union rin def rout => { | rin } -> { | rout }
defaultize = flip union $ withNothingDefaults (RLProxy :: RLProxy rl)

logMyJSON :: { | MyJSON } -> Effect Unit
logMyJSON = log <<< show

main :: Effect Unit
main = do
  logMyJSON $ defaultize { c: 1, e: "" }

hasAZero :: L.List (Tuple String Int) -> L.List String
hasAZero L.Nil = L.Nil

hasAZero ((Tuple a 0) : rest) = a : hasAZero rest

hasAZero (a : rest) = hasAZero rest

-- hasAZero :: Set (Tuple String Int) -> Set String
