module WAGS.Template where

import Prelude

import Data.Symbol (class IsSymbol)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Typelevel.Num (class Pos, class Pred, D0, toInt')
import Data.Vec as V
import Prim.Row (class Lacks, class Cons)
import Prim.RowList (RowList)
import Prim.RowList as RowList
import Prim.Symbol as Symbol
import Record as Record
import Type.Proxy (Proxy(..))
import WAGS.Create.Optionals (gain)
import WAGS.Graph.AudioUnit (Gain)
import WAGS.Graph.Parameter (AudioParameter)
import WAGS.Util (class NatToSym)

class SuffixAllRecords (s :: Symbol) (i :: Row Type) (o :: Row Type) | s i -> o where
  suffixize :: forall proxy. proxy s -> { | i } -> { | o }

instance suffixAllRecordsAll :: (RowList.RowToList i rl, SuffixAllRecordsRL rl s i o) => SuffixAllRecords s i o where
  suffixize = suffixizeRL (Proxy :: _ rl)

class SuffixAllRecordsRL (rl :: RowList Type) (s :: Symbol) (i :: Row Type) (o :: Row Type) | rl s i -> o where
  suffixizeRL :: forall proxyA proxyB. proxyA rl -> proxyB s -> { | i } -> { | o }

instance suffixAllRecordsRLCons ::
  ( SuffixAllRecordsRL c s i o'
  , IsSymbol a
  , IsSymbol aSuffix
  , Cons a (b /\ rest) x i
  , Symbol.Append a s aSuffix
  , Lacks aSuffix o'
  , SuffixAllRecordsRec s rest newRest
  , Cons aSuffix (b /\ newRest) o' o
  ) =>
  SuffixAllRecordsRL (RowList.Cons a (b /\ rest) c) s i o where
  suffixizeRL _ px i = Record.insert (Proxy :: _ aSuffix) (map (suffixizeRec px) (Record.get (Proxy :: _ a) i)) (suffixizeRL (Proxy :: _ c) (Proxy :: _ s) i)

instance suffixAllRecordsRLNil :: SuffixAllRecordsRL RowList.Nil s i () where
  suffixizeRL _ _ _ = {}

class SuffixAllRecordsRec (s :: Symbol) i o | s i -> o where
  suffixizeRec :: forall proxy. proxy s -> i -> o

instance suffixAllRecordsRec :: SuffixAllRecords s i o => SuffixAllRecordsRec s { | i } { | o } where
  suffixizeRec = suffixize

instance suffixAllRecordsTupRec :: SuffixAllRecordsRec s i o => SuffixAllRecordsRec s (a /\ i) (a /\ o) where
  suffixizeRec s (a /\ b) = a /\ suffixizeRec s b

class PoolWithTemplate' (suffix :: Symbol) (n :: Type) (a :: Type) (g :: Type) (o :: Row Type) | suffix n a g -> o where
  fromTemplate' :: forall proxy. proxy suffix -> V.Vec n a -> (Int -> a -> g) -> { | o }

instance poolWithTemplate'D0 :: PoolWithTemplate' suffix D0 a g () where
  fromTemplate' _ _ _ = {}
else instance poolWithTemplate'D ::
  ( Pred n n'
  , PoolWithTemplate' suffix n' a g x
  , NatToSym n sn
  , Symbol.Append "busFor_" sn s0
  , Symbol.Append s0 suffix sym
  , Symbol.Append "_unitFor_" sn s1
  , Symbol.Append s1 suffix sym'
  , SuffixAllRecordsRec sym' g gg
  , Cons sym gg x o
  , Lacks sym x
  , IsSymbol sym
  ) =>
  PoolWithTemplate' suffix n a g o where
  fromTemplate' px v fa =
    let
      uc = V.uncons v
    in
      Record.insert (Proxy :: _ sym)
        (suffixizeRec (Proxy :: _ sym') (fa (toInt' (Proxy :: _ n)) uc.head))
        (fromTemplate' px uc.tail fa)

class
  Pos n <= PoolWithTemplate (suffix :: Symbol) (n :: Type) (a :: Type) (g :: Type) (o :: Type) | suffix n a g -> o where
  fromTemplate :: forall proxy. proxy suffix -> V.Vec n a -> (Int -> a -> g) -> o

instance poolWithTemplateAll :: (Pos n, PoolWithTemplate' suffix n a g o) => PoolWithTemplate suffix n a g (Gain AudioParameter /\ { | o }) where
  fromTemplate a b c = gain 1.0 (fromTemplate' a b c)
