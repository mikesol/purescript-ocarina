module WAGS.Graph.Decorators where

import Data.Identity (Identity(..))
import Data.Symbol (class IsSymbol)
import Prim.Row (class Cons, class Lacks)
import Prim.RowList as RL
import Data.Tuple.Nested ((/\), type (/\))
import Record (insert, modify)
import Type.Proxy (Proxy(..))

type Decorated a = forall b c. Decorate a b c => { | b } /\ { | c }

class Decorate (a :: Row Type) (b :: Row Type) (c :: Row Type) | a -> b c where
  decorate :: forall d. ({ | a } -> d) -> { | b } /\ { | c }

class MakeDecorators (rl :: RL.RowList Type) b | rl -> b where
  makeDecorators :: Proxy rl -> { | b }

instance makeDecoratorsRLCons :: (IsSymbol sym, Lacks sym rest, Cons sym (Decorating Identity) rest r, MakeDecorators b rest) => MakeDecorators (RL.Cons sym (Decorating f) b) r where
  makeDecorators _ = insert (Proxy :: _ sym) (Decorating Identity) (makeDecorators (Proxy :: _ b))

instance makeDecoratorsRLNil :: MakeDecorators RL.Nil () where
  makeDecorators _ = {}

class MakeFocusing (rl :: RL.RowList Type) (b :: Row Type) (c :: Row Type) | rl b -> c where
  makeFocusing :: Proxy rl -> { | b } -> { | c }

instance makeMakeFocusingRLCons :: (IsSymbol sym, Lacks sym rest, Cons sym (Decorating Identity) q r, Cons sym (Decorating Focus) q r', Cons sym (Record r') rest c, MakeFocusing b r rest) => MakeFocusing (RL.Cons sym (Decorating f) b) r c where
  makeFocusing _ r = insert (Proxy :: _ sym) (modify (Proxy :: Proxy sym) (\_ -> Decorating Focus) r) (makeFocusing (Proxy :: _ b) r)

instance makeFocusingNil :: MakeFocusing RL.Nil x () where
  makeFocusing _ _ = {}

instance decorateDecorating :: (RL.RowToList a rl, MakeDecorators rl b, MakeFocusing rl b c) => Decorate a b c where
  decorate _ = idy /\ (makeFocusing (Proxy :: _ rl) idy)
    where
    idy = makeDecorators (Proxy :: _ rl)

newtype Decorating f
  = Decorating (forall a. a -> f a)

dk :: forall f. Decorating f -> (forall a. a -> f a)
dk (Decorating f) = f

data Focus a
  = Focus a
