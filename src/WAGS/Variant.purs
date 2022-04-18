-- | Tools for working with variants in wags.
-- | Could be another module/project.
module WAGS.Variant where


import Data.Maybe (Maybe)
import Data.Symbol (class IsSymbol)
import Data.Variant (Variant, inj, prj)
import Prim.Row as Row
import Prim.RowList as RL
import Record as R
import Type.Proxy (Proxy(..))

class Injs (x :: Row Type) (rl :: RL.RowList Type) (r :: Row Type) | x rl -> r where
  injs' :: forall proxyR proxyRL. proxyR x -> proxyRL rl -> { | r }

instance Injs x RL.Nil () where
  injs' _ _ = {}

instance
  ( IsSymbol key
  , Injs x rest r'
  , Row.Lacks key x'
  , Row.Lacks key r'
  , Row.Cons key val x' x
  , Row.Cons key (val -> Variant x) r' r
  ) =>
  Injs x (RL.Cons key val rest) r where
  injs' x _ = R.insert (Proxy :: _ key) (inj (Proxy :: _ key)) (injs' x (Proxy :: _ rest))

injs :: forall proxy x r rl. RL.RowToList x rl => Injs x rl r => proxy x -> { | r }
injs px = injs' px (Proxy :: _ rl)

injs_ :: forall proxy x r rl. RL.RowToList x rl => Injs x rl r => proxy (Variant x) -> { | r }
injs_ _ = injs' (Proxy :: _ x) (Proxy :: _ rl)

class Prjs (x :: Row Type) (rl :: RL.RowList Type) (r :: Row Type) | x rl -> r where
  prjs' :: forall proxyR proxyRL. proxyR x -> proxyRL rl -> { | r }

instance Prjs x RL.Nil () where
  prjs' _ _ = {}

instance
  ( IsSymbol key
  , Prjs x rest r'
  , Row.Lacks key x'
  , Row.Lacks key r'
  , Row.Cons key val x' x
  , Row.Cons key (Variant x -> Maybe val) r' r
  ) =>
  Prjs x (RL.Cons key val rest) r where
  prjs' x _ = R.insert (Proxy :: _ key) (prj (Proxy :: _ key)) (prjs' x (Proxy :: _ rest))

prjs :: forall proxy x r rl. RL.RowToList x rl => Prjs x rl r => proxy x -> { | r }
prjs px = prjs' px (Proxy :: _ rl)

prjs_ :: forall proxy x r rl. RL.RowToList x rl => Prjs x rl r => proxy (Variant x) -> { | r }
prjs_ _ = prjs' (Proxy :: _ x) (Proxy :: _ rl)
