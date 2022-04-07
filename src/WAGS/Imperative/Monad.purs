-- | Definitions for the indexed graph builder monad.
module WAGS.Imperative.Monad where

import Prelude

import Control.Alternative (empty, (<|>))
import Control.Monad.Indexed (class IxApplicative, class IxApply, class IxBind, class IxMonad)
import Data.Functor.Indexed (class IxFunctor)
import Effect (Effect)
import FRP.Event (Event)
import FRP.Event.Class (class IsEvent)
import FRP.Event.Phantom (PhantomEvent, Proof0)
import Prim.Boolean (True, False)
import Prim.RowList as RL
import Prim.TypeError (class Fail, Beside, Text)
import WAGS.Core (AudioInterpret)
import WAGS.Imperative.Types (type (\/))
import WAGS.Interpret (FFIAudioSnapshot, effectfulAudioInterpret)

-- | The indexed graph builder monad.
-- |
-- | In order:
-- | `e` - the event type
-- | `p` - the payload yielded by the event
-- | `i` - the input index
-- | `o` - the output index
-- | `a` - the result of the computation
-- |
-- | This is implemented internally as a `Reader` whose environment is
-- | the `AudioInterpret` type, returning a result `a` accompanied by
-- | an event `e p`.
newtype GraphBuilder :: (Type -> Type -> Type) -> Type -> Type -> Type -> Type -> Type -> Type
newtype GraphBuilder e x p i o a = GraphBuilder
  (AudioInterpret e x p -> { event :: e x p, result :: a })

-- | The initial index of the graph builder.
-- |
-- | In order:
-- | * A row containing all currently connected nodes
-- | * A row containing which nodes reach an exit
-- | * A boolean determining whether the graph makes sound
type InitialIndex :: Type
type InitialIndex = (() :: Row Type) \/ (() :: Row Boolean) \/ False

-- | A graph builder with the initial index as its input.
-- |
-- | This is particularly helpful when writing top-level signatures.
type InitialGraphBuilder e x p = GraphBuilder e x p InitialIndex

-- .==========================================================================.

instance functorGraphBuilder :: Functor (GraphBuilder e x p i i) where
  map f (GraphBuilder g) = GraphBuilder (g >>> \n -> n { result = f n.result })

instance applyGraphBuilder :: IsEvent (e x) => Apply (GraphBuilder e x p i i) where
  apply (GraphBuilder f) (GraphBuilder g) = GraphBuilder h
    where
    h audioInterpret =
      let
        f' = f audioInterpret
        g' = g audioInterpret
      in
        { event: f'.event <|> g'.event
        , result: f'.result g'.result
        }

instance applicativeGraphBuilder ::
  IsEvent (e x) =>
  Applicative (GraphBuilder e x p i i) where
  pure result = GraphBuilder \_ -> { event: empty, result }

instance bindGraphBuilder :: IsEvent (e x) => Bind (GraphBuilder e x p i i) where
  bind (GraphBuilder f) mkG = GraphBuilder h
    where
    h audioInterpret =
      let
        f' = f audioInterpret
        (GraphBuilder g) = mkG f'.result
        g' = g audioInterpret
      in
        { event: f'.event <|> g'.event
        , result: g'.result
        }

instance monadGraphBuilder :: IsEvent (e x) => Monad (GraphBuilder e x p i i)

instance ixFunctorGraphBuilder :: IxFunctor (GraphBuilder e x p) where
  imap f (GraphBuilder g) = GraphBuilder (g >>> \n -> n { result = f n.result })

instance ixApplyGraphBuilder :: IsEvent (e x) => IxApply (GraphBuilder e x p) where
  iapply (GraphBuilder f) (GraphBuilder g) = GraphBuilder h
    where
    h audioInterpret =
      let
        f' = f audioInterpret
        g' = g audioInterpret
      in
        { event: f'.event <|> g'.event
        , result: f'.result g'.result
        }

instance ixApplicativeGraphBuilder ::
  IsEvent (e x) =>
  IxApplicative (GraphBuilder e x p) where
  ipure result = GraphBuilder \_ -> { event: empty, result }

instance ixBindGraphBuilder :: IsEvent (e x) => IxBind (GraphBuilder e x p) where
  ibind (GraphBuilder f) mkG = GraphBuilder h
    where
    h audioInterpret =
      let
        f' = f audioInterpret
        (GraphBuilder g) = mkG f'.result
        g' = g audioInterpret
      in
        { event: f'.event <|> g'.event
        , result: g'.result
        }

instance ixMonadGraphBuilder :: IsEvent (e x) => IxMonad (GraphBuilder e x p)

-- .==========================================================================.

-- | A constraint that determines whether the built graph emits a sound.
class GraphMakesSound :: Type -> Constraint
class GraphMakesSound o

instance graphMakesSoundTrue :: GraphMakesSound (c \/ t \/ True)
instance graphMakesSoundFalse ::
  ( Fail
      ( Text
          "GraphMakesSound: the graph has no nodes that allow it to make sound."
      )
  ) =>
  GraphMakesSound (c \/ t \/ False)

-- | An auxiliary constraint used by `GraphNodesAreTerminal`.
class GraphNodesAreTerminalRL :: RL.RowList Boolean -> Constraint
class GraphNodesAreTerminalRL o

instance graphNodesAreTerminalRLNil :: GraphNodesAreTerminalRL RL.Nil
else instance graphNodesAreTerminalRLTrue ::
  ( GraphNodesAreTerminalRL rest
  ) =>
  GraphNodesAreTerminalRL (RL.Cons id True rest)
else instance graphNodesAreTerminalRLFalse ::
  ( Fail
      ( Beside
          (Text "GraphNodesAreTerminal: the graph node '")
          ( Beside
              (Text id)
              (Text "' is not connected to a terminal node.")
          )
      )
  ) =>
  GraphNodesAreTerminalRL (RL.Cons id False rest)

-- | A constraint that determines whether all graph nodes reach an exit.
class GraphNodesAreTerminal :: Type -> Constraint
class GraphNodesAreTerminal o

instance graphNodesAreTerminalDefault ::
  ( RL.RowToList t t'
  , GraphNodesAreTerminalRL t'
  ) =>
  GraphNodesAreTerminal (c \/ t \/ s)

-- .==========================================================================.

-- | Run the graph builder without checks.
unGraphBuilder
  :: forall e x p i o a
   . IsEvent (e x)
  => GraphBuilder e x p i o a
  -> AudioInterpret e x p
  -> { event :: e x p, result :: a }
unGraphBuilder (GraphBuilder f) = f

-- | Run the graph builder with checks.
runGraphBuilder_
  :: forall e x p o a
   . IsEvent (e x)
  => GraphMakesSound o
  => GraphNodesAreTerminal o
  => InitialGraphBuilder e x p o a
  -> AudioInterpret e x p
  -> { event :: e x p, result :: a }
runGraphBuilder_ = unGraphBuilder

-- | Run the graph builder with checks, discarding the `result`.
runGraphBuilder
  :: forall e x p o a
   . IsEvent (e x)
  => GraphMakesSound o
  => GraphNodesAreTerminal o
  => InitialGraphBuilder e x p o a
  -> AudioInterpret e x p
  -> e x p
runGraphBuilder graphBuilder = runGraphBuilder_ graphBuilder >>> _.event

-- | Run the graph builder with checks using `effectfulAudioInterpret`.
effectfulGraphBuilder_
  :: forall o a
   . GraphMakesSound o
  => GraphNodesAreTerminal o
  => InitialGraphBuilder PhantomEvent Proof0 (FFIAudioSnapshot -> Effect Unit) o a
  -> { event :: PhantomEvent Proof0 (FFIAudioSnapshot -> Effect Unit)
     , result :: a
     }
effectfulGraphBuilder_ = flip runGraphBuilder_ effectfulAudioInterpret

-- | Run the graph builder with checks using `effectfulAudioInterpret`,
-- | discarding the `result`.
effectfulGraphBuilder
  :: forall o a
   . GraphMakesSound o
  => GraphNodesAreTerminal o
  => InitialGraphBuilder PhantomEvent Proof0 (FFIAudioSnapshot -> Effect Unit) o a
  -> PhantomEvent Proof0 (FFIAudioSnapshot -> Effect Unit)
effectfulGraphBuilder = effectfulGraphBuilder_ >>> _.event
