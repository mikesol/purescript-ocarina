module WAGS.Imperative where

import Prelude

import Control.Alternative (empty, (<|>))
import Control.Monad.Indexed (class IxApplicative, class IxApply, class IxBind, class IxMonad)
import Data.Functor.Indexed (class IxFunctor)
import Data.Newtype (unwrap)
import Data.Variant (match)
import Data.Variant.Maybe (nothing)
import FRP.Behavior (sample_)
import FRP.Event.Class (class IsEvent, keepLatest)
import Prim.Boolean (False, True)
import Prim.Row as Row
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import WAGS.Common as Common
import WAGS.Core as C

--
data TypePair :: forall k l. k -> l -> Type
data TypePair a b

infixr 6 type TypePair as \/

--
newtype GraphBuilder :: (Type -> Type) -> Type -> Type -> Type -> Type -> Type
newtype GraphBuilder e p i o a = GraphBuilder
  (C.AudioInterpret e p -> { event :: e p, result :: a })

unsafeGraphBuilder :: forall i o e p a. GraphBuilder e p i o a
unsafeGraphBuilder = unsafeCoerce unit

type InitialGraphBuilderIndex :: Type
type InitialGraphBuilderIndex = (() :: Row Type) \/ (() :: Row Boolean) \/ False

evalGraphBuilder
  :: forall o e p a. GraphBuilder e p InitialGraphBuilderIndex o a -> Proxy o
evalGraphBuilder _ = Proxy

instance functorGraphBuilder :: Functor (GraphBuilder e p i i) where
  map f (GraphBuilder g) = GraphBuilder (g >>> \n -> n { result = f n.result })

instance applyGraphBuilder :: IsEvent e => Apply (GraphBuilder e p i i) where
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
  IsEvent e =>
  Applicative (GraphBuilder e p i i) where
  pure result = GraphBuilder \_ -> { event: empty, result }

instance bindGraphBuilder :: IsEvent e => Bind (GraphBuilder e p i i) where
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

instance monadGraphBuilder :: IsEvent e => Monad (GraphBuilder e p i i)

instance ixFunctorGraphBuilder :: IxFunctor (GraphBuilder e p) where
  imap f (GraphBuilder g) = GraphBuilder (g >>> \n -> n { result = f n.result })

instance ixApplyGraphBuilder :: IsEvent e => IxApply (GraphBuilder e p) where
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
  IsEvent e =>
  IxApplicative (GraphBuilder e p) where
  ipure result = GraphBuilder \_ -> { event: empty, result }

instance ixBindGraphBuilder :: IsEvent e => IxBind (GraphBuilder e p) where
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

instance ixMonadGraphBuilder :: IsEvent e => IxMonad (GraphBuilder e p)

--
data Node

foreign import data Speaker :: Node

foreign import data Gain :: Node

foreign import data SinOsc :: Node

data GraphUnit :: Symbol -> Node -> Type
data GraphUnit id node = GraphUnit

class HasInput :: Node -> Constraint
class HasInput node

instance hasInputSpeaker :: HasInput Speaker
instance hasInputGain :: HasInput Gain

class HasOutput :: Node -> Constraint
class HasOutput node

instance hasOutputSpeaker :: HasOutput Speaker
instance hasOutputGain :: HasOutput Gain
instance hasOutputSinOsc :: HasOutput SinOsc

class HasSound :: Node -> Boolean -> Constraint
class HasSound node tOrF | node -> tOrF

instance hasSoundSinOsc :: HasSound SinOsc True
instance hasSoundGain :: HasSound Gain False
instance hasSoundSpeaker :: HasSound Speaker False

--
class InsertTerminal :: Type -> Symbol -> Boolean -> Type -> Constraint
class InsertTerminal i id tOrF o | i id tOrF -> o

instance insertTerminalDefault ::
  ( Row.Cons id tOrf t t'
  ) =>
  InsertTerminal (c \/ t \/ s) id tOrF (c \/ t' \/ s)

--
createSpeaker
  :: forall e p i o id
   . IsEvent e
  => InsertTerminal i id True o
  => Proxy id
  -> GraphBuilder e p i o (GraphUnit id Speaker)
createSpeaker _ = GraphBuilder go
  where
  go (C.AudioInterpret { ids, makeSpeaker }) =
    { event: sample_ ids (pure unit) <#> makeSpeaker <<< { id: _ }
    , result: GraphUnit
    }

createGain
  :: forall e p i o id initialGain
   . IsEvent e
  => Common.InitialGain initialGain
  => InsertTerminal i id False o
  => Proxy id
  -> initialGain
  -> e C.Gain
  -> GraphBuilder e p i o (GraphUnit id Gain)
createGain _ initialGain attributes = GraphBuilder go
  where
  { gain } = unwrap $ Common.toInitializeGain initialGain
  go (C.AudioInterpret { ids, makeGain, setGain }) =
    { event: keepLatest $ sample_ ids (pure unit) <#> \id ->
        let
          event0 = pure $
            makeGain { id, parent: nothing, gain }
          eventN = attributes <#> unwrap >>> match
            { gain: setGain <<< { id, gain: _ }
            }
        in
          event0 <|> eventN
    , result: GraphUnit
    }

createSinOsc
  :: forall e p i o id initialSinOsc
   . IsEvent e
  => Common.InitialSinOsc initialSinOsc
  => InsertTerminal i id False o
  => Proxy id
  -> initialSinOsc
  -> e C.SinOsc
  -> GraphBuilder e p i o (GraphUnit id Gain)
createSinOsc _ initialSinOsc attributes = GraphBuilder go
  where
  { frequency } = unwrap $ Common.toInitializeSinOsc initialSinOsc
  go (C.AudioInterpret { ids, makeSinOsc, setFrequency, setOnOff }) =
    { event: keepLatest $ sample_ ids (pure unit) <#> \id ->
        let
          event0 = pure $
            makeSinOsc { id, parent: nothing, frequency }
          eventN = attributes <#> unwrap >>> match
            { frequency: setFrequency <<< { id, frequency: _ }
            , onOff: setOnOff <<< { id, onOff: _ }
            }
        in
          event0 <|> eventN
    , result: GraphUnit
    }
