module WAGS.Imperative where

import Prelude

import Control.Alternative (empty, (<|>))
import Control.Monad.Indexed (class IxApplicative, class IxApply, class IxBind, class IxMonad)
import Data.Functor.Indexed (class IxFunctor)
import FRP.Behavior (sample_)
import FRP.Event.Class (class IsEvent)
import Prim.Boolean (False, True)
import Prim.Row as Row
import Prim.Symbol as Symbol
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import WAGS.Core (AudioInterpret(..))

--
data Tuple :: forall k l. k -> l -> Type
data Tuple a b

infixr 6 type Tuple as /\

--
newtype GraphBuilder :: (Type -> Type) -> Type -> Type -> Type -> Type -> Type
newtype GraphBuilder e p i o a = GraphBuilder (AudioInterpret e p -> { event :: e p, result :: a })

unsafeGraphBuilder :: forall i o e p a. GraphBuilder e p i o a
unsafeGraphBuilder = unsafeCoerce unit

type InitialGraphBuilderIndex :: Type
type InitialGraphBuilderIndex = "_" /\ (() :: Row Type) /\ (() :: Row Boolean) /\ False

evalGraphBuilder :: forall o e p a. GraphBuilder e p InitialGraphBuilderIndex o a -> Proxy o
evalGraphBuilder _ = Proxy

instance functorGraphBuilder :: Functor (GraphBuilder e p i i) where
  map f (GraphBuilder g) = GraphBuilder (g >>> \n -> n { result = f n.result })

instance applyGraphBuilder :: IsEvent e => Apply (GraphBuilder e p i i) where
  apply (GraphBuilder f) (GraphBuilder g) = GraphBuilder h
    where
    h audioInterpret =
      let f' = f audioInterpret
          g' = g audioInterpret
      in { event: f'.event <|> g'.event
         , result: f'.result g'.result
         }

instance applicativeGraphBuilder :: IsEvent e => Applicative (GraphBuilder e p i i) where
  pure result = GraphBuilder \_ -> { event: empty, result }

instance bindGraphBuilder :: IsEvent e => Bind (GraphBuilder e p i i) where
  bind (GraphBuilder f) mkG = GraphBuilder h
    where
    h audioInterpret =
      let f' = f audioInterpret
          (GraphBuilder g) = mkG f'.result
          g' = g audioInterpret
      in { event: f'.event <|> g'.event
         , result: g'.result
         }

instance monadGraphBuilder :: IsEvent e => Monad (GraphBuilder e p i i)

instance ixFunctorGraphBuilder :: IxFunctor (GraphBuilder e p) where
  imap f (GraphBuilder g) = GraphBuilder (g >>> \n -> n { result = f n.result })

instance ixApplyGraphBuilder :: IsEvent e => IxApply (GraphBuilder e p) where
  iapply (GraphBuilder f) (GraphBuilder g) = GraphBuilder h
    where
    h audioInterpret =
      let f' = f audioInterpret
          g' = g audioInterpret
      in { event: f'.event <|> g'.event
         , result: f'.result g'.result
         }

instance ixApplicativeGraphBuilder :: IsEvent e => IxApplicative (GraphBuilder e p) where
  ipure result = GraphBuilder \_ -> { event: empty, result }

instance ixBindGraphBuilder :: IsEvent e => IxBind (GraphBuilder e p) where
  ibind (GraphBuilder f) mkG = GraphBuilder h
    where
    h audioInterpret =
      let f' = f audioInterpret
          (GraphBuilder g) = mkG f'.result
          g' = g audioInterpret
      in { event: f'.event <|> g'.event
         , result: g'.result
         }

instance ixMonadGraphBuilder :: IsEvent e => IxMonad (GraphBuilder e p)

--
data Speaker = Speaker

data Gain = Gain

data SinOsc = SinOsc

data GraphUnit :: Symbol -> Type -> Type
data GraphUnit id node = GraphUnit

class IsNode :: Type -> Constraint
class IsNode node

instance isNodeSpeaker :: IsNode Speaker
instance isNodeGain :: IsNode Gain
instance isNodeSinOsc :: IsNode SinOsc

class HasInput :: Type -> Constraint
class HasInput node

instance hasInputSpeaker :: HasInput Speaker
instance hasInputGain :: HasInput Gain

class HasOutput :: Type -> Constraint
class HasOutput node

instance hasOutputSpeaker :: HasOutput Speaker
instance hasOutputGain :: HasOutput Gain
instance hasOutputSinOsc :: HasOutput SinOsc

class HasSound :: Type -> Boolean -> Constraint
class HasSound node yesOrNo | node -> yesOrNo

instance hasSoundSinOsc :: HasSound SinOsc True
else instance hasSoundDefault :: HasSound node False

--
class Create :: Type -> Symbol -> Type -> Type -> Constraint
class Create i id node o | i node -> id o where
  create :: forall e p. IsEvent e => node -> GraphBuilder e p i o (GraphUnit id node)

instance createSpeaker ::
  ( Row.Cons n True t t'
  , Symbol.Cons "_" n n'
  ) =>
  Create (n /\ c /\ t /\ s) n Speaker (n' /\ c /\ t' /\ s) where
  create _ = GraphBuilder \(AudioInterpret { ids, makeSpeaker }) ->
    { event: sample_ ids (pure unit) <#> \id ->
        makeSpeaker { id }
    , result: GraphUnit
    }

else instance createNode ::
  ( Row.Cons n False t t'
  , Symbol.Cons "_" n n'
  ) =>
  Create (n /\ c /\ t /\ s) n node (n' /\ c /\ t' /\ s) where
  create _ = unsafeGraphBuilder

class IntoIsTerminal :: Boolean -> Constraint
class IntoIsTerminal isTerminal

instance intoIsTerminalTrue :: IntoIsTerminal True

class MakesSound :: Type -> Boolean -> Boolean -> Constraint
class MakesSound node n f | node n -> f

instance makesSoundAlready :: MakesSound node True True
else instance makesSoundFuture :: (HasSound node yesOrNo) => MakesSound node tOrF yesOrNo

class Connect :: Type -> Symbol -> Type -> Symbol -> Type -> Type -> Constraint
class Connect i fId fNode iId iNode o | i fId fNode iId iNode -> o where
  connect
    :: forall e p
     . { from :: GraphUnit fId fNode
       , into :: GraphUnit iId iNode
       }
    -> GraphBuilder e p i o Unit

instance connectDefault ::
  ( HasOutput fNode
  , HasInput iNode
  , Row.Cons iId iIsTerminal t_ t
  , IntoIsTerminal iIsTerminal
  , Row.Cons fId tOrF t' t
  , Row.Cons fId True t' t''
  , Symbol.Cons "." iId iId'
  , Symbol.Append fId iId' cId
  , Row.Lacks cId c
  , Row.Cons cId Unit c c'
  , MakesSound fNode s s'
  ) =>
  Connect (n /\ c /\ t /\ s) fId fNode iId iNode (n /\ c' /\ t'' /\ s') where
  connect _ = unsafeGraphBuilder
