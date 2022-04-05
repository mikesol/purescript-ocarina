module WAGS.Imperative where

import Prelude

import Control.Monad.Indexed (class IxApplicative, class IxApply, class IxBind, class IxMonad)
import Control.Monad.Indexed.Qualified as Ix
import Data.Functor.Indexed (class IxFunctor)
import FRP.Behavior (sample_)
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
-- data GraphBuilder :: Type -> Type -> Type -> Type
data GraphBuilder e p i o a = GraphBuilder (AudioInterpret e p -> { event :: e p, result :: a })

unsafeGraphBuilder :: forall i o e p a. GraphBuilder e p i o a
unsafeGraphBuilder = unsafeCoerce unit

type InitialGraphBuilderIndex :: Type
type InitialGraphBuilderIndex = "_" /\ (() :: Row Type) /\ (() :: Row Boolean) /\ False

evalGraphBuilder :: forall o e p a. GraphBuilder e p InitialGraphBuilderIndex o a -> Proxy o
evalGraphBuilder _ = Proxy

instance functorGraphBuilder :: Functor (GraphBuilder e p i i) where
  map _ _ = unsafeGraphBuilder

instance applyGraphBuilder :: Apply (GraphBuilder e p i i) where
  apply _ _ = unsafeGraphBuilder

instance applicativeGraphBuilder :: Applicative (GraphBuilder e p i i) where
  pure _ = unsafeGraphBuilder

instance bindGraphBuilder :: Bind (GraphBuilder e p i i) where
  bind _ _ = unsafeGraphBuilder

instance monadGraphBuilder :: Monad (GraphBuilder e p i i)

instance ixFunctorGraphBuilder :: IxFunctor (GraphBuilder e p) where
  imap _ _ = unsafeGraphBuilder

instance ixApplyGraphBuilder :: IxApply (GraphBuilder e p) where
  iapply _ _ = unsafeGraphBuilder

instance ixApplicativeGraphBuilder :: IxApplicative (GraphBuilder e p) where
  ipure _ = unsafeGraphBuilder

instance ixBindGraphBuilder :: IxBind (GraphBuilder e p) where
  ibind _ _ = unsafeGraphBuilder

instance ixMonadGraphBuilder :: IxMonad (GraphBuilder e p)

--
data Speaker = Speaker

data Gain = Gain

data SinOsc = SinOsc

-- data GraphUnit :: Symbol -> Type -> Type
data GraphUnit id node event payload = GraphUnit (event payload)

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
  create :: forall (event :: Type -> Type) (payload :: Type). node -> GraphBuilder event payload i o (GraphUnit id node event payload)

instance createSpeaker ::
  ( Row.Cons n True t t'
  , Symbol.Cons "_" n n'
  ) =>
  Create (n /\ c /\ t /\ s) n Speaker (n' /\ c /\ t' /\ s) where
  create _ = unsafeGraphBuilder

else instance createNode ::
  ( Row.Cons n False t t'
  , Symbol.Cons "_" n n'
  ) =>
  Create (n /\ c /\ t /\ s) n node (n' /\ c /\ t' /\ s) where
  create _ = unsafeGraphBuilder

speaker (AudioInterpret { ids, makeSpeaker }) =
  Ix.pure $ GraphUnit $
    ( (sample_ ids (pure unit)) <#> \me ->
         makeSpeaker { id: me }
    )

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
    :: forall (event :: Type -> Type) (payload :: Type)
     . { from :: GraphUnit fId fNode event payload
       , into :: GraphUnit iId iNode event payload
       }
    -> GraphBuilder event payload i o Unit

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

-- evalGraphBuilder :: forall i o a. GraphBuilder i o a -> AudioInterpret event payload -> event payload

--
-- 1.