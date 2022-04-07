module WAGS.Imperative.Connect where

import Prelude

import FRP.Event.Class (class IsEvent)
import Prim.Boolean (True)
import Prim.Row as Row
import Prim.Symbol as Symbol
import Type.Prelude (class IsSymbol, reflectSymbol, Proxy(..))
import WAGS.Core as Core
import WAGS.Imperative.Monad (GraphBuilder(..))
import WAGS.Imperative.Types (type (\/))
import WAGS.Imperative.Types as T

class IntoIsTerminal :: Boolean -> Constraint
class IntoIsTerminal isTerminal

instance intoIsTerminalTrue :: IntoIsTerminal True

class MakesSound :: T.Node -> Boolean -> Boolean -> Constraint
class MakesSound node n f | node n -> f

instance makesSoundAlready :: MakesSound node True True
else instance makesSoundNotYet ::
  ( T.HasSound node hasSound
  ) =>
  MakesSound node tOrF hasSound

class Connect
  :: Type
  -> (Type -> Type)
  -> Symbol
  -> T.Node
  -> Symbol
  -> T.Node
  -> Type
  -> Constraint
class Connect i e fId fNode iId iNode o | i fId fNode iId iNode -> o where
  connect
    :: forall p
     . { from :: T.GraphUnit fId fNode
       , into :: T.GraphUnit iId iNode
       }
    -> GraphBuilder e p i o Unit

instance connectDefault ::
  ( IsEvent e
  , IsSymbol fId
  , IsSymbol iId
  , T.HasOutput fNode
  , T.HasInput iNode
  , Row.Cons iId iIsTerminal t_ t
  , IntoIsTerminal iIsTerminal
  , Row.Cons fId tOrF t' t
  , Row.Cons fId True t' t''
  , Symbol.Append "=>" iId iId'
  , Symbol.Append fId iId' cId
  , Row.Lacks cId c
  , Row.Cons cId Unit c c'
  , MakesSound fNode s s'
  ) =>
  Connect (c \/ t \/ s) e fId fNode iId iNode (c' \/ t'' \/ s') where
  connect _ = GraphBuilder go
    where
    go (Core.AudioInterpret { connectXToY }) =
      { event: pure $ connectXToY
          { from: reflectSymbol (Proxy :: _ fId)
          , to: reflectSymbol (Proxy :: _ iId)
          }
      , result: unit
      }
