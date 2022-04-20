module WAGS.Imperative.Connect where

import Prelude

import FRP.Event.Class (class IsEvent, bang)
import Prim.Boolean (True, False)
import Prim.Row as Row
import Prim.Symbol as Symbol
import Prim.TypeError (class Fail, Beside, Text)
import Row.Extra as RowExtra
import Type.Prelude (class IsSymbol, reflectSymbol, Proxy(..))
import WAGS.Core as Core
import WAGS.Imperative.Monad (GraphBuilder(..))
import WAGS.Imperative.Types (type (\/))
import WAGS.Imperative.Types as T

-- | Fails if `tOrF` resolves to `False`.
class IsTerminalNode :: Symbol -> Boolean -> Constraint
class IsTerminalNode id tOrF

instance isTerminalTrue :: IsTerminalNode id True
instance isTerminalFalse ::
  ( Fail
      ( Beside
          ( Beside
              ( Text "ConnectNodes: A node with the id '"
              )
              ( Text id
              )
          )
          ( Text "' is not a terminal node."
          )
      )
  ) =>
  IsTerminalNode id False

-- | Fails if `tOrF` resolves to `False`.
class IsNewConnection :: Symbol -> Symbol -> Boolean -> Constraint
class IsNewConnection fId iId tOrF

instance alreadyConnectedTrue :: IsNewConnection fId iId True
instance alreadyConnectedFalse ::
  ( Fail
      ( Beside
          ( Beside
              ( Text "ConnectNodes: A node with the id '"
              )
              ( Text fId
              )
          )
          ( Beside
              ( Text "' is already connected to a node with the id '"
              )
              ( Beside
                  ( Text iId
                  )
                  ( Text "'."
                  )
              )
          )
      )
  ) =>
  IsNewConnection fId iId False

-- | An `or` operation between a node and the current state.
class MakesSound :: T.Node -> Boolean -> Boolean -> Constraint
class MakesSound node n f | node n -> f

instance makesSoundAlready :: MakesSound node True True
else instance makesSoundNotYet ::
  ( T.HasSound node hasSound
  ) =>
  MakesSound node tOrF hasSound

class ConnectNodes
  :: Type
  -> (Type -> Type)
  -> Symbol
  -> T.Node
  -> Symbol
  -> T.Node
  -> Type
  -> Constraint
class ConnectNodes i e fId fNode iId iNode o | i fId fNode iId iNode -> o where
  -- | Connects a `from` node to an `into` node.
  -- |
  -- | ```purescript
  -- | connect { from: sinOsc, into: speaker }
  -- | ```
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
  , Row.Cons iId iIsTerminalNode t_ t
  , IsTerminalNode iId iIsTerminalNode
  , Row.Cons fId tOrF t' t
  , Row.Cons fId True t' t''
  , Symbol.Append "=>" iId iId'
  , Symbol.Append fId iId' cId
  , RowExtra.Lacks cId c isNewConnection
  , IsNewConnection fId iId isNewConnection
  , Row.Cons cId Unit c c'
  , MakesSound fNode s s'
  ) =>
  ConnectNodes (c \/ t \/ s) e fId fNode iId iNode (c' \/ t'' \/ s') where
  connect _ = GraphBuilder go
    where
    go (Core.AudioInterpret { connectXToY }) =
      { event: bang $ connectXToY
          { from: reflectSymbol (Proxy :: _ fId)
          , to: reflectSymbol (Proxy :: _ iId)
          }
      , result: unit
      }
