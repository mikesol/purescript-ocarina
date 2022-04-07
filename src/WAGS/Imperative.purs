module WAGS.Imperative where

import Prelude

import Control.Alternative ((<|>))
import Data.Newtype (unwrap)
import Data.Variant (match)
import Data.Variant.Maybe (nothing)
import FRP.Event.Class (class IsEvent)
import Prim.Boolean (False, True)
import Prim.Row as Row
import Prim.Symbol as Symbol
import Type.Prelude (class IsSymbol, reflectSymbol)
import Type.Proxy (Proxy(..))
import WAGS.Common as Common
import WAGS.Core as Core
import WAGS.Imperative.Monad (GraphBuilder(..))
import WAGS.Imperative.Types (type (\/))
import WAGS.Imperative.Types as T

--
class InsertTerminal :: Type -> Symbol -> Boolean -> Type -> Constraint
class InsertTerminal i id tOrF o | i id tOrF -> o

instance insertTerminalDefault ::
  ( Row.Lacks id t
  , Row.Cons id tOrF t t'
  ) =>
  InsertTerminal (c \/ t \/ s) id tOrF (c \/ t' \/ s)

--
createSpeaker
  :: forall e p i o id
   . IsEvent e
  => IsSymbol id
  => InsertTerminal i id True o
  => Proxy id
  -> GraphBuilder e p i o (T.GraphUnit id T.Speaker)
createSpeaker _ = GraphBuilder go
  where
  go (Core.AudioInterpret { makeSpeaker }) =
    { event: pure $ makeSpeaker { id: reflectSymbol (Proxy :: _ id) }
    , result: T.GraphUnit
    }

createGain
  :: forall e p i o id initialGain
   . IsEvent e
  => IsSymbol id
  => Common.InitialGain initialGain
  => InsertTerminal i id False o
  => Proxy id
  -> initialGain
  -> e Core.Gain
  -> GraphBuilder e p i o (T.GraphUnit id T.Gain)
createGain _ initialGain attributes = GraphBuilder go
  where
  { gain } = unwrap $ Common.toInitializeGain initialGain
  go (Core.AudioInterpret { makeGain, setGain }) =
    { event:
        let
          id = reflectSymbol (Proxy :: _ id)
          event0 = pure $
            makeGain { id, parent: nothing, gain }
          eventN = attributes <#> unwrap >>> match
            { gain: setGain <<< { id, gain: _ }
            }
        in
          event0 <|> eventN
    , result: T.GraphUnit
    }

createSinOsc
  :: forall e p i o id initialSinOsc
   . IsEvent e
  => IsSymbol id
  => Common.InitialSinOsc initialSinOsc
  => InsertTerminal i id False o
  => Proxy id
  -> initialSinOsc
  -> e Core.SinOsc
  -> GraphBuilder e p i o (T.GraphUnit id T.SinOsc)
createSinOsc _ initialSinOsc attributes = GraphBuilder go
  where
  { frequency } = unwrap $ Common.toInitializeSinOsc initialSinOsc
  go (Core.AudioInterpret { makeSinOsc, setFrequency, setOnOff }) =
    { event:
        let
          id = reflectSymbol (Proxy :: _ id)
          event0 = pure $
            makeSinOsc { id, parent: nothing, frequency }
          eventN = attributes <#> unwrap >>> match
            { frequency: setFrequency <<< { id, frequency: _ }
            , onOff: setOnOff <<< { id, onOff: _ }
            }
        in
          event0 <|> eventN
    , result: T.GraphUnit
    }

createPlayBuf
  :: forall e p i o id initialPlayBuf
   . IsEvent e
  => IsSymbol id
  => Common.InitialPlayBuf initialPlayBuf
  => InsertTerminal i id False o
  => Proxy id
  -> initialPlayBuf
  -> e Core.PlayBuf
  -> GraphBuilder e p i o (T.GraphUnit id T.PlayBuf)
createPlayBuf _ initialPlayBuf attributes = GraphBuilder go
  where
  { buffer, playbackRate, bufferOffset, duration } = unwrap $
    Common.toInitializePlayBuf initialPlayBuf
  go
    ( Core.AudioInterpret
        { makePlayBuf, setBuffer, setOnOff, setPlaybackRate, setBufferOffset }
    ) =
    { event:
        let
          id = reflectSymbol (Proxy :: _ id)
          event0 = pure $ makePlayBuf
            { id
            , parent: nothing
            , buffer
            , playbackRate
            , bufferOffset
            , duration
            }
          eventN = attributes <#> unwrap >>> match
            { buffer: setBuffer <<< { id, buffer: _ }
            , playbackRate: setPlaybackRate <<< { id, playbackRate: _ }
            , bufferOffset: setBufferOffset <<< { id, bufferOffset: _ }
            , onOff: setOnOff <<< { id, onOff: _ }
            }
        in
          event0 <|> eventN
    , result: T.GraphUnit
    }

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
