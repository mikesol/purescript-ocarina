-- | Definitions for creating nodes in the graph builder.
-- |
-- | It's recommended to import this module as qualified, like so:
-- | ```purescript
-- | import WAGS.Imperative.Create as Create
-- | ```
module WAGS.Imperative.Create where

import Prelude

import Control.Alternative ((<|>))
import Data.Newtype (unwrap)
import Data.Variant (match)
import Data.Variant.Maybe (just, nothing)
import FRP.Event.Class (class IsEvent)
import FRP.Event.Phantom (Proof0)
import Prim.Boolean (True, False)
import Prim.Row as Row
import Prim.TypeError (class Fail, Beside, Text)
import Row.Extra as RowExtra
import Type.Prelude (class IsSymbol, Proxy(..), reflectSymbol)
import WAGS.Common as Common
import WAGS.Core as Core
import WAGS.Imperative.Monad (GraphBuilder(..))
import WAGS.Imperative.Types (type (\/))
import WAGS.Imperative.Types as T

-- | Fails if `tOrF` resolves to `False`.
class IdNotInUse :: Symbol -> Boolean -> Constraint
class IdNotInUse id tOrF

instance idNotInUseFalse :: IdNotInUse id True
instance idNotInUseTrue ::
  ( Fail
      ( Beside
          ( Beside
              ( Text "CreateNode: A node with the id '"
              )
              ( Text id
              )
          )
          ( Text "' has already been created."
          )
      )
  ) =>
  IdNotInUse id False

-- | A constraint that modifies the graph builder index such that the
-- | terminality of freshly-created nodes are tracked.
class CreateNode :: Type -> Symbol -> Boolean -> Type -> Constraint
class CreateNode i id isTerminal o | i id isTerminal -> o

instance createNodeDefault ::
  ( RowExtra.Lacks id t idNotInUse
  , IdNotInUse id idNotInUse
  , Row.Cons id isTerminal t t'
  ) =>
  CreateNode (c \/ t \/ s) id isTerminal (c \/ t' \/ s)

-- | Creates a `Speaker` node.
-- |
-- | Note that since speakers are terminal nodes, they serve as the anchor to
-- | which all other nodes must connect or reach into.
-- |
-- | ```purescript
-- | speaker <- Create.speaker (Proxy :: Proxy "speaker")
-- | ```
speaker
  :: forall e p i o id
   . IsEvent (e Proof0)
  => IsSymbol id
  => CreateNode i id True o
  => Proxy id
  -> GraphBuilder e Proof0 p i o (T.GraphUnit id T.Speaker)
speaker _ = GraphBuilder go
  where
  go (Core.AudioInterpret { makeSpeaker }) =
    { event: pure $ makeSpeaker { id: reflectSymbol (Proxy :: _ id) }
    , result: T.GraphUnit
    }

-- | Creates a `Gain` node.
-- |
-- | ```purescript
-- | gain <- Create.gain (Proxy :: _ "gain") 1.0 empty
-- | ```
gain
  :: forall e x p i o id initialGain
   . IsEvent (e x)
  => IsSymbol id
  => Common.InitialGain initialGain
  => CreateNode i id False o
  => Proxy id
  -> initialGain
  -> e x Core.Gain
  -> GraphBuilder e x p i o (T.GraphUnit id T.Gain)
gain _ initialGain attributes = GraphBuilder go
  where
  initializeGain = unwrap $ Common.toInitializeGain initialGain
  go (Core.AudioInterpret { scope, makeGain, setGain }) =
    { event:
        let
          id = reflectSymbol (Proxy :: _ id)
          event0 = pure $
            makeGain
              { id
              , parent: nothing
              , gain: initializeGain.gain
              , scope: just scope
              }
          eventN = attributes <#> unwrap >>> match
            { gain: setGain <<< { id, gain: _ }
            }
        in
          event0 <|> eventN
    , result: T.GraphUnit
    }

-- | Creates a `SinOsc` node.
-- |
-- | ```purescript
-- | sinOsc <- Create.sinOsc (Proxy :: _ "sinOsc") 440.0 pureOn
-- | ```
sinOsc
  :: forall e x p i o id initialSinOsc
   . IsEvent (e x)
  => IsSymbol id
  => Common.InitialSinOsc initialSinOsc
  => CreateNode i id False o
  => Proxy id
  -> initialSinOsc
  -> e x Core.SinOsc
  -> GraphBuilder e x p i o (T.GraphUnit id T.SinOsc)
sinOsc _ initialSinOsc attributes = GraphBuilder go
  where
  { frequency } = unwrap $ Common.toInitializeSinOsc initialSinOsc
  go (Core.AudioInterpret { scope, makeSinOsc, setFrequency, setOnOff }) =
    { event:
        let
          id = reflectSymbol (Proxy :: _ id)
          event0 = pure $
            makeSinOsc { id, parent: nothing, frequency, scope: just scope }
          eventN = attributes <#> unwrap >>> match
            { frequency: setFrequency <<< { id, frequency: _ }
            , onOff: setOnOff <<< { id, onOff: _ }
            }
        in
          event0 <|> eventN
    , result: T.GraphUnit
    }

-- | Creates a `PlayBuf` node.
-- |
-- | ```purescript
-- | playBuf <- Create.playBuf (Proxy :: _ "playBuf") audioBuffer pureOn
-- | ```
playBuf
  :: forall e x p i o id initialPlayBuf
   . IsEvent (e x)
  => IsSymbol id
  => Common.InitialPlayBuf initialPlayBuf
  => CreateNode i id False o
  => Proxy id
  -> initialPlayBuf
  -> e x Core.PlayBuf
  -> GraphBuilder e x p i o (T.GraphUnit id T.PlayBuf)
playBuf _ initialPlayBuf attributes = GraphBuilder go
  where
  { buffer, playbackRate, bufferOffset, duration } = unwrap $
    Common.toInitializePlayBuf initialPlayBuf
  go
    ( Core.AudioInterpret
        { scope
        , makePlayBuf
        , setBuffer
        , setOnOff
        , setPlaybackRate
        , setBufferOffset
        }
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
            , scope: just scope
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
