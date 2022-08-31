-- | Definitions for creating nodes in the graph builder.
-- |
-- | It's recommended to import this module as qualified, like so:
-- | ```purescript
-- | import Ocarina.Imperative.Create as Create
-- | ```
module Ocarina.Imperative.Create where

import Prelude

import Bolson.EffectFn.Core (Scope(..))
import Control.Alternative ((<|>))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Variant (match)
import FRP.Event.EffectFn (Event, keepLatest)
import Ocarina.Common as Common
import Ocarina.Core as Core
import Ocarina.Imperative.Monad (GraphBuilder(..))
import Ocarina.Imperative.Types (type (\/))
import Ocarina.Imperative.Types as T
import Prim.Boolean (True, False)
import Prim.Row as Row
import Prim.TypeError (class Fail, Beside, Text)
import Row.Extra as RowExtra
import Type.Prelude (class IsSymbol, Proxy(..), reflectSymbol)

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
  :: forall p i o id
   . IsSymbol id
  => CreateNode i id True o
  => Proxy id
  -> GraphBuilder p i o (T.GraphUnit id T.Speaker)
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
  :: forall l p i o id initialGain
   . IsSymbol id
  => Common.InitialGain initialGain
  => CreateNode i id False o
  => Proxy id
  -> initialGain
  -> Event (Core.Gain l p)
  -> GraphBuilder p i o (T.GraphUnit id T.Gain)
gain _ initialGain attributes = GraphBuilder go
  where
  initializeGain = unwrap $ Common.toInitializeGain initialGain
  go di@(Core.AudioInterpret { makeGain, setGain }) =
    { event:
        let
          id = reflectSymbol (Proxy :: _ id)
          event0 = pure $
            makeGain
              { id
              , parent: Nothing
              , gain: initializeGain.gain
              , scope: Just "imperative"
              }
          eventN = keepLatest (attributes <#> unwrap >>> match
            { gain: Common.resolveAU di (setGain <<< { id, gain: _ })
            })
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
  :: forall l p i o id initialSinOsc
   . IsSymbol id
  => Common.InitialSinOsc initialSinOsc
  => CreateNode i id False o
  => Proxy id
  -> initialSinOsc
  -> Event (Core.SinOsc l p)
  -> GraphBuilder p i o (T.GraphUnit id T.SinOsc)
sinOsc _ initialSinOsc attributes = GraphBuilder go
  where
  { frequency } = unwrap $ Common.toInitializeSinOsc initialSinOsc
  go di@(Core.AudioInterpret { makeSinOsc, setFrequency, setOnOff }) =
    { event:
        let
          id = reflectSymbol (Proxy :: _ id)
          event0 = pure $
            makeSinOsc { id, parent: Nothing, frequency, scope: Just "imperative" }
          eventN = keepLatest (attributes <#> unwrap >>> match
            { frequency: Common.resolveAU di (setFrequency <<< { id, frequency: _ })
            , onOff: pure <<< setOnOff <<< { id, onOff: _ }
            })
        in
          event0 <|> eventN
    , result: T.GraphUnit
    }

-- | Creates a `PlayBuf` node.
-- |
-- | ```purescript
-- | playBuf <- Create.playBuf (Proxy :: _ "playBuf") audioBuffer bangOn
-- | ```
playBuf
  :: forall l p i o id initialPlayBuf
   . IsSymbol id
  => Common.InitialPlayBuf initialPlayBuf
  => CreateNode i id False o
  => Proxy id
  -> initialPlayBuf
  -> Event (Core.PlayBuf l p)
  -> GraphBuilder p i o (T.GraphUnit id T.PlayBuf)
playBuf _ initialPlayBuf attributes = GraphBuilder go
  where
  { buffer, playbackRate, bufferOffset, duration } = unwrap $
    Common.toInitializePlayBuf initialPlayBuf
  go
    di@( Core.AudioInterpret
        { makePlayBuf
        , setBuffer
        , setOnOff
        , setDuration
        , setPlaybackRate
        , setBufferOffset
        }
    ) =
    { event:
        let
          id = reflectSymbol (Proxy :: _ id)
          event0 = pure $ makePlayBuf
            { id
            , parent: Nothing
            , buffer
            , playbackRate
            , bufferOffset
            , duration
            , scope: Just "imperative"
            }
          eventN = keepLatest (attributes <#> unwrap >>> match
            { buffer: pure <<< setBuffer <<< { id, buffer: _ }
            , playbackRate: Common.resolveAU di (setPlaybackRate <<< { id, playbackRate: _ })
            , bufferOffset: pure <<< setBufferOffset <<< { id, bufferOffset: _ }
            , duration: pure <<< setDuration <<< { id, duration: _ }
            , onOff: pure <<< setOnOff <<< { id, onOff: _ }
            })
        in
          event0 <|> eventN
    , result: T.GraphUnit
    }
