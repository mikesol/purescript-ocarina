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
import Data.Variant.Maybe (nothing)
import FRP.Event (Event, keepLatest)
import FRP.Event.Class (bang)
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

-- | Creates a `SinOsc` node.
-- |
-- | ```purescript
-- | sinOsc <- Create.sinOsc (Proxy :: _ "sinOsc") 440.0 bangOn
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
          event0 = bang $
            makeSinOsc { id, parent: nothing, frequency, scope: "imperative" }
          eventN = keepLatest
            ( attributes <#> unwrap >>> match
                { frequency: Common.resolveAU di (setFrequency <<< { id, frequency: _ })
                , onOff: bang <<< setOnOff <<< { id, onOff: _ }
                }
            )
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
    di@
      ( Core.AudioInterpret
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
          event0 = bang $ makePlayBuf
            { id
            , parent: nothing
            , buffer
            , playbackRate
            , bufferOffset
            , duration
            , scope: "imperative"
            }
          eventN = keepLatest
            ( attributes <#> unwrap >>> match
                { buffer: bang <<< setBuffer <<< { id, buffer: _ }
                , playbackRate: Common.resolveAU di (setPlaybackRate <<< { id, playbackRate: _ })
                , bufferOffset: bang <<< setBufferOffset <<< { id, bufferOffset: _ }
                , duration: bang <<< setDuration <<< { id, duration: _ }
                , onOff: bang <<< setOnOff <<< { id, onOff: _ }
                }
            )
        in
          event0 <|> eventN
    , result: T.GraphUnit
    }
