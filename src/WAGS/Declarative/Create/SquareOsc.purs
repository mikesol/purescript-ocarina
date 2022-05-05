module WAGS.Declarative.Create.SquareOsc where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Variant (match)
import FRP.Event (Event, makeEvent, subscribe, keepLatest)
import FRP.Event.Class (bang)
import WAGS.Common.Parameters.SquareOsc as Parameters
import WAGS.Core as Core
import WAGS.Declarative.Create.Gain (tmpResolveAU)

__squareOsc
  :: forall i outputChannels lock payload
   . Parameters.InitialSquareOsc i
  => i
  -> Event (Core.SquareOsc lock payload)
  -> Core.Node outputChannels lock payload
__squareOsc i' atts = Core.Node go
  where
  Core.InitializeSquareOsc i = Parameters.toInitializeSquareOsc i'
  go
    parent
    di@(Core.AudioInterpret { ids, deleteFromCache, makeSquareOsc, setFrequency, setOnOff }) =
    makeEvent \k -> do
      me <- ids
      parent.raiseId me
      map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
        bang
          ( makeSquareOsc
              { id: me
              , parent: parent.parent
              , scope: parent.scope
              , frequency: i.frequency
              }
          )
          <|>
            ( keepLatest $ map
                ( \(Core.SquareOsc e) -> match
                    { frequency: tmpResolveAU parent.scope di (setFrequency <<< { id: me, frequency: _ })
                    , onOff: \onOff -> bang $ setOnOff { id: me, onOff }
                    }
                    e
                )
                atts
            )

squareOsc
  :: forall i outputChannels lock payload
   . Parameters.InitialSquareOsc i
  => i
  -> Event (Core.SquareOsc lock payload)
  -> Core.Node outputChannels lock payload
squareOsc = __squareOsc

squareOsc_
  :: forall i outputChannels lock payload
   . Parameters.InitialSquareOsc i
  => i
  -> Core.Node outputChannels lock payload
squareOsc_ a = squareOsc a empty
