module WAGS.Declarative.Create.SinOsc where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Variant (match)
import FRP.Event (Event, makeEvent, subscribe, keepLatest)
import FRP.Event.Class (bang)
import WAGS.Common.Parameters.SinOsc as Parameters
import WAGS.Core as Core
import WAGS.Declarative.Create.Gain (tmpResolveAU)

__sinOsc
  :: forall i outputChannels lock payload
   . Parameters.InitialSinOsc i
  => i
  -> Event (Core.SinOsc lock payload)
  -> Core.Node outputChannels lock payload
__sinOsc i' atts = Core.Node go
  where
  Core.InitializeSinOsc i = Parameters.toInitializeSinOsc i'
  go
    parent
    di@(Core.AudioInterpret { ids, deleteFromCache, makeSinOsc, setFrequency, setOnOff }) =
    makeEvent \k -> do
      me <- ids
      parent.raiseId me
      map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
        bang
          ( makeSinOsc
              { id: me
              , parent: parent.parent
              , scope: parent.scope
              , frequency: i.frequency
              }
          )
          <|>
            ( keepLatest $ map
                ( \(Core.SinOsc e) -> match
                    { frequency: tmpResolveAU parent.scope di (setFrequency <<< { id: me, frequency: _ })
                    , onOff: \onOff -> bang $ setOnOff { id: me, onOff }
                    }
                    e
                )
                atts
            )

sinOsc
  :: forall i outputChannels lock payload
   . Parameters.InitialSinOsc i
  => i
  -> Event (Core.SinOsc lock payload)
  -> Core.Node outputChannels lock payload
sinOsc = __sinOsc

sinOsc_
  :: forall i outputChannels lock payload
   . Parameters.InitialSinOsc i
  => i
  -> Core.Node outputChannels lock payload
sinOsc_ a = sinOsc a empty
