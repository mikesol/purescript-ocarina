module WAGS.Declarative.Create.SawtoothOsc where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Variant (match)
import FRP.Event (Event, makeEvent, subscribe, keepLatest)
import FRP.Event.Class (bang)
import WAGS.Common.Parameters.SawtoothOsc as Parameters
import WAGS.Core as Core
import WAGS.Declarative.Create.Gain (tmpResolveAU)

__sawtoothOsc
  :: forall i outputChannels lock payload
   . Parameters.InitialSawtoothOsc i
  => i
  -> Event (Core.SawtoothOsc lock payload)
  -> Core.Node outputChannels lock payload
__sawtoothOsc i' atts = Core.Node go
  where
  Core.InitializeSawtoothOsc i = Parameters.toInitializeSawtoothOsc i'
  go
    parent
    di@(Core.AudioInterpret { ids, deleteFromCache, makeSawtoothOsc, setFrequency, setOnOff }) =
    makeEvent \k -> do
      me <- ids
      parent.raiseId me
      map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
        bang
          ( makeSawtoothOsc
              { id: me
              , parent: parent.parent
              , scope: parent.scope
              , frequency: i.frequency
              }
          )
          <|>
            ( keepLatest $ map
                ( \(Core.SawtoothOsc e) -> match
                    { frequency: tmpResolveAU parent.scope di (setFrequency <<< { id: me, frequency: _ })
                    , onOff: \onOff -> bang $ setOnOff { id: me, onOff }
                    }
                    e
                )
                atts
            )

sawtoothOsc
  :: forall i outputChannels lock payload
   . Parameters.InitialSawtoothOsc i
  => i
  -> Event (Core.SawtoothOsc lock payload)
  -> Core.Node outputChannels lock payload
sawtoothOsc = __sawtoothOsc

sawtoothOsc_
  :: forall i outputChannels lock payload
   . Parameters.InitialSawtoothOsc i
  => i
  -> Core.Node outputChannels lock payload
sawtoothOsc_ a = sawtoothOsc a empty
