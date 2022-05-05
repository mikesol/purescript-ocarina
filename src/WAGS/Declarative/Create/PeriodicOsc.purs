module WAGS.Declarative.Create.PeriodicOsc where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Variant (match)
import FRP.Event (Event, bang, makeEvent, subscribe, keepLatest)
import WAGS.Common.Parameters.PeriodicOsc as Parameters
import WAGS.Core as Core
import WAGS.Declarative.Create.Gain (tmpResolveAU)

__periodicOsc
  :: forall i outputChannels lock payload
   . Parameters.InitialPeriodicOsc i
  => i
  -> Event (Core.PeriodicOsc lock payload)
  -> Core.Node outputChannels lock payload
__periodicOsc i' atts = Core.Node go
  where
  Core.InitializePeriodicOsc i = Parameters.toInitializePeriodicOsc i'
  go
    parent
    di@
      ( Core.AudioInterpret
          { ids, deleteFromCache, makePeriodicOsc, setFrequency, setOnOff, setPeriodicOsc }
      ) =
    makeEvent \k -> do
      me <- ids
      parent.raiseId me
      map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
        bang
          ( makePeriodicOsc
              { id: me
              , parent: parent.parent
              , scope: parent.scope
              , frequency: i.frequency
              , spec: i.spec
              }
          )
          <|>
            ( keepLatest $ map
                ( \(Core.PeriodicOsc e) -> match
                    { frequency: tmpResolveAU parent.scope di (setFrequency <<< { id: me, frequency: _ })
                    , onOff: \onOff -> bang $ setOnOff { id: me, onOff }
                    , spec: \spec -> bang $ setPeriodicOsc { id: me, spec }
                    }
                    e
                )
                atts
            )

periodicOsc
  :: forall i outputChannels lock payload
   . Parameters.InitialPeriodicOsc i
  => i
  -> Event (Core.PeriodicOsc lock payload)
  -> Core.Node outputChannels lock payload
periodicOsc = __periodicOsc

periodicOsc_
  :: forall i outputChannels lock payload
   . Parameters.InitialPeriodicOsc i
  => i
  -> Core.Node outputChannels lock payload
periodicOsc_ i = periodicOsc i empty
