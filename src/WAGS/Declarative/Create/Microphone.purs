module WAGS.Declarative.Create.Microphone where

import Prelude

import FRP.Event (bang, makeEvent, subscribe)
import WAGS.Common.Parameters.Microphone as Parameters
import WAGS.Core as Core

__microphone
  :: forall i outputChannels lock payload
   . Parameters.InitialMicrophone i
  => i
  -> Core.Node outputChannels lock payload
__microphone i' = Core.Node go
  where
  Core.InitializeMicrophone i = Parameters.toInitializeMicrophone i'
  go parent (Core.AudioInterpret { ids, deleteFromCache, makeMicrophone }) =
    makeEvent \k -> do
      me <- ids
      parent.raiseId me
      map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
        bang
          ( makeMicrophone
              { id: me
              , parent: parent.parent
              , scope: parent.scope
              , microphone: i.microphone
              }
          )

microphone
  :: forall i outputChannels lock payload
   . Parameters.InitialMicrophone i
  => i
  -> Core.Node outputChannels lock payload
microphone = __microphone
