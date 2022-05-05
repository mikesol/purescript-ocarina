module WAGS.Declarative.Create.MediaElement where

import Prelude

import FRP.Event (bang, makeEvent, subscribe)
import WAGS.Core as Core

__mediaElement
  :: forall outputChannels lock payload
   . Core.InitializeMediaElement
  -> Core.Node outputChannels lock payload
__mediaElement (Core.InitializeMediaElement i) = Core.Node go
  where
  go parent (Core.AudioInterpret { ids, deleteFromCache, makeMediaElement }) =
    makeEvent \k -> do
      me <- ids
      parent.raiseId me
      map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
        bang
          ( makeMediaElement
              { id: me
              , parent: parent.parent
              , scope: parent.scope
              , element: i.element
              }
          )

mediaElement
  :: forall outputChannels lock payload
   . Core.InitializeMediaElement
  -> Core.Node outputChannels lock payload
mediaElement = __mediaElement
