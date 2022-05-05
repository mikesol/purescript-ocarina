module WAGS.Declarative.Create.LoopBuf where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Variant (match)
import FRP.Event (Event, bang, keepLatest, makeEvent, subscribe)
import WAGS.Common.Parameters.LoopBuf as Parameters
import WAGS.Core as Core
import WAGS.Declarative.Create.Gain (tmpResolveAU)

__loopBuf
  :: forall i outputChannels lock payload
   . Parameters.InitialLoopBuf i
  => i
  -> Event (Core.LoopBuf lock payload)
  -> Core.Node outputChannels lock payload
__loopBuf i' atts = Core.Node go
  where
  Core.InitializeLoopBuf i = Parameters.toInitializeLoopBuf i'
  go
    parent
    di@
      ( Core.AudioInterpret
          { ids
          , deleteFromCache
          , makeLoopBuf
          , setBuffer
          , setOnOff
          , setPlaybackRate
          , setLoopStart
          , setLoopEnd
          }
      ) =
    makeEvent \k -> do
      me <- ids
      parent.raiseId me
      map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
        bang
          ( makeLoopBuf
              { id: me
              , parent: parent.parent
              , scope: parent.scope
              , buffer: i.buffer
              , playbackRate: i.playbackRate
              , loopStart: i.loopStart
              , loopEnd: i.loopEnd
              , duration: i.duration
              }
          )
          <|>
            ( keepLatest $ map
                ( \(Core.LoopBuf e) -> match
                    { buffer: \buffer -> bang $ setBuffer { id: me, buffer }
                    , playbackRate: tmpResolveAU parent.scope di (setPlaybackRate <<< { id: me, playbackRate: _ })
                    , loopStart: \loopStart -> bang $ setLoopStart { id: me, loopStart }
                    , loopEnd: \loopEnd -> bang $ setLoopEnd { id: me, loopEnd }
                    , onOff: \onOff -> bang $ setOnOff { id: me, onOff }
                    }
                    e
                )
                atts
            )

loopBuf
  :: forall i outputChannels lock payload
   . Parameters.InitialLoopBuf i
  => i
  -> Event (Core.LoopBuf lock payload)
  -> Core.Node outputChannels lock payload
loopBuf = __loopBuf

loopBuf_
  :: forall i outputChannels lock payload
   . Parameters.InitialLoopBuf i
  => i
  -> Core.Node outputChannels lock payload
loopBuf_ i = loopBuf i empty
