module WAGS.Declarative.Create.PlayBuf where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Variant (match)
import FRP.Event (Event, bang, keepLatest, makeEvent, subscribe)
import WAGS.Common.Parameters.PlayBuf as Parameters
import WAGS.Core as Core
import WAGS.Declarative.Create.Gain (tmpResolveAU)

__playBuf
  :: forall i outputChannels lock payload
   . Parameters.InitialPlayBuf i
  => i
  -> Event (Core.PlayBuf lock payload)
  -> Core.Node outputChannels lock payload
__playBuf i' atts = Core.Node go
  where
  Core.InitializePlayBuf i = Parameters.toInitializePlayBuf i'
  go
    parent
    di@
      ( Core.AudioInterpret
          { ids
          , deleteFromCache
          , makePlayBuf
          , setBuffer
          , setOnOff
          , setDuration
          , setPlaybackRate
          , setBufferOffset
          }
      ) =
    makeEvent \k -> do
      me <- ids
      parent.raiseId me
      map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
        bang
          ( makePlayBuf
              { id: me
              , parent: parent.parent
              , scope: parent.scope
              , buffer: i.buffer
              , playbackRate: i.playbackRate
              , bufferOffset: i.bufferOffset
              , duration: i.duration
              }
          )
          <|>
            ( keepLatest $ map
                ( \(Core.PlayBuf e) -> match
                    { buffer: \buffer -> bang $ setBuffer { id: me, buffer }
                    , playbackRate: tmpResolveAU parent.scope di
                        ( setPlaybackRate <<<
                            { id: me, playbackRate: _ }
                        )
                    , bufferOffset: \bufferOffset -> bang $ setBufferOffset
                        { id: me, bufferOffset }
                    , onOff: \onOff -> bang $ setOnOff { id: me, onOff }
                    , duration: \duration -> bang $ setDuration { id: me, duration }
                    }
                    e
                )
                atts
            )

playBuf
  :: forall i outputChannels lock payload
   . Parameters.InitialPlayBuf i
  => i
  -> Event (Core.PlayBuf lock payload)
  -> Core.Node outputChannels lock payload
playBuf = __playBuf

playBuf_
  :: forall i outputChannels lock payload
   . Parameters.InitialPlayBuf i
  => i
  -> Core.Node outputChannels lock payload
playBuf_ i = playBuf i empty
