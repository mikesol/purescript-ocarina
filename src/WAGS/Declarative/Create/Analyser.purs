module WAGS.Declarative.Create.Analyser where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Int (pow)
import Data.Variant (match)
import Data.Variant.Maybe (just)
import FRP.Event (Event, bang, makeEvent, subscribe)
import WAGS.Core (ChannelCountMode(..), ChannelInterpretation(..), Po2(..))

import WAGS.Common.Parameters.Analyser as Parameters
import WAGS.Core as Core

analyser
  :: forall i aud outputChannels lock payload
   . Parameters.InitialAnalyser i
  => Core.Mix aud (Core.Audible outputChannels lock payload)
  => i
  -> Event Core.Analyser
  -> aud
  -> Core.Node outputChannels lock payload
analyser i' atts elts = Core.Node go
  where
  Core.InitializeAnalyser i = Parameters.toInitializeAnalyser i'
  go
    parent
    di@(Core.AudioInterpret { ids, deleteFromCache, makeAnalyser, setAnalyserNodeCb }) =
    makeEvent \k -> do
      me <- ids
      parent.raiseId me
      map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
        bang
          ( makeAnalyser
              { id: me
              , parent: parent.parent
              , scope: parent.scope
              , cb: i.cb
              , fftSize: 2 `pow`
                  ( case i.fftSize of
                      TTT7 -> 7
                      TTT8 -> 8
                      TTT9 -> 9
                      TTT10 -> 10
                      TTT11 -> 11
                      TTT12 -> 12
                      TTT13 -> 13
                  )
              , maxDecibels: i.maxDecibels
              , minDecibels: i.minDecibels
              , smoothingTimeConstant: i.smoothingTimeConstant
              , channelCount: i.channelCount
              , channelCountMode: case i.channelCountMode of
                  Explicit -> "explicit"
                  Max -> "max"
                  ClampedMax -> "clamped-max"
              , channelInterpretation: case i.channelInterpretation of
                  Speakers -> "speakers"
                  Discrete -> "discrete"
              }
          )
          <|> map
            ( \(Core.Analyser e) -> match
                { cb: \cb -> setAnalyserNodeCb { id: me, cb }
                }
                e
            )
            atts
          <|> Core.__internalWagsFlatten (just me) parent.scope di (Core.mix elts)

analyser_
  :: forall i aud outputChannels lock payload
   . Parameters.InitialAnalyser i
  => Core.Mix aud (Core.Audible outputChannels lock payload)
  => i
  -> aud
  -> Core.Node outputChannels lock payload
analyser_ i = analyser i empty
