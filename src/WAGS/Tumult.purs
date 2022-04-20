module WAGS.Tumult where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Array ((!!))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Profunctor (lcmap)
import Data.Set as Set
import Data.String as String
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Data.Variant (match)
import Data.Variant.Maybe (just)
import FRP.Behavior (sample_)
import FRP.Event (class IsEvent, fold, keepLatest)
import FRP.Event.Class (bang)
import Type.Proxy (Proxy(..))
import WAGS.Core (AudioWorkletNodeOptions_(..), Instruction(..), MeOrParent(..), useMeIfMe)
import WAGS.Core as C
import WAGS.Parameter (AudioCancel(..), AudioEnvelope(..), AudioNumeric(..), AudioParameter(..), AudioSudden(..))
import WAGS.Tumult.Connect (__inputMonicker)
import WAGS.Tumult.Tumult (Tumultuous, safeUntumult)
import WAGS.Tumult.Tumult.Reconciliation (reconcileTumult)

-- tumult

tumult
  :: forall outputChannels terminus lock event payload
   . IsEvent event
  => IsSymbol terminus
  => event (Tumultuous terminus lock)
  -> C.Node outputChannels lock event payload
tumult atts' = C.Node go
  where
  asNumber (AudioParameter v) = match
    { numeric: \(AudioNumeric { n }) -> n
    , cancel: \(AudioCancel _) -> 0.0
    , envelope: \(AudioEnvelope _) -> 0.0
    , sudden: \(AudioSudden { n }) -> n
    }
    v
  terminus = reflectSymbol (Proxy :: _ terminus)
  go prnt (C.AudioInterpret ai@{ ids, scope, connectXToY }) =
    let
      atts = map fst $ fold (\a (_ /\ b1) -> reconcileTumult a b1 /\ a)
        ( map (\t -> safeUntumult t)
            atts'
        )
        (Set.empty /\ Set.empty)
    in
      keepLatest
        ( (sample_ ids (bang unit)) <#> \msfx'' ->
            let
              msfx' = useMeIfMe prnt msfx''
              sfx i = i <> "_" <> msfx'

              isfx
                :: forall r a
                 . ({ id :: String | r } -> a)
                -> ({ id :: String | r } -> a)
              isfx i = lcmap (\ii -> ii { id = sfx ii.id }) i
            in
              keepLatest
                ( map
                    ( \instr -> foldl
                        ( \b (Instruction i) -> b <|> match
                            { makeAllpass: \{ id, frequency, q, parent } -> bang
                                $
                                  ai.makeAllpass
                                    { id: sfx id
                                    , scope: just scope
                                    , frequency: asNumber frequency
                                    , q: asNumber q
                                    , parent
                                    }
                            , makeAnalyser:
                                \{ id
                                 , channelCount
                                 , channelCountMode
                                 , channelInterpretation
                                 , fftSize
                                 , cb
                                 , maxDecibels
                                 , minDecibels
                                 , parent
                                 , smoothingTimeConstant
                                 } -> bang $
                                  ai.makeAnalyser
                                    { id: sfx id
                                    , scope: just scope
                                    , channelCount
                                    , channelCountMode
                                    , channelInterpretation
                                    , fftSize
                                    , cb
                                    , maxDecibels
                                    , minDecibels
                                    , parent
                                    , smoothingTimeConstant
                                    }
                            , makeAudioWorkletNode:
                                \{ id
                                 , options:
                                     ( AudioWorkletNodeOptions_
                                         { name
                                         , numberOfInputs
                                         , numberOfOutputs
                                         , outputChannelCount
                                         , parameterData
                                         , processorOptions
                                         }
                                     )
                                 , parent
                                 } ->
                                  bang $ ai.makeAudioWorkletNode
                                    { id: sfx id
                                    , scope: just scope
                                    , options:
                                        AudioWorkletNodeOptions_
                                          { name
                                          , numberOfInputs
                                          , numberOfOutputs
                                          , outputChannelCount
                                          , parameterData: map asNumber
                                              parameterData
                                          , processorOptions
                                          }
                                    , parent
                                    }
                            , makeBandpass: \{ id, frequency, q, parent } ->
                                bang
                                  $
                                    ai.makeBandpass
                                      { id: sfx id
                                      , scope: just scope
                                      , frequency: asNumber frequency
                                      , q: asNumber q
                                      , parent
                                      }
                            , makeConstant: \{ id, onOff, offset, parent } ->
                                ( bang
                                    $
                                      ai.makeConstant
                                        { id: sfx id
                                        , scope: just scope
                                        , offset: asNumber offset
                                        , parent
                                        }
                                ) <|> (bang $ ai.setOnOff { id: sfx id, onOff })
                            , makeConvolver: \{ id, buffer, parent } -> bang $
                                ai.makeConvolver
                                  { id: sfx id
                                  , scope: just scope
                                  , buffer
                                  , parent
                                  }
                            , makeDelay: \{ id, delayTime, parent } -> bang $
                                ai.makeDelay
                                  { id: sfx id
                                  , scope: just scope
                                  , delayTime: asNumber delayTime
                                  , parent
                                  }
                            , makeDynamicsCompressor:
                                \{ id
                                 , knee
                                 , threshold
                                 , ratio
                                 , attack
                                 , release
                                 , parent
                                 } -> bang $
                                  ai.makeDynamicsCompressor
                                    { id: sfx id
                                    , scope: just scope
                                    , knee: asNumber knee
                                    , threshold: asNumber threshold
                                    , ratio: asNumber ratio
                                    , attack: asNumber attack
                                    , release: asNumber release
                                    , parent
                                    }
                            , makeGain: \{ id, gain, parent } -> bang $
                                ai.makeGain
                                  { id: sfx id
                                  , scope: just scope
                                  , gain: asNumber gain
                                  , parent
                                  }
                            , makeHighpass: \{ id, frequency, q, parent } ->
                                bang
                                  $
                                    ai.makeHighpass
                                      { id: sfx id
                                      , scope: just scope
                                      , frequency: asNumber frequency
                                      , q: asNumber q
                                      , parent
                                      }
                            , makeHighshelf: \{ id, frequency, gain, parent } ->
                                bang
                                  $
                                    ai.makeHighshelf
                                      { id: sfx id
                                      , scope: just scope
                                      , frequency: asNumber frequency
                                      , gain: asNumber gain
                                      , parent
                                      }
                            , makeLoopBuf:
                                \{ id
                                 , loopStart
                                 , loopEnd
                                 , buffer
                                 , onOff
                                 , playbackRate
                                 , duration
                                 , parent
                                 } ->
                                  ( bang $
                                      ai.makeLoopBuf
                                        { id: sfx id
                                        , scope: just scope
                                        , loopStart
                                        , loopEnd
                                        , buffer
                                        , playbackRate: asNumber playbackRate
                                        , parent
                                        , duration
                                        }
                                  ) <|>
                                    (bang $ ai.setOnOff { id: sfx id, onOff })
                            , makeLowpass: \{ id, frequency, q, parent } -> bang
                                $
                                  ai.makeLowpass
                                    { id: sfx id
                                    , scope: just scope
                                    , frequency: asNumber frequency
                                    , q: asNumber q
                                    , parent
                                    }
                            , makeLowshelf: \{ id, frequency, gain, parent } ->
                                bang
                                  $
                                    ai.makeLowshelf
                                      { id: sfx id
                                      , scope: just scope
                                      , frequency: asNumber frequency
                                      , gain: asNumber gain
                                      , parent
                                      }
                            , makeMediaElement: \{ id, element, parent } -> bang
                                $
                                  ai.makeMediaElement
                                    { id: sfx id
                                    , scope: just scope
                                    , element
                                    , parent
                                    }
                            , makeMicrophone: \{ id, microphone, parent } ->
                                bang
                                  $
                                    ai.makeMicrophone
                                      { id: sfx id
                                      , scope: just scope
                                      , microphone
                                      , parent
                                      }
                            , makeNotch: \{ id, frequency, q, parent } -> bang $
                                ai.makeNotch
                                  { id: sfx id
                                  , scope: just scope
                                  , frequency: asNumber frequency
                                  , q: asNumber q
                                  , parent
                                  }
                            , makePeaking:
                                \{ id, frequency, q, gain, parent } ->
                                  bang $
                                    ai.makePeaking
                                      { id: sfx id
                                      , scope: just scope
                                      , frequency: asNumber frequency
                                      , gain: asNumber gain
                                      , q: asNumber q
                                      , parent
                                      }
                            , makePeriodicOsc:
                                \{ id, frequency, spec, onOff, parent } ->
                                  ( bang $
                                      ai.makePeriodicOsc
                                        { id: sfx id
                                        , scope: just scope
                                        , frequency: asNumber frequency
                                        , spec: spec
                                        , parent
                                        }
                                  ) <|>
                                    (bang $ ai.setOnOff { id: sfx id, onOff })
                            , makePlayBuf:
                                \{ id
                                 , playbackRate
                                 , onOff
                                 , duration
                                 , bufferOffset
                                 , buffer
                                 , parent
                                 } ->
                                  ( bang $
                                      ai.makePlayBuf
                                        { id: sfx id
                                        , scope: just scope
                                        , playbackRate: asNumber playbackRate
                                        , buffer
                                        , bufferOffset
                                        , duration
                                        , parent
                                        }
                                  ) <|>
                                    (bang $ ai.setOnOff { id: sfx id, onOff })
                            , makeRecorder: \{ id, cb, parent } -> bang $
                                ai.makeRecorder
                                  { id: sfx id, scope: just scope, cb, parent }
                            , makeSawtoothOsc:
                                \{ id, frequency, onOff, parent } ->
                                  ( bang $
                                      ai.makeSawtoothOsc
                                        { id: sfx id
                                        , scope: just scope
                                        , frequency: asNumber frequency
                                        , parent
                                        }
                                  ) <|>
                                    (bang $ ai.setOnOff { id: sfx id, onOff })
                            , makeSinOsc: \{ id, frequency, onOff, parent } ->
                                ( bang $
                                    ai.makeSinOsc
                                      { id: sfx id
                                      , scope: just scope
                                      , frequency: asNumber frequency
                                      , parent
                                      }
                                ) <|> (bang $ ai.setOnOff { id: sfx id, onOff })
                            , makeSquareOsc:
                                \{ id, frequency, onOff, parent } ->
                                  ( bang $
                                      ai.makeSquareOsc
                                        { id: sfx id
                                        , scope: just scope
                                        , frequency: asNumber frequency
                                        , parent
                                        }
                                  ) <|>
                                    (bang $ ai.setOnOff { id: sfx id, onOff })
                            , makeStereoPanner: \{ id, pan, parent } -> bang $
                                ai.makeStereoPanner
                                  { id: sfx id
                                  , scope: just scope
                                  , pan: asNumber pan
                                  , parent
                                  }
                            , makeTriangleOsc:
                                \{ id, frequency, onOff, parent } ->
                                  ( bang $
                                      ai.makeTriangleOsc
                                        { id: sfx id
                                        , scope: just scope
                                        , frequency: asNumber frequency
                                        , parent
                                        }
                                  ) <|>
                                    (bang $ ai.setOnOff { id: sfx id, onOff })
                            , makeWaveShaper:
                                \{ id, oversample, curve, parent } -> bang $
                                  ai.makeWaveShaper
                                    { id: sfx id
                                    , scope: just scope
                                    , oversample
                                    , curve
                                    , parent
                                    }
                            -- inputs come from the outside, so they do not need to be made
                            , makeInput: \_ -> empty
                            , connectXToY: \{ from, to } -> bang $
                                ai.connectXToY
                                  { from:
                                      case
                                        ( String.split
                                            (String.Pattern __inputMonicker)
                                            from
                                        ) !! 1
                                        of
                                        Just s -> s
                                        Nothing -> sfx from
                                  , to: sfx to
                                  }
                            -- when disconnecting, we work off of
                            -- omp for `from`
                            -- as we can only ever disconnect a previous input
                            , disconnectXFromY: \{ from, to } -> bang $
                                ai.disconnectXFromY
                                  { from:
                                      case
                                        ( String.split
                                            (String.Pattern __inputMonicker)
                                            from
                                        ) !! 1
                                        of
                                        Just s -> s
                                        Nothing -> sfx from
                                  , to: sfx to
                                  }
                            -- we never destroy inputs
                            , destroyUnit: \{ id } -> bang $ ai.destroyUnit
                                { id: sfx id }
                            , setAnalyserNodeCb: isfx \ii -> bang $
                                ai.setAnalyserNodeCb ii
                            , setMediaRecorderCb: isfx \ii -> bang $
                                ai.setMediaRecorderCb ii
                            , setAudioWorkletParameter: isfx \ii -> bang $
                                ai.setAudioWorkletParameter ii
                            , setBuffer: isfx \ii -> bang $ ai.setBuffer ii
                            , setConvolverBuffer: isfx \ii -> bang $
                                ai.setConvolverBuffer ii
                            , setPeriodicOsc: isfx \ii -> bang $
                                ai.setPeriodicOsc
                                  ii
                            , setOnOff: isfx \ii -> bang $ ai.setOnOff ii
                            , setBufferOffset: isfx \ii -> bang $
                                ai.setBufferOffset ii
                            , setDuration: isfx \ii -> bang $
                                ai.setDuration ii
                            , setLoopStart: isfx \ii -> bang $ ai.setLoopStart
                                ii
                            , setLoopEnd: isfx \ii -> bang $ ai.setLoopEnd ii
                            , setRatio: isfx \ii -> bang $ ai.setRatio ii
                            , setOffset: isfx \ii -> bang $ ai.setOffset ii
                            , setAttack: isfx \ii -> bang $ ai.setAttack ii
                            , setGain: isfx \ii -> bang $ ai.setGain ii
                            , setQ: isfx \ii -> bang $ ai.setQ ii
                            , setPan: isfx \ii -> bang $ ai.setPan ii
                            , setThreshold: isfx \ii -> bang $ ai.setThreshold
                                ii
                            , setRelease: isfx \ii -> bang $ ai.setRelease ii
                            , setKnee: isfx \ii -> bang $ ai.setKnee ii
                            , setDelay: isfx \ii -> bang $ ai.setDelay ii
                            , setPlaybackRate: isfx \ii -> bang $
                                ai.setPlaybackRate ii
                            , setFrequency: isfx \ii -> bang $ ai.setFrequency
                                ii
                            , setWaveShaperCurve: isfx \ii -> bang $
                                ai.setWaveShaperCurve ii
                            }
                            i
                        )
                        empty
                        instr

                    )
                    atts
                )
                <|> case prnt of
                            Parent prnnnnt -> bang (connectXToY { from: sfx terminus, to: prnnnnt })
                            Me _ -> empty
        )
