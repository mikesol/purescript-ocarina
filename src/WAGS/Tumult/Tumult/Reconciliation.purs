module WAGS.Tumult.Tumult.Reconciliation where

import Prelude

import Data.List (List(..), (:))
import Data.List as List
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (uncurry)
import Data.Variant (Variant, match, on)
import Foreign.Object as Object
import Type.Proxy (Proxy(..))
import WAGS.Core (Instruction(..))
import WAGS.Tumult.Instructions as R

du
  :: forall r
   . String
  -> { id :: String | r }
  -> Set Instruction
  -> Set Instruction
du u = Set.insert <<< R.iDestroyUnit <<< { unit: u, id: _ } <<< _.id

derogative :: Instruction -> Set Instruction -> Set Instruction
derogative (Instruction i) = match
  { disconnectXFromY: const identity
  , destroyUnit: const identity
  , makeAllpass: du "Allpass"
  , makeAnalyser: du "Analyser"
  , makeAudioWorkletNode: du "AudioWorkletNode"
  , makeBandpass: du "Bandpass"
  , makeConstant: du "Constant"
  , makeConvolver: du "Convolver"
  , makeDelay: du "Delay"
  , makeDynamicsCompressor: du "DynamicsCompressor"
  , makeGain: du "Gain"
  , makeHighpass: du "Highpass"
  , makeHighshelf: du "Highshelf"
  , makeLoopBuf: du "LoopBuf"
  , makeLowpass: du "Lowpass"
  , makeLowshelf: du "Lowshelf"
  , makeMediaElement: du "MediaElement"
  , makeMicrophone: \_ -> Set.insert $ R.iDestroyUnit
      { id: "microphone", unit: "Microphone" }
  , makeNotch: du "Notch"
  , makePeaking: du "Peaking"
  , makePeriodicOsc: du "PeriodicOsc"
  , makePlayBuf: du "PlayBuf"
  , makeRecorder: du "Recorder"
  , makeSawtoothOsc: du "SawtoothOsc"
  , makeSinOsc: du "SinOsc"
  , makeSquareOsc: du "SquareOsc"
  , makeStereoPanner: du "StereoPanner"
  , makeTriangleOsc: du "TriangleOsc"
  , makeWaveShaper: du "WaveShaper"
  , connectXToY: Set.insert <<< R.iDisconnectXFromY
  , setAnalyserNodeCb: const identity
  , setMediaRecorderCb: const identity
  , setAudioWorkletParameter: const identity
  , setBuffer: const identity
  , setConvolverBuffer: const identity
  , setPeriodicOsc: const identity
  , setOnOff: const identity
  , setBufferOffset: const identity
  , setLoopStart: const identity
  , setLoopEnd: const identity
  , setRatio: const identity
  , setOffset: const identity
  , setAttack: const identity
  , setGain: const identity
  , setQ: const identity
  , setPan: const identity
  , setThreshold: const identity
  , setRelease: const identity
  , setKnee: const identity
  , setDelay: const identity
  , setPlaybackRate: const identity
  , setFrequency: const identity
  , setWaveShaperCurve: const identity
  } i

reconcileTumult :: Set Instruction -> Set Instruction -> Set Instruction
reconcileTumult new old = result
  where
  result = go primus secondus Set.empty
  primus = List.fromFoldable new
  secondus = List.fromFoldable old

  usingDefault
    :: forall v
     . List Instruction
    -> Instruction
    -> List Instruction
    -> List Instruction
    -> Instruction
    -> List Instruction
    -> Set Instruction
    -> Variant v
    -> Set Instruction
  usingDefault l0 h0 t0 l1 h1 t1 set _ =
    case compare h0 h1 of
      LT -> go t0 l1 $ Set.insert h0 set
      GT -> go l0 t1 $ derogative h1 set
      EQ -> go t0 t1 set

  comparable
    :: forall r0 r1
     . { id :: String
       | r0
       }
    -> { id :: String
       | r1
       }
    -> List Instruction
    -> Instruction
    -> List Instruction
    -> List Instruction
    -> Instruction
    -> List Instruction
    -> Set Instruction
    -> (Set Instruction -> Set Instruction)
    -> Set Instruction
  comparable a b l0 h0 t0 l1 h1 t1 set setf
    | a.id < b.id = go t0 l1 $ Set.insert h0 set
    | b.id < a.id = go l0 t1 $ derogative h1 set
    | otherwise = go t0 t1 (setf set)

  go
    :: List Instruction
    -> List Instruction
    -> Set Instruction
    -> Set Instruction
  go Nil Nil set = set
  go (h0 : t0) Nil set = go t0 Nil (Set.insert h0 set)
  go Nil (h1 : t1) set = go Nil t1 (derogative h1 set)
  go l0@(h0@(Instruction i0) : t0) l1@(h1@(Instruction i1) : t1) set =
    let
      udef :: forall v. Variant v -> Set Instruction
      udef = usingDefault l0 h0 t0 l1 h1 t1 set
    in
      i0 # match
        { connectXToY: \a -> i1 #
            ( udef
                # on (Proxy :: _ "connectXToY") \b ->
                    let
                      o
                        | a.from < b.from = go t0 l1 $ Set.insert
                            (R.iConnectXToY a)
                            set
                        | a.from > b.from = go l0 t1 $ Set.insert
                            (R.iDisconnectXFromY b)
                            set
                        | a.to < b.to = go t0 l1 $ Set.insert
                            (R.iConnectXToY a)
                            set
                        | a.to > b.to = go l0 t1 $ Set.insert
                            (R.iDisconnectXFromY b)
                            set
                        | a.fromUnit /= b.fromUnit = go l0 t1
                            $ Set.insert (R.iDisconnectXFromY b)
                            $ Set.insert (R.iConnectXToY a) set
                        | a.toUnit /= b.toUnit = go l0 t1
                            $ Set.insert (R.iDisconnectXFromY b)
                            $ Set.insert (R.iConnectXToY a) set
                        | otherwise = go t0 t1 set
                    in
                      o
            )
        , makeAllpass: \a -> i1 #
            ( udef
                # on (Proxy :: _ "makeAllpass") \b ->
                    comparable a b l0 h0 t0 l1 h1 t1 set
                      ( Set.insert
                          (R.iSetFrequency { id: a.id, frequency: a.frequency })
                          <<< Set.insert (R.iSetQ { id: a.id, q: a.q })
                      )
            )
        , makeAnalyser: \a -> i1 #
            ( udef
                # on (Proxy :: _ "makeAllpass") \b ->
                    comparable a b l0 h0 t0 l1 h1 t1 set
                      (Set.insert (R.iSetAnalyserNodeCb { id: a.id, cb: a.cb }))
            )
        , disconnectXFromY: \_ -> i1 # udef
        , destroyUnit: \_ -> i1 # udef
        , makeAudioWorkletNode: \a -> i1 #
            ( udef
                # on (Proxy :: _ "makeAudioWorkletNode") \b ->
                    comparable a b l0 h0 t0 l1 h1 t1 set
                      ( let
                          fn = uncurry $
                            ( \paramName paramValue ->
                                R.iSetAudioWorkletParameter
                                  { id: a.id, paramName, paramValue }
                            )
                        in
                          Set.union
                            ( Set.fromFoldable
                                $ (identity :: Array ~> Array)
                                $ map fn
                                $ Object.toUnfoldable
                                    (unwrap a.options).parameterData
                            )
                      )
            )
        , makeBandpass: \a -> i1 #
            ( udef
                # on (Proxy :: _ "makeBandpass") \b ->
                    comparable a b l0 h0 t0 l1 h1 t1 set
                      ( Set.insert
                          (R.iSetFrequency { id: a.id, frequency: a.frequency })
                          <<< Set.insert (R.iSetQ { id: a.id, q: a.q })
                      )
            )
        , makeConstant: \a -> i1 #
            ( udef
                # on (Proxy :: _ "makeConstant") \b ->
                    comparable a b l0 h0 t0 l1 h1 t1 set
                      ( Set.insert (R.iSetOffset { id: a.id, offset: a.offset })
                      )
            )
        , makeConvolver: \a -> i1 #
            ( udef
                # on (Proxy :: _ "makeConvolver") \b ->
                    comparable a b l0 h0 t0 l1 h1 t1 set identity
            )
        , makeDelay: \a -> i1 #
            ( udef
                # on (Proxy :: _ "makeDelay") \b ->
                    comparable a b l0 h0 t0 l1 h1 t1 set
                      ( Set.insert
                          (R.iSetDelay { id: a.id, delayTime: a.delayTime })
                      )
            )
        , makeDynamicsCompressor: \a -> i1 #
            ( udef
                # on (Proxy :: _ "makeDynamicsCompressor") \b ->
                    comparable a b l0 h0 t0 l1 h1 t1 set
                      ( Set.insert (R.iSetAttack { id: a.id, attack: a.attack })
                          <<< Set.insert
                            (R.iSetRelease { id: a.id, release: a.release })
                          <<< Set.insert
                            ( R.iSetThreshold
                                { id: a.id, threshold: a.threshold }
                            )
                          <<< Set.insert
                            (R.iSetRatio { id: a.id, ratio: a.ratio })
                          <<< Set.insert (R.iSetKnee { id: a.id, knee: a.knee })
                      )
            )
        , makeGain: \a -> i1 #
            ( udef
                # on (Proxy :: _ "makeGain") \b ->
                    comparable a b l0 h0 t0 l1 h1 t1 set
                      ( Set.insert (R.iSetGain { id: a.id, gain: a.gain })
                      )
            )
        , makeHighpass: \a -> i1 #
            ( udef
                # on (Proxy :: _ "makeHighpass") \b ->
                    comparable a b l0 h0 t0 l1 h1 t1 set
                      ( Set.insert
                          (R.iSetFrequency { id: a.id, frequency: a.frequency })
                          <<< Set.insert (R.iSetQ { id: a.id, q: a.q })
                      )
            )
        , makeHighshelf: \a -> i1 #
            ( udef
                # on (Proxy :: _ "makeHighshelf") \b ->
                    comparable a b l0 h0 t0 l1 h1 t1 set
                      ( Set.insert
                          (R.iSetFrequency { id: a.id, frequency: a.frequency })
                          <<< Set.insert (R.iSetGain { id: a.id, gain: a.gain })
                      )
            )
        , makeLoopBuf: \a -> i1 #
            ( udef
                # on (Proxy :: _ "makeLoopBuf") \b ->
                    comparable a b l0 h0 t0 l1 h1 t1 set
                      ( Set.insert (R.iSetBuffer { id: a.id, buffer: a.buffer })
                          <<< Set.insert
                            (R.iSetOnOff { id: a.id, onOff: a.onOff })
                          <<< Set.insert
                            ( R.iSetPlaybackRate
                                { id: a.id, playbackRate: a.playbackRate }
                            )
                          <<< Set.insert
                            ( R.iSetLoopStart
                                { id: a.id, loopStart: a.loopStart }
                            )
                          <<< Set.insert
                            (R.iSetLoopEnd { id: a.id, loopEnd: a.loopEnd })
                      )
            )
        , makeLowpass: \a -> i1 #
            ( udef
                # on (Proxy :: _ "makeLowpass") \b ->
                    comparable a b l0 h0 t0 l1 h1 t1 set
                      ( Set.insert
                          (R.iSetFrequency { id: a.id, frequency: a.frequency })
                          <<< Set.insert (R.iSetQ { id: a.id, q: a.q })
                      )
            )
        , makeLowshelf: \a -> i1 #
            ( udef
                # on (Proxy :: _ "makeLowshelf") \b ->
                    comparable a b l0 h0 t0 l1 h1 t1 set
                      ( Set.insert
                          (R.iSetFrequency { id: a.id, frequency: a.frequency })
                          <<< Set.insert (R.iSetGain { id: a.id, gain: a.gain })
                      )
            )
        , makeMediaElement: \a -> i1 #
            ( udef
                # on (Proxy :: _ "makeMediaElement") \b ->
                    comparable a b l0 h0 t0 l1 h1 t1 set identity
            )
        , makeMicrophone: \_ -> i1 # udef
        , makePeaking: \a -> i1 #
            ( udef
                # on (Proxy :: _ "makeNotch") \b ->
                    comparable a b l0 h0 t0 l1 h1 t1 set
                      ( Set.insert
                          (R.iSetFrequency { id: a.id, frequency: a.frequency })
                          <<< Set.insert (R.iSetQ { id: a.id, q: a.q })
                          <<< Set.insert (R.iSetGain { id: a.id, gain: a.gain })
                      )
            )
        , makeNotch: \a -> i1 #
            ( udef
                # on (Proxy :: _ "makeNotch") \b ->
                    comparable a b l0 h0 t0 l1 h1 t1 set
                      ( Set.insert
                          (R.iSetFrequency { id: a.id, frequency: a.frequency })
                          <<< Set.insert (R.iSetQ { id: a.id, q: a.q })
                      )
            )
        , makePeriodicOsc: \a -> i1 #
            ( udef
                # on (Proxy :: _ "makePeriodicOsc") \b ->
                    comparable a b l0 h0 t0 l1 h1 t1 set
                      ( Set.insert
                          (R.iSetPeriodicOsc { id: a.id, spec: a.spec })
                          <<< Set.insert
                            (R.iSetOnOff { id: a.id, onOff: a.onOff })
                          <<< Set.insert
                            (R.iSetFrequency { id: a.id, frequency: a.frequency })
                      )
            )
        , makePlayBuf: \a -> i1 #
            ( udef
                # on (Proxy :: _ "makePlayBuf") \b ->
                    comparable a b l0 h0 t0 l1 h1 t1 set
                      ( Set.insert (R.iSetBuffer { id: a.id, buffer: a.buffer })
                          <<< Set.insert
                            (R.iSetOnOff { id: a.id, onOff: a.onOff })
                          <<< Set.insert
                            ( R.iSetPlaybackRate
                                { id: a.id, playbackRate: a.playbackRate }
                            )
                          <<< Set.insert
                            ( R.iSetBufferOffset
                                { id: a.id, bufferOffset: a.bufferOffset }
                            )
                      )
            )
        , makeRecorder: \a -> i1 #
            ( udef
                # on (Proxy :: _ "makeRecorder") \b ->
                    comparable a b l0 h0 t0 l1 h1 t1 set identity
            )
        , makeSawtoothOsc: \a -> i1 #
            ( udef
                # on (Proxy :: _ "makeSawtoothOsc") \b ->
                    comparable a b l0 h0 t0 l1 h1 t1 set
                      ( Set.insert (R.iSetOnOff { id: a.id, onOff: a.onOff })
                          <<< Set.insert
                            (R.iSetFrequency { id: a.id, frequency: a.frequency })
                      )
            )
        , makeSinOsc: \a -> i1 #
            ( udef
                # on (Proxy :: _ "makeSinOsc") \b ->
                    comparable a b l0 h0 t0 l1 h1 t1 set
                      ( Set.insert (R.iSetOnOff { id: a.id, onOff: a.onOff })
                          <<< Set.insert
                            (R.iSetFrequency { id: a.id, frequency: a.frequency })
                      )
            )
        , makeSquareOsc: \a -> i1 #
            ( udef
                # on (Proxy :: _ "makeSquareOsc") \b ->
                    comparable a b l0 h0 t0 l1 h1 t1 set
                      ( Set.insert (R.iSetOnOff { id: a.id, onOff: a.onOff })
                          <<< Set.insert
                            (R.iSetFrequency { id: a.id, frequency: a.frequency })
                      )
            )
        , makeStereoPanner: \a -> i1 #
            ( udef
                # on (Proxy :: _ "makeStereoPanner") \b ->
                    comparable a b l0 h0 t0 l1 h1 t1 set
                      (Set.insert (R.iSetPan { id: a.id, pan: a.pan }))
            )
        , makeTriangleOsc: \a -> i1 #
            ( udef
                # on (Proxy :: _ "makeTriangleOsc") \b ->
                    comparable a b l0 h0 t0 l1 h1 t1 set
                      ( Set.insert (R.iSetOnOff { id: a.id, onOff: a.onOff })
                          <<< Set.insert
                            (R.iSetFrequency { id: a.id, frequency: a.frequency })
                      )
            )
        , makeWaveShaper: \a -> i1 #
            ( udef
                # on (Proxy :: _ "makeWaveShaper") \b ->
                    comparable a b l0 h0 t0 l1 h1 t1 set identity
            )
        , setAnalyserNodeCb: \_ -> i1 # udef
        , setMediaRecorderCb: \_ -> i1 # udef
        , setAudioWorkletParameter: \_ -> i1 # udef
        , setBuffer: \_ -> i1 # udef
        , setConvolverBuffer: \_ -> i1 # udef
        , setPeriodicOsc: \_ -> i1 # udef
        , setOnOff: \_ -> i1 # udef
        , setBufferOffset: \_ -> i1 # udef
        , setLoopStart: \_ -> i1 # udef
        , setLoopEnd: \_ -> i1 # udef
        , setRatio: \_ -> i1 # udef
        , setOffset: \_ -> i1 # udef
        , setAttack: \_ -> i1 # udef
        , setGain: \_ -> i1 # udef
        , setQ: \_ -> i1 # udef
        , setPan: \_ -> i1 # udef
        , setThreshold: \_ -> i1 # udef
        , setRelease: \_ -> i1 # udef
        , setKnee: \_ -> i1 # udef
        , setDelay: \_ -> i1 # udef
        , setPlaybackRate: \_ -> i1 # udef
        , setFrequency: \_ -> i1 # udef
        , setWaveShaperCurve: \_ -> i1 # udef
        }
