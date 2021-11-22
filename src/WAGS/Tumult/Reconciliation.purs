module WAGS.Tumult.Reconciliation where

import Prelude

import Data.List (List(..), (:))
import Data.List as List
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (uncurry)
import Data.Variant (Variant, default, match, on)
import Foreign.Object as Object
import Type.Proxy (Proxy(..))
import WAGS.Rendered (Instruction(..))
import WAGS.Rendered as R

du
  :: forall r
   . String
  -> { id :: String | r }
  -> Set Instruction
  -> Set Instruction
du u = Set.insert <<< R.iDestroyUnit <<< { unit: u, id: _ } <<< _.id

derogative :: Instruction -> Set Instruction -> Set Instruction
derogative = unwrap >>> match
  { disconnectXFromY: const $ identity
  , destroyUnit: const $ identity
  , makeAllpass: du "Allpass"
  , makeAnalyser: du "Analyser"
  , makeAudioWorkletNode: du "AudioWorkletNode"
  , makeBandpass: du "Bandpass"
  , makeConstant: du "Constant"
  , makePassthroughConvolver: du "Convolver"
  , makeConvolver: du "Convolver"
  , makeDelay: du "Delay"
  , makeDynamicsCompressor: du "DynamicsCompressor"
  , makeGain: du "Gain"
  , makeHighpass: du "Highpass"
  , makeHighshelf: du "Highshelf"
  , makeInput: du "Input"
  , makeLoopBuf: du "LoopBuf"
  , makeLoopBufWithDeferredBuffer: du "LoopBuf"
  , makeLowpass: du "Lowpass"
  , makeLowshelf: du "Lowshelf"
  , makeMicrophone: const $ Set.insert $ R.iDestroyUnit { id: "microphone", unit: "Microphone" }
  , makeNotch: du "Notch"
  , makePeaking: du "Peaking"
  , makePeriodicOscWithDeferredOsc: du "PeriodicOsc"
  , makePeriodicOsc: du "PeriodicOsc"
  , makePlayBuf: du "PlayBuf"
  , makePlayBufWithDeferredBuffer: du "PlayBuf"
  , makeRecorder: du "Recorder"
  , makeSawtoothOsc: du "SawtoothOsc"
  , makeSinOsc: du "SinOsc"
  , makeSquareOsc: du "SquareOsc"
  , makeSpeaker: const $ Set.insert $ R.iDestroyUnit { id: "speaker", unit: "Microphone" }
  , makeStereoPanner: du "StereoPanner"
  , makeTriangleOsc: du "TriangleOsc"
  , makeWaveShaper: du "WaveShaper"
  , makeSubgraph: du "Subgraph"
  , makeSubgraphWithDeferredScene: du "Subgraph"
  , makeTumult: du "Tumult"
  , makeTumultWithDeferredGraph: du "Tumult"
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
  , setInput: const identity
  , setSubgraph: const identity
  , setTumult: const identity
  }

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
  usingDefault l0 h0 t0 l1 h1 t1 set = default
    ( case compare h0 h1 of
        LT -> go t0 l1 $ Set.insert h0 set
        GT -> go l0 t1 $ derogative h1 set
        EQ -> go t0 t1 set
    )
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
  go :: List Instruction -> List Instruction -> Set Instruction -> Set Instruction
  go Nil Nil set = set
  go (h0 : t0) Nil set = go t0 Nil (Set.insert h0 set)
  go Nil (h1 : t1) set = go Nil t1 (derogative h1 set)
  go l0@(h0@(Instruction i0) : t0) l1@(h1@(Instruction i1) : t1) set = i0 # match
    { connectXToY: \a -> i1 #
        ( usingDefault l0 h0 t0 l1 h1 t1 set
            # on (Proxy :: _ "connectXToY") \b ->
              let
                o
                  | a.fromId < b.fromId = go t0 l1 $ Set.insert (R.iConnectXToY a) set
                  | a.fromId > b.fromId = go l0 t1 $ Set.insert (R.iDisconnectXFromY b) set
                  | a.toId < b.toId = go t0 l1 $ Set.insert (R.iConnectXToY a) set
                  | a.toId > b.fromId = go l0 t1 $ Set.insert (R.iDisconnectXFromY b) set
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
        ( usingDefault l0 h0 t0 l1 h1 t1 set
            # on (Proxy :: _ "makeAllpass") \b ->
              comparable a b l0 h0 t0 l1 h1 t1 set
                ( Set.insert (R.iSetFrequency { id: a.id, frequency: a.freq })
                    <<< Set.insert (R.iSetQ { id: a.id, q: a.q })
                )
        )
    , makeAnalyser: \a -> i1 #
        ( usingDefault l0 h0 t0 l1 h1 t1 set
            # on (Proxy :: _ "makeAllpass") \b ->
              comparable a b l0 h0 t0 l1 h1 t1 set
                (Set.insert (R.iSetAnalyserNodeCb { id: a.id, cb: a.cb }))
        )
    , disconnectXFromY: const $ i1 # usingDefault l0 h0 t0 l1 h1 t1 set
    , destroyUnit: const $ i1 # usingDefault l0 h0 t0 l1 h1 t1 set
    , makeAudioWorkletNode: \a -> i1 #
        ( usingDefault l0 h0 t0 l1 h1 t1 set
            # on (Proxy :: _ "makeAudioWorkletNode") \b ->
              comparable a b l0 h0 t0 l1 h1 t1 set
                ( let
                    fn = uncurry $
                      ( \paramName paramValue ->
                          R.iSetAudioWorkletParameter { id: a.id, paramName, paramValue }
                      )
                  in
                    Set.union
                      ( Set.fromFoldable
                          $ (identity :: Array ~> Array)
                          $ map fn
                          $ Object.toUnfoldable (unwrap a.options).parameterData
                      )
                )
        )
    , makeBandpass: \a -> i1 #
        ( usingDefault l0 h0 t0 l1 h1 t1 set
            # on (Proxy :: _ "makeBandpass") \b ->
              comparable a b l0 h0 t0 l1 h1 t1 set
                ( Set.insert (R.iSetFrequency { id: a.id, frequency: a.freq })
                    <<< Set.insert (R.iSetQ { id: a.id, q: a.q })
                )
        )
    , makeConstant: \a -> i1 #
        ( usingDefault l0 h0 t0 l1 h1 t1 set
            # on (Proxy :: _ "makeConstant") \b ->
              comparable a b l0 h0 t0 l1 h1 t1 set
                ( Set.insert (R.iSetOffset { id: a.id, offset: a.offset })
                )
        )
    , makePassthroughConvolver: \a -> i1 #
        ( usingDefault l0 h0 t0 l1 h1 t1 set
            # on (Proxy :: _ "makeConvolver") \b ->
              comparable a b l0 h0 t0 l1 h1 t1 set identity
        )
    , makeConvolver: \a -> i1 #
        ( usingDefault l0 h0 t0 l1 h1 t1 set
            # on (Proxy :: _ "makeConvolver") \b ->
              comparable a b l0 h0 t0 l1 h1 t1 set identity
        )
    , makeDelay: \a -> i1 #
        ( usingDefault l0 h0 t0 l1 h1 t1 set
            # on (Proxy :: _ "makeDelay") \b ->
              comparable a b l0 h0 t0 l1 h1 t1 set
                ( Set.insert (R.iSetDelay { id: a.id, delay: a.delayTime })
                )
        )
    , makeDynamicsCompressor: \a -> i1 #
        ( usingDefault l0 h0 t0 l1 h1 t1 set
            # on (Proxy :: _ "makeDynamicsCompressor") \b ->
              comparable a b l0 h0 t0 l1 h1 t1 set
                ( Set.insert (R.iSetAttack { id: a.id, attack: a.attack })
                    <<< Set.insert (R.iSetRelease { id: a.id, release: a.release })
                    <<< Set.insert (R.iSetThreshold { id: a.id, threshold: a.threshold })
                    <<< Set.insert (R.iSetRatio { id: a.id, ratio: a.ratio })
                    <<< Set.insert (R.iSetKnee { id: a.id, knee: a.knee })
                )
        )
    , makeGain: \a -> i1 #
        ( usingDefault l0 h0 t0 l1 h1 t1 set
            # on (Proxy :: _ "makeGain") \b ->
              comparable a b l0 h0 t0 l1 h1 t1 set
                ( Set.insert (R.iSetGain { id: a.id, gain: a.gain })
                )
        )
    , makeHighpass: \a -> i1 #
        ( usingDefault l0 h0 t0 l1 h1 t1 set
            # on (Proxy :: _ "makeHighpass") \b ->
              comparable a b l0 h0 t0 l1 h1 t1 set
                ( Set.insert (R.iSetFrequency { id: a.id, frequency: a.freq })
                    <<< Set.insert (R.iSetQ { id: a.id, q: a.q })
                )
        )
    , makeHighshelf: \a -> i1 #
        ( usingDefault l0 h0 t0 l1 h1 t1 set
            # on (Proxy :: _ "makeHighshelf") \b ->
              comparable a b l0 h0 t0 l1 h1 t1 set
                ( Set.insert (R.iSetFrequency { id: a.id, frequency: a.freq })
                    <<< Set.insert (R.iSetGain { id: a.id, gain: a.gain })
                )
        )
    , makeInput: \a -> i1 #
        ( usingDefault l0 h0 t0 l1 h1 t1 set
            # on (Proxy :: _ "makeInput") \b ->
              comparable a b l0 h0 t0 l1 h1 t1 set identity -- todo: should this be identity?
        )
    , makeLoopBuf: \a -> i1 #
        ( usingDefault l0 h0 t0 l1 h1 t1 set
            # on (Proxy :: _ "makeLoopBuf") \b ->
              comparable a b l0 h0 t0 l1 h1 t1 set
                ( Set.insert (R.iSetBuffer { id: a.id, buffer: a.buffer })
                    <<< Set.insert (R.iSetOnOff { id: a.id, onOff: a.onOff })
                    <<< Set.insert (R.iSetPlaybackRate { id: a.id, playbackRate: a.playbackRate })
                    <<< Set.insert (R.iSetLoopStart { id: a.id, loopStart: a.loopStart })
                    <<< Set.insert (R.iSetLoopEnd { id: a.id, loopEnd: a.loopEnd })
                )
        )
    , makeLoopBufWithDeferredBuffer: \a -> i1 #
        ( usingDefault l0 h0 t0 l1 h1 t1 set
            # on (Proxy :: _ "makeLoopBufWithDeferredBuffer") \b ->
              comparable a b l0 h0 t0 l1 h1 t1 set identity
        )
    , makeLowpass: \a -> i1 #
        ( usingDefault l0 h0 t0 l1 h1 t1 set
            # on (Proxy :: _ "makeLowpass") \b ->
              comparable a b l0 h0 t0 l1 h1 t1 set
                ( Set.insert (R.iSetFrequency { id: a.id, frequency: a.freq })
                    <<< Set.insert (R.iSetQ { id: a.id, q: a.q })
                )
        )
    , makeLowshelf: \a -> i1 #
        ( usingDefault l0 h0 t0 l1 h1 t1 set
            # on (Proxy :: _ "makeLowshelf") \b ->
              comparable a b l0 h0 t0 l1 h1 t1 set
                ( Set.insert (R.iSetFrequency { id: a.id, frequency: a.freq })
                    <<< Set.insert (R.iSetGain { id: a.id, gain: a.gain })
                )
        )
    , makeMicrophone: const $ i1 # usingDefault l0 h0 t0 l1 h1 t1 set
    , makePeaking: \a -> i1 #
        ( usingDefault l0 h0 t0 l1 h1 t1 set
            # on (Proxy :: _ "makeNotch") \b ->
              comparable a b l0 h0 t0 l1 h1 t1 set
                ( Set.insert (R.iSetFrequency { id: a.id, frequency: a.freq })
                    <<< Set.insert (R.iSetQ { id: a.id, q: a.q })
                    <<< Set.insert (R.iSetGain { id: a.id, gain: a.gain })
                )
        )
    , makeNotch: \a -> i1 #
        ( usingDefault l0 h0 t0 l1 h1 t1 set
            # on (Proxy :: _ "makeNotch") \b ->
              comparable a b l0 h0 t0 l1 h1 t1 set
                ( Set.insert (R.iSetFrequency { id: a.id, frequency: a.freq })
                    <<< Set.insert (R.iSetQ { id: a.id, q: a.q })
                )
        )
    , makePeriodicOscWithDeferredOsc: \a -> i1 #
        ( usingDefault l0 h0 t0 l1 h1 t1 set
            # on (Proxy :: _ "makePeriodicOscWithDeferredOsc") \b ->
              comparable a b l0 h0 t0 l1 h1 t1 set identity
        )
    , makePeriodicOsc: \a -> i1 #
        ( usingDefault l0 h0 t0 l1 h1 t1 set
            # on (Proxy :: _ "makePeriodicOsc") \b ->
              comparable a b l0 h0 t0 l1 h1 t1 set
                ( Set.insert (R.iSetPeriodicOsc { id: a.id, periodicOsc: a.spec })
                    <<< Set.insert (R.iSetOnOff { id: a.id, onOff: a.onOff })
                    <<< Set.insert (R.iSetFrequency { id: a.id, frequency: a.freq })
                )
        )
    , makePlayBuf: \a -> i1 #
        ( usingDefault l0 h0 t0 l1 h1 t1 set
            # on (Proxy :: _ "makePlayBuf") \b ->
              comparable a b l0 h0 t0 l1 h1 t1 set
                ( Set.insert (R.iSetBuffer { id: a.id, buffer: a.buffer })
                    <<< Set.insert (R.iSetOnOff { id: a.id, onOff: a.onOff })
                    <<< Set.insert (R.iSetPlaybackRate { id: a.id, playbackRate: a.playbackRate })
                    <<< Set.insert (R.iSetBufferOffset { id: a.id, bufferOffset: a.bufferOffset })
                )
        )
    , makePlayBufWithDeferredBuffer: \a -> i1 #
        ( usingDefault l0 h0 t0 l1 h1 t1 set
            # on (Proxy :: _ "makePlayBufWithDeferredBuffer") \b ->
              comparable a b l0 h0 t0 l1 h1 t1 set identity
        )
    , makeRecorder: \a -> i1 #
        ( usingDefault l0 h0 t0 l1 h1 t1 set
            # on (Proxy :: _ "makeRecorder") \b ->
              comparable a b l0 h0 t0 l1 h1 t1 set identity
        )
    , makeSawtoothOsc: \a -> i1 #
        ( usingDefault l0 h0 t0 l1 h1 t1 set
            # on (Proxy :: _ "makeSawtoothOsc") \b ->
              comparable a b l0 h0 t0 l1 h1 t1 set
                ( Set.insert (R.iSetOnOff { id: a.id, onOff: a.onOff })
                    <<< Set.insert (R.iSetFrequency { id: a.id, frequency: a.freq })
                )
        )
    , makeSinOsc: \a -> i1 #
        ( usingDefault l0 h0 t0 l1 h1 t1 set
            # on (Proxy :: _ "makeSinOsc") \b ->
              comparable a b l0 h0 t0 l1 h1 t1 set
                ( Set.insert (R.iSetOnOff { id: a.id, onOff: a.onOff })
                    <<< Set.insert (R.iSetFrequency { id: a.id, frequency: a.freq })
                )
        )
    , makeSquareOsc: \a -> i1 #
        ( usingDefault l0 h0 t0 l1 h1 t1 set
            # on (Proxy :: _ "makeSquareOsc") \b ->
              comparable a b l0 h0 t0 l1 h1 t1 set
                ( Set.insert (R.iSetOnOff { id: a.id, onOff: a.onOff })
                    <<< Set.insert (R.iSetFrequency { id: a.id, frequency: a.freq })
                )
        )
    , makeSpeaker: const $ i1 # usingDefault l0 h0 t0 l1 h1 t1 set
    , makeStereoPanner: \a -> i1 #
        ( usingDefault l0 h0 t0 l1 h1 t1 set
            # on (Proxy :: _ "makeStereoPanner") \b ->
              comparable a b l0 h0 t0 l1 h1 t1 set
                (Set.insert (R.iSetPan { id: a.id, pan: a.pan }))
        )
    , makeTriangleOsc: \a -> i1 #
        ( usingDefault l0 h0 t0 l1 h1 t1 set
            # on (Proxy :: _ "makeTriangleOsc") \b ->
              comparable a b l0 h0 t0 l1 h1 t1 set
                ( Set.insert (R.iSetOnOff { id: a.id, onOff: a.onOff })
                    <<< Set.insert (R.iSetFrequency { id: a.id, frequency: a.freq })
                )
        )
    , makeWaveShaper: \a -> i1 #
        ( usingDefault l0 h0 t0 l1 h1 t1 set
            # on (Proxy :: _ "makeWaveShaper") \b ->
              comparable a b l0 h0 t0 l1 h1 t1 set identity
        )
    , makeSubgraph: \a -> i1 #
        ( usingDefault l0 h0 t0 l1 h1 t1 set
            # on (Proxy :: _ "makeSubgraph") \b ->
              comparable a b l0 h0 t0 l1 h1 t1 set identity
        )
    , makeSubgraphWithDeferredScene: \a -> i1 #
        ( usingDefault l0 h0 t0 l1 h1 t1 set
            # on (Proxy :: _ "makeSubgraphWithDeferredScene") \b ->
              comparable a b l0 h0 t0 l1 h1 t1 set identity
        )
    , makeTumult: \a -> i1 #
        ( usingDefault l0 h0 t0 l1 h1 t1 set
            # on (Proxy :: _ "makeTumult") \b ->
              comparable a b l0 h0 t0 l1 h1 t1 set identity
        )
    , makeTumultWithDeferredGraph: \a -> i1 #
        ( usingDefault l0 h0 t0 l1 h1 t1 set
            # on (Proxy :: _ "makeTumultWithDeferredGraph") \b ->
              comparable a b l0 h0 t0 l1 h1 t1 set identity
        )
    , setAnalyserNodeCb: const $ i1 # usingDefault l0 h0 t0 l1 h1 t1 set
    , setMediaRecorderCb: const $ i1 # usingDefault l0 h0 t0 l1 h1 t1 set
    , setAudioWorkletParameter: const $ i1 # usingDefault l0 h0 t0 l1 h1 t1 set
    , setBuffer: const $ i1 # usingDefault l0 h0 t0 l1 h1 t1 set
    , setConvolverBuffer: const $ i1 # usingDefault l0 h0 t0 l1 h1 t1 set
    , setPeriodicOsc: const $ i1 # usingDefault l0 h0 t0 l1 h1 t1 set
    , setOnOff: const $ i1 # usingDefault l0 h0 t0 l1 h1 t1 set
    , setBufferOffset: const $ i1 # usingDefault l0 h0 t0 l1 h1 t1 set
    , setLoopStart: const $ i1 # usingDefault l0 h0 t0 l1 h1 t1 set
    , setLoopEnd: const $ i1 # usingDefault l0 h0 t0 l1 h1 t1 set
    , setRatio: const $ i1 # usingDefault l0 h0 t0 l1 h1 t1 set
    , setOffset: const $ i1 # usingDefault l0 h0 t0 l1 h1 t1 set
    , setAttack: const $ i1 # usingDefault l0 h0 t0 l1 h1 t1 set
    , setGain: const $ i1 # usingDefault l0 h0 t0 l1 h1 t1 set
    , setQ: const $ i1 # usingDefault l0 h0 t0 l1 h1 t1 set
    , setPan: const $ i1 # usingDefault l0 h0 t0 l1 h1 t1 set
    , setThreshold: const $ i1 # usingDefault l0 h0 t0 l1 h1 t1 set
    , setRelease: const $ i1 # usingDefault l0 h0 t0 l1 h1 t1 set
    , setKnee: const $ i1 # usingDefault l0 h0 t0 l1 h1 t1 set
    , setDelay: const $ i1 # usingDefault l0 h0 t0 l1 h1 t1 set
    , setPlaybackRate: const $ i1 # usingDefault l0 h0 t0 l1 h1 t1 set
    , setFrequency: const $ i1 # usingDefault l0 h0 t0 l1 h1 t1 set
    , setWaveShaperCurve: const $ i1 # usingDefault l0 h0 t0 l1 h1 t1 set
    , setInput: const $ i1 # usingDefault l0 h0 t0 l1 h1 t1 set
    , setSubgraph: const $ i1 # usingDefault l0 h0 t0 l1 h1 t1 set
    , setTumult: const $ i1 # usingDefault l0 h0 t0 l1 h1 t1 set
    }

{-
    go l0@(vA@(MakeAudioWorkletNode ptr0 valA0) : t0) l1@(MakeAudioWorkletNode ptr1 _ : t1) set
    | ptr0 < ptr1 = go t0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 t1 (Set.insert (DestroyUnit ptr1 "AudioWorkletNode") set)
    | otherwise =
        let
          fn = uncurry $ SetAudioWorkletParameter ptr0
        in
          go t0 t1
            $ Set.union
              ( Set.fromFoldable
                  $ (identity :: Array ~> Array)
                  $ map fn
                  $ Object.toUnfoldable (unwrap valA0).parameterData
              )
              set
  go (vA@(MakeAudioWorkletNode _ _) : t0) t1 set =
    go t0 t1 (Set.insert vA set)
  go t0 (MakeAudioWorkletNode ptr1 _ : t1) set =
    go t0 t1 (Set.insert (DestroyUnit ptr1 "AudioWorkletNode") set)
  go l0@(vA@(MakeBandpass ptr0 valA0 valB0) : t0) l1@(MakeBandpass ptr1 _ _ : t1) set
    | ptr0 < ptr1 = go t0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 t1 (Set.insert (DestroyUnit ptr1 "Bandpass") set)
    | otherwise = go t0 t1 (Set.insert (SetFrequency ptr0 valA0) $ Set.insert (SetQ ptr0 valB0) set)
  go (vA@(MakeBandpass _ _ _) : t0) l1 set =
    go t0 l1 (Set.insert vA set)
  go l0 (MakeBandpass ptr1 _ _ : t1) set =
    go l0 t1 (Set.insert (DestroyUnit ptr1 "Bandpass") set)
  go l0@(vA@(MakeConstant ptr0 valA0 valB0) : t0) l1@(MakeConstant ptr1 _ _ : t1) set
    | ptr0 < ptr1 = go t0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 t1 (Set.insert (DestroyUnit ptr1 "Constant") set)
    | otherwise = go t0 t1 (Set.insert (SetOnOff ptr0 valA0) $ Set.insert (SetOffset ptr0 valB0) set)
  go (vA@(MakeConstant _ _ _) : t0) l1 set =
    go t0 l1 (Set.insert vA set)
  go l0 (MakeConstant ptr1 _ _ : t1) set =
    go l0 t1 (Set.insert (DestroyUnit ptr1 "Constant") set)
  go l0@(vA@(MakePassthroughConvolver ptr0) : t0) l1@(MakePassthroughConvolver ptr1 : t1) set
    | ptr0 < ptr1 = go t0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 t1 (Set.insert (DestroyUnit ptr1 "Convolver") set)
    | otherwise = go t0 t1 set
  go (vA@(MakePassthroughConvolver _) : t0) l1 set =
    go t0 l1 (Set.insert vA set)
  go l0 (MakePassthroughConvolver ptr1 : t1) set =
    go l0 t1 (Set.insert (DestroyUnit ptr1 "Convolver") set)
  go l0@(vA@(MakeConvolver ptr0 _) : t0) l1@(MakeConvolver ptr1 _ : t1) set
    | ptr0 < ptr1 = go t0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 t1 (Set.insert (DestroyUnit ptr1 "Convolver") set)
    | otherwise = go t0 t1 set
  go (vA@(MakeConvolver _ _) : t0) l1 set =
    go t0 l1 (Set.insert vA set)
  go l0 (MakeConvolver ptr1 _ : t1) set =
    go l0 t1 (Set.insert (DestroyUnit ptr1 "Convolver") set)
  go l0@(vA@(MakeDelay ptr0 valA0) : t0) l1@(MakeDelay ptr1 _ : t1) set
    | ptr0 < ptr1 = go t0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 t1 (Set.insert (DestroyUnit ptr1 "Delay") set)
    | otherwise = go t0 t1 (Set.insert (SetDelay ptr0 valA0) set)
  go (vA@(MakeDelay _ _) : t0) l1 set =
    go t0 l1 (Set.insert vA set)
  go l0 (MakeDelay ptr1 _ : t1) set =
    go l0 t1 (Set.insert (DestroyUnit ptr1 "Delay") set)
  go l0@(vA@(MakeDynamicsCompressor ptr0 valA0 valB0 valC0 valD0 valE0) : t0) l1@(MakeDynamicsCompressor ptr1 _ _ _ _ _ : t1) set
    | ptr0 < ptr1 = go t0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 t1 (Set.insert (DestroyUnit ptr1 "DynamicsCompressor") set)
    | otherwise = go t0 t1
        $ Set.insert (SetThreshold ptr0 valA0)
        $ Set.insert (SetKnee ptr0 valB0)
        $ Set.insert (SetRatio ptr0 valC0)
        $ Set.insert (SetAttack ptr0 valD0)
        $ Set.insert (SetRelease ptr0 valE0) set
  go (vA@(MakeDynamicsCompressor _ _ _ _ _ _) : t0) l1 set =
    go t0 l1 (Set.insert vA set)
  go l0 (MakeDynamicsCompressor ptr1 _ _ _ _ _ : t1) set =
    go l0 t1 (Set.insert (DestroyUnit ptr1 "DynamicsCompressor") set)
  go l0@(vA@(MakeGain ptr0 valA0) : t0) l1@(MakeGain ptr1 _ : t1) set
    | ptr0 < ptr1 = go t0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 t1 (Set.insert (DestroyUnit ptr1 "Gain") set)
    | otherwise = go t0 t1 (Set.insert (SetGain ptr0 valA0) set)
  go (vA@(MakeGain _ _) : t0) l1 set =
    go t0 l1 (Set.insert vA set)
  go l0 (MakeGain ptr1 _ : t1) set =
    go l0 t1 (Set.insert (DestroyUnit ptr1 "Gain") set)
  go l0@(vA@(MakeHighpass ptr0 valA0 valB0) : t0) l1@(MakeHighpass ptr1 _ _ : t1) set
    | ptr0 < ptr1 = go t0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 t1 (Set.insert (DestroyUnit ptr1 "Highpass") set)
    | otherwise = go t0 t1 (Set.insert (SetFrequency ptr0 valA0) $ Set.insert (SetQ ptr0 valB0) set)
  go (vA@(MakeHighpass _ _ _) : t0) l1 set =
    go t0 l1 (Set.insert vA set)
  go l0 (MakeHighpass ptr1 _ _ : t1) set =
    go l0 t1 (Set.insert (DestroyUnit ptr1 "Highpass") set)
  go l0@(vA@(MakeHighshelf ptr0 valA0 valB0) : t0) l1@(MakeHighshelf ptr1 _ _ : t1) set
    | ptr0 < ptr1 = go t0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 t1 (Set.insert (DestroyUnit ptr1 "Highshelf") set)
    | otherwise = go t0 t1 (Set.insert (SetFrequency ptr0 valA0) $ Set.insert (SetGain ptr0 valB0) set)
  go (vA@(MakeHighshelf _ _ _) : t0) l1 set =
    go t0 l1 (Set.insert vA set)
  go l0 (MakeHighshelf ptr1 _ _ : t1) set =
    go l0 t1 (Set.insert (DestroyUnit ptr1 "Highshelf") set)
  go l0@(vA@(MakeInput ptr0 _) : t0) l1@(MakeInput ptr1 _ : t1) set
    | ptr0 < ptr1 = go t0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 t1 (Set.insert (DestroyUnit ptr1 "Input") set)
    | otherwise = go t0 t1 set
  go (vA@(MakeInput _ _) : t0) l1 set =
    go t0 l1 (Set.insert vA set)
  go l0 (MakeInput ptr1 _ : t1) set =
    go l0 t1 (Set.insert (DestroyUnit ptr1 "Input") set)
  go l0@(vA@(MakeLoopBuf ptr0 valA0 valB0 valC0 valD0 valE0) : t0) l1@(MakeLoopBuf ptr1 _ _ _ _ _ : t1) set
    | ptr0 < ptr1 = go t0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 t1 (Set.insert (DestroyUnit ptr1 "LoopBuf") set)
    | otherwise = go t0 t1
        $ Set.insert (SetBuffer ptr0 valA0)
        $ Set.insert (SetOnOff ptr0 valB0)
        $ Set.insert (SetPlaybackRate ptr0 valC0)
        $ Set.insert (SetLoopStart ptr0 valD0)
        $ Set.insert (SetLoopEnd ptr0 valE0) set
  go (vA@(MakeLoopBuf _ _ _ _ _ _) : t0) l1 set =
    go t0 l1 (Set.insert vA set)
  go l0 (MakeLoopBuf ptr1 _ _ _ _ _ : t1) set =
    go l0 t1 (Set.insert (DestroyUnit ptr1 "LoopBuf") set)
  go l0@(vA@(MakeLoopBufWithDeferredBuffer ptr0) : t0) l1@(MakeLoopBufWithDeferredBuffer ptr1 : t1) set
    | ptr0 < ptr1 = go t0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 t1 (Set.insert (DestroyUnit ptr1 "LoopBuf") set)
    | otherwise = go t0 t1 set
  go (vA@(MakeLoopBufWithDeferredBuffer _) : t0) l1 set =
    go t0 l1 (Set.insert vA set)
  go l0 (MakeLoopBufWithDeferredBuffer ptr1 : t1) set =
    go l0 t1 (Set.insert (DestroyUnit ptr1 "LoopBuf") set)
  go l0@(vA@(MakeLowpass ptr0 valA0 valB0) : t0) l1@(MakeLowpass ptr1 _ _ : t1) set
    | ptr0 < ptr1 = go t0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 t1 (Set.insert (DestroyUnit ptr1 "Lowpass") set)
    | otherwise = go t0 t1 (Set.insert (SetFrequency ptr0 valA0) $ Set.insert (SetQ ptr0 valB0) set)
  go (vA@(MakeLowpass _ _ _) : t0) l1 set =
    go t0 l1 (Set.insert vA set)
  go l0 (MakeLowpass ptr1 _ _ : t1) set =
    go l0 t1 (Set.insert (DestroyUnit ptr1 "Lowpass") set)
  go l0@(vA@(MakeLowshelf ptr0 valA0 valB0) : t0) l1@(MakeLowshelf ptr1 _ _ : t1) set
    | ptr0 < ptr1 = go t0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 t1 (Set.insert (DestroyUnit ptr1 "Lowshelf") set)
    | otherwise = go t0 t1 (Set.insert (SetFrequency ptr0 valA0) $ Set.insert (SetGain ptr0 valB0) set)
  go (vA@(MakeLowshelf _ _ _) : t0) l1 set =
    go t0 l1 (Set.insert vA set)
  go l0 (MakeLowshelf ptr1 _ _ : t1) set =
    go l0 t1 (Set.insert (DestroyUnit ptr1 "Lowshelf") set)
  go (MakeMicrophone _ : t0) (MakeMicrophone _ : t1) set = go t0 t1 set
  go (vA@(MakeMicrophone _) : t0) l1 set =
    go t0 l1 (Set.insert vA set)
  go l0 (MakeMicrophone _ : t1) set =
    go l0 t1 (Set.insert (DestroyUnit "microphone" "Microphone") set)
  go l0@(vA@(MakeNotch ptr0 valA0 valB0) : t0) l1@(MakeNotch ptr1 _ _ : t1) set
    | ptr0 < ptr1 = go t0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 t1 (Set.insert (DestroyUnit ptr1 "Notch") set)
    | otherwise = go t0 t1 (Set.insert (SetFrequency ptr0 valA0) $ Set.insert (SetQ ptr0 valB0) set)
  go (vA@(MakeNotch _ _ _) : t0) l1 set =
    go t0 l1 (Set.insert vA set)
  go l0 (MakeNotch ptr1 _ _ : t1) set =
    go l0 t1 (Set.insert (DestroyUnit ptr1 "Notch") set)
  go l0@(vA@(MakePeaking ptr0 valA0 valB0 valC0) : t0) l1@(MakePeaking ptr1 _ _ _ : t1) set
    | ptr0 < ptr1 = go t0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 t1 (Set.insert (DestroyUnit ptr1 "Peaking") set)
    | otherwise = go t0 t1
        $ Set.insert (SetFrequency ptr0 valA0)
        $ Set.insert (SetQ ptr0 valB0)
        $ Set.insert (SetGain ptr0 valC0) set
  go (vA@(MakePeaking _ _ _ _) : t0) l1 set =
    go t0 l1 (Set.insert vA set)
  go l0 (MakePeaking ptr1 _ _ _ : t1) set =
    go l0 t1 (Set.insert (DestroyUnit ptr1 "Peaking") set)
  go l0@(vA@(MakePeriodicOscWithDeferredOsc ptr0) : t0) l1@(MakePeriodicOscWithDeferredOsc ptr1 : t1) set
    | ptr0 < ptr1 = go t0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 t1 (Set.insert (DestroyUnit ptr1 "PeriodicOsc") set)
    | otherwise = go t0 t1 set
  go (vA@(MakePeriodicOscWithDeferredOsc _) : t0) l1 set =
    go t0 l1 (Set.insert vA set)
  go l0 (MakePeriodicOscWithDeferredOsc ptr1 : t1) set =
    go l0 t1 (Set.insert (DestroyUnit ptr1 "PeriodicOsc") set)
  go l0@(vA@(MakePeriodicOsc ptr0 valA0 valB0 valC0) : t0) l1@(MakePeriodicOsc ptr1 _ _ _ : t1) set
    | ptr0 < ptr1 = go t0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 t1 (Set.insert (DestroyUnit ptr1 "PeriodicOsc") set)
    | otherwise = go t0 t1 $ Set.insert (SetPeriodicOsc ptr0 valA0) $ Set.insert (SetOnOff ptr0 valB0) $ Set.insert (SetFrequency ptr0 valC0) set
  go (vA@(MakePeriodicOsc _ _ _ _) : t0) l1 set =
    go t0 l1 (Set.insert vA set)
  go l0 (MakePeriodicOsc ptr1 _ _ _ : t1) set =
    go l0 t1 (Set.insert (DestroyUnit ptr1 "PeriodicOsc") set)
  go l0@(vA@(MakePlayBuf ptr0 valA0 valB0 valC0 valD0) : t0) l1@(MakePlayBuf ptr1 _ _ _ _ : t1) set
    | ptr0 < ptr1 = go t0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 t1 (Set.insert (DestroyUnit ptr1 "PlayBuf") set)
    | otherwise = go t0 t1
        $ Set.insert (SetBuffer ptr0 valA0)
        $ Set.insert (SetBufferOffset ptr0 valB0)
        $ Set.insert (SetOnOff ptr0 valC0)
        $ Set.insert (SetPlaybackRate ptr0 valD0) set
  go (vA@(MakePlayBuf _ _ _ _ _) : t0) l1 set =
    go t0 l1 (Set.insert vA set)
  go l0 (MakePlayBuf ptr1 _ _ _ _ : t1) set =
    go l0 t1 (Set.insert (DestroyUnit ptr1 "PlayBuf") set)
  go l0@(vA@(MakePlayBufWithDeferredBuffer ptr0) : t0) l1@(MakePlayBufWithDeferredBuffer ptr1 : t1) set
    | ptr0 < ptr1 = go t0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 t1 (Set.insert (DestroyUnit ptr1 "PlayBuf") set)
    | otherwise = go t0 t1 set
  go (vA@(MakePlayBufWithDeferredBuffer _) : t0) l1 set =
    go t0 l1 (Set.insert vA set)
  go l0 (MakePlayBufWithDeferredBuffer ptr1 : t1) set =
    go l0 t1 (Set.insert (DestroyUnit ptr1 "PlayBuf") set)
  go l0@(vA@(MakeRecorder ptr0 _) : t0) l1@(MakeRecorder ptr1 _ : t1) set
    | ptr0 < ptr1 = go t0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 t1 (Set.insert (DestroyUnit ptr1 "Recorder") set)
    | otherwise = go t0 t1 set
  go (vA@(MakeRecorder _ _) : t0) l1 set =
    go t0 l1 (Set.insert vA set)
  go l0 (MakeRecorder ptr1 _ : t1) set =
    go l0 t1 (Set.insert (DestroyUnit ptr1 "Recorder") set)
  go l0@(vA@(MakeSawtoothOsc ptr0 valA0 valB0) : t0) l1@(MakeSawtoothOsc ptr1 _ _ : t1) set
    | ptr0 < ptr1 = go t0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 t1 (Set.insert (DestroyUnit ptr1 "SawtoothOsc") set)
    | otherwise = go t0 t1 $ Set.insert (SetOnOff ptr0 valA0) $ Set.insert (SetFrequency ptr0 valB0) set
  go (vA@(MakeSawtoothOsc _ _ _) : t0) l1 set =
    go t0 l1 (Set.insert vA set)
  go l0 (MakeSawtoothOsc ptr1 _ _ : t1) set =
    go l0 t1 (Set.insert (DestroyUnit ptr1 "SawtoothOsc") set)
  go l0@(vA@(MakeSinOsc ptr0 valA0 valB0) : t0) l1@(MakeSinOsc ptr1 _ _ : t1) set
    | ptr0 < ptr1 = go t0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 t1 (Set.insert (DestroyUnit ptr1 "SinOsc") set)
    | otherwise = go t0 t1 $ Set.insert (SetOnOff ptr0 valA0) $ Set.insert (SetFrequency ptr0 valB0) set
  go (vA@(MakeSinOsc _ _ _) : t0) l1 set =
    go t0 l1 (Set.insert vA set)
  go l0 (MakeSinOsc ptr1 _ _ : t1) set =
    go l0 t1 (Set.insert (DestroyUnit ptr1 "SinOsc") set)
  go l0@(vA@(MakeSquareOsc ptr0 valA0 valB0) : t0) l1@(MakeSquareOsc ptr1 _ _ : t1) set
    | ptr0 < ptr1 = go t0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 t1 (Set.insert (DestroyUnit ptr1 "SquareOsc") set)
    | otherwise = go t0 t1 $ Set.insert (SetOnOff ptr0 valA0) $ Set.insert (SetFrequency ptr0 valB0) set
  go (vA@(MakeSquareOsc _ _ _) : t0) l1 set =
    go t0 l1 (Set.insert vA set)
  go l0 (MakeSquareOsc ptr1 _ _ : t1) set =
    go l0 t1 (Set.insert (DestroyUnit ptr1 "SquareOsc") set)
  go (MakeSpeaker : t0) (MakeSpeaker : t1) set = go t0 t1 set
  go (vA@(MakeSpeaker) : t0) l1 set =
    go t0 l1 (Set.insert vA set)
  go l0 (MakeSpeaker : t1) set =
    go l0 t1 (Set.insert (DestroyUnit "speaker" "Speaker") set)
  go l0@(vA@(MakeStereoPanner ptr0 valA0) : t0) l1@(MakeStereoPanner ptr1 _ : t1) set
    | ptr0 < ptr1 = go t0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 t1 (Set.insert (DestroyUnit ptr1 "StereoPanner") set)
    | otherwise = go t0 t1 (Set.insert (SetPan ptr0 valA0) set)
  go (vA@(MakeStereoPanner _ _) : t0) l1 set =
    go t0 l1 (Set.insert vA set)
  go l0 (MakeStereoPanner ptr1 _ : t1) set =
    go l0 t1 (Set.insert (DestroyUnit ptr1 "StereoPanner") set)
  go l0@(vA@(MakeTriangleOsc ptr0 valA0 valB0) : t0) l1@(MakeTriangleOsc ptr1 _ _ : t1) set
    | ptr0 < ptr1 = go t0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 t1 (Set.insert (DestroyUnit ptr1 "TriangleOsc") set)
    | otherwise = go t0 t1 $ Set.insert (SetOnOff ptr0 valA0) $ Set.insert (SetFrequency ptr0 valB0) set
  go (vA@(MakeTriangleOsc _ _ _) : t0) l1 set =
    go t0 l1 (Set.insert vA set)
  go l0 (MakeTriangleOsc ptr1 _ _ : t1) set =
    go l0 t1 (Set.insert (DestroyUnit ptr1 "TriangleOsc") set)
  go l0@(vA@(MakeWaveShaper ptr0 _ _) : t0) l1@(MakeWaveShaper ptr1 _ _ : t1) set
    | ptr0 < ptr1 = go t0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 t1 (Set.insert (DestroyUnit ptr1 "WaveShaper") set)
    | otherwise = go t0 t1 set
  go (vA@(MakeWaveShaper _ _ _) : t0) l1 set =
    go t0 l1 (Set.insert vA set)
  go l0 (MakeWaveShaper ptr1 _ _ : t1) set =
    go l0 t1 (Set.insert (DestroyUnit ptr1 "WaveShaper") set)
  -- todo: find implementation for subgraph and tumult
  -- issue is that, because we are building a flat list, we cannot adequately
  -- create the nesting necessary for subgraphs
  -- need to change the data structure
  go l0@(vA@(MakeSubgraph ptr0 _) : t0) l1@(MakeSubgraph ptr1 _ : t1) set
    | ptr0 < ptr1 = go t0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 t1 (Set.insert (DestroyUnit ptr1 "Subgraph") set)
    | otherwise = go t0 t1 set
  go (vA@(MakeSubgraph _ _) : t0) l1 set =
    go t0 l1 (Set.insert vA set)
  go l0 (MakeSubgraph ptr1 _ : t1) set =
    go l0 t1 (Set.insert (DestroyUnit ptr1 "Subgraph") set)
  go l0@(vA@(MakeSubgraphWithDeferredScene ptr0) : t0) l1@(MakeSubgraphWithDeferredScene ptr1 : t1) set
    | ptr0 < ptr1 = go t0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 t1 (Set.insert (DestroyUnit ptr1 "Subgraph") set)
    | otherwise = go t0 t1 set
  go (vA@(MakeSubgraphWithDeferredScene _) : t0) l1 set =
    go t0 l1 (Set.insert vA set)
  go l0 (MakeSubgraphWithDeferredScene ptr1 : t1) set =
    go l0 t1 (Set.insert (DestroyUnit ptr1 "Subgraph") set)
  go l0@(vA@(MakeTumult ptr0 _ _) : t0) l1@(MakeTumult ptr1 _ _ : t1) set
    | ptr0 < ptr1 = go t0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 t1 (Set.insert (DestroyUnit ptr1 "Tumult") set)
    | otherwise = go t0 t1 set
  go (vA@(MakeTumult _ _ _) : t0) l1 set =
    go t0 l1 (Set.insert vA set)
  go l0 (MakeTumult ptr1 _ _ : t1) set =
    go l0 t1 (Set.insert (DestroyUnit ptr1 "Tumult") set)
  go l0@(vA@(MakeTumultWithDeferredGraph ptr0) : t0) l1@(MakeTumultWithDeferredGraph ptr1 : t1) set
    | ptr0 < ptr1 = go t0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 t1 (Set.insert (DestroyUnit ptr1 "Tumult") set)
    | otherwise = go t0 t1 set
  go (vA@(MakeTumultWithDeferredGraph _) : t0) l1 set =
    go t0 l1 (Set.insert vA set)
  go l0 (MakeTumultWithDeferredGraph ptr1 : t1) set =
    go l0 t1 (Set.insert (DestroyUnit ptr1 "Tumult") set)
  go (DestroyUnit x0 xName : t0) l1 set = go t0 l1 $ Set.insert (DestroyUnit x0 xName) set
  go l0 (DestroyUnit _ _ : t1) set = go l0 t1 set
  go (DisconnectXFromY x0 xName y0 yName : t0) l1 set = go t0 l1 $ Set.insert (DisconnectXFromY x0 xName y0 yName) set
  go l0 (DisconnectXFromY _ _ _ _ : t1) set = go l0 t1 set
  go (SetAnalyserNodeCb x0 y0 : t0) l1 set = go t0 l1 $ Set.insert (SetAnalyserNodeCb x0 y0) set
  go l0 (SetAnalyserNodeCb _ _ : t1) set = go l0 t1 set
  go (SetMediaRecorderCb x0 y0 : t0) l1 set = go t0 l1 $ Set.insert (SetMediaRecorderCb x0 y0) set
  go l0 (SetMediaRecorderCb _ _ : t1) set = go l0 t1 set
  go (SetAudioWorkletParameter x0 y0 z0 : t0) l1 set = go t0 l1 $ Set.insert (SetAudioWorkletParameter x0 y0 z0) set
  go l0 (SetAudioWorkletParameter _ _ _ : t1) set = go l0 t1 set
  go (SetBuffer x0 y0 : t0) l1 set = go t0 l1 $ Set.insert (SetBuffer x0 y0) set
  go l0 (SetBuffer _ _ : t1) set = go l0 t1 set
  go (SetConvolverBuffer x0 y0 : t0) l1 set = go t0 l1 $ Set.insert (SetConvolverBuffer x0 y0) set
  go l0 (SetConvolverBuffer _ _ : t1) set = go l0 t1 set
  go (SetPeriodicOsc x0 y0 : t0) l1 set = go t0 l1 $ Set.insert (SetPeriodicOsc x0 y0) set
  go l0 (SetPeriodicOsc _ _ : t1) set = go l0 t1 set
  go (SetOnOff x0 y0 : t0) l1 set = go t0 l1 $ Set.insert (SetOnOff x0 y0) set
  go l0 (SetOnOff _ _ : t1) set = go l0 t1 set
  go (SetBufferOffset x0 y0 : t0) l1 set = go t0 l1 $ Set.insert (SetBufferOffset x0 y0) set
  go l0 (SetBufferOffset _ _ : t1) set = go l0 t1 set
  go (SetLoopStart x0 y0 : t0) l1 set = go t0 l1 $ Set.insert (SetLoopStart x0 y0) set
  go l0 (SetLoopStart _ _ : t1) set = go l0 t1 set
  go (SetLoopEnd x0 y0 : t0) l1 set = go t0 l1 $ Set.insert (SetLoopEnd x0 y0) set
  go l0 (SetLoopEnd _ _ : t1) set = go l0 t1 set
  go (SetRatio x0 y0 : t0) l1 set = go t0 l1 $ Set.insert (SetRatio x0 y0) set
  go l0 (SetRatio _ _ : t1) set = go l0 t1 set
  go (SetOffset x0 y0 : t0) l1 set = go t0 l1 $ Set.insert (SetOffset x0 y0) set
  go l0 (SetOffset _ _ : t1) set = go l0 t1 set
  go (SetAttack x0 y0 : t0) l1 set = go t0 l1 $ Set.insert (SetAttack x0 y0) set
  go l0 (SetAttack _ _ : t1) set = go l0 t1 set
  go (SetGain x0 y0 : t0) l1 set = go t0 l1 $ Set.insert (SetGain x0 y0) set
  go l0 (SetGain _ _ : t1) set = go l0 t1 set
  go (SetQ x0 y0 : t0) l1 set = go t0 l1 $ Set.insert (SetQ x0 y0) set
  go l0 (SetQ _ _ : t1) set = go l0 t1 set
  go (SetPan x0 y0 : t0) l1 set = go t0 l1 $ Set.insert (SetPan x0 y0) set
  go l0 (SetPan _ _ : t1) set = go l0 t1 set
  go (SetThreshold x0 y0 : t0) l1 set = go t0 l1 $ Set.insert (SetThreshold x0 y0) set
  go l0 (SetThreshold _ _ : t1) set = go l0 t1 set
  go (SetRelease x0 y0 : t0) l1 set = go t0 l1 $ Set.insert (SetRelease x0 y0) set
  go l0 (SetRelease _ _ : t1) set = go l0 t1 set
  go (SetKnee x0 y0 : t0) l1 set = go t0 l1 $ Set.insert (SetKnee x0 y0) set
  go l0 (SetKnee _ _ : t1) set = go l0 t1 set
  go (SetDelay x0 y0 : t0) l1 set = go t0 l1 $ Set.insert (SetDelay x0 y0) set
  go l0 (SetDelay _ _ : t1) set = go l0 t1 set
  go (SetPlaybackRate x0 y0 : t0) l1 set = go t0 l1 $ Set.insert (SetPlaybackRate x0 y0) set
  go l0 (SetPlaybackRate _ _ : t1) set = go l0 t1 set
  go (SetFrequency x0 y0 : t0) l1 set = go t0 l1 $ Set.insert (SetFrequency x0 y0) set
  go l0 (SetFrequency _ _ : t1) set = go l0 t1 set
  go (SetWaveShaperCurve x0 y0 : t0) l1 set = go t0 l1 $ Set.insert (SetWaveShaperCurve x0 y0) set
  go l0 (SetWaveShaperCurve _ _ : t1) set = go l0 t1 set
  go (SetInput x0 y0 : t0) l1 set = go t0 l1 $ Set.insert (SetInput x0 y0) set
  go l0 (SetInput _ _ : t1) set = go l0 t1 set
  go (SetSubgraph x0 y0 : t0) l1 set = go t0 l1 $ Set.insert (SetSubgraph x0 y0) set
  go l0 (SetSubgraph _ _ : t1) set = go l0 t1 set
  go (SetTumult x0 y0 z0 : t0) l1 set = go t0 l1 $ Set.insert (SetTumult x0 y0 z0) set
  go l0 (SetTumult _ _ _ : t1) set = go l0 t1 set
-}