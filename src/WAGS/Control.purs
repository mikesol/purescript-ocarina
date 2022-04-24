module WAGS.Control where

import Prelude

import Control.Alt ((<|>))
import Control.Comonad (extract)
import Control.Plus (empty)
import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Data.Either (Either(..))
import Data.Foldable (oneOf)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Homogeneous (class HomogeneousRowLabels)
import Data.Homogeneous.Variant (homogeneous)
import Data.Int (pow)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Typelevel.Num (class Lt, class Nat, class Pos, class Pred, D1, D2, pred, toInt)
import Data.Variant (Unvariant(..), match, unvariant)
import Data.Variant.Maybe (Maybe, just, nothing)
import Data.Vec (Vec, toArray)
import Effect (Effect)
import Effect.AVar (tryPut)
import Effect.AVar as AVar
import Effect.Exception (throwException)
import FRP.Event (Event, makeEvent, subscribe)
import FRP.Event.Class (bang)
import Foreign.Object (fromHomogeneous)
import Safe.Coerce (coerce)
import Simple.JSON as JSON
import Type.Equality (proof)
import Type.Proxy (Proxy(..))
import Type.Row.Homogeneous (class Homogeneous)
import Unsafe.Coerce (unsafeCoerce)
import WAGS.Common as Common
import WAGS.Core (ChannelCountMode(..), ChannelInterpretation(..), Po2(..), __internalWagsFlatten, mix)
import WAGS.Core as C
import WAGS.Parameter (AudioParameter, InitialAudioParameter)
import WAGS.WebAPI (AnalyserNodeCb(..), BrowserAudioBuffer)

-- allpass

allpass
  :: forall i aud (outputChannels :: Type) lock payload
   . Common.InitialAllpass i
  => C.Mix aud (C.Streamy outputChannels lock payload)
  => i
  -> Event C.Allpass
  -> aud -- Array (C.Node outputChannels lock payload)
  -> C.Node outputChannels lock payload
allpass i' atts elts = C.Node go
  where
  C.InitializeAllpass i = Common.toInitializeAllpass i'
  go parent di@(C.AudioInterpret { ids, makeAllpass, setFrequency, setQ }) =     makeEvent \k -> do
      me <- ids
      parent.raiseId me
      flip subscribe k $

        bang
          ( makeAllpass
              { id: me, parent: just parent.parent, scope: parent.scope, frequency: i.frequency, q: i.q }
          )
          <|> map
            ( \(C.Allpass e) -> match
                { frequency: \g -> setFrequency { id: me, frequency: g }
                , q: \g -> setQ { id: me, q: g }
                }
                e
            )
            atts
          <|> __internalWagsFlatten me di (mix elts)


allpass_
  :: forall i aud (outputChannels :: Type) lock payload
   . Common.InitialAllpass i
  => C.Mix aud (C.Streamy outputChannels lock payload)
  => i
  -> aud
  -> C.Node outputChannels lock payload
allpass_ i a = allpass i empty a

-- analyser

data AnalyserOptions = AnalyserOptions

instance
  ConvertOption AnalyserOptions
    "playbackRate"
    InitialAudioParameter
    InitialAudioParameter where
  convertOption _ _ = identity

instance
  ConvertOption AnalyserOptions
    "channelInterpretation"
    ChannelInterpretation
    ChannelInterpretation where
  convertOption _ _ = identity

instance
  ConvertOption AnalyserOptions
    "channelCountMode"
    ChannelCountMode
    ChannelCountMode where
  convertOption _ _ = identity

instance ConvertOption AnalyserOptions "channelCount" Int Int where
  convertOption _ _ = identity

instance ConvertOption AnalyserOptions "smoothingTimeConstant" Number Number where
  convertOption _ _ = identity

instance ConvertOption AnalyserOptions "maxDecibels" Number Number where
  convertOption _ _ = identity

instance ConvertOption AnalyserOptions "minDecibels" Number Number where
  convertOption _ _ = identity

instance ConvertOption AnalyserOptions "fftSize" Po2 Po2 where
  convertOption _ _ = identity

instance
  ConvertOption AnalyserOptions "cb" AnalyserNodeCb AnalyserNodeCb where
  convertOption _ _ = identity

type AnalyserOptional =
  ( cb :: AnalyserNodeCb
  , fftSize :: Po2
  , maxDecibels :: Number
  , minDecibels :: Number
  , smoothingTimeConstant :: Number
  , channelCount :: Int
  , channelCountMode :: ChannelCountMode
  , channelInterpretation :: ChannelInterpretation
  )

type AnalyserAll = (| AnalyserOptional)

defaultAnalyser :: { | AnalyserOptional }
defaultAnalyser =
  { cb: AnalyserNodeCb \_ -> pure (pure unit)
  , fftSize: TTT11
  , maxDecibels: -30.0
  , minDecibels: -100.0
  , smoothingTimeConstant: 0.8
  , channelCount: 2
  , channelCountMode: Max
  , channelInterpretation: Speakers
  }

class InitialAnalyser i where
  toInitializeAnalyser :: i -> C.InitializeAnalyser

instance InitialAnalyser C.InitializeAnalyser where
  toInitializeAnalyser = identity

instance InitialAnalyser AnalyserNodeCb where
  toInitializeAnalyser cb = toInitializeAnalyser { cb }

instance
  ConvertOptionsWithDefaults AnalyserOptions { | AnalyserOptional }
    { | provided }
    { | AnalyserAll } =>
  InitialAnalyser { | provided } where
  toInitializeAnalyser provided = C.InitializeAnalyser
    (convertOptionsWithDefaults AnalyserOptions defaultAnalyser provided)

analyser
  :: forall i aud outputChannels lock payload
   . InitialAnalyser i
  => C.Mix aud (C.Streamy outputChannels lock payload)
  => i
  -> Event C.Analyser
  -> aud
  -> C.Node outputChannels lock payload
analyser i' atts elts = C.Node go
  where
  C.InitializeAnalyser i = toInitializeAnalyser i'
  go
    parent
    di@(C.AudioInterpret { ids, makeAnalyser, setAnalyserNodeCb }) =
    makeEvent \k -> do
      me <- ids
      parent.raiseId me
      flip subscribe k $
        bang
          ( makeAnalyser
              { id: me
              , parent: just parent.parent
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
            ( \(C.Analyser e) -> match
                { cb: \cb -> setAnalyserNodeCb { id: me, cb }
                }
                e
            )
            atts
          <|> __internalWagsFlatten me di (mix elts)

analyser_
  :: forall i aud outputChannels lock payload
   . InitialAnalyser i
  => C.Mix aud (C.Streamy outputChannels lock payload)
  => i
  -> C.Node outputChannels lock payload
  -> C.Node outputChannels lock payload
analyser_ i = analyser i empty

-- audio worklet

class
  ValidateOutputChannelCount
    (numberOfOutputs :: Type)
    (outputChannelCount :: Type) where
  toOutputChannelCount
    :: numberOfOutputs
    -> outputChannelCount
    -> Array Int

instance validateOutputChannelCountD1 ::
  Pos n =>
  ValidateOutputChannelCount D1 n where
  toOutputChannelCount _ n = [ toInt n ]
else instance validateOutputChannelCountN ::
  ( Pred x xMinus1
  , Pos n
  , ValidateOutputChannelCount xMinus1 r
  ) =>
  ValidateOutputChannelCount x (n /\ r) where
  toOutputChannelCount x (n /\ r) = [ toInt n ] <> toOutputChannelCount
    (pred x)
    r

__audioWorklet
  :: forall name numberOfInputs numberOfOutputs outputChannelCount parameterData
       parameterDataRL
       processorOptions lock payload
   . IsSymbol name
  => Nat numberOfInputs
  => Pos numberOfOutputs
  => ValidateOutputChannelCount numberOfOutputs outputChannelCount
  => Homogeneous parameterData InitialAudioParameter
  => HomogeneousRowLabels parameterData AudioParameter parameterDataRL
  => JSON.WriteForeign { | processorOptions }
  => C.InitializeAudioWorkletNode name numberOfInputs numberOfOutputs
       outputChannelCount
       parameterData
       processorOptions
  -> Event (C.AudioWorkletNode parameterData)
  -> C.Node numberOfOutputs lock payload
  -> C.Node numberOfOutputs lock payload
__audioWorklet (C.InitializeAudioWorkletNode i) atts elt = C.Node go
  where
  go
    parent
    di@
      ( C.AudioInterpret
          { ids, makeAudioWorkletNode, setAudioWorkletParameter }
      ) =
    makeEvent \k -> do
      me <- ids
      parent.raiseId me
      flip subscribe k $
        bang
          ( makeAudioWorkletNode
              { id: me
              , parent: just parent.parent
              , scope: parent.scope
              , options:
                  C.AudioWorkletNodeOptions_
                    { name: reflectSymbol (Proxy :: _ name)
                    , numberOfInputs: toInt i.numberOfInputs
                    , numberOfOutputs: toInt i.numberOfOutputs
                    , outputChannelCount: toOutputChannelCount
                        i.numberOfOutputs
                        i.outputChannelCount
                    , parameterData: fromHomogeneous i.parameterData
                    , processorOptions: JSON.writeImpl i.processorOptions
                    }
              }
          )
          <|> map
            ( \(C.AudioWorkletNode e) -> setAudioWorkletParameter
                { id: me
                , paramName: (let Unvariant e' = unvariant e in e')
                    (\sym _ -> reflectSymbol sym)
                , paramValue: extract (homogeneous e)
                }
            )
            atts
          <|> __internalWagsFlatten me di (mix elt)

audioWorklet
  :: forall name numberOfInputs numberOfOutputs outputChannelCount parameterData
       parameterDataRL
       processorOptions lock payload
   . IsSymbol name
  => Nat numberOfInputs
  => Pos numberOfOutputs
  => ValidateOutputChannelCount numberOfOutputs outputChannelCount
  => Homogeneous parameterData InitialAudioParameter
  => HomogeneousRowLabels parameterData AudioParameter parameterDataRL
  => JSON.WriteForeign { | processorOptions }
  => C.InitializeAudioWorkletNode name numberOfInputs numberOfOutputs
       outputChannelCount
       parameterData
       processorOptions
  -> Event (C.AudioWorkletNode parameterData)
  -> C.Node numberOfOutputs lock payload
  -> C.Node numberOfOutputs lock payload
audioWorklet = __audioWorklet

-- bandpass
bandpass
  :: forall i aud (outputChannels :: Type) lock payload
   . Common.InitialBandpass i
  => C.Mix aud (C.Streamy outputChannels lock payload)
  => i
  -> Event C.Bandpass
  -> aud
  -> C.Node outputChannels lock payload
bandpass i' atts elts = C.Node go
  where
  C.InitializeBandpass i = Common.toInitializeBandpass i'
  go parent di@(C.AudioInterpret { ids, makeBandpass, setFrequency, setQ }) =     makeEvent \k -> do
      me <- ids
      parent.raiseId me
      flip subscribe k $

        bang
          ( makeBandpass
              { id: me, parent: just parent.parent, scope: parent.scope, frequency: i.frequency, q: i.q }
          )
          <|> map
            ( \(C.Bandpass e) -> match
                { frequency: \g -> setFrequency { id: me, frequency: g }
                , q: \g -> setQ { id: me, q: g }
                }
                e
            )
            atts
          <|> __internalWagsFlatten me di (mix elts)


bandpass_
  :: forall i aud (outputChannels :: Type) lock payload
   . Common.InitialBandpass i
  => C.Mix aud (C.Streamy outputChannels lock payload)
  => i
  -> aud
  -> C.Node outputChannels lock payload
bandpass_ i a = bandpass i empty a

-- constant

__constant
  :: forall i outputChannels lock payload
   . Common.InitialConstant i
  => i
  -> Event C.Constant
  -> C.Node outputChannels lock payload
__constant i' atts = C.Node go
  where
  C.InitializeConstant i = Common.toInitializeConstant i'
  go parent (C.AudioInterpret { ids, makeConstant, setOffset, setOnOff }) =
    makeEvent \k -> do
      me <- ids
      parent.raiseId me
      flip subscribe k $
        bang
          ( makeConstant
              { id: me
              , parent: just parent.parent
              , scope: parent.scope
              , offset: i.offset
              }
          )
          <|> map
            ( \(C.Constant e) -> match
                { offset: \offset -> setOffset
                    { id: me, offset }
                , onOff: \onOff -> setOnOff { id: me, onOff }
                }
                e
            )
            atts

constant
  :: forall i outputChannels lock payload
   . Common.InitialConstant i
  => i
  -> Event C.Constant
  -> C.Node outputChannels lock payload
constant = __constant

constant_
  :: forall i outputChannels lock payload
   . Common.InitialConstant i
  => i
  -> C.Node outputChannels lock payload
constant_ i = constant i empty

-- convolver

convolver
  :: forall i aud (outputChannels :: Type) lock payload
   . Common.InitialConvolver i
  => C.Mix aud (C.Streamy outputChannels lock payload)
  => i
  -> aud
  -> C.Node outputChannels lock payload
convolver i' elts = C.Node go
  where
  C.InitializeConvolver i = Common.toInitializeConvolver i'
  go
    parent
    di@
      ( C.AudioInterpret
          { ids
          , makeConvolver
          }
      ) =
    makeEvent \k -> do
      me <- ids
      parent.raiseId me
      flip subscribe k $
        bang
          ( makeConvolver
              { id: me
              , parent: just parent.parent
              , scope: parent.scope
              , buffer: i.buffer
              }
          )
          <|> __internalWagsFlatten me di (mix elts)

-- delay
delay
  :: forall i aud (outputChannels :: Type) lock payload
   . Common.InitialDelay i
  => C.Mix aud (C.Streamy outputChannels lock payload)
  => i
  -> Event C.Delay
  -> aud
  -> C.Node outputChannels lock payload
delay i' atts elts = C.Node go
  where
  C.InitializeDelay i = Common.toInitializeDelay i'
  go parent di@(C.AudioInterpret { ids, makeDelay, setDelay }) =     makeEvent \k -> do
      me <- ids
      parent.raiseId me
      flip subscribe k $

        bang
          ( makeDelay
              { id: me, parent: just parent.parent, scope: parent.scope, delayTime: i.delayTime, maxDelayTime: i.maxDelayTime }
          )
          <|> map
            ( \(C.Delay e) -> match
                { delayTime: \g -> setDelay { id: me, delayTime: g }
                }
                e
            )
            atts
          <|> __internalWagsFlatten me di (mix elts)


delay_
  :: forall i aud (outputChannels :: Type) lock payload
   . Common.InitialDelay i
  => C.Mix aud (C.Streamy outputChannels lock payload)
  => i
  -> aud
  -> C.Node outputChannels lock payload
delay_ i a = delay i empty a

-- dynamics compressor
dynamicsCompressor
  :: forall i aud (outputChannels :: Type) lock payload
   . Common.InitialDynamicsCompressor i
  => C.Mix aud (C.Streamy outputChannels lock payload)
  => i
  -> Event C.DynamicsCompressor
  -> aud
  -> C.Node outputChannels lock payload
dynamicsCompressor i' atts elts = C.Node go
  where
  C.InitializeDynamicsCompressor i = Common.toInitializeDynamicsCompressor i'
  go
    parent
    di@
      ( C.AudioInterpret
          { ids
          , makeDynamicsCompressor
          , setThreshold
          , setRatio
          , setKnee
          , setAttack
          , setRelease
          }
      ) =
    makeEvent \k -> do
      me <- ids
      parent.raiseId me
      flip subscribe k $
        bang
          ( makeDynamicsCompressor
              { id: me
              , parent: just parent.parent
              , scope: parent.scope
              , threshold: i.threshold
              , ratio: i.ratio
              , knee: i.knee
              , attack: i.attack
              , release: i.release
              }
          )
          <|> map
            ( \(C.DynamicsCompressor e) -> match
                { threshold: \threshold -> setThreshold
                    { id: me, threshold }
                , ratio: \ratio -> setRatio
                    { id: me, ratio }
                , knee: \knee -> setKnee
                    { id: me, knee }
                , attack: \attack -> setAttack
                    { id: me, attack }
                , release: \release -> setRelease
                    { id: me, release }
                }
                e
            )
            atts
          <|> __internalWagsFlatten me di (mix elts)

dynamicsCompressor_
  :: forall i aud (outputChannels :: Type) lock payload
   . Common.InitialDynamicsCompressor i
  => C.Mix aud (C.Streamy outputChannels lock payload)
  => i
  -> aud
  -> C.Node outputChannels lock payload
dynamicsCompressor_ i = dynamicsCompressor i empty

-- gain
gain
  :: forall i aud (outputChannels :: Type) lock payload
   . Common.InitialGain i
  => C.Mix aud (C.Streamy outputChannels lock payload)
  => i
  -> Event C.Gain
  -> aud
  -> C.Node outputChannels lock payload
gain i' atts elts = C.Node go
  where
  C.InitializeGain i = Common.toInitializeGain i'
  go parent di@(C.AudioInterpret { ids, makeGain, setGain }) =     makeEvent \k -> do
      me <- ids
      parent.raiseId me
      flip subscribe k $

        bang
          ( makeGain
              { id: me, parent: just parent.parent, scope: parent.scope, gain: i.gain }
          )
          <|> map
            ( \(C.Gain e) -> match
                { gain: \g -> setGain { id: me, gain: g }
                }
                e
            )
            atts
          <|> __internalWagsFlatten me di (mix elts)


gain_
  :: forall i aud (outputChannels :: Type) lock payload
   . Common.InitialGain i
  => C.Mix aud (C.Streamy outputChannels lock payload)
  => i
  -> aud
  -> C.Node outputChannels lock payload
gain_ i a = gain i empty a

-- highpass
highpass
  :: forall i aud (outputChannels :: Type) lock payload
   . Common.InitialHighpass i
  => C.Mix aud (C.Streamy outputChannels lock payload)
  => i
  -> Event C.Highpass
  -> aud
  -> C.Node outputChannels lock payload
highpass i' atts elts = C.Node go
  where
  C.InitializeHighpass i = Common.toInitializeHighpass i'
  go parent di@(C.AudioInterpret { ids, makeHighpass, setFrequency, setQ }) =     makeEvent \k -> do
      me <- ids
      parent.raiseId me
      flip subscribe k $

        bang
          ( makeHighpass
              { id: me, parent: just parent.parent, scope: parent.scope, frequency: i.frequency, q: i.q }
          )
          <|> map
            ( \(C.Highpass e) -> match
                { frequency: \g -> setFrequency { id: me, frequency: g }
                , q: \g -> setQ { id: me, q: g }
                }
                e
            )
            atts
          <|> __internalWagsFlatten me di (mix elts)


highpass_
  :: forall i aud (outputChannels :: Type) lock payload
   . Common.InitialHighpass i
  => C.Mix aud (C.Streamy outputChannels lock payload)
  => i
  -> aud
  -> C.Node outputChannels lock payload
highpass_ i a = highpass i empty a

-- highshelf
highshelf
  :: forall i aud (outputChannels :: Type) lock payload
   . Common.InitialHighshelf i
  => C.Mix aud (C.Streamy outputChannels lock payload)
  => i
  -> Event C.Highshelf
  -> aud
  -> C.Node outputChannels lock payload
highshelf i' atts elts = C.Node go
  where
  C.InitializeHighshelf i = Common.toInitializeHighshelf i'
  go parent di@(C.AudioInterpret { ids, makeHighshelf, setFrequency, setGain }) =     makeEvent \k -> do
      me <- ids
      parent.raiseId me
      flip subscribe k $

        bang
          ( makeHighshelf
              { id: me, parent: just parent.parent, scope: parent.scope, frequency: i.frequency, gain: i.gain }
          )
          <|> map
            ( \(C.Highshelf e) -> match
                { frequency: \g -> setFrequency { id: me, frequency: g }
                , gain: \g -> setGain { id: me, gain: g }
                }
                e
            )
            atts
          <|> __internalWagsFlatten me di (mix elts)


highshelf_
  :: forall i aud (outputChannels :: Type) lock payload
   . Common.InitialHighshelf i
  => C.Mix aud (C.Streamy outputChannels lock payload)
  => i
  -> aud
  -> C.Node outputChannels lock payload
highshelf_ i a = highshelf i empty a

-- iirFilter

iirFilter
  :: forall i aud (feedforward :: Type) (feedback :: Type) (outputChannels :: Type) lock
       payload
   . Lt D2 feedforward
  => Lt D2 feedback
  => C.Mix aud (C.Streamy outputChannels lock payload)
  => Common.InitialIIRFilter i feedforward feedback
  => C.Mix aud (C.Streamy outputChannels lock payload)
  => i
  -> aud
  -> C.Node outputChannels lock payload
iirFilter = iirFilter' (Proxy :: _ feedforward) (Proxy :: _ feedback)

iirFilter'
  :: forall i aud proxy (feedforward :: Type) (feedback :: Type) (outputChannels :: Type) lock
       payload
   . Lt D2 feedforward
  => Lt D2 feedback
  => Common.InitialIIRFilter i feedforward feedback
  => C.Mix aud (C.Streamy outputChannels lock payload)
  => proxy feedforward
  -> proxy feedback
  -> i
  -> aud
  -> C.Node outputChannels lock payload
iirFilter' fwd bk i' elts = C.Node go
  where
  C.InitializeIIRFilter i = Common.toInitializeIIRFilter i' fwd bk
  go
    parent
    di@
      ( C.AudioInterpret
          { ids
          , makeIIRFilter
          }
      ) =
    makeEvent \k -> do
      me <- ids
      parent.raiseId me
      flip subscribe k $
        bang
          ( makeIIRFilter
              { id: me
              , parent: just parent.parent
              , scope: parent.scope
              , feedforward: toArray i.feedforward
              , feedback: toArray i.feedback
              }
          )
          <|> __internalWagsFlatten me di (mix elts)

-- lowpass
lowpass
  :: forall i aud (outputChannels :: Type) lock payload
   . Common.InitialLowpass i
  => C.Mix aud (C.Streamy outputChannels lock payload)
  => i
  -> Event C.Lowpass
  -> aud
  -> C.Node outputChannels lock payload
lowpass i' atts elts = C.Node go
  where
  C.InitializeLowpass i = Common.toInitializeLowpass i'
  go parent di@(C.AudioInterpret { ids, makeLowpass, setFrequency, setQ }) =     makeEvent \k -> do
      me <- ids
      parent.raiseId me
      flip subscribe k $

        bang
          ( makeLowpass
              { id: me, parent: just parent.parent, scope: parent.scope, frequency: i.frequency, q: i.q }
          )
          <|> map
            ( \(C.Lowpass e) -> match
                { frequency: \g -> setFrequency { id: me, frequency: g }
                , q: \g -> setQ { id: me, q: g }
                }
                e
            )
            atts
          <|> __internalWagsFlatten me di (mix elts)


lowpass_
  :: forall i aud (outputChannels :: Type) lock payload
   . Common.InitialLowpass i
  => C.Mix aud (C.Streamy outputChannels lock payload)
  => i
  -> aud
  -> C.Node outputChannels lock payload
lowpass_ i a = lowpass i empty a

-- lowshelf
lowshelf
  :: forall i aud (outputChannels :: Type) lock payload
   . Common.InitialLowshelf i
  => C.Mix aud (C.Streamy outputChannels lock payload)
  => i
  -> Event C.Lowshelf
  -> aud
  -> C.Node outputChannels lock payload
lowshelf i' atts elts = C.Node go
  where
  C.InitializeLowshelf i = Common.toInitializeLowshelf i'
  go parent di@(C.AudioInterpret { ids, makeLowshelf, setFrequency, setGain }) =     makeEvent \k -> do
      me <- ids
      parent.raiseId me
      flip subscribe k $

        bang
          ( makeLowshelf
              { id: me, parent: just parent.parent, scope: parent.scope, frequency: i.frequency, gain: i.gain }
          )
          <|> map
            ( \(C.Lowshelf e) -> match
                { frequency: \g -> setFrequency { id: me, frequency: g }
                , gain: \g -> setGain { id: me, gain: g }
                }
                e
            )
            atts
          <|> __internalWagsFlatten me di (mix elts)


lowshelf_
  :: forall i aud (outputChannels :: Type) lock payload
   . Common.InitialLowshelf i
  => C.Mix aud (C.Streamy outputChannels lock payload)
  => i
  -> aud
  -> C.Node outputChannels lock payload
lowshelf_ i a = lowshelf i empty a

-- loopBuf

__loopBuf
  :: forall i outputChannels lock payload
   . Common.InitialLoopBuf i
  => i
  -> Event C.LoopBuf
  -> C.Node outputChannels lock payload
__loopBuf i' atts = C.Node go
  where
  C.InitializeLoopBuf i = Common.toInitializeLoopBuf i'
  go
    parent
    ( C.AudioInterpret
        { ids
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
      flip subscribe k $
        bang
          ( makeLoopBuf
              { id: me
              , parent: just parent.parent
              , scope: parent.scope
              , buffer: i.buffer
              , playbackRate: i.playbackRate
              , loopStart: i.loopStart
              , loopEnd: i.loopEnd
              , duration: i.duration
              }
          )
          <|> map
            ( \(C.LoopBuf e) -> match
                { buffer: \buffer -> setBuffer { id: me, buffer }
                , playbackRate: \playbackRate -> setPlaybackRate
                    { id: me, playbackRate }
                , loopStart: \loopStart -> setLoopStart { id: me, loopStart }
                , loopEnd: \loopEnd -> setLoopEnd { id: me, loopEnd }
                , onOff: \onOff -> setOnOff { id: me, onOff }
                }
                e
            )
            atts

loopBuf
  :: forall i outputChannels lock payload
   . Common.InitialLoopBuf i
  => i
  -> Event C.LoopBuf
  -> C.Node outputChannels lock payload
loopBuf = __loopBuf

loopBuf_
  :: forall i outputChannels lock payload
   . Common.InitialLoopBuf i
  => i
  -> C.Node outputChannels lock payload
loopBuf_ i = loopBuf i empty

-- mediaElement

__mediaElement
  :: forall outputChannels lock payload
   . C.InitializeMediaElement
  -> C.Node outputChannels lock payload
__mediaElement (C.InitializeMediaElement i) = C.Node go
  where
  go parent (C.AudioInterpret { ids, makeMediaElement }) =
    makeEvent \k -> do
      me <- ids
      parent.raiseId me
      flip subscribe k $
        bang
          ( makeMediaElement
              { id: me
              , parent: just parent.parent
              , scope: parent.scope
              , element: i.element
              }
          )

mediaElement
  :: forall outputChannels lock payload
   . C.InitializeMediaElement
  -> C.Node outputChannels lock payload
mediaElement = __mediaElement

-- microphone

__microphone
  :: forall i outputChannels lock payload
   . Common.InitialMicrophone i
  => i
  -> C.Node outputChannels lock payload
__microphone i' = C.Node go
  where
  C.InitializeMicrophone i = Common.toInitializeMicrophone i'
  go parent (C.AudioInterpret { ids, makeMicrophone }) =
    makeEvent \k -> do
      me <- ids
      parent.raiseId me
      flip subscribe k $
        bang
          ( makeMicrophone
              { id: me
              , parent: just parent.parent
              , scope: parent.scope
              , microphone: i.microphone
              }
          )

microphone
  :: forall i outputChannels lock payload
   . Common.InitialMicrophone i
  => i
  -> C.Node outputChannels lock payload
microphone = __microphone

-- notch
notch
  :: forall i aud (outputChannels :: Type) lock payload
   . Common.InitialNotch i
  => C.Mix aud (C.Streamy outputChannels lock payload)
  => i
  -> Event C.Notch
  -> aud
  -> C.Node outputChannels lock payload
notch i' atts elts = C.Node go
  where
  C.InitializeNotch i = Common.toInitializeNotch i'
  go parent di@(C.AudioInterpret { ids, makeNotch, setFrequency, setQ }) =     makeEvent \k -> do
      me <- ids
      parent.raiseId me
      flip subscribe k $

        bang
          ( makeNotch
              { id: me, parent: just parent.parent, scope: parent.scope, frequency: i.frequency, q: i.q }
          )
          <|> map
            ( \(C.Notch e) -> match
                { frequency: \g -> setFrequency { id: me, frequency: g }
                , q: \g -> setQ { id: me, q: g }
                }
                e
            )
            atts
          <|> __internalWagsFlatten me di (mix elts)


notch_
  :: forall i aud (outputChannels :: Type) lock payload
   . Common.InitialNotch i
  => C.Mix aud (C.Streamy outputChannels lock payload)
  => i
  -> aud
  -> C.Node outputChannels lock payload
notch_ i a = notch i empty a

-- peaking
peaking
  :: forall i aud (outputChannels :: Type) lock payload
   . Common.InitialPeaking i
  => C.Mix aud (C.Streamy outputChannels lock payload)
  => i
  -> Event C.Peaking
  -> aud
  -> C.Node outputChannels lock payload
peaking i' atts elts = C.Node go
  where
  C.InitializePeaking i = Common.toInitializePeaking i'
  go parent di@(C.AudioInterpret { ids, makePeaking, setFrequency, setQ, setGain }) =     makeEvent \k -> do
      me <- ids
      parent.raiseId me
      flip subscribe k $

        bang
          ( makePeaking
              { id: me, parent: just parent.parent, scope: parent.scope, frequency: i.frequency, q: i.q, gain: i.gain }
          )
          <|> map
            ( \(C.Peaking e) -> match
                { frequency: \g -> setFrequency { id: me, frequency: g }
                , q: \g -> setQ { id: me, q: g }
                , gain: \g -> setGain { id: me, gain: g }
                }
                e
            )
            atts
          <|> __internalWagsFlatten me di (mix elts)


peaking_
  :: forall i aud (outputChannels :: Type) lock payload
   . Common.InitialPeaking i
  => C.Mix aud (C.Streamy outputChannels lock payload)
  => i
  -> aud
  -> C.Node outputChannels lock payload
peaking_ i a = peaking i empty a

-- periodicOsc

__periodicOsc
  :: forall i outputChannels lock payload
   . Common.InitialPeriodicOsc i
  => i
  -> Event C.PeriodicOsc
  -> C.Node outputChannels lock payload
__periodicOsc i' atts = C.Node go
  where
  C.InitializePeriodicOsc i = Common.toInitializePeriodicOsc i'
  go
    parent
    ( C.AudioInterpret
        { ids, makePeriodicOsc, setFrequency, setOnOff, setPeriodicOsc }
    ) =
    makeEvent \k -> do
      me <- ids
      parent.raiseId me
      flip subscribe k $
        bang
          ( makePeriodicOsc
              { id: me
              , parent: just parent.parent
              , scope: parent.scope
              , frequency: i.frequency
              , spec: i.spec
              }
          )
          <|> map
            ( \(C.PeriodicOsc e) -> match
                { frequency: \frequency -> setFrequency
                    { id: me, frequency }
                , onOff: \onOff -> setOnOff { id: me, onOff }
                , spec: \spec -> setPeriodicOsc { id: me, spec }
                }
                e
            )
            atts

periodicOsc
  :: forall i outputChannels lock payload
   . Common.InitialPeriodicOsc i
  => i
  -> Event C.PeriodicOsc
  -> C.Node outputChannels lock payload
periodicOsc = __periodicOsc

periodicOsc_
  :: forall i outputChannels lock payload
   . Common.InitialPeriodicOsc i
  => i
  -> C.Node outputChannels lock payload
periodicOsc_ i = periodicOsc i empty

-- playBuf

data PlayBufOptions = PlayBufOptions

instance
  ConvertOption PlayBufOptions
    "playbackRate"
    InitialAudioParameter
    InitialAudioParameter where
  convertOption _ _ = identity

instance ConvertOption PlayBufOptions "duration" Number (Maybe Number) where
  convertOption _ _ = just

instance ConvertOption PlayBufOptions "bufferOffset" Number Number where
  convertOption _ _ = identity

instance
  ConvertOption PlayBufOptions "buffer" BrowserAudioBuffer BrowserAudioBuffer where
  convertOption _ _ = identity

type PlayBufOptional =
  ( bufferOffset :: Number
  , playbackRate :: InitialAudioParameter
  , duration :: Maybe Number
  )

type PlayBufAll =
  ( buffer :: BrowserAudioBuffer
  | PlayBufOptional
  )

defaultPlayBuf :: { | PlayBufOptional }
defaultPlayBuf =
  { bufferOffset: 0.0
  , playbackRate: 1.0
  , duration: nothing
  }

class InitialPlayBuf i where
  toInitializePlayBuf :: i -> C.InitializePlayBuf

instance InitialPlayBuf C.InitializePlayBuf where
  toInitializePlayBuf = identity

instance InitialPlayBuf BrowserAudioBuffer where
  toInitializePlayBuf = toInitializePlayBuf <<< { buffer: _ }

instance
  ConvertOptionsWithDefaults PlayBufOptions { | PlayBufOptional } { | provided }
    { | PlayBufAll } =>
  InitialPlayBuf { | provided } where
  toInitializePlayBuf provided = C.InitializePlayBuf
    (convertOptionsWithDefaults PlayBufOptions defaultPlayBuf provided)

__playBuf
  :: forall i outputChannels lock payload
   . Common.InitialPlayBuf i
  => i
  -> Event C.PlayBuf
  -> C.Node outputChannels lock payload
__playBuf i' atts = C.Node go
  where
  C.InitializePlayBuf i = Common.toInitializePlayBuf i'
  go
    parent
    ( C.AudioInterpret
        { ids
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
      flip subscribe k $
        bang
          ( makePlayBuf
              { id: me
              , parent: just parent.parent
              , scope: parent.scope
              , buffer: i.buffer
              , playbackRate: i.playbackRate
              , bufferOffset: i.bufferOffset
              , duration: i.duration
              }
          )
          <|> map
            ( \(C.PlayBuf e) -> match
                { buffer: \buffer -> setBuffer { id: me, buffer }
                , playbackRate: \playbackRate -> setPlaybackRate
                    { id: me, playbackRate }
                , bufferOffset: \bufferOffset -> setBufferOffset
                    { id: me, bufferOffset }
                , onOff: \onOff -> setOnOff { id: me, onOff }
                , duration: \duration -> setDuration { id: me, duration }
                }
                e
            )
            atts

playBuf
  :: forall i outputChannels lock payload
   . Common.InitialPlayBuf i
  => i
  -> Event C.PlayBuf
  -> C.Node outputChannels lock payload
playBuf = __playBuf

playBuf_
  :: forall i outputChannels lock payload
   . Common.InitialPlayBuf i
  => i
  -> C.Node outputChannels lock payload
playBuf_ i = playBuf i empty

-- recorder
recorder
  :: forall i outputChannels lock payload
   . Common.InitialRecorder i
  => i
  -> C.Node outputChannels lock payload
  -> C.Node outputChannels lock payload
recorder i' elt = C.Node go
  where
  C.InitializeRecorder i = Common.toInitializeRecorder i'
  go parent di@(C.AudioInterpret { ids, makeRecorder }) =
    makeEvent \k -> do
      me <- ids
      parent.raiseId me
      flip subscribe k $
        bang
          ( makeRecorder
              { id: me, parent: just parent.parent, scope: parent.scope, cb: i.cb }
          )
          <|> __internalWagsFlatten me di (mix elt)

-- sawtoothOsc

__sawtoothOsc
  :: forall i outputChannels lock payload
   . Common.InitialSawtoothOsc i
  => i
  -> Event C.SawtoothOsc
  -> C.Node outputChannels lock payload
__sawtoothOsc i' atts = C.Node go
  where
  C.InitializeSawtoothOsc i = Common.toInitializeSawtoothOsc i'
  go
    parent
    (C.AudioInterpret { ids, makeSawtoothOsc, setFrequency, setOnOff }) =
    makeEvent \k -> do
      me <- ids
      parent.raiseId me
      flip subscribe k $
        bang
          ( makeSawtoothOsc
              { id: me
              , parent: just parent.parent
              , scope: parent.scope
              , frequency: i.frequency
              }
          )
          <|> map
            ( \(C.SawtoothOsc e) -> match
                { frequency: \frequency -> setFrequency
                    { id: me, frequency }
                , onOff: \onOff -> setOnOff { id: me, onOff }
                }
                e
            )
            atts

sawtoothOsc
  :: forall i outputChannels lock payload
   . Common.InitialSawtoothOsc i
  => i
  -> Event C.SawtoothOsc
  -> C.Node outputChannels lock payload
sawtoothOsc = __sawtoothOsc

sawtoothOsc_
  :: forall i outputChannels lock payload
   . Common.InitialSawtoothOsc i
  => i
  -> C.Node outputChannels lock payload
sawtoothOsc_ i = sawtoothOsc i empty

-- sinOsc

__sinOsc
  :: forall i outputChannels lock payload
   . Common.InitialSinOsc i
  => i
  -> Event C.SinOsc
  -> C.Node outputChannels lock payload
__sinOsc i' atts = C.Node go
  where
  C.InitializeSinOsc i = Common.toInitializeSinOsc i'
  go
    parent
    (C.AudioInterpret { ids, makeSinOsc, setFrequency, setOnOff }) =
    makeEvent \k -> do
      me <- ids
      parent.raiseId me
      flip subscribe k $
        bang
          ( makeSinOsc
              { id: me
              , parent: just parent.parent
              , scope: parent.scope
              , frequency: i.frequency
              }
          )
          <|> map
            ( \(C.SinOsc e) -> match
                { frequency: \frequency -> setFrequency
                    { id: me, frequency }
                , onOff: \onOff -> setOnOff { id: me, onOff }
                }
                e
            )
            atts

sinOsc
  :: forall i outputChannels lock payload
   . Common.InitialSinOsc i
  => i
  -> Event C.SinOsc
  -> C.Node outputChannels lock payload
sinOsc = __sinOsc

sinOsc_
  :: forall i outputChannels lock payload
   . Common.InitialSinOsc i
  => i
  -> C.Node outputChannels lock payload
sinOsc_ a = sinOsc a empty

-- squareOsc

__squareOsc
  :: forall i outputChannels lock payload
   . Common.InitialSquareOsc i
  => i
  -> Event C.SquareOsc
  -> C.Node outputChannels lock payload
__squareOsc i' atts = C.Node go
  where
  C.InitializeSquareOsc i = Common.toInitializeSquareOsc i'
  go
    parent
    (C.AudioInterpret { ids, makeSquareOsc, setFrequency, setOnOff }) =
    makeEvent \k -> do
      me <- ids
      parent.raiseId me
      flip subscribe k $
        bang
          ( makeSquareOsc
              { id: me
              , parent: just parent.parent
              , scope: parent.scope
              , frequency: i.frequency
              }
          )
          <|> map
            ( \(C.SquareOsc e) -> match
                { frequency: \frequency -> setFrequency
                    { id: me, frequency }
                , onOff: \onOff -> setOnOff { id: me, onOff }
                }
                e
            )
            atts

squareOsc
  :: forall i outputChannels lock payload
   . Common.InitialSquareOsc i
  => i
  -> Event C.SquareOsc
  -> C.Node outputChannels lock payload
squareOsc = __squareOsc

squareOsc_
  :: forall i outputChannels lock payload
   . Common.InitialSquareOsc i
  => i
  -> C.Node outputChannels lock payload
squareOsc_ i = squareOsc i empty

-- speaker
speaker
  :: forall outputChannels payload
   . (forall lock. Array (C.Node outputChannels lock payload))
  -> C.AudioInterpret payload
  -> Event payload
speaker elts di@(C.AudioInterpret { ids, makeSpeaker }) = makeEvent \k -> do
  id <- ids
  k (makeSpeaker { id })
  subscribe (__internalWagsFlatten id di (mix elts)) k

speaker2
  :: forall payload
   . (forall lock. Array (C.Node D2 lock payload))
  -> C.AudioInterpret payload
  -> Event payload
speaker2 = speaker

-- pan
pan
  :: forall i aud (outputChannels :: Type) lock payload
   . Common.InitialStereoPanner i
  => C.Mix aud (C.Streamy outputChannels lock payload)
  => i
  -> Event C.StereoPanner
  -> aud
  -> C.Node outputChannels lock payload
pan i' atts elts = C.Node go
  where
  C.InitializeStereoPanner i = Common.toInitializeStereoPanner i'
  go parent di@(C.AudioInterpret { ids, makeStereoPanner, setPan }) =     makeEvent \k -> do
      me <- ids
      parent.raiseId me
      flip subscribe k $

        bang
          ( makeStereoPanner
              { id: me, parent: just parent.parent, scope: parent.scope, pan: i.pan }
          )
          <|> map
            ( \(C.StereoPanner e) -> match
                { pan: \g -> setPan { id: me, pan: g }
                }
                e
            )
            atts
          <|> __internalWagsFlatten me di (mix elts)


pan_
  :: forall i aud (outputChannels :: Type) lock payload
   . Common.InitialStereoPanner i
  => C.Mix aud (C.Streamy outputChannels lock payload)
  => i
  -> aud
  -> C.Node outputChannels lock payload
pan_ i = pan i empty

-- triangleOsc

__triangleOsc
  :: forall i outputChannels lock payload
   . Common.InitialTriangleOsc i
  => i
  -> Event C.TriangleOsc
  -> C.Node outputChannels lock payload
__triangleOsc i' atts = C.Node go
  where
  C.InitializeTriangleOsc i = Common.toInitializeTriangleOsc i'
  go
    parent
    (C.AudioInterpret { ids, makeTriangleOsc, setFrequency, setOnOff }) =
    makeEvent \k -> do
      me <- ids
      parent.raiseId me
      flip subscribe k $
        bang
          ( makeTriangleOsc
              { id: me
              , parent: just parent.parent
              , scope: parent.scope
              , frequency: i.frequency
              }
          )
          <|> map
            ( \(C.TriangleOsc e) -> match
                { frequency: \frequency -> setFrequency
                    { id: me, frequency }
                , onOff: \onOff -> setOnOff { id: me, onOff }
                }
                e
            )
            atts

triangleOsc
  :: forall i outputChannels lock payload
   . Common.InitialTriangleOsc i
  => i
  -> Event C.TriangleOsc
  -> C.Node outputChannels lock payload
triangleOsc = __triangleOsc

triangleOsc_
  :: forall i outputChannels lock payload
   . Common.InitialTriangleOsc i
  => i
  -> C.Node outputChannels lock payload
triangleOsc_ i = triangleOsc i empty

-- waveShaper

waveShaper
  :: forall i aud (outputChannels :: Type) lock payload
   . Common.InitialWaveShaper i
  => C.Mix aud (C.Streamy outputChannels lock payload)
  => i
  -> aud
  -> C.Node outputChannels lock payload
waveShaper i' elts = C.Node go
  where
  C.InitializeWaveShaper i = Common.toInitializeWaveShaper i'
  go parent di@(C.AudioInterpret { ids, makeWaveShaper }) =
    makeEvent \k -> do
      me <- ids
      parent.raiseId me
      flip subscribe k $
        bang
          ( makeWaveShaper
              { id: me
              , parent: just parent.parent
              , scope: parent.scope
              , curve: i.curve
              , oversample: i.oversample
              }
          ) <|> __internalWagsFlatten me di (mix elts)

----------

newtype MutAr a = MutAr (Array a)

foreign import mutAr :: forall a. Array a -> Effect (MutAr a)
foreign import unsafeUpdateMutAr :: forall a. Int -> a -> MutAr a -> Effect Unit
foreign import readAr :: forall a. MutAr a -> Effect (Array a)

-- todo: this is almost literally a copy-paste now of Deku
-- are the libraries close enough now where we can merge the two functions into one?
-- the only differences I can see are:
-- - the deconstruction and reconstruction of Node
-- - the name of the connection function
internalFan
  :: forall n outputChannels lock0 lock1 payload
   . (String -> String)
  -> Vec n (C.Node outputChannels lock0 payload)
  -> ( Vec n (C.Node outputChannels lock1 payload)
       -> (C.Node outputChannels lock0 payload -> C.Node outputChannels lock1 payload)
       -> Event (Event (C.StreamingAudio outputChannels lock1 payload))
     )
  -> C.Node outputChannels lock0 payload
internalFan scopeF gaga closure = C.Node go
  where
  go psr di = makeEvent \k -> do
    av <- mutAr (map (const "") $ toArray gaga)
    let
      actualized = oneOf $ mapWithIndex
        ( \ix (C.Node gogo) ->
            gogo
              { parent: "@fan@"
              , scope: scopeF psr.scope
              , raiseId: \id -> unsafeUpdateMutAr ix id av
              }
              di
        )
        gaga
    u0 <- subscribe actualized k
    av2 <- AVar.empty
    let
      asIds :: Array String -> Vec n String
      asIds = unsafeCoerce
    idz <- asIds <$> readAr av
    let
      -- we never connect or disconnect the referentially opaque node
      -- instead, it is always managed inside a referentially transparent gain node
      -- that can be properly connected and disconnected
      injectable = map
        ( \id -> gain_ 1.0
            ( C.Node
                \{ parent } (C.AudioInterpret { connectXToY }) ->
                  bang (connectXToY { from: id, to: parent })
            )
        )
        idz
      realized = __internalWagsFlatten psr.parent di
        (proof (coerce (closure injectable (\(C.Node q) -> C.Node q))))
    u <- subscribe realized k
    void $ tryPut u av2
    -- cancel immediately, as it should be run synchronously
    -- so if this actually does something then we have a problem
    pure do
      u0
      cncl2 <- AVar.take av2 \q -> case q of
        Right usu -> usu
        Left e -> throwException e
      -- cancel immediately, as it should be run synchronously
      -- so if this actually does something then we have a problem
      cncl2

globalFan
  :: forall n outputChannels lock payload
   . Vec n (C.Node outputChannels lock payload)
  -> (Vec n (C.Node outputChannels lock payload) -> Event (Event (C.StreamingAudio outputChannels lock payload)))
  -> C.Node outputChannels lock payload
globalFan e f = internalFan (const "@portal@") e (\x _ -> f x)

fan
  :: forall n outputChannels lock0 payload
   . Vec n (C.Node outputChannels lock0 payload)
  -> ( forall lock1
        . Vec n (C.Node outputChannels lock1 payload)
       -> (C.Node outputChannels lock0 payload -> C.Node outputChannels lock1 payload)
       -> Event (Event (C.StreamingAudio outputChannels lock1 payload))
     )
  -> C.Node outputChannels lock0 payload
fan e = internalFan identity e

---- fix
fix
  :: forall outputChannels lock payload
   . (C.Node outputChannels lock payload -> C.Node outputChannels lock payload)
  -> C.Node outputChannels lock payload
fix f = C.Node go
  where
  go i di@(C.AudioInterpret { connectXToY }) = makeEvent \k -> do
    av <- AVar.empty
    let
      -- we never connect or disconnect the referentially opaque node
      -- instead, it is always managed inside a referentially transparent gain node
      -- that can be properly connected and disconnected
      C.Node nn = f $ gain_ 1.0 $ C.Node \ii _ -> makeEvent \k -> do
        -- we never unsubscribe, as we always need this value
        void $ AVar.read av case _ of
          Left e -> throwException e
          -- todo: test silence
          Right r -> k (connectXToY { from: r, to: ii.parent })
        pure (pure unit)
    subscribe
      ( nn
          { parent: i.parent
          , scope: i.scope
          , raiseId: \s -> do
              i.raiseId s
              void $ tryPut s av
          }
          di
      )
      k

silence
  :: forall outputChannels lock payload
   . C.Node outputChannels lock payload
silence = fix identity


-----
-- starts work on merge
-- merge
--   :: forall i n lock payload
--    . IsEvent event
--   => Pos n
--   => Vec n (C.Node D1 lock payload)
--   -> C.Node n lock payload
-- merge elts = C.Node go
--   where
--   go
--     parent
--     di@
--       ( C.AudioInterpret
--           { ids
--           , scope
--           , makeMerger
--           }
--       ) =
--     keepLatest
--       ( (sample_ ids (bang unit)) <#> \me ->
--           bang
--             ( makeMerger
--                 { id: me
--                 , parent: just parent.parent
--                 , scope: parent.scope
--                 }
--             )
--             <|> oneOf
--               ( ( mapWithIndex
--                     -- parent needs to accept an ix for this to work
--                     ( \ix (id /\ elt) -> (((\y -> let C.Node x = y in x) elt) (Parent me ix) di)
--                     )
--                     elts
--                 )
--               )
--       )