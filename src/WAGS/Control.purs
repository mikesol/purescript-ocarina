module WAGS.Control where

import Prelude

import Control.Alt ((<|>))
import Control.Comonad (extract)
import Control.Plus (empty)
import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Data.Foldable (oneOf)
import Data.Homogeneous (class HomogeneousRowLabels)
import Data.Homogeneous.Variant (homogeneous)
import Data.Int (pow)
import Data.Profunctor (lcmap)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Typelevel.Num (class Lt, class Nat, class Pos, class Pred, D1, D2, pred, toInt)
import Data.Variant (Unvariant(..), match, unvariant)
import Data.Variant.Maybe (Maybe, just, maybe, nothing)
import Data.Vec (toArray)
import FRP.Behavior (sample_)
import FRP.Event (class IsEvent, keepLatest)
import FRP.Event.Class (bang)
import Foreign.Object (fromHomogeneous)
import Simple.JSON as JSON
import Type.Proxy (Proxy(..))
import Type.Row.Homogeneous (class Homogeneous)
import WAGS.Common as Common
import WAGS.Core (ChannelCountMode(..), ChannelInterpretation(..), MeOrParent(..), Po2(..), useParentIfParent)
import WAGS.Core as C
import WAGS.Parameter (AudioParameter, InitialAudioParameter)
import WAGS.WebAPI (AnalyserNodeCb(..), BrowserAudioBuffer)

-- __maybeUseName scope mId
-- tmpIdentity = identity
parentalSupervision :: forall a. MeOrParent -> (String -> a) -> String -> a
parentalSupervision (Me me) = lcmap (const me)
parentalSupervision _ = identity

__appendScopeToNamedInput :: String -> String -> String
__appendScopeToNamedInput i scope = i <> "!" <> scope

__maybeUseName
  :: forall a. String -> Maybe String -> (String -> a) -> String -> a
--__maybeUseName s f = maybe f (\i -> const (f i)) s
__maybeUseName scope perhapsName f = maybe f
  (\i _ -> (f $ __appendScopeToNamedInput i scope))
  perhapsName

-- allpass

allpass
  :: forall i (outputChannels :: Type) lock event payload
   . IsEvent event
  => Common.InitialAllpass i
  => i
  -> event C.Allpass
  -> Array (C.Node outputChannels lock event payload)
  -> C.Node outputChannels lock event payload
allpass i' atts elts = C.Node go
  where
  C.InitializeAllpass i = Common.toInitializeAllpass i'
  go parent di@(C.AudioInterpret { ids, scope, makeAllpass, setFrequency, setQ }) = keepLatest
    ( (sample_ ids (bang unit)) <#> parentalSupervision parent \me ->
        bang
          ( makeAllpass
              { id: me, parent: useParentIfParent parent, scope: just scope, frequency: i.frequency, q: i.q }
          )
          <|> map
            ( \(C.Allpass e) -> match
                { frequency: \g -> setFrequency { id: me, frequency: g }
                , q: \g -> setQ { id: me, q: g }
                }
                e
            )
            atts
          <|> oneOf
            (map (\elt -> ((\y -> let C.Node x = y in x) elt) (Parent me) di) elts)
    )

allpass_
  :: forall i (outputChannels :: Type) lock event payload
   . IsEvent event
  => Common.InitialAllpass i
  => i
  -> Array (C.Node outputChannels lock event payload)
  -> C.Node outputChannels lock event payload
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
  :: forall i outputChannels lock event payload
   . IsEvent event
  => InitialAnalyser i
  => i
  -> event C.Analyser
  -> C.Node outputChannels lock event payload
  -> C.Node outputChannels lock event payload
analyser i' atts elt = C.Node go
  where
  C.InitializeAnalyser i = toInitializeAnalyser i'
  go
    parent
    di@(C.AudioInterpret { ids, scope, makeAnalyser, setAnalyserNodeCb }) =
    keepLatest
      ( (sample_ ids (bang unit)) <#> parentalSupervision parent \me ->
          bang
            ( makeAnalyser
                { id: me
                , parent: useParentIfParent parent
                , scope: just scope
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
            <|> ((\y -> let C.Node x = y in x) elt) (Parent me) di
      )

analyser_
  :: forall i outputChannels lock event payload
   . IsEvent event
  => InitialAnalyser i
  => i
  -> C.Node outputChannels lock event payload
  -> C.Node outputChannels lock event payload
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
       processorOptions lock event payload
   . IsSymbol name
  => Nat numberOfInputs
  => Pos numberOfOutputs
  => ValidateOutputChannelCount numberOfOutputs outputChannelCount
  => Homogeneous parameterData InitialAudioParameter
  => HomogeneousRowLabels parameterData AudioParameter parameterDataRL
  => JSON.WriteForeign { | processorOptions }
  => IsEvent event
  => C.InitializeAudioWorkletNode name numberOfInputs numberOfOutputs
       outputChannelCount
       parameterData
       processorOptions
  -> event (C.AudioWorkletNode parameterData)
  -> C.Node numberOfOutputs lock event payload
  -> C.Node numberOfOutputs lock event payload
__audioWorklet (C.InitializeAudioWorkletNode i) atts elt = C.Node go
  where
  go
    parent
    di@
      ( C.AudioInterpret
          { ids, scope, makeAudioWorkletNode, setAudioWorkletParameter }
      ) =
    keepLatest
      ( (sample_ ids (bang unit)) <#> parentalSupervision parent \me ->
          bang
            ( makeAudioWorkletNode
                { id: me
                , parent: useParentIfParent parent
                , scope: just scope
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
            <|> ((\y -> let C.Node x = y in x) elt) (Parent me) di
      )

audioWorklet
  :: forall name numberOfInputs numberOfOutputs outputChannelCount parameterData
       parameterDataRL
       processorOptions lock event payload
   . IsSymbol name
  => Nat numberOfInputs
  => Pos numberOfOutputs
  => ValidateOutputChannelCount numberOfOutputs outputChannelCount
  => Homogeneous parameterData InitialAudioParameter
  => HomogeneousRowLabels parameterData AudioParameter parameterDataRL
  => JSON.WriteForeign { | processorOptions }
  => IsEvent event
  => C.InitializeAudioWorkletNode name numberOfInputs numberOfOutputs
       outputChannelCount
       parameterData
       processorOptions
  -> event (C.AudioWorkletNode parameterData)
  -> C.Node numberOfOutputs lock event payload
  -> C.Node numberOfOutputs lock event payload
audioWorklet = __audioWorklet

-- bandpass
bandpass
  :: forall i (outputChannels :: Type) lock event payload
   . IsEvent event
  => Common.InitialBandpass i
  => i
  -> event C.Bandpass
  -> Array (C.Node outputChannels lock event payload)
  -> C.Node outputChannels lock event payload
bandpass i' atts elts = C.Node go
  where
  C.InitializeBandpass i = Common.toInitializeBandpass i'
  go parent di@(C.AudioInterpret { ids, scope, makeBandpass, setFrequency, setQ }) = keepLatest
    ( (sample_ ids (bang unit)) <#> parentalSupervision parent \me ->
        bang
          ( makeBandpass
              { id: me, parent: useParentIfParent parent, scope: just scope, frequency: i.frequency, q: i.q }
          )
          <|> map
            ( \(C.Bandpass e) -> match
                { frequency: \g -> setFrequency { id: me, frequency: g }
                , q: \g -> setQ { id: me, q: g }
                }
                e
            )
            atts
          <|> oneOf
            ( (map (\elt -> ((\y -> let C.Node x = y in x) elt) (Parent me) di) elts)
            )
    )

bandpass_
  :: forall i (outputChannels :: Type) lock event payload
   . IsEvent event
  => Common.InitialBandpass i
  => i
  -> Array (C.Node outputChannels lock event payload)
  -> C.Node outputChannels lock event payload
bandpass_ i a = bandpass i empty a

-- constant

__constant
  :: forall i outputChannels lock event payload
   . IsEvent event
  => Common.InitialConstant i
  => i
  -> event C.Constant
  -> C.Node outputChannels lock event payload
__constant i' atts = C.Node go
  where
  C.InitializeConstant i = Common.toInitializeConstant i'
  go parent (C.AudioInterpret { ids, scope, makeConstant, setOffset, setOnOff }) =
    keepLatest
      ( (sample_ ids (bang unit)) <#> parentalSupervision parent \me ->
          bang
            ( makeConstant
                { id: me
                , parent: useParentIfParent parent
                , scope: just scope
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
      )

constant
  :: forall i outputChannels lock event payload
   . IsEvent event
  => Common.InitialConstant i
  => i
  -> event C.Constant
  -> C.Node outputChannels lock event payload
constant = __constant

constant_
  :: forall i outputChannels lock event payload
   . IsEvent event
  => Common.InitialConstant i
  => i
  -> C.Node outputChannels lock event payload
constant_ i = constant i empty

-- convolver

convolver
  :: forall i (outputChannels :: Type) lock event payload
   . IsEvent event
  => Common.InitialConvolver i
  => i
  -> Array (C.Node outputChannels lock event payload)
  -> C.Node outputChannels lock event payload
convolver i' elts = C.Node go
  where
  C.InitializeConvolver i = Common.toInitializeConvolver i'
  go
    parent
    di@
      ( C.AudioInterpret
          { ids
          , scope
          , makeConvolver
          }
      ) =
    keepLatest
      ( (sample_ ids (bang unit)) <#> parentalSupervision parent \me ->
          bang
            ( makeConvolver
                { id: me
                , parent: useParentIfParent parent
                , scope: just scope
                , buffer: i.buffer
                }
            )
            <|> oneOf
              ( (map (\elt -> ((\y -> let C.Node x = y in x) elt) (Parent me) di) elts)
              )
      )

-- delay
delay
  :: forall i (outputChannels :: Type) lock event payload
   . IsEvent event
  => Common.InitialDelay i
  => i
  -> event C.Delay
  -> Array (C.Node outputChannels lock event payload)
  -> C.Node outputChannels lock event payload
delay i' atts elts = C.Node go
  where
  C.InitializeDelay i = Common.toInitializeDelay i'
  go parent di@(C.AudioInterpret { ids, scope, makeDelay, setDelay }) = keepLatest
    ( (sample_ ids (bang unit)) <#> parentalSupervision parent \me ->
        bang
          ( makeDelay
              { id: me, parent: useParentIfParent parent, scope: just scope, delayTime: i.delayTime, maxDelayTime: i.maxDelayTime }
          )
          <|> map
            ( \(C.Delay e) -> match
                { delayTime: \g -> setDelay { id: me, delayTime: g }
                }
                e
            )
            atts
          <|> oneOf
            ( (map (\elt -> ((\y -> let C.Node x = y in x) elt) (Parent me) di) elts)
            )
    )

delay_
  :: forall i (outputChannels :: Type) lock event payload
   . IsEvent event
  => Common.InitialDelay i
  => i
  -> Array (C.Node outputChannels lock event payload)
  -> C.Node outputChannels lock event payload
delay_ i a = delay i empty a

-- dynamics compressor
dynamicsCompressor
  :: forall i (outputChannels :: Type) lock event payload
   . IsEvent event
  => Common.InitialDynamicsCompressor i
  => i
  -> event C.DynamicsCompressor
  -> Array (C.Node outputChannels lock event payload)
  -> C.Node outputChannels lock event payload
dynamicsCompressor i' atts elts = C.Node go
  where
  C.InitializeDynamicsCompressor i = Common.toInitializeDynamicsCompressor i'
  go
    parent
    di@
      ( C.AudioInterpret
          { ids
          , scope
          , makeDynamicsCompressor
          , setThreshold
          , setRatio
          , setKnee
          , setAttack
          , setRelease
          }
      ) =
    keepLatest
      ( (sample_ ids (bang unit)) <#> parentalSupervision parent \me ->
          bang
            ( makeDynamicsCompressor
                { id: me
                , parent: useParentIfParent parent
                , scope: just scope
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
            <|> oneOf
              ( (map (\elt -> ((\y -> let C.Node x = y in x) elt) (Parent me) di) elts)
              )
      )

dynamicsCompressor_
  :: forall i (outputChannels :: Type) lock event payload
   . IsEvent event
  => Common.InitialDynamicsCompressor i
  => i
  -> Array (C.Node outputChannels lock event payload)
  -> C.Node outputChannels lock event payload
dynamicsCompressor_ i = dynamicsCompressor i empty

-- gain
gain
  :: forall i (outputChannels :: Type) lock event payload
   . IsEvent event
  => Common.InitialGain i
  => i
  -> event C.Gain
  -> Array (C.Node outputChannels lock event payload)
  -> C.Node outputChannels lock event payload
gain i' atts elts = C.Node go
  where
  C.InitializeGain i = Common.toInitializeGain i'
  go parent di@(C.AudioInterpret { ids, scope, makeGain, setGain }) = keepLatest
    ( (sample_ ids (bang unit)) <#> parentalSupervision parent \me ->
        bang
          ( makeGain
              { id: me, parent: useParentIfParent parent, scope: just scope, gain: i.gain }
          )
          <|> map
            ( \(C.Gain e) -> match
                { gain: \g -> setGain { id: me, gain: g }
                }
                e
            )
            atts
          <|> oneOf
            ( (map (\elt -> ((\y -> let C.Node x = y in x) elt) (Parent me) di) elts)
            )
    )

gain_
  :: forall i (outputChannels :: Type) lock event payload
   . IsEvent event
  => Common.InitialGain i
  => i
  -> Array (C.Node outputChannels lock event payload)
  -> C.Node outputChannels lock event payload
gain_ i a = gain i empty a

-- highpass
highpass
  :: forall i (outputChannels :: Type) lock event payload
   . IsEvent event
  => Common.InitialHighpass i
  => i
  -> event C.Highpass
  -> Array (C.Node outputChannels lock event payload)
  -> C.Node outputChannels lock event payload
highpass i' atts elts = C.Node go
  where
  C.InitializeHighpass i = Common.toInitializeHighpass i'
  go parent di@(C.AudioInterpret { ids, scope, makeHighpass, setFrequency, setQ }) = keepLatest
    ( (sample_ ids (bang unit)) <#> parentalSupervision parent \me ->
        bang
          ( makeHighpass
              { id: me, parent: useParentIfParent parent, scope: just scope, frequency: i.frequency, q: i.q }
          )
          <|> map
            ( \(C.Highpass e) -> match
                { frequency: \g -> setFrequency { id: me, frequency: g }
                , q: \g -> setQ { id: me, q: g }
                }
                e
            )
            atts
          <|> oneOf
            ( (map (\elt -> ((\y -> let C.Node x = y in x) elt) (Parent me) di) elts)
            )
    )

highpass_
  :: forall i (outputChannels :: Type) lock event payload
   . IsEvent event
  => Common.InitialHighpass i
  => i
  -> Array (C.Node outputChannels lock event payload)
  -> C.Node outputChannels lock event payload
highpass_ i a = highpass i empty a

-- highshelf
highshelf
  :: forall i (outputChannels :: Type) lock event payload
   . IsEvent event
  => Common.InitialHighshelf i
  => i
  -> event C.Highshelf
  -> Array (C.Node outputChannels lock event payload)
  -> C.Node outputChannels lock event payload
highshelf i' atts elts = C.Node go
  where
  C.InitializeHighshelf i = Common.toInitializeHighshelf i'
  go parent di@(C.AudioInterpret { ids, scope, makeHighshelf, setFrequency, setGain }) = keepLatest
    ( (sample_ ids (bang unit)) <#> parentalSupervision parent \me ->
        bang
          ( makeHighshelf
              { id: me, parent: useParentIfParent parent, scope: just scope, frequency: i.frequency, gain: i.gain }
          )
          <|> map
            ( \(C.Highshelf e) -> match
                { frequency: \g -> setFrequency { id: me, frequency: g }
                , gain: \g -> setGain { id: me, gain: g }
                }
                e
            )
            atts
          <|> oneOf
            ( (map (\elt -> ((\y -> let C.Node x = y in x) elt) (Parent me) di) elts)
            )
    )

highshelf_
  :: forall i (outputChannels :: Type) lock event payload
   . IsEvent event
  => Common.InitialHighshelf i
  => i
  -> Array (C.Node outputChannels lock event payload)
  -> C.Node outputChannels lock event payload
highshelf_ i a = highshelf i empty a

-- iirFilter

iirFilter
  :: forall i (feedforward :: Type) (feedback :: Type) (outputChannels :: Type) lock event
       payload
   . IsEvent event
  => Lt D2 feedforward
  => Lt D2 feedback
  => Common.InitialIIRFilter i feedforward feedback
  => i
  -> Array (C.Node outputChannels lock event payload)
  -> C.Node outputChannels lock event payload
iirFilter = iirFilter' (Proxy :: _ feedforward) (Proxy :: _ feedback)

iirFilter'
  :: forall i proxy (feedforward :: Type) (feedback :: Type) (outputChannels :: Type) lock event
       payload
   . IsEvent event
  => Lt D2 feedforward
  => Lt D2 feedback
  => Common.InitialIIRFilter i feedforward feedback
  => proxy feedforward
  -> proxy feedback
  -> i
  -> Array (C.Node outputChannels lock event payload)
  -> C.Node outputChannels lock event payload
iirFilter' fwd bk i' elts = C.Node go
  where
  C.InitializeIIRFilter i = Common.toInitializeIIRFilter i' fwd bk
  go
    parent
    di@
      ( C.AudioInterpret
          { ids
          , scope
          , makeIIRFilter
          }
      ) =
    keepLatest
      ( (sample_ ids (bang unit)) <#> parentalSupervision parent \me ->
          bang
            ( makeIIRFilter
                { id: me
                , parent: useParentIfParent parent
                , scope: just scope
                , feedforward: toArray i.feedforward
                , feedback: toArray i.feedback
                }
            )
            <|> oneOf
              ( (map (\elt -> ((\y -> let C.Node x = y in x) elt) (Parent me) di) elts)
              )
      )

-- lowpass
lowpass
  :: forall i (outputChannels :: Type) lock event payload
   . IsEvent event
  => Common.InitialLowpass i
  => i
  -> event C.Lowpass
  -> Array (C.Node outputChannels lock event payload)
  -> C.Node outputChannels lock event payload
lowpass i' atts elts = C.Node go
  where
  C.InitializeLowpass i = Common.toInitializeLowpass i'
  go parent di@(C.AudioInterpret { ids, scope, makeLowpass, setFrequency, setQ }) = keepLatest
    ( (sample_ ids (bang unit)) <#> parentalSupervision parent \me ->
        bang
          ( makeLowpass
              { id: me, parent: useParentIfParent parent, scope: just scope, frequency: i.frequency, q: i.q }
          )
          <|> map
            ( \(C.Lowpass e) -> match
                { frequency: \g -> setFrequency { id: me, frequency: g }
                , q: \g -> setQ { id: me, q: g }
                }
                e
            )
            atts
          <|> oneOf
            ( (map (\elt -> ((\y -> let C.Node x = y in x) elt) (Parent me) di) elts)
            )
    )

lowpass_
  :: forall i (outputChannels :: Type) lock event payload
   . IsEvent event
  => Common.InitialLowpass i
  => i
  -> Array (C.Node outputChannels lock event payload)
  -> C.Node outputChannels lock event payload
lowpass_ i a = lowpass i empty a

-- lowshelf
lowshelf
  :: forall i (outputChannels :: Type) lock event payload
   . IsEvent event
  => Common.InitialLowshelf i
  => i
  -> event C.Lowshelf
  -> Array (C.Node outputChannels lock event payload)
  -> C.Node outputChannels lock event payload
lowshelf i' atts elts = C.Node go
  where
  C.InitializeLowshelf i = Common.toInitializeLowshelf i'
  go parent di@(C.AudioInterpret { ids, scope, makeLowshelf, setFrequency, setGain }) = keepLatest
    ( (sample_ ids (bang unit)) <#> parentalSupervision parent \me ->
        bang
          ( makeLowshelf
              { id: me, parent: useParentIfParent parent, scope: just scope, frequency: i.frequency, gain: i.gain }
          )
          <|> map
            ( \(C.Lowshelf e) -> match
                { frequency: \g -> setFrequency { id: me, frequency: g }
                , gain: \g -> setGain { id: me, gain: g }
                }
                e
            )
            atts
          <|> oneOf
            ( (map (\elt -> ((\y -> let C.Node x = y in x) elt) (Parent me) di) elts)
            )
    )

lowshelf_
  :: forall i (outputChannels :: Type) lock event payload
   . IsEvent event
  => Common.InitialLowshelf i
  => i
  -> Array (C.Node outputChannels lock event payload)
  -> C.Node outputChannels lock event payload
lowshelf_ i a = lowshelf i empty a

-- loopBuf

__loopBuf
  :: forall i outputChannels lock event payload
   . IsEvent event
  => Common.InitialLoopBuf i
  => i
  -> event C.LoopBuf
  -> C.Node outputChannels lock event payload
__loopBuf i' atts = C.Node go
  where
  C.InitializeLoopBuf i = Common.toInitializeLoopBuf i'
  go
    parent
    ( C.AudioInterpret
        { ids
        , scope
        , makeLoopBuf
        , setBuffer
        , setOnOff
        , setPlaybackRate
        , setLoopStart
        , setLoopEnd
        }
    ) =
    keepLatest
      ( (sample_ ids (bang unit)) <#> parentalSupervision parent \me ->
          bang
            ( makeLoopBuf
                { id: me
                , parent: useParentIfParent parent
                , scope: just scope
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
      )

loopBuf
  :: forall i outputChannels lock event payload
   . IsEvent event
  => Common.InitialLoopBuf i
  => i
  -> event C.LoopBuf
  -> C.Node outputChannels lock event payload
loopBuf = __loopBuf

loopBuf_
  :: forall i outputChannels lock event payload
   . IsEvent event
  => Common.InitialLoopBuf i
  => i
  -> C.Node outputChannels lock event payload
loopBuf_ i = loopBuf i empty

-- mediaElement

__mediaElement
  :: forall outputChannels lock event payload
   . IsEvent event
  => C.InitializeMediaElement
  -> C.Node outputChannels lock event payload
__mediaElement (C.InitializeMediaElement i) = C.Node go
  where
  go parent (C.AudioInterpret { ids, scope, makeMediaElement }) =
    keepLatest
      ( (sample_ ids (bang unit)) <#> parentalSupervision parent \me ->
          bang
            ( makeMediaElement
                { id: me
                , parent: useParentIfParent parent
                , scope: just scope
                , element: i.element
                }
            )
      )

mediaElement
  :: forall outputChannels lock event payload
   . IsEvent event
  => C.InitializeMediaElement
  -> C.Node outputChannels lock event payload
mediaElement = __mediaElement

-- microphone

__microphone
  :: forall i outputChannels lock event payload
   . IsEvent event
  => Common.InitialMicrophone i
  => i
  -> C.Node outputChannels lock event payload
__microphone i' = C.Node go
  where
  C.InitializeMicrophone i = Common.toInitializeMicrophone i'
  go parent (C.AudioInterpret { ids, scope, makeMicrophone }) =
    keepLatest
      ( (sample_ ids (bang unit)) <#> parentalSupervision parent \me ->
          bang
            ( makeMicrophone
                { id: me
                , parent: useParentIfParent parent
                , scope: just scope
                , microphone: i.microphone
                }
            )
      )

microphone
  :: forall i outputChannels lock event payload
   . IsEvent event
  => Common.InitialMicrophone i
  => i
  -> C.Node outputChannels lock event payload
microphone = __microphone

-- notch
notch
  :: forall i (outputChannels :: Type) lock event payload
   . IsEvent event
  => Common.InitialNotch i
  => i
  -> event C.Notch
  -> Array (C.Node outputChannels lock event payload)
  -> C.Node outputChannels lock event payload
notch i' atts elts = C.Node go
  where
  C.InitializeNotch i = Common.toInitializeNotch i'
  go parent di@(C.AudioInterpret { ids, scope, makeNotch, setFrequency, setQ }) = keepLatest
    ( (sample_ ids (bang unit)) <#> parentalSupervision parent \me ->
        bang
          ( makeNotch
              { id: me, parent: useParentIfParent parent, scope: just scope, frequency: i.frequency, q: i.q }
          )
          <|> map
            ( \(C.Notch e) -> match
                { frequency: \g -> setFrequency { id: me, frequency: g }
                , q: \g -> setQ { id: me, q: g }
                }
                e
            )
            atts
          <|> oneOf
            ( (map (\elt -> ((\y -> let C.Node x = y in x) elt) (Parent me) di) elts)
            )
    )

notch_
  :: forall i (outputChannels :: Type) lock event payload
   . IsEvent event
  => Common.InitialNotch i
  => i
  -> Array (C.Node outputChannels lock event payload)
  -> C.Node outputChannels lock event payload
notch_ i a = notch i empty a

-- peaking
peaking
  :: forall i (outputChannels :: Type) lock event payload
   . IsEvent event
  => Common.InitialPeaking i
  => i
  -> event C.Peaking
  -> Array (C.Node outputChannels lock event payload)
  -> C.Node outputChannels lock event payload
peaking i' atts elts = C.Node go
  where
  C.InitializePeaking i = Common.toInitializePeaking i'
  go parent di@(C.AudioInterpret { ids, scope, makePeaking, setFrequency, setQ, setGain }) = keepLatest
    ( (sample_ ids (bang unit)) <#> parentalSupervision parent \me ->
        bang
          ( makePeaking
              { id: me, parent: useParentIfParent parent, scope: just scope, frequency: i.frequency, q: i.q, gain: i.gain }
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
          <|> oneOf
            ( (map (\elt -> ((\y -> let C.Node x = y in x) elt) (Parent me) di) elts)
            )
    )

peaking_
  :: forall i (outputChannels :: Type) lock event payload
   . IsEvent event
  => Common.InitialPeaking i
  => i
  -> Array (C.Node outputChannels lock event payload)
  -> C.Node outputChannels lock event payload
peaking_ i a = peaking i empty a

-- periodicOsc

__periodicOsc
  :: forall i outputChannels lock event payload
   . IsEvent event
  => Common.InitialPeriodicOsc i
  => i
  -> event C.PeriodicOsc
  -> C.Node outputChannels lock event payload
__periodicOsc i' atts = C.Node go
  where
  C.InitializePeriodicOsc i = Common.toInitializePeriodicOsc i'
  go
    parent
    ( C.AudioInterpret
        { ids, scope, makePeriodicOsc, setFrequency, setOnOff, setPeriodicOsc }
    ) =
    keepLatest
      ( (sample_ ids (bang unit)) <#> parentalSupervision parent \me ->
          bang
            ( makePeriodicOsc
                { id: me
                , parent: useParentIfParent parent
                , scope: just scope
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
      )

periodicOsc
  :: forall i outputChannels lock event payload
   . IsEvent event
  => Common.InitialPeriodicOsc i
  => i
  -> event C.PeriodicOsc
  -> C.Node outputChannels lock event payload
periodicOsc = __periodicOsc

periodicOsc_
  :: forall i outputChannels lock event payload
   . IsEvent event
  => Common.InitialPeriodicOsc i
  => i
  -> C.Node outputChannels lock event payload
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
  :: forall i outputChannels lock event payload
   . IsEvent event
  => Common.InitialPlayBuf i
  => i
  -> event C.PlayBuf
  -> C.Node outputChannels lock event payload
__playBuf i' atts = C.Node go
  where
  C.InitializePlayBuf i = Common.toInitializePlayBuf i'
  go
    parent
    ( C.AudioInterpret
        { ids
        , scope
        , makePlayBuf
        , setBuffer
        , setOnOff
        , setDuration
        , setPlaybackRate
        , setBufferOffset
        }
    ) =
    keepLatest
      ( (sample_ ids (bang unit)) <#> parentalSupervision parent \me ->
          bang
            ( makePlayBuf
                { id: me
                , parent: useParentIfParent parent
                , scope: just scope
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
      )

playBuf
  :: forall i outputChannels lock event payload
   . IsEvent event
  => Common.InitialPlayBuf i
  => i
  -> event C.PlayBuf
  -> C.Node outputChannels lock event payload
playBuf = __playBuf

playBuf_
  :: forall i outputChannels lock event payload
   . IsEvent event
  => Common.InitialPlayBuf i
  => i
  -> C.Node outputChannels lock event payload
playBuf_ i = playBuf i empty

-- recorder
recorder
  :: forall i outputChannels lock event payload
   . IsEvent event
  => Common.InitialRecorder i
  => i
  -> C.Node outputChannels lock event payload
  -> C.Node outputChannels lock event payload
recorder i' elt = C.Node go
  where
  C.InitializeRecorder i = Common.toInitializeRecorder i'
  go parent di@(C.AudioInterpret { ids, scope, makeRecorder }) =
    keepLatest
      ( (sample_ ids (bang unit)) <#> parentalSupervision parent \me ->
          bang
            ( makeRecorder
                { id: me, parent: useParentIfParent parent, scope: just scope, cb: i.cb }
            )
            <|> ((\y -> let C.Node x = y in x) elt) (Parent me) di

      )

-- sawtoothOsc

__sawtoothOsc
  :: forall i outputChannels lock event payload
   . IsEvent event
  => Common.InitialSawtoothOsc i
  => i
  -> event C.SawtoothOsc
  -> C.Node outputChannels lock event payload
__sawtoothOsc i' atts = C.Node go
  where
  C.InitializeSawtoothOsc i = Common.toInitializeSawtoothOsc i'
  go
    parent
    (C.AudioInterpret { ids, scope, makeSawtoothOsc, setFrequency, setOnOff }) =
    keepLatest
      ( (sample_ ids (bang unit)) <#> parentalSupervision parent \me ->
          bang
            ( makeSawtoothOsc
                { id: me
                , parent: useParentIfParent parent
                , scope: just scope
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
      )

sawtoothOsc
  :: forall i outputChannels lock event payload
   . IsEvent event
  => Common.InitialSawtoothOsc i
  => i
  -> event C.SawtoothOsc
  -> C.Node outputChannels lock event payload
sawtoothOsc = __sawtoothOsc

sawtoothOsc_
  :: forall i outputChannels lock event payload
   . IsEvent event
  => Common.InitialSawtoothOsc i
  => i
  -> C.Node outputChannels lock event payload
sawtoothOsc_ i = sawtoothOsc i empty

-- sinOsc

__sinOsc
  :: forall i outputChannels lock event payload
   . IsEvent event
  => Common.InitialSinOsc i
  => i
  -> event C.SinOsc
  -> C.Node outputChannels lock event payload
__sinOsc i' atts = C.Node go
  where
  C.InitializeSinOsc i = Common.toInitializeSinOsc i'
  go
    parent
    (C.AudioInterpret { ids, scope, makeSinOsc, setFrequency, setOnOff }) =
    keepLatest
      ( (sample_ ids (bang unit)) <#> parentalSupervision parent \me ->
          bang
            ( makeSinOsc
                { id: me
                , parent: useParentIfParent parent
                , scope: just scope
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
      )

sinOsc
  :: forall i outputChannels lock event payload
   . IsEvent event
  => Common.InitialSinOsc i
  => i
  -> event C.SinOsc
  -> C.Node outputChannels lock event payload
sinOsc = __sinOsc

sinOsc_
  :: forall i outputChannels lock event payload
   . IsEvent event
  => Common.InitialSinOsc i
  => i
  -> C.Node outputChannels lock event payload
sinOsc_ a = sinOsc a empty

-- squareOsc

__squareOsc
  :: forall i outputChannels lock event payload
   . IsEvent event
  => Common.InitialSquareOsc i
  => i
  -> event C.SquareOsc
  -> C.Node outputChannels lock event payload
__squareOsc i' atts = C.Node go
  where
  C.InitializeSquareOsc i = Common.toInitializeSquareOsc i'
  go
    parent
    (C.AudioInterpret { ids, scope, makeSquareOsc, setFrequency, setOnOff }) =
    keepLatest
      ( (sample_ ids (bang unit)) <#> parentalSupervision parent \me ->
          bang
            ( makeSquareOsc
                { id: me
                , parent: useParentIfParent parent
                , scope: just scope
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
      )

squareOsc
  :: forall i outputChannels lock event payload
   . IsEvent event
  => Common.InitialSquareOsc i
  => i
  -> event C.SquareOsc
  -> C.Node outputChannels lock event payload
squareOsc = __squareOsc

squareOsc_
  :: forall i outputChannels lock event payload
   . IsEvent event
  => Common.InitialSquareOsc i
  => i
  -> C.Node outputChannels lock event payload
squareOsc_ i = squareOsc i empty

-- speaker
speaker
  :: forall outputChannels event payload
   . IsEvent event
  => (forall lock. Array (C.Node outputChannels lock event payload))
  -> C.AudioInterpret event payload
  -> event payload
speaker elts di@(C.AudioInterpret { ids, makeSpeaker }) =
  keepLatest
    ( (sample_ ids (bang unit)) <#> \me ->
        bang (makeSpeaker { id: me })
          <|> oneOf
            (map (\elt -> ((\y -> let C.Node x = y in x) elt) (Parent me) di) elts)
    )

speaker2
  :: forall event payload
   . IsEvent event
  => (forall lock. Array (C.Node D2 lock event payload))
  -> C.AudioInterpret event payload
  -> event payload
speaker2 = speaker

-- pan
pan
  :: forall i (outputChannels :: Type) lock event payload
   . IsEvent event
  => Common.InitialStereoPanner i
  => i
  -> event C.StereoPanner
  -> Array (C.Node outputChannels lock event payload)
  -> C.Node outputChannels lock event payload
pan i' atts elts = C.Node go
  where
  C.InitializeStereoPanner i = Common.toInitializeStereoPanner i'
  go parent di@(C.AudioInterpret { ids, scope, makeStereoPanner, setPan }) = keepLatest
    ( (sample_ ids (bang unit)) <#> parentalSupervision parent \me ->
        bang
          ( makeStereoPanner
              { id: me, parent: useParentIfParent parent, scope: just scope, pan: i.pan }
          )
          <|> map
            ( \(C.StereoPanner e) -> match
                { pan: \g -> setPan { id: me, pan: g }
                }
                e
            )
            atts
          <|> oneOf
            ( (map (\elt -> ((\y -> let C.Node x = y in x) elt) (Parent me) di) elts)
            )
    )

pan_
  :: forall i (outputChannels :: Type) lock event payload
   . IsEvent event
  => Common.InitialStereoPanner i
  => i
  -> Array (C.Node outputChannels lock event payload)
  -> C.Node outputChannels lock event payload
pan_ i = pan i empty

-- triangleOsc

__triangleOsc
  :: forall i outputChannels lock event payload
   . IsEvent event
  => Common.InitialTriangleOsc i
  => i
  -> event C.TriangleOsc
  -> C.Node outputChannels lock event payload
__triangleOsc i' atts = C.Node go
  where
  C.InitializeTriangleOsc i = Common.toInitializeTriangleOsc i'
  go
    parent
    (C.AudioInterpret { ids, scope, makeTriangleOsc, setFrequency, setOnOff }) =
    keepLatest
      ( (sample_ ids (bang unit)) <#> parentalSupervision parent \me ->
          bang
            ( makeTriangleOsc
                { id: me
                , parent: useParentIfParent parent
                , scope: just scope
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
      )

triangleOsc
  :: forall i outputChannels lock event payload
   . IsEvent event
  => Common.InitialTriangleOsc i
  => i
  -> event C.TriangleOsc
  -> C.Node outputChannels lock event payload
triangleOsc = __triangleOsc

triangleOsc_
  :: forall i outputChannels lock event payload
   . IsEvent event
  => Common.InitialTriangleOsc i
  => i
  -> C.Node outputChannels lock event payload
triangleOsc_ i = triangleOsc i empty

-- waveShaper

waveShaper
  :: forall i (outputChannels :: Type) lock event payload
   . IsEvent event
  => Common.InitialWaveShaper i
  => i
  -> Array (C.Node outputChannels lock event payload)
  -> C.Node outputChannels lock event payload
waveShaper i' elts = C.Node go
  where
  C.InitializeWaveShaper i = Common.toInitializeWaveShaper i'
  go parent di@(C.AudioInterpret { ids, scope, makeWaveShaper }) =
    keepLatest
      ( (sample_ ids (bang unit)) <#> parentalSupervision parent \me ->
          bang
            ( makeWaveShaper
                { id: me
                , parent: useParentIfParent parent
                , scope: just scope
                , curve: i.curve
                , oversample: i.oversample
                }
            ) <|> oneOf
            ( (map (\elt -> ((\y -> let C.Node x = y in x) elt) (Parent me) di) elts)
            )
      )

-----
-- starts work on merge
-- merge
--   :: forall i n lock event payload
--    . IsEvent event
--   => Pos n
--   => Vec n (C.Node D1 lock event payload)
--   -> C.Node n lock event payload
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
--       ( (sample_ ids (bang unit)) <#> parentalSupervision parent \me ->
--           bang
--             ( makeMerger
--                 { id: me
--                 , parent: useParentIfParent parent
--                 , scope: just scope
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