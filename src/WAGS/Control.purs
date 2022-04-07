module WAGS.Control where

import Prelude

import Control.Alt ((<|>))
import Control.Comonad (extract)
import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Data.Array.NonEmpty (fromNonEmpty)
import Data.Array.NonEmpty as NEA
import Data.Foldable (oneOf)
import Data.Homogeneous (class HomogeneousRowLabels)
import Data.Homogeneous.Variant (homogeneous)
import Data.Int (pow)
import Data.NonEmpty ((:|))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Typelevel.Num (class Nat, class Pos, class Pred, D1, D2, pred, toInt)
import Data.Variant (Unvariant(..), match, unvariant)
import Data.Variant.Maybe (Maybe, just, maybe, nothing)
import FRP.Behavior (sample_)
import FRP.Event (class IsEvent, keepLatest)
import FRP.Event.Phantom (Proof0)
import Foreign.Object (fromHomogeneous)
import Prim.Row (class Cons, class Nub, class Union)
import Safe.Coerce (coerce)
import Simple.JSON as JSON
import Type.Proxy (Proxy(..))
import Type.Row.Homogeneous (class Homogeneous)
import WAGS.Common as Common
import WAGS.Core (ChannelCountMode(..), ChannelInterpretation(..), Po2(..))
import WAGS.Core as C
import WAGS.Parameter (AudioParameter, InitialAudioParameter)
import WAGS.WebAPI (AnalyserNodeCb(..), BrowserAudioBuffer)

__appendScopeToNamedInput :: String -> String -> String
__appendScopeToNamedInput i scope = i <> "!" <> scope

__maybeUseName :: forall a. String -> Maybe String -> (String -> a) -> String -> a
--__maybeUseName s f = maybe f (\i -> const (f i)) s
__maybeUseName scope perhapsName f = maybe f (\i _ -> (f $ __appendScopeToNamedInput i scope)) perhapsName

-- gain input
singleton
  :: forall outputChannels produced consumed event proof payload
   . IsEvent (event proof)
  => C.Node outputChannels produced consumed event proof payload
  -> C.GainInput outputChannels produced consumed event proof payload
singleton a = C.GainInput (NEA.singleton a)

gainInputCons
  :: forall outputChannels produced0 produced1 produced2 consumed0 consumed1
       consumed2 event proof  payload
   . IsEvent (event proof)
  => Union produced0 produced1 produced2
  => Union consumed0 consumed1 consumed2
  => C.Node outputChannels produced0 consumed0 event proof payload
  -> Array (C.Node outputChannels produced1 consumed1 event proof payload)
  -> C.GainInput outputChannels produced2 consumed2 event proof payload
gainInputCons a b = C.GainInput (fromNonEmpty (coerce a :| coerce b))

infixr 6 gainInputCons as :*

gainInputCons2
  :: forall outputChannels produced0 produced1 produced2 consumed0 consumed1
       consumed2 event proof payload
   . IsEvent (event proof)
  => Union produced0 produced1 produced2
  => Union consumed0 consumed1 consumed2
  => C.Node outputChannels produced0 consumed0 event proof payload
  -> C.GainInput outputChannels produced1 consumed1 event proof payload
  -> C.GainInput outputChannels produced2 consumed2 event proof payload
gainInputCons2 a b = C.GainInput (NEA.cons (coerce a) (coerce b))

infixr 6 gainInputCons2 as ::*

gainInputAppend
  :: forall outputChannels produced0 produced1 produced2 consumed0 consumed1
       consumed2 event proof payload
   . IsEvent (event proof)
  => Union produced0 produced1 produced2
  => Union consumed0 consumed1 consumed2
  => C.GainInput outputChannels produced0 consumed0 event proof payload
  -> C.GainInput outputChannels produced1 consumed1 event proof payload
  -> C.GainInput outputChannels produced2 consumed2 event proof payload
gainInputAppend a b = C.GainInput (coerce a <> coerce b)

infixr 6 gainInputAppend as <>*

-- allpass

__allpass
  :: forall outputChannels producedI consumedI producedO consumedO event proof
       payload
   . IsEvent (event proof)
  => Maybe String
  -> C.InitializeAllpass
  -> event proof C.Allpass
  -> C.Node outputChannels producedI consumedI event proof payload
  -> C.Node outputChannels producedO consumedO event proof payload
__allpass mId (C.InitializeAllpass i) atts elt = C.Node go
  where
  go
    parent
    di@(C.AudioInterpret { ids, scope, makeAllpass, setFrequency, setQ }) =
    keepLatest
      ( (sample_ ids (pure unit)) <#> __maybeUseName scope mId \me ->
          ( pure
              ( makeAllpass
                  { id: me
                  , parent: just parent
                  , scope: just scope
                  , frequency: i.frequency
                  , q: i.q
                  }
              )
          )
            <|> map
              ( \(C.Allpass e) -> match
                  { frequency: \frequency -> setFrequency
                      { id: me, frequency }
                  , q: \q -> setQ { id: me, q }
                  }
                  e
              )
              atts
            <|> ((\y -> let C.Node x = y in x) elt) me di

      )

allpass
  :: forall outputChannels produced consumed event proof payload
   . IsEvent (event proof)
  => C.InitializeAllpass
  -> event proof C.Allpass
  -> C.Node outputChannels produced consumed event proof payload
  -> C.Node outputChannels produced consumed event proof payload
allpass = __allpass nothing

allpass'
  :: forall proxy sym outputChannels produced produced' consumed event proof
       payload
   . IsEvent (event proof)
  => IsSymbol sym
  => Cons sym C.Input produced' produced
  => proxy sym
  -> C.InitializeAllpass
  -> event proof C.Allpass
  -> C.Node outputChannels produced' consumed event proof payload
  -> C.Node outputChannels produced consumed event proof payload
allpass' px = __allpass (just (reflectSymbol px))

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
  toInitialAnalyser :: i -> C.InitializeAnalyser

instance InitialAnalyser C.InitializeAnalyser where
  toInitialAnalyser = identity

instance InitialAnalyser AnalyserNodeCb where
  toInitialAnalyser cb = toInitialAnalyser { cb }

instance
  ConvertOptionsWithDefaults AnalyserOptions { | AnalyserOptional }
    { | provided }
    { | AnalyserAll } =>
  InitialAnalyser { | provided } where
  toInitialAnalyser provided = C.InitializeAnalyser
    (convertOptionsWithDefaults AnalyserOptions defaultAnalyser provided)

analyser
  :: forall i outputChannels produced consumed event proof payload
   . IsEvent (event proof)
  => InitialAnalyser i
  => i
  -> event proof C.Analyser
  -> C.Node outputChannels produced consumed event proof payload
  -> C.Node outputChannels produced consumed event proof payload
analyser i' atts elt = C.Node go
  where
  C.InitializeAnalyser i = toInitialAnalyser i'
  go
    parent
    di@(C.AudioInterpret { ids, scope, makeAnalyser, setAnalyserNodeCb }) =
    keepLatest
      ( (sample_ ids (pure unit)) <#> \me ->
          pure
            ( makeAnalyser
                { id: me
                , parent: just parent
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
            <|> ((\y -> let C.Node x = y in x) elt) me di

      )

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
       processorOptions produced consumed event proof payload
   . IsSymbol name
  => Nat numberOfInputs
  => Pos numberOfOutputs
  => ValidateOutputChannelCount numberOfOutputs outputChannelCount
  => Homogeneous parameterData InitialAudioParameter
  => HomogeneousRowLabels parameterData AudioParameter parameterDataRL
  => JSON.WriteForeign { | processorOptions }
  => IsEvent (event proof)
  => Maybe String
  -> C.InitializeAudioWorkletNode name numberOfInputs numberOfOutputs
       outputChannelCount
       parameterData
       processorOptions
  -> event proof (C.AudioWorkletNode parameterData)
  -> C.Node numberOfOutputs produced consumed event proof payload
  -> C.Node numberOfOutputs produced consumed event proof payload
__audioWorklet mId (C.InitializeAudioWorkletNode i) atts elt = C.Node go
  where
  go
    parent
    di@
      ( C.AudioInterpret
          { ids, scope, makeAudioWorkletNode, setAudioWorkletParameter }
      ) =
    keepLatest
      ( (sample_ ids (pure unit)) <#> __maybeUseName scope mId \me ->
          pure
            ( makeAudioWorkletNode
                { id: me
                , parent: just parent
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
            <|> ((\y -> let C.Node x = y in x) elt) me di
      )

audioWorklet
  :: forall name numberOfInputs numberOfOutputs outputChannelCount parameterData
       parameterDataRL
       processorOptions produced consumed event proof payload
   . IsSymbol name
  => Nat numberOfInputs
  => Pos numberOfOutputs
  => ValidateOutputChannelCount numberOfOutputs outputChannelCount
  => Homogeneous parameterData InitialAudioParameter
  => HomogeneousRowLabels parameterData AudioParameter parameterDataRL
  => JSON.WriteForeign { | processorOptions }
  => IsEvent (event proof)
  => C.InitializeAudioWorkletNode name numberOfInputs numberOfOutputs
       outputChannelCount
       parameterData
       processorOptions
  -> event proof (C.AudioWorkletNode parameterData)
  -> C.Node numberOfOutputs produced consumed event proof payload
  -> C.Node numberOfOutputs produced consumed event proof payload
audioWorklet = __audioWorklet nothing

audioWorklet'
  :: forall proxy sym name numberOfInputs numberOfOutputs outputChannelCount
       parameterData parameterDataRL processorOptions produced consumed event proof
       payload
   . IsSymbol name
  => Nat numberOfInputs
  => Pos numberOfOutputs
  => ValidateOutputChannelCount numberOfOutputs outputChannelCount
  => Homogeneous parameterData InitialAudioParameter
  => HomogeneousRowLabels parameterData AudioParameter parameterDataRL
  => JSON.WriteForeign { | processorOptions }
  => IsEvent (event proof)
  => IsSymbol sym
  => proxy sym
  -> C.InitializeAudioWorkletNode name numberOfInputs numberOfOutputs
       outputChannelCount
       parameterData
       processorOptions
  -> event proof (C.AudioWorkletNode parameterData)
  -> C.Node numberOfOutputs produced consumed event proof payload
  -> C.Node numberOfOutputs produced consumed event proof payload
audioWorklet' px = __audioWorklet (just (reflectSymbol px))

-- bandpass

__bandpass
  :: forall outputChannels producedI consumedI producedO consumedO event proof
       payload
   . IsEvent (event proof)
  => Maybe String
  -> C.InitializeBandpass
  -> event proof C.Bandpass
  -> C.Node outputChannels producedI consumedI event proof payload
  -> C.Node outputChannels producedO consumedO event proof payload
__bandpass mId (C.InitializeBandpass i) atts elt = C.Node go
  where
  go
    parent
    di@(C.AudioInterpret { ids, scope, makeBandpass, setFrequency, setQ }) =
    keepLatest
      ( (sample_ ids (pure unit)) <#> __maybeUseName scope mId \me ->
          pure
            ( makeBandpass
                { id: me
                , parent: just parent
                , scope: just scope
                , frequency: i.frequency
                , q: i.q
                }
            )
            <|> map
              ( \(C.Bandpass e) -> match
                  { frequency: \frequency -> setFrequency
                      { id: me, frequency }
                  , q: \q -> setQ { id: me, q }
                  }
                  e
              )
              atts
            <|> ((\y -> let C.Node x = y in x) elt) me di
      )

bandpass
  :: forall outputChannels produced consumed event proof payload
   . IsEvent (event proof)
  => C.InitializeBandpass
  -> event proof C.Bandpass
  -> C.Node outputChannels produced consumed event proof payload
  -> C.Node outputChannels produced consumed event proof payload
bandpass = __bandpass nothing

bandpass'
  :: forall proxy sym outputChannels produced produced' consumed event proof
       payload
   . IsEvent (event proof)
  => IsSymbol sym
  => Cons sym C.Input produced' produced
  => proxy sym
  -> C.InitializeBandpass
  -> event proof C.Bandpass
  -> C.Node outputChannels produced' consumed event proof payload
  -> C.Node outputChannels produced consumed event proof payload
bandpass' px = __bandpass (just (reflectSymbol px))

-- constant

__constant
  :: forall outputChannels produced consumed event proof payload
   . IsEvent (event proof)
  => Maybe String
  -> C.InitializeConstant
  -> event proof C.Constant
  -> C.Node outputChannels produced consumed event proof payload
__constant mId (C.InitializeConstant i) atts = C.Node go
  where
  go parent (C.AudioInterpret { ids, scope, makeConstant, setOffset, setOnOff }) =
    keepLatest
      ( (sample_ ids (pure unit)) <#> __maybeUseName scope mId \me ->
          pure
            ( makeConstant
                { id: me
                , parent: just parent
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
  :: forall outputChannels event proof payload
   . IsEvent (event proof)
  => C.InitializeConstant
  -> event proof C.Constant
  -> C.Node outputChannels () () event proof payload
constant = __constant nothing

constant'
  :: forall proxy sym outputChannels produced event proof payload
   . IsEvent (event proof)
  => IsSymbol sym
  => Cons sym C.Input () produced
  => proxy sym
  -> C.InitializeConstant
  -> event proof C.Constant
  -> C.Node outputChannels produced () event proof payload
constant' px = __constant (just (reflectSymbol px))

-- convolver

__convolver
  :: forall outputChannels producedI consumedI producedO consumedO event proof
       payload
   . IsEvent (event proof)
  => Maybe String
  -> C.InitializeConvolver
  -> C.Node outputChannels producedI consumedI event proof payload
  -> C.Node outputChannels producedO consumedO event proof payload
__convolver mId (C.InitializeConvolver i) elt = C.Node go
  where
  go parent di@(C.AudioInterpret { ids, scope, makeConvolver }) =
    keepLatest
      ( (sample_ ids (pure unit)) <#> __maybeUseName scope mId \me ->
          pure
            ( makeConvolver
                { id: me
                , parent: just parent
                , scope: just scope
                , buffer: i.buffer
                }
            ) <|> ((\y -> let C.Node x = y in x) elt) me di
      )

convolver
  :: forall outputChannels produced consumed event proof payload
   . IsEvent (event proof)
  => C.InitializeConvolver
  -> C.Node outputChannels produced consumed event proof payload
  -> C.Node outputChannels produced consumed event proof payload
convolver = __convolver nothing

convolver'
  :: forall proxy sym outputChannels produced produced' consumed event proof
       payload
   . IsEvent (event proof)
  => IsSymbol sym
  => Cons sym C.Input produced' produced
  => proxy sym
  -> C.InitializeConvolver
  -> C.Node outputChannels produced' consumed event proof payload
  -> C.Node outputChannels produced consumed event proof payload
convolver' px = __convolver (just (reflectSymbol px))

-- delay

__delay
  :: forall outputChannels producedI consumedI producedO consumedO event proof
       payload
   . IsEvent (event proof)
  => Maybe String
  -> C.InitializeDelay
  -> event proof C.Delay
  -> C.Node outputChannels producedI consumedI event proof payload
  -> C.Node outputChannels producedO consumedO event proof payload
__delay mId (C.InitializeDelay i) atts elt = C.Node go
  where
  go parent di@(C.AudioInterpret { ids, scope, makeDelay, setDelay }) =
    keepLatest
      ( (sample_ ids (pure unit)) <#> __maybeUseName scope mId \me ->
          pure
            ( makeDelay
                { id: me
                , parent: just parent
                , scope: just scope
                , delayTime: i.delayTime
                }
            )
            <|> map
              ( \(C.Delay e) -> match
                  { delayTime: \delayTime -> setDelay
                      { id: me, delayTime }
                  }
                  e
              )
              atts
            <|> ((\y -> let C.Node x = y in x) elt) me di
      )

delay
  :: forall outputChannels produced consumed event proof payload
   . IsEvent (event proof)
  => C.InitializeDelay
  -> event proof C.Delay
  -> C.Node outputChannels produced consumed event proof payload
  -> C.Node outputChannels produced consumed event proof payload
delay = __delay nothing

delay'
  :: forall proxy sym outputChannels produced produced' consumed event proof
       payload
   . IsEvent (event proof)
  => IsSymbol sym
  => Cons sym C.Input produced' produced
  => proxy sym
  -> C.InitializeDelay
  -> event proof C.Delay
  -> C.Node outputChannels produced' consumed event proof payload
  -> C.Node outputChannels produced consumed event proof payload
delay' px = __delay (just (reflectSymbol px))

-- dynamics compressor

__dynamicsCompressor
  :: forall outputChannels producedI consumedI producedO consumedO event proof
       payload
   . IsEvent (event proof)
  => Maybe String
  -> C.InitializeDynamicsCompressor
  -> event proof C.DynamicsCompressor
  -> C.Node outputChannels producedI consumedI event proof payload
  -> C.Node outputChannels producedO consumedO event proof payload
__dynamicsCompressor mId (C.InitializeDynamicsCompressor i) atts elt = C.Node go
  where
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
      ( (sample_ ids (pure unit)) <#> __maybeUseName scope mId \me ->
          pure
            ( makeDynamicsCompressor
                { id: me
                , parent: just parent
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
            <|> ((\y -> let C.Node x = y in x) elt) me di
      )

dynamicsCompressor
  :: forall outputChannels produced consumed event proof payload
   . IsEvent (event proof)
  => C.InitializeDynamicsCompressor
  -> event proof C.DynamicsCompressor
  -> C.Node outputChannels produced consumed event proof payload
  -> C.Node outputChannels produced consumed event proof payload
dynamicsCompressor = __dynamicsCompressor nothing

dynamicsCompressor'
  :: forall proxy sym outputChannels produced produced' consumed event proof
       payload
   . IsEvent (event proof)
  => IsSymbol sym
  => Cons sym C.Input produced' produced
  => proxy sym
  -> C.InitializeDynamicsCompressor
  -> event proof C.DynamicsCompressor
  -> C.Node outputChannels produced' consumed event proof payload
  -> C.Node outputChannels produced consumed event proof payload
dynamicsCompressor' px = __dynamicsCompressor (just (reflectSymbol px))

-- gain
gain__
  :: forall i outputChannels produced consumed event proof payload
   . IsEvent (event proof)
  => Common.InitialGain i
  => i
  -> event proof C.Gain
  -> C.Node outputChannels produced consumed event proof payload
  -> C.Node outputChannels produced consumed event proof payload
gain__ i atts h = gain i atts
  (C.GainInput (NEA.fromNonEmpty (h :| [])))

gain_
  :: forall i outputChannels produced consumed event proof payload
   . IsEvent (event proof)
  => Common.InitialGain i
  => i
  -> event proof C.Gain
  -> C.Node outputChannels produced consumed event proof payload
  -> Array (C.Node outputChannels produced consumed event proof payload)
  -> C.Node outputChannels produced consumed event proof payload
gain_ i atts h t = gain i atts (C.GainInput (NEA.fromNonEmpty (h :| t)))

__gain
  :: forall i outputChannels producedI consumedI producedO consumedO event proof
       payload
   . IsEvent (event proof)
  => Common.InitialGain i
  => Maybe String
  -> i
  -> event proof C.Gain
  -> C.GainInput outputChannels producedI consumedI event proof payload
  -> C.Node outputChannels producedO consumedO event proof payload
__gain mId i' atts (C.GainInput elts) = C.Node go
  where
  C.InitializeGain i = Common.toInitializeGain i'
  go parent di@(C.AudioInterpret { ids, scope, makeGain, setGain }) = keepLatest
    ( (sample_ ids (pure unit)) <#> __maybeUseName scope mId \me ->
        pure
          ( makeGain
              { id: me, parent: just parent, scope: just scope, gain: i.gain }
          )
          <|> map
            ( \(C.Gain e) -> match
                { gain: \g -> setGain { id: me, gain: g }
                }
                e
            )
            atts
          <|> oneOf
            ( NEA.toArray
                (map (\elt -> ((\y -> let C.Node x = y in x) elt) me di) elts)
            )
    )

gain
  :: forall i outputChannels produced consumed event proof payload
   . IsEvent (event proof)
  => Common.InitialGain i
  => i
  -> event proof C.Gain
  -> C.GainInput outputChannels produced consumed event proof payload
  -> C.Node outputChannels produced consumed event proof payload
gain = __gain nothing

gain'
  :: forall proxy sym i outputChannels produced produced' consumed event proof
       payload
   . IsEvent (event proof)
  => IsSymbol sym
  => Cons sym C.Input produced' produced
  => Common.InitialGain i
  => proxy sym
  -> i
  -> event proof C.Gain
  -> C.GainInput outputChannels produced' consumed event proof payload
  -> C.Node outputChannels produced consumed event proof payload
gain' px = __gain (just (reflectSymbol px))

-- highpass

__highpass
  :: forall i outputChannels producedI consumedI producedO consumedO event proof
       payload
   . IsEvent (event proof)
  => Common.InitialHighpass i
  => Maybe String
  -> i
  -> event proof C.Highpass
  -> C.Node outputChannels producedI consumedI event proof payload
  -> C.Node outputChannels producedO consumedO event proof payload
__highpass mId i' atts elt = C.Node go
  where
  C.InitializeHighpass i = Common.toInitialHighpass i'
  go
    parent
    di@(C.AudioInterpret { ids, scope, makeHighpass, setFrequency, setQ }) =
    keepLatest
      ( (sample_ ids (pure unit)) <#> __maybeUseName scope mId \me ->
          pure
            ( makeHighpass
                { id: me
                , parent: just parent
                , scope: just scope
                , frequency: i.frequency
                , q: i.q
                }
            )
            <|> map
              ( \(C.Highpass e) -> match
                  { frequency: \frequency -> setFrequency
                      { id: me, frequency }
                  , q: \q -> setQ { id: me, q }
                  }
                  e
              )
              atts
            <|> ((\y -> let C.Node x = y in x) elt) me di
      )

highpass
  :: forall i outputChannels produced consumed event proof payload
   . IsEvent (event proof)
  => Common.InitialHighpass i
  => i
  -> event proof C.Highpass
  -> C.Node outputChannels produced consumed event proof payload
  -> C.Node outputChannels produced consumed event proof payload
highpass = __highpass nothing

highpass'
  :: forall proxy sym i outputChannels produced produced' consumed event proof
       payload
   . IsEvent (event proof)
  => IsSymbol sym
  => Common.InitialHighpass i
  => Cons sym C.Input produced' produced
  => proxy sym
  -> i
  -> event proof C.Highpass
  -> C.Node outputChannels produced' consumed event proof payload
  -> C.Node outputChannels produced consumed event proof payload
highpass' px = __highpass (just (reflectSymbol px))

-- highshelf

__highshelf
  :: forall outputChannels producedI consumedI producedO consumedO event proof
       payload
   . IsEvent (event proof)
  => Maybe String
  -> C.InitializeHighshelf
  -> event proof C.Highshelf
  -> C.Node outputChannels producedI consumedI event proof payload
  -> C.Node outputChannels producedO consumedO event proof payload
__highshelf mId (C.InitializeHighshelf i) atts elt = C.Node go
  where
  go
    parent
    di@(C.AudioInterpret { ids, scope, makeHighshelf, setFrequency, setGain }) =
    keepLatest
      ( (sample_ ids (pure unit)) <#> __maybeUseName scope mId \me ->
          pure
            ( makeHighshelf
                { id: me
                , parent: just parent
                , scope: just scope
                , frequency: i.frequency
                , gain: i.gain
                }
            )
            <|> map
              ( \(C.Highshelf e) -> match
                  { frequency: \frequency -> setFrequency
                      { id: me, frequency }
                  , gain: \gn -> setGain { id: me, gain: gn }
                  }
                  e
              )
              atts
            <|> ((\y -> let C.Node x = y in x) elt) me di
      )

highshelf
  :: forall outputChannels produced consumed event proof payload
   . IsEvent (event proof)
  => C.InitializeHighshelf
  -> event proof C.Highshelf
  -> C.Node outputChannels produced consumed event proof payload
  -> C.Node outputChannels produced consumed event proof payload
highshelf = __highshelf nothing

highshelf'
  :: forall proxy sym outputChannels produced produced' consumed event proof
       payload
   . IsEvent (event proof)
  => IsSymbol sym
  => Cons sym C.Input produced' produced
  => proxy sym
  -> C.InitializeHighshelf
  -> event proof C.Highshelf
  -> C.Node outputChannels produced' consumed event proof payload
  -> C.Node outputChannels produced consumed event proof payload
highshelf' px = __highshelf (just (reflectSymbol px))

-- input

input
  :: forall outputChannels event proof payload
   . IsEvent (event proof)
  => C.Input
  -> C.Node outputChannels () () event proof payload
input (C.Input me) = C.Node go
  where
  go parent (C.AudioInterpret { scope, makeInput }) = pure
    ( makeInput
        { id: me, parent: just parent, scope: just scope }
    )

-- lowpass

__lowpass
  :: forall i outputChannels producedI consumedI producedO consumedO event proof
       payload
   . IsEvent (event proof)
  => Common.InitialLowpass i
  => Maybe String
  -> i
  -> event proof C.Lowpass
  -> C.Node outputChannels producedI consumedI event proof payload
  -> C.Node outputChannels producedO consumedO event proof payload
__lowpass mId i' atts elt = C.Node go
  where
  C.InitializeLowpass i = Common.toInitialLowpass i'
  go
    parent
    di@(C.AudioInterpret { ids, scope, makeLowpass, setFrequency, setQ }) =
    keepLatest
      ( (sample_ ids (pure unit)) <#> __maybeUseName scope mId \me ->
          pure
            ( makeLowpass
                { id: me
                , parent: just parent
                , scope: just scope
                , frequency: i.frequency
                , q: i.q
                }
            )
            <|> map
              ( \(C.Lowpass e) -> match
                  { frequency: \frequency -> setFrequency
                      { id: me, frequency }
                  , q: \q -> setQ { id: me, q }
                  }
                  e
              )
              atts
            <|> ((\y -> let C.Node x = y in x) elt) me di
      )

lowpass
  :: forall i outputChannels produced consumed event proof payload
   . IsEvent (event proof)
  => Common.InitialLowpass i
  => i
  -> event proof C.Lowpass
  -> C.Node outputChannels produced consumed event proof payload
  -> C.Node outputChannels produced consumed event proof payload
lowpass = __lowpass nothing

lowpass'
  :: forall i proxy sym outputChannels produced produced' consumed event proof
       payload
   . IsEvent (event proof)
  => IsSymbol sym
  => Cons sym C.Input produced' produced
  => Common.InitialLowpass i
  => proxy sym
  -> i
  -> event proof C.Lowpass
  -> C.Node outputChannels produced' consumed event proof payload
  -> C.Node outputChannels produced consumed event proof payload
lowpass' px = __lowpass (just (reflectSymbol px))

-- lowshelf

__lowshelf
  :: forall outputChannels producedI consumedI producedO consumedO event proof
       payload
   . IsEvent (event proof)
  => Maybe String
  -> C.InitializeLowshelf
  -> event proof C.Lowshelf
  -> C.Node outputChannels producedI consumedI event proof payload
  -> C.Node outputChannels producedO consumedO event proof payload
__lowshelf mId (C.InitializeLowshelf i) atts elt = C.Node go
  where
  go
    parent
    di@(C.AudioInterpret { ids, scope, makeLowshelf, setFrequency, setGain }) =
    keepLatest
      ( (sample_ ids (pure unit)) <#> __maybeUseName scope mId \me ->
          pure
            ( makeLowshelf
                { id: me
                , parent: just parent
                , scope: just scope
                , frequency: i.frequency
                , gain: i.gain
                }
            )
            <|> map
              ( \(C.Lowshelf e) -> match
                  { frequency: \frequency -> setFrequency
                      { id: me, frequency }
                  , gain: \gn -> setGain { id: me, gain: gn }
                  }
                  e
              )
              atts
            <|> ((\y -> let C.Node x = y in x) elt) me di
      )

lowshelf
  :: forall outputChannels produced consumed event proof payload
   . IsEvent (event proof)
  => C.InitializeLowshelf
  -> event proof C.Lowshelf
  -> C.Node outputChannels produced consumed event proof payload
  -> C.Node outputChannels produced consumed event proof payload
lowshelf = __lowshelf nothing

lowshelf'
  :: forall proxy sym outputChannels produced produced' consumed event proof
       payload
   . IsEvent (event proof)
  => IsSymbol sym
  => Cons sym C.Input produced' produced
  => proxy sym
  -> C.InitializeLowshelf
  -> event proof C.Lowshelf
  -> C.Node outputChannels produced' consumed event proof payload
  -> C.Node outputChannels produced consumed event proof payload
lowshelf' px = __lowshelf (just (reflectSymbol px))

-- loopBuf

__loopBuf
  :: forall i outputChannels produced consumed event proof payload
   . IsEvent (event proof)
  => Common.InitialLoopBuf i
  => Maybe String
  -> i
  -> event proof C.LoopBuf
  -> C.Node outputChannels produced consumed event proof payload
__loopBuf mId i' atts = C.Node go
  where
  C.InitializeLoopBuf i = Common.toInitialLoopBuf i'
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
      ( (sample_ ids (pure unit)) <#> __maybeUseName scope mId \me ->
          pure
            ( makeLoopBuf
                { id: me
                , parent: just parent
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
  :: forall i outputChannels event proof payload
   . IsEvent (event proof)
  => Common.InitialLoopBuf i
  => i
  -> event proof C.LoopBuf
  -> C.Node outputChannels () () event proof payload
loopBuf = __loopBuf nothing

loopBuf'
  :: forall proxy sym outputChannels produced event proof payload
   . IsEvent (event proof)
  => IsSymbol sym
  => Cons sym C.Input () produced
  => proxy sym
  -> C.InitializeLoopBuf
  -> event proof C.LoopBuf
  -> C.Node outputChannels produced () event proof payload
loopBuf' px = __loopBuf (just (reflectSymbol px))

-- mediaElement

__mediaElement
  :: forall outputChannels produced consumed event proof payload
   . IsEvent (event proof)
  => Maybe String
  -> C.InitializeMediaElement
  -> C.Node outputChannels produced consumed event proof payload
__mediaElement mId (C.InitializeMediaElement i) = C.Node go
  where
  go parent (C.AudioInterpret { ids, scope, makeMediaElement }) =
    keepLatest
      ( (sample_ ids (pure unit)) <#> __maybeUseName scope mId \me ->
          pure
            ( makeMediaElement
                { id: me
                , parent: just parent
                , scope: just scope
                , element: i.element
                }
            )
      )

mediaElement
  :: forall outputChannels event proof payload
   . IsEvent (event proof)
  => C.InitializeMediaElement
  -> C.Node outputChannels () () event proof payload
mediaElement = __mediaElement nothing

mediaElement'
  :: forall proxy sym outputChannels produced event proof payload
   . IsEvent (event proof)
  => IsSymbol sym
  => Cons sym C.Input () produced
  => proxy sym
  -> C.InitializeMediaElement
  -> C.Node outputChannels produced () event proof payload
mediaElement' px = __mediaElement (just (reflectSymbol px))

-- microphone

__microphone
  :: forall outputChannels produced consumed event proof payload
   . IsEvent (event proof)
  => Maybe String
  -> C.InitializeMicrophone
  -> C.Node outputChannels produced consumed event proof payload
__microphone mId (C.InitializeMicrophone i) = C.Node go
  where
  go parent (C.AudioInterpret { ids, scope, makeMicrophone }) =
    keepLatest
      ( (sample_ ids (pure unit)) <#> __maybeUseName scope mId \me ->
          pure
            ( makeMicrophone
                { id: me
                , parent: just parent
                , scope: just scope
                , microphone: i.microphone
                }
            )
      )

microphone
  :: forall outputChannels event proof payload
   . IsEvent (event proof)
  => C.InitializeMicrophone
  -> C.Node outputChannels () () event proof payload
microphone = __microphone nothing

microphone'
  :: forall proxy sym outputChannels produced event proof payload
   . IsEvent (event proof)
  => IsSymbol sym
  => Cons sym C.Input () produced
  => proxy sym
  -> C.InitializeMicrophone
  -> C.Node outputChannels produced () event proof payload
microphone' px = __microphone (just (reflectSymbol px))

-- notch

__notch
  :: forall outputChannels producedI consumedI producedO consumedO event proof
       payload
   . IsEvent (event proof)
  => Maybe String
  -> C.InitializeNotch
  -> event proof C.Notch
  -> C.Node outputChannels producedI consumedI event proof payload
  -> C.Node outputChannels producedO consumedO event proof payload
__notch mId (C.InitializeNotch i) atts elt = C.Node go
  where
  go parent di@(C.AudioInterpret { ids, scope, makeNotch, setFrequency, setQ }) =
    keepLatest
      ( (sample_ ids (pure unit)) <#> __maybeUseName scope mId \me ->
          pure
            ( makeNotch
                { id: me
                , parent: just parent
                , scope: just scope
                , frequency: i.frequency
                , q: i.q
                }
            )
            <|> map
              ( \(C.Notch e) -> match
                  { frequency: \frequency -> setFrequency
                      { id: me, frequency }
                  , q: \q -> setQ { id: me, q }
                  }
                  e
              )
              atts
            <|> ((\y -> let C.Node x = y in x) elt) me di
      )

notch
  :: forall outputChannels produced consumed event proof payload
   . IsEvent (event proof)
  => C.InitializeNotch
  -> event proof C.Notch
  -> C.Node outputChannels produced consumed event proof payload
  -> C.Node outputChannels produced consumed event proof payload
notch = __notch nothing

notch'
  :: forall proxy sym outputChannels produced produced' consumed event proof
       payload
   . IsEvent (event proof)
  => IsSymbol sym
  => Cons sym C.Input produced' produced
  => proxy sym
  -> C.InitializeNotch
  -> event proof C.Notch
  -> C.Node outputChannels produced' consumed event proof payload
  -> C.Node outputChannels produced consumed event proof payload
notch' px = __notch (just (reflectSymbol px))

-- peaking

__peaking
  :: forall outputChannels producedI consumedI producedO consumedO event proof
       payload
   . IsEvent (event proof)
  => Maybe String
  -> C.InitializePeaking
  -> event proof C.Peaking
  -> C.Node outputChannels producedI consumedI event proof payload
  -> C.Node outputChannels producedO consumedO event proof payload
__peaking mId (C.InitializePeaking i) atts elt = C.Node go
  where
  go
    parent
    di@
      ( C.AudioInterpret
          { ids, scope, makePeaking, setFrequency, setQ, setGain }
      ) =
    keepLatest
      ( (sample_ ids (pure unit)) <#> __maybeUseName scope mId \me ->
          pure
            ( makePeaking
                { id: me
                , parent: just parent
                , scope: just scope
                , frequency: i.frequency
                , q: i.q
                , gain: i.gain
                }
            )
            <|> map
              ( \(C.Peaking e) -> match
                  { frequency: \frequency -> setFrequency
                      { id: me, frequency }
                  , q: \q -> setQ { id: me, q }
                  , gain: \gn -> setGain { id: me, gain: gn }
                  }
                  e
              )
              atts
            <|> ((\y -> let C.Node x = y in x) elt) me di
      )

peaking
  :: forall outputChannels produced consumed event proof payload
   . IsEvent (event proof)
  => C.InitializePeaking
  -> event proof C.Peaking
  -> C.Node outputChannels produced consumed event proof payload
  -> C.Node outputChannels produced consumed event proof payload
peaking = __peaking nothing

peaking'
  :: forall proxy sym outputChannels produced produced' consumed event proof
       payload
   . IsEvent (event proof)
  => IsSymbol sym
  => Cons sym C.Input produced' produced
  => proxy sym
  -> C.InitializePeaking
  -> event proof C.Peaking
  -> C.Node outputChannels produced' consumed event proof payload
  -> C.Node outputChannels produced consumed event proof payload
peaking' px = __peaking (just (reflectSymbol px))

-- periodicOsc

__periodicOsc
  :: forall outputChannels produced consumed event proof payload
   . IsEvent (event proof)
  => Maybe String
  -> C.InitializePeriodicOsc
  -> event proof C.PeriodicOsc
  -> C.Node outputChannels produced consumed event proof payload
__periodicOsc mId (C.InitializePeriodicOsc i) atts = C.Node go
  where
  go
    parent
    ( C.AudioInterpret
        { ids, scope, makePeriodicOsc, setFrequency, setOnOff, setPeriodicOsc }
    ) =
    keepLatest
      ( (sample_ ids (pure unit)) <#> __maybeUseName scope mId \me ->
          pure
            ( makePeriodicOsc
                { id: me
                , parent: just parent
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
  :: forall outputChannels event proof payload
   . IsEvent (event proof)
  => C.InitializePeriodicOsc
  -> event proof C.PeriodicOsc
  -> C.Node outputChannels () () event proof payload
periodicOsc = __periodicOsc nothing

periodicOsc'
  :: forall proxy sym outputChannels produced event proof payload
   . IsEvent (event proof)
  => IsSymbol sym
  => Cons sym C.Input () produced
  => proxy sym
  -> C.InitializePeriodicOsc
  -> event proof C.PeriodicOsc
  -> C.Node outputChannels produced () event proof payload
periodicOsc' px = __periodicOsc (just (reflectSymbol px))

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
  toInitialPlayBuf :: i -> C.InitializePlayBuf

instance InitialPlayBuf C.InitializePlayBuf where
  toInitialPlayBuf = identity

instance InitialPlayBuf BrowserAudioBuffer where
  toInitialPlayBuf = toInitialPlayBuf <<< { buffer: _ }

instance
  ConvertOptionsWithDefaults PlayBufOptions { | PlayBufOptional } { | provided }
    { | PlayBufAll } =>
  InitialPlayBuf { | provided } where
  toInitialPlayBuf provided = C.InitializePlayBuf
    (convertOptionsWithDefaults PlayBufOptions defaultPlayBuf provided)

__playBuf
  :: forall i outputChannels produced consumed event proof payload
   . IsEvent (event proof)
  => InitialPlayBuf i
  => Maybe String
  -> i
  -> event proof C.PlayBuf
  -> C.Node outputChannels produced consumed event proof payload
__playBuf mId i' atts = C.Node go
  where
  C.InitializePlayBuf i = toInitialPlayBuf i'
  go
    parent
    ( C.AudioInterpret
        { ids
        , scope
        , makePlayBuf
        , setBuffer
        , setOnOff
        , setPlaybackRate
        , setBufferOffset
        }
    ) =
    keepLatest
      ( (sample_ ids (pure unit)) <#> __maybeUseName scope mId \me ->
          pure
            ( makePlayBuf
                { id: me
                , parent: just parent
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
                  }
                  e
              )
              atts
      )

playBuf
  :: forall i outputChannels event proof payload
   . IsEvent (event proof)
  => InitialPlayBuf i
  => i
  -> event proof C.PlayBuf
  -> C.Node outputChannels () () event proof payload
playBuf = __playBuf nothing

playBuf'
  :: forall proxy sym outputChannels produced event proof payload
   . IsEvent (event proof)
  => IsSymbol sym
  => Cons sym C.Input () produced
  => proxy sym
  -> C.InitializePlayBuf
  -> event proof C.PlayBuf
  -> C.Node outputChannels produced () event proof payload
playBuf' px = __playBuf (just (reflectSymbol px))

-- recorder
recorder
  :: forall outputChannels produced consumed event proof payload
   . IsEvent (event proof)
  => C.InitializeRecorder
  -> C.Node outputChannels produced consumed event proof payload
  -> C.Node outputChannels produced consumed event proof payload
recorder (C.InitializeRecorder i) elt = C.Node go
  where
  go parent di@(C.AudioInterpret { ids, scope, makeRecorder }) =
    keepLatest
      ( (sample_ ids (pure unit)) <#> \me ->
          pure
            ( makeRecorder
                { id: me, parent: just parent, scope: just scope, cb: i.cb }
            )
            <|> ((\y -> let C.Node x = y in x) elt) me di

      )

-- ref

ref
  :: forall proxy sym outputChannels consumed event proof payload
   . IsEvent (event proof)
  => IsSymbol sym
  => Cons sym C.Input () consumed
  => proxy sym
  -> C.Node outputChannels () consumed event proof payload
ref px = C.Node go
  where
  go parent (C.AudioInterpret { scope, connectXToY }) =
    pure (connectXToY { from: __appendScopeToNamedInput (reflectSymbol px) scope, to: parent })

-- sawtoothOsc

__sawtoothOsc
  :: forall outputChannels produced consumed event proof payload
   . IsEvent (event proof)
  => Maybe String
  -> C.InitializeSawtoothOsc
  -> event proof C.SawtoothOsc
  -> C.Node outputChannels produced consumed event proof payload
__sawtoothOsc mId (C.InitializeSawtoothOsc i) atts = C.Node go
  where
  go
    parent
    (C.AudioInterpret { ids, scope, makeSawtoothOsc, setFrequency, setOnOff }) =
    keepLatest
      ( (sample_ ids (pure unit)) <#> __maybeUseName scope mId \me ->
          pure
            ( makeSawtoothOsc
                { id: me
                , parent: just parent
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
  :: forall outputChannels event proof payload
   . IsEvent (event proof)
  => C.InitializeSawtoothOsc
  -> event proof C.SawtoothOsc
  -> C.Node outputChannels () () event proof payload
sawtoothOsc = __sawtoothOsc nothing

sawtoothOsc'
  :: forall proxy sym outputChannels produced event proof payload
   . IsEvent (event proof)
  => IsSymbol sym
  => Cons sym C.Input () produced
  => proxy sym
  -> C.InitializeSawtoothOsc
  -> event proof C.SawtoothOsc
  -> C.Node outputChannels produced () event proof payload
sawtoothOsc' px = __sawtoothOsc (just (reflectSymbol px))

-- sinOsc

__sinOsc
  :: forall i outputChannels produced consumed event proof payload
   . IsEvent (event proof)
  => Common.InitialSinOsc i
  => Maybe String
  -> i
  -> event proof C.SinOsc
  -> C.Node outputChannels produced consumed event proof payload
__sinOsc mId i' atts = C.Node go
  where
  C.InitializeSinOsc i = Common.toInitializeSinOsc i'
  go
    parent
    (C.AudioInterpret { ids, scope, makeSinOsc, setFrequency, setOnOff }) =
    keepLatest
      ( (sample_ ids (pure unit)) <#> __maybeUseName scope mId \me ->
          pure
            ( makeSinOsc
                { id: me
                , parent: just parent
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
  :: forall i outputChannels event proof payload
   . IsEvent (event proof)
  => Common.InitialSinOsc i
  => i
  -> event proof C.SinOsc
  -> C.Node outputChannels () () event proof payload
sinOsc = __sinOsc nothing

sinOsc'
  :: forall proxy sym i outputChannels produced event proof payload
   . IsEvent (event proof)
  => IsSymbol sym
  => Common.InitialSinOsc i
  => Cons sym C.Input () produced
  => proxy sym
  -> i
  -> event proof C.SinOsc
  -> C.Node outputChannels produced () event proof payload
sinOsc' px = __sinOsc (just (reflectSymbol px))

-- squareOsc

__squareOsc
  :: forall outputChannels produced consumed event proof payload
   . IsEvent (event proof)
  => Maybe String
  -> C.InitializeSquareOsc
  -> event proof C.SquareOsc
  -> C.Node outputChannels produced consumed event proof payload
__squareOsc mId (C.InitializeSquareOsc i) atts = C.Node go
  where
  go
    parent
    (C.AudioInterpret { ids, scope, makeSquareOsc, setFrequency, setOnOff }) =
    keepLatest
      ( (sample_ ids (pure unit)) <#> __maybeUseName scope mId \me ->
          pure
            ( makeSquareOsc
                { id: me
                , parent: just parent
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
  :: forall outputChannels event proof payload
   . IsEvent (event proof)
  => C.InitializeSquareOsc
  -> event proof C.SquareOsc
  -> C.Node outputChannels () () event proof payload
squareOsc = __squareOsc nothing

squareOsc'
  :: forall proxy sym outputChannels produced event proof payload
   . IsEvent (event proof)
  => IsSymbol sym
  => Cons sym C.Input () produced
  => proxy sym
  -> C.InitializeSquareOsc
  -> event proof C.SquareOsc
  -> C.Node outputChannels produced () event proof payload
squareOsc' px = __squareOsc (just (reflectSymbol px))

-- speaker
speaker
  :: forall outputChannels produced produced' consumed event payload
   . IsEvent (event Proof0)
  => Nub produced produced
  => Union produced consumed produced'
  => Nub produced' produced
  => C.GainInput outputChannels produced consumed event Proof0 payload
  -> C.AudioInterpret event Proof0 payload
  -> event Proof0 payload
speaker (C.GainInput elts) di@(C.AudioInterpret { ids, makeSpeaker }) =
  keepLatest
    ( (sample_ ids (pure unit)) <#> \me ->
        pure (makeSpeaker { id: me })
          <|> oneOf
            (map (\elt -> ((\y -> let C.Node x = y in x) elt) me di) elts)
    )

speaker2
  :: forall produced produced' consumed event payload
   . IsEvent (event Proof0)
  => Nub produced produced
  => Union produced consumed produced'
  => Nub produced' produced
  => C.GainInput D2 produced consumed event Proof0 payload
  -> C.AudioInterpret event Proof0 payload
  -> event Proof0 payload
speaker2 = speaker

-- pan

__pan
  :: forall outputChannels producedI consumedI producedO consumedO event proof
       payload
   . IsEvent (event proof)
  => Maybe String
  -> C.InitializeStereoPanner
  -> event proof C.StereoPanner
  -> C.Node outputChannels producedI consumedI event proof payload
  -> C.Node outputChannels producedO consumedO event proof payload
__pan mId (C.InitializeStereoPanner i) atts elt = C.Node go
  where
  go parent di@(C.AudioInterpret { ids, scope, makeStereoPanner, setPan }) =
    keepLatest
      ( (sample_ ids (pure unit)) <#> __maybeUseName scope mId \me ->
          pure
            ( makeStereoPanner
                { id: me, parent: just parent, scope: just scope, pan: i.pan }
            )
            <|> map
              ( \(C.StereoPanner e) -> match
                  { pan: \pn -> setPan
                      { id: me, pan: pn }
                  }
                  e
              )
              atts
            <|> ((\y -> let C.Node x = y in x) elt) me di
      )

pan
  :: forall outputChannels produced consumed event proof payload
   . IsEvent (event proof)
  => C.InitializeStereoPanner
  -> event proof C.StereoPanner
  -> C.Node outputChannels produced consumed event proof payload
  -> C.Node outputChannels produced consumed event proof payload
pan = __pan nothing

pan'
  :: forall proxy sym outputChannels produced produced' consumed event proof
       payload
   . IsEvent (event proof)
  => IsSymbol sym
  => Cons sym C.Input produced' produced
  => proxy sym
  -> C.InitializeStereoPanner
  -> event proof C.StereoPanner
  -> C.Node outputChannels produced' consumed event proof payload
  -> C.Node outputChannels produced consumed event proof payload
pan' px = __pan (just (reflectSymbol px))

-- triangleOsc

__triangleOsc
  :: forall outputChannels produced consumed event proof payload
   . IsEvent (event proof)
  => Maybe String
  -> C.InitializeTriangleOsc
  -> event proof C.TriangleOsc
  -> C.Node outputChannels produced consumed event proof payload
__triangleOsc mId (C.InitializeTriangleOsc i) atts = C.Node go
  where
  go
    parent
    (C.AudioInterpret { ids, scope, makeTriangleOsc, setFrequency, setOnOff }) =
    keepLatest
      ( (sample_ ids (pure unit)) <#> __maybeUseName scope mId \me ->
          pure
            ( makeTriangleOsc
                { id: me
                , parent: just parent
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
  :: forall outputChannels event proof payload
   . IsEvent (event proof)
  => C.InitializeTriangleOsc
  -> event proof C.TriangleOsc
  -> C.Node outputChannels () () event proof payload
triangleOsc = __triangleOsc nothing

triangleOsc'
  :: forall proxy sym outputChannels produced event proof payload
   . IsEvent (event proof)
  => IsSymbol sym
  => Cons sym C.Input () produced
  => proxy sym
  -> C.InitializeTriangleOsc
  -> event proof C.TriangleOsc
  -> C.Node outputChannels produced () event proof payload
triangleOsc' px = __triangleOsc (just (reflectSymbol px))

-- waveshaper

__waveshaper
  :: forall outputChannels producedI consumedI producedO consumedO event proof
       payload
   . IsEvent (event proof)
  => Maybe String
  -> C.InitializeWaveshaper
  -> C.Node outputChannels producedI consumedI event proof payload
  -> C.Node outputChannels producedO consumedO event proof payload
__waveshaper mId (C.InitializeWaveshaper i) elt = C.Node go
  where
  go parent di@(C.AudioInterpret { ids, scope, makeWaveShaper }) =
    keepLatest
      ( (sample_ ids (pure unit)) <#> __maybeUseName scope mId \me ->
          pure
            ( makeWaveShaper
                { id: me
                , parent: just parent
                , scope: just scope
                , curve: i.curve
                , oversample: i.oversample
                }
            ) <|> ((\y -> let C.Node x = y in x) elt) me di
      )

waveshaper
  :: forall outputChannels produced consumed event proof payload
   . IsEvent (event proof)
  => C.InitializeWaveshaper
  -> C.Node outputChannels produced consumed event proof payload
  -> C.Node outputChannels produced consumed event proof payload
waveshaper = __waveshaper nothing

waveshaper'
  :: forall proxy sym outputChannels produced' produced consumed event proof
       payload
   . IsEvent (event proof)
  => IsSymbol sym
  => Cons sym C.Input produced' produced
  => proxy sym
  -> C.InitializeWaveshaper
  -> C.Node outputChannels produced' consumed event proof payload
  -> C.Node outputChannels produced consumed event proof payload
waveshaper' px = __waveshaper (just (reflectSymbol px))

-- todo: tumult
