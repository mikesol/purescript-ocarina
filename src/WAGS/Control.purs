module WAGS.Control where

import Prelude

import Control.Alt ((<|>))
import Control.Comonad (extract)
import Control.Plus (empty)
import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Data.Array (uncons)
import Data.Array.NonEmpty (fromNonEmpty)
import Data.Array.NonEmpty as NEA
import Data.Foldable (oneOf)
import Data.Homogeneous (class HomogeneousRowLabels)
import Data.Homogeneous.Variant (homogeneous)
import Data.Int (pow)
import Data.NonEmpty ((:|))
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
import Prim.Row (class Union, class Nub)
import Prim.Symbol as Sym
import Safe.Coerce (coerce)
import Simple.JSON as JSON
import Type.Proxy (Proxy(..))
import Type.Row.Homogeneous (class Homogeneous)
import WAGS.Common as Common
import WAGS.Common as Comomn
import WAGS.Core (class TLOrd, ChannelCountMode(..), ChannelInterpretation(..), MeOrParent(..), Po2(..), useMeIfMe, useParentIfParent)
import WAGS.Core as C
import WAGS.Parameter (AudioParameter, InitialAudioParameter)
import WAGS.WebAPI (AnalyserNodeCb(..), BrowserAudioBuffer)

-- __maybeUseName scope mId
-- tmpIdentity = identity
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

-- gain input
singleton
  :: forall outputChannels produced consumed event payload
   . IsEvent event
  => C.Node outputChannels produced consumed event payload
  -> C.AudioInput outputChannels produced consumed event payload
singleton a = C.AudioInput (NEA.singleton a)

audioInputCons
  :: forall outputChannels produced0 produced1 produced2 ord consumed0 consumed1
       consumed2 consumed3 event payload
   . IsEvent event
  => Sym.Compare produced0 produced1 ord
  => TLOrd ord produced1 produced1 produced0 produced2
  => Union consumed0 consumed1 consumed2
  => Nub consumed2 consumed3
  => C.Node outputChannels produced0 consumed0 event payload
  -> Array (C.Node outputChannels produced1 consumed1 event payload)
  -> C.AudioInput outputChannels produced2 consumed3 event payload
audioInputCons a b = C.AudioInput (fromNonEmpty (coerce a :| coerce b))

infixr 6 audioInputCons as :*

audioInputCons2
  :: forall outputChannels produced0 produced1 produced2 ord consumed0 consumed1
       consumed2 consumed3 event payload
   . IsEvent event
  => Sym.Compare produced0 produced1 ord
  => TLOrd ord produced1 produced1 produced0 produced2
  => Union consumed0 consumed1 consumed2
  => Nub consumed2 consumed3
  => C.Node outputChannels produced0 consumed0 event payload
  -> C.AudioInput outputChannels produced1 consumed1 event payload
  -> C.AudioInput outputChannels produced2 consumed3 event payload
audioInputCons2 a b = C.AudioInput (NEA.cons (coerce a) (coerce b))

audioInputAdd
  :: forall outputChannels produced0 produced1 produced2 ord consumed0 consumed1
       consumed2 consumed3 event payload
   . IsEvent event
  => Sym.Compare produced0 produced1 ord
  => TLOrd ord produced1 produced1 produced0 produced2
  => Union consumed0 consumed1 consumed2
  => Nub consumed2 consumed3
  => C.Node outputChannels produced0 consumed0 event payload
  -> C.Node outputChannels produced1 consumed1 event payload
  -> C.AudioInput outputChannels produced2 consumed3 event payload
audioInputAdd a b = C.AudioInput (NEA.cons (coerce a) (NEA.singleton (coerce b)))

class AudioInputSmoosh f where
  audioInputSmoosh
    :: forall outputChannels produced0 produced1 produced2 ord consumed0 consumed1
         consumed2 consumed3 event payload
     . IsEvent event
    => Sym.Compare produced0 produced1 ord
    => TLOrd ord produced1 produced1 produced0 produced2
    => Union consumed0 consumed1 consumed2
    => Nub consumed2 consumed3
    => C.Node outputChannels produced0 consumed0 event payload
    -> f outputChannels produced1 consumed1 event payload
    -> C.AudioInput outputChannels produced2 consumed3 event payload

instance AudioInputSmoosh C.AudioInput where
  audioInputSmoosh = audioInputCons2

instance AudioInputSmoosh C.Node where
  audioInputSmoosh = audioInputAdd

infixr 6 audioInputSmoosh as ~

audioInputCons3
  :: forall outputChannels produced consumed event payload
   . IsEvent event
  => C.Node outputChannels produced consumed event payload
  -> Array (C.Node outputChannels produced consumed event payload)
  -> C.AudioInput outputChannels produced consumed event payload
audioInputCons3 a b = C.AudioInput (fromNonEmpty (a :| b))

infixr 6 audioInputCons3 as :::*

audioInputAppend
  :: forall outputChannels produced0 produced1 produced2 ord consumed0 consumed1
       consumed2 consumed3 event payload
   . IsEvent event
  => Sym.Compare produced0 produced1 ord
  => TLOrd ord produced1 produced1 produced0 produced2
  => Union consumed0 consumed1 consumed2
  => Nub consumed2 consumed3
  => C.AudioInput outputChannels produced0 consumed0 event payload
  -> C.AudioInput outputChannels produced1 consumed1 event payload
  -> C.AudioInput outputChannels produced2 consumed3 event payload
audioInputAppend a b = C.AudioInput (coerce a <> coerce b)

infixr 6 audioInputAppend as <>*

-- allpass

class AllpassCtor f where
  allpass
    :: forall i (outputChannels :: Type) produced consumed event payload
     . IsEvent event
    => Common.InitialAllpass i
    => i
    -> event C.Allpass
    -> f outputChannels produced consumed event payload
    -> C.Node outputChannels produced consumed event payload

instance AllpassCtor C.Node where
  allpass i' atts n = allpass i' atts (singleton n)

instance AllpassCtor C.AudioInput where
  allpass i' atts (C.AudioInput elts) = C.Node go
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
              ( NEA.toArray
                  (map (\elt -> ((\y -> let C.Node x = y in x) elt) (Parent me) di) elts)
              )
      )

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
  :: forall i outputChannels produced consumed event payload
   . IsEvent event
  => InitialAnalyser i
  => i
  -> event C.Analyser
  -> C.Node outputChannels produced consumed event payload
  -> C.Node outputChannels produced consumed event payload
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
       processorOptions produced consumed event payload
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
  -> C.Node numberOfOutputs produced consumed event payload
  -> C.Node numberOfOutputs produced consumed event payload
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
       processorOptions produced consumed event payload
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
  -> C.Node numberOfOutputs produced consumed event payload
  -> C.Node numberOfOutputs produced consumed event payload
audioWorklet = __audioWorklet

-- bandpass
class BandpassCtor f where
  bandpass
    :: forall i (outputChannels :: Type) produced consumed event payload
     . IsEvent event
    => Common.InitialBandpass i
    => i
    -> event C.Bandpass
    -> f outputChannels produced consumed event payload
    -> C.Node outputChannels produced consumed event payload

instance BandpassCtor C.Node where
  bandpass i' atts n = bandpass i' atts (singleton n)

instance BandpassCtor C.AudioInput where
  bandpass i' atts (C.AudioInput elts) = C.Node go
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
              ( NEA.toArray
                  (map (\elt -> ((\y -> let C.Node x = y in x) elt) (Parent me) di) elts)
              )
      )

bandpass_ i a = bandpass i empty a

-- constant

__constant
  :: forall i outputChannels produced consumed event payload
   . IsEvent event
  => Common.InitialConstant i
  => i
  -> event C.Constant
  -> C.Node outputChannels produced consumed event payload
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
  :: forall i outputChannels event payload
   . IsEvent event
  => Common.InitialConstant i
  => i
  -> event C.Constant
  -> C.Node outputChannels "" C.G_ event payload
constant = __constant

constant_ i = constant i empty

-- convolver

class ConvolverCtor f where
  convolver
    :: forall i (outputChannels :: Type) produced consumed event payload
     . IsEvent event
    => Common.InitialConvolver i
    => i
    -> f outputChannels produced consumed event payload
    -> C.Node outputChannels produced consumed event payload

instance ConvolverCtor C.AudioInput where
  convolver i' (C.AudioInput elts) = C.Node go
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
                ( NEA.toArray
                    (map (\elt -> ((\y -> let C.Node x = y in x) elt) (Parent me) di) elts)
                )
        )

instance ConvolverCtor C.Node where
  convolver i' n = convolver i' (singleton n)

-- delay
class DelayCtor f where
  delay
    :: forall i (outputChannels :: Type) produced consumed event payload
     . IsEvent event
    => Common.InitialDelay i
    => i
    -> event C.Delay
    -> f outputChannels produced consumed event payload
    -> C.Node outputChannels produced consumed event payload

instance DelayCtor C.Node where
  delay i' atts n = delay i' atts (singleton n)

instance DelayCtor C.AudioInput where
  delay i' atts (C.AudioInput elts) = C.Node go
    where
    C.InitializeDelay i = Common.toInitializeDelay i'
    go parent di@(C.AudioInterpret { ids, scope, makeDelay, setDelay }) = keepLatest
      ( (sample_ ids (bang unit)) <#> parentalSupervision parent \me ->
          bang
            ( makeDelay
                { id: me, parent: useParentIfParent parent, scope: just scope, delayTime: i.delayTime }
            )
            <|> map
              ( \(C.Delay e) -> match
                  { delayTime: \g -> setDelay { id: me, delayTime: g }
                  }
                  e
              )
              atts
            <|> oneOf
              ( NEA.toArray
                  (map (\elt -> ((\y -> let C.Node x = y in x) elt) (Parent me) di) elts)
              )
      )

delay_ i a = delay i empty a

-- dynamics compressor
class DynamicsCompressorCtor f where
  dynamicsCompressor
    :: forall i (outputChannels :: Type) produced consumed event payload
     . IsEvent event
    => Common.InitialDynamicsCompressor i
    => i
    -> event C.DynamicsCompressor
    -> f outputChannels produced consumed event payload
    -> C.Node outputChannels produced consumed event payload

instance DynamicsCompressorCtor C.Node where
  dynamicsCompressor i' atts n = dynamicsCompressor i' atts (singleton n)

instance DynamicsCompressorCtor C.AudioInput where
  dynamicsCompressor i' atts (C.AudioInput elts) = C.Node go
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
                ( NEA.toArray
                    (map (\elt -> ((\y -> let C.Node x = y in x) elt) (Parent me) di) elts)
                )
        )

dynamicsCompressor_ i = dynamicsCompressor i empty

-- gain
class GainCtor f where
  gain
    :: forall i (outputChannels :: Type) produced consumed event payload
     . IsEvent event
    => Common.InitialGain i
    => i
    -> event C.Gain
    -> f outputChannels produced consumed event payload
    -> C.Node outputChannels produced consumed event payload

instance GainCtor C.Node where
  gain i' atts n = gain i' atts (singleton n)

instance GainCtor C.AudioInput where
  gain i' atts (C.AudioInput elts) = C.Node go
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
              ( NEA.toArray
                  (map (\elt -> ((\y -> let C.Node x = y in x) elt) (Parent me) di) elts)
              )
      )

gain_ i a = gain i empty a

-- highpass
class HighpassCtor f where
  highpass
    :: forall i (outputChannels :: Type) produced consumed event payload
     . IsEvent event
    => Common.InitialHighpass i
    => i
    -> event C.Highpass
    -> f outputChannels produced consumed event payload
    -> C.Node outputChannels produced consumed event payload

instance HighpassCtor C.Node where
  highpass i' atts n = highpass i' atts (singleton n)

instance HighpassCtor C.AudioInput where
  highpass i' atts (C.AudioInput elts) = C.Node go
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
              ( NEA.toArray
                  (map (\elt -> ((\y -> let C.Node x = y in x) elt) (Parent me) di) elts)
              )
      )

highpass_ i a = highpass i empty a

-- highshelf
class HighshelfCtor f where
  highshelf
    :: forall i (outputChannels :: Type) produced consumed event payload
     . IsEvent event
    => Common.InitialHighshelf i
    => i
    -> event C.Highshelf
    -> f outputChannels produced consumed event payload
    -> C.Node outputChannels produced consumed event payload

instance HighshelfCtor C.Node where
  highshelf i' atts n = highshelf i' atts (singleton n)

instance HighshelfCtor C.AudioInput where
  highshelf i' atts (C.AudioInput elts) = C.Node go
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
              ( NEA.toArray
                  (map (\elt -> ((\y -> let C.Node x = y in x) elt) (Parent me) di) elts)
              )
      )

highshelf_ i a = highshelf i empty a

-- iirFilter

iirFilter
  :: forall i f proxy (feedforward :: Type) (feedback :: Type) (outputChannels :: Type) produced consumed event
       payload
   . IsEvent event
  => IIRFilterCtor f
  => Lt D2 feedforward
  => Lt D2 feedback
  => Common.InitialIIRFilter i feedforward feedback
  => i
  -> f outputChannels produced consumed event payload
  -> C.Node outputChannels produced consumed event payload
iirFilter = iirFilter' (Proxy :: _ feedforward) (Proxy :: _ feedback)

class IIRFilterCtor f where
  iirFilter'
    :: forall i proxy (feedforward :: Type) (feedback :: Type) (outputChannels :: Type) produced consumed event
         payload
     . IsEvent event
    => Lt D2 feedforward
    => Lt D2 feedback
    => Common.InitialIIRFilter i feedforward feedback
    => proxy feedforward
    -> proxy feedback
    -> i
    -> f outputChannels produced consumed event payload
    -> C.Node outputChannels produced consumed event payload

instance IIRFilterCtor C.Node where
  iirFilter' fwd bk i' n = iirFilter' fwd bk i' (singleton n)

instance IIRFilterCtor C.AudioInput where
  iirFilter' fwd bk i' (C.AudioInput elts) = C.Node go
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
                ( NEA.toArray
                    (map (\elt -> ((\y -> let C.Node x = y in x) elt) (Parent me) di) elts)
                )
        )

-- lowpass
class LowpassCtor f where
  lowpass
    :: forall i (outputChannels :: Type) produced consumed event payload
     . IsEvent event
    => Common.InitialLowpass i
    => i
    -> event C.Lowpass
    -> f outputChannels produced consumed event payload
    -> C.Node outputChannels produced consumed event payload

instance LowpassCtor C.Node where
  lowpass i' atts n = lowpass i' atts (singleton n)

instance LowpassCtor C.AudioInput where
  lowpass i' atts (C.AudioInput elts) = C.Node go
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
              ( NEA.toArray
                  (map (\elt -> ((\y -> let C.Node x = y in x) elt) (Parent me) di) elts)
              )
      )

lowpass_ i a = lowpass i empty a

-- lowshelf
class LowshelfCtor f where
  lowshelf
    :: forall i (outputChannels :: Type) produced consumed event payload
     . IsEvent event
    => Common.InitialLowshelf i
    => i
    -> event C.Lowshelf
    -> f outputChannels produced consumed event payload
    -> C.Node outputChannels produced consumed event payload

instance LowshelfCtor C.Node where
  lowshelf i' atts n = lowshelf i' atts (singleton n)

instance LowshelfCtor C.AudioInput where
  lowshelf i' atts (C.AudioInput elts) = C.Node go
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
              ( NEA.toArray
                  (map (\elt -> ((\y -> let C.Node x = y in x) elt) (Parent me) di) elts)
              )
      )

lowshelf_ i a = lowshelf i empty a

-- loopBuf

__loopBuf
  :: forall i outputChannels produced consumed event payload
   . IsEvent event
  => Common.InitialLoopBuf i
  => i
  -> event C.LoopBuf
  -> C.Node outputChannels produced consumed event payload
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
  :: forall i outputChannels event payload
   . IsEvent event
  => Common.InitialLoopBuf i
  => i
  -> event C.LoopBuf
  -> C.Node outputChannels "" C.G_ event payload
loopBuf = __loopBuf

loopBuf_ i = loopBuf i empty

-- mediaElement

__mediaElement
  :: forall outputChannels produced consumed event payload
   . IsEvent event
  => C.InitializeMediaElement
  -> C.Node outputChannels produced consumed event payload
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
  :: forall outputChannels event payload
   . IsEvent event
  => C.InitializeMediaElement
  -> C.Node outputChannels "" C.G_ event payload
mediaElement = __mediaElement

-- microphone

__microphone
  :: forall i outputChannels produced consumed event payload
   . IsEvent event
  => Common.InitialMicrophone i
  => i
  -> C.Node outputChannels produced consumed event payload
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
  :: forall i outputChannels event payload
   . IsEvent event
  => Common.InitialMicrophone i
  => i
  -> C.Node outputChannels "" C.G_ event payload
microphone = __microphone

-- notch
class NotchCtor f where
  notch
    :: forall i (outputChannels :: Type) produced consumed event payload
     . IsEvent event
    => Common.InitialNotch i
    => i
    -> event C.Notch
    -> f outputChannels produced consumed event payload
    -> C.Node outputChannels produced consumed event payload

instance NotchCtor C.Node where
  notch i' atts n = notch i' atts (singleton n)

instance NotchCtor C.AudioInput where
  notch i' atts (C.AudioInput elts) = C.Node go
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
              ( NEA.toArray
                  (map (\elt -> ((\y -> let C.Node x = y in x) elt) (Parent me) di) elts)
              )
      )

notch_ i a = notch i empty a

-- peaking
class PeakingCtor f where
  peaking
    :: forall i (outputChannels :: Type) produced consumed event payload
     . IsEvent event
    => Common.InitialPeaking i
    => i
    -> event C.Peaking
    -> f outputChannels produced consumed event payload
    -> C.Node outputChannels produced consumed event payload

instance PeakingCtor C.Node where
  peaking i' atts n = peaking i' atts (singleton n)

instance PeakingCtor C.AudioInput where
  peaking i' atts (C.AudioInput elts) = C.Node go
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
              ( NEA.toArray
                  (map (\elt -> ((\y -> let C.Node x = y in x) elt) (Parent me) di) elts)
              )
      )

peaking_ i a = peaking i empty a

-- periodicOsc

__periodicOsc
  :: forall i outputChannels produced consumed event payload
   . IsEvent event
  => Common.InitialPeriodicOsc i
  => i
  -> event C.PeriodicOsc
  -> C.Node outputChannels produced consumed event payload
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
  :: forall i outputChannels event payload
   . IsEvent event
  => Common.InitialPeriodicOsc i
  => i
  -> event C.PeriodicOsc
  -> C.Node outputChannels "" C.G_ event payload
periodicOsc = __periodicOsc

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
  :: forall i outputChannels produced consumed event payload
   . IsEvent event
  => Common.InitialPlayBuf i
  => i
  -> event C.PlayBuf
  -> C.Node outputChannels produced consumed event payload
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
  :: forall i outputChannels event payload
   . IsEvent event
  => Common.InitialPlayBuf i
  => i
  -> event C.PlayBuf
  -> C.Node outputChannels "" C.G_ event payload
playBuf = __playBuf

-- recorder
recorder
  :: forall i outputChannels produced consumed event payload
   . IsEvent event
  => Common.InitialRecorder i
  => i
  -> C.Node outputChannels produced consumed event payload
  -> C.Node outputChannels produced consumed event payload
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
  :: forall i outputChannels produced consumed event payload
   . IsEvent event
  => Common.InitialSawtoothOsc i
  => i
  -> event C.SawtoothOsc
  -> C.Node outputChannels produced consumed event payload
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
  :: forall i outputChannels event payload
   . IsEvent event
  => Common.InitialSawtoothOsc i
  => i
  -> event C.SawtoothOsc
  -> C.Node outputChannels "" C.G_ event payload
sawtoothOsc = __sawtoothOsc

sawtoothOsc_ i = sawtoothOsc i empty

-- sinOsc

__sinOsc
  :: forall i outputChannels produced consumed event payload
   . IsEvent event
  => Common.InitialSinOsc i
  => i
  -> event C.SinOsc
  -> C.Node outputChannels produced consumed event payload
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
  :: forall i outputChannels event payload
   . IsEvent event
  => Common.InitialSinOsc i
  => i
  -> event C.SinOsc
  -> C.Node outputChannels "" C.G_ event payload
sinOsc = __sinOsc

sinOsc_ a = sinOsc a empty

-- squareOsc

__squareOsc
  :: forall i outputChannels produced consumed event payload
   . IsEvent event
  => Common.InitialSquareOsc i
  => i
  -> event C.SquareOsc
  -> C.Node outputChannels produced consumed event payload
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
  :: forall i outputChannels event payload
   . IsEvent event
  => Common.InitialSquareOsc i
  => i
  -> event C.SquareOsc
  -> C.Node outputChannels "" C.G_ event payload
squareOsc = __squareOsc

squareOsc_ i = squareOsc i empty

-- speaker
speaker
  :: forall outputChannels event payload
   . IsEvent event
  => C.AudioInput outputChannels "" C.G_ event payload
  -> C.AudioInterpret event payload
  -> event payload
speaker (C.AudioInput elts) di@(C.AudioInterpret { ids, makeSpeaker }) =
  keepLatest
    ( (sample_ ids (bang unit)) <#> \me ->
        bang (makeSpeaker { id: me })
          <|> oneOf
            (map (\elt -> ((\y -> let C.Node x = y in x) elt) (Parent me) di) elts)
    )

speaker2
  :: forall event payload
   . IsEvent event
  => C.AudioInput D2 "" C.G_ event payload
  -> C.AudioInterpret event payload
  -> event payload
speaker2 = speaker

-- pan
class StereoPannerCtor f where
  pan
    :: forall i (outputChannels :: Type) produced consumed event payload
     . IsEvent event
    => Common.InitialStereoPanner i
    => i
    -> event C.StereoPanner
    -> f outputChannels produced consumed event payload
    -> C.Node outputChannels produced consumed event payload

instance StereoPannerCtor C.Node where
  pan i' atts n = pan i' atts (singleton n)

instance StereoPannerCtor C.AudioInput where
  pan i' atts (C.AudioInput elts) = C.Node go
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
              ( NEA.toArray
                  (map (\elt -> ((\y -> let C.Node x = y in x) elt) (Parent me) di) elts)
              )
      )

pan_ i a = pan i empty a

-- triangleOsc

__triangleOsc
  :: forall i outputChannels produced consumed event payload
   . IsEvent event
  => Common.InitialTriangleOsc i
  => i
  -> event C.TriangleOsc
  -> C.Node outputChannels produced consumed event payload
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
  :: forall i outputChannels event payload
   . IsEvent event
  => Common.InitialTriangleOsc i
  => i
  -> event C.TriangleOsc
  -> C.Node outputChannels "" C.G_ event payload
triangleOsc = __triangleOsc

triangleOsc_ i = triangleOsc i empty

-- waveShaper

class WaveShaperCtor f where
  waveShaper
    :: forall i (outputChannels :: Type) produced consumed event payload
     . IsEvent event
    => Common.InitialWaveShaper i
    => i
    -> f outputChannels produced consumed event payload
    -> C.Node outputChannels produced consumed event payload

instance WaveShaperCtor C.Node where
  waveShaper i' n = waveShaper i' (singleton n)

instance WaveShaperCtor C.AudioInput where
  waveShaper i' (C.AudioInput elts) = C.Node go
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
              ( NEA.toArray
                  (map (\elt -> ((\y -> let C.Node x = y in x) elt) (Parent me) di) elts)
              )
        )
