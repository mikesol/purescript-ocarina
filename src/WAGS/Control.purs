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
import Data.Maybe as DM
import Data.NonEmpty ((:|))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Typelevel.Num (class Lt, class Nat, class Pos, class Pred, D1, D2, pred, toInt)
import Data.Variant (Unvariant(..), match, unvariant)
import Data.Variant.Maybe (Maybe, just, maybe, nothing)
import Data.Vec (Vec, toArray)
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
tmpIdentity = identity

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

gainInputCons
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
gainInputCons a b = C.AudioInput (fromNonEmpty (coerce a :| coerce b))

infixr 6 gainInputCons as :*

gainInputCons2
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
gainInputCons2 a b = C.AudioInput (NEA.cons (coerce a) (coerce b))

infixr 6 gainInputCons2 as ::*

gainInputCons3
  :: forall outputChannels produced consumed event payload
   . IsEvent event
  => C.Node outputChannels produced consumed event payload
  -> Array (C.Node outputChannels produced consumed event payload)
  -> C.AudioInput outputChannels produced consumed event payload
gainInputCons3 a b = C.AudioInput (fromNonEmpty (a :| b))

infixr 6 gainInputCons3 as :::*

gainInputAppend
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
gainInputAppend a b = C.AudioInput (coerce a <> coerce b)

infixr 6 gainInputAppend as <>*

-- allpass

-- allpass
__allpass
  :: forall i outputChannels producedI consumedI producedO consumedO event
       payload
   . IsEvent event
  => Common.InitialAllpass i
  => i
  -> event C.Allpass
  -> C.AudioInput outputChannels producedI consumedI event payload
  -> C.Node outputChannels producedO consumedO event payload
__allpass i' atts (C.AudioInput elts) = C.Node go
  where
  C.InitializeAllpass i = Common.toInitializeAllpass i'
  go parent di@(C.AudioInterpret { ids, scope, makeAllpass, setFrequency, setQ }) = keepLatest
    ( (sample_ ids (bang unit)) <#> tmpIdentity \me ->
        bang
          ( makeAllpass
              { id: useMeIfMe parent me, parent: useParentIfParent parent, scope: just scope, frequency: i.frequency, q: i.q }
          )
          <|> map
            ( \(C.Allpass e) -> match
                { frequency: \g -> setFrequency { id: useMeIfMe parent me, frequency: g }
                , q: \g -> setQ { id: useMeIfMe parent me, q: g }
                }
                e
            )
            atts
          <|> oneOf
            ( NEA.toArray
                (map (\elt -> ((\y -> let C.Node x = y in x) elt) (Parent me) di) elts)
            )
    )

allpass
  :: forall i outputChannels produced consumed ord event payload
   . IsEvent event
  => Common.InitialAllpass i
  => Sym.Compare "" produced ord
  => TLOrd ord produced produced "" produced
  => Nub consumed consumed
  => i
  -> event C.Allpass
  -> Array (C.Node outputChannels produced consumed event payload)
  -> C.Node outputChannels produced consumed event payload
allpass i e a = case uncons a of
  DM.Nothing -> allpassx i e (constant 0.0 empty :* ([] :: Array (C.Node outputChannels produced consumed event payload)))
  DM.Just { head, tail } -> allpassx i e (head :::* tail)

allpass_ i a = allpass i empty a

allpassx
  :: forall i outputChannels produced consumed event payload
   . IsEvent event
  => Common.InitialAllpass i
  => i
  -> event C.Allpass
  -> C.AudioInput outputChannels produced consumed event payload
  -> C.Node outputChannels produced consumed event payload
allpassx = __allpass

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
      ( (sample_ ids (bang unit)) <#> \me ->
          bang
            ( makeAnalyser
                { id: useMeIfMe parent me
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
                  { cb: \cb -> setAnalyserNodeCb { id: useMeIfMe parent me, cb }
                  }
                  e
              )
              atts
            <|> ((\y -> let C.Node x = y in x) elt) (Parent me) di

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
      ( (sample_ ids (bang unit)) <#> tmpIdentity \me ->
          bang
            ( makeAudioWorkletNode
                { id: useMeIfMe parent me
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
                  { id: useMeIfMe parent me
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
__bandpass
  :: forall i outputChannels producedI consumedI producedO consumedO event
       payload
   . IsEvent event
  => Common.InitialBandpass i
  => i
  -> event C.Bandpass
  -> C.AudioInput outputChannels producedI consumedI event payload
  -> C.Node outputChannels producedO consumedO event payload
__bandpass i' atts (C.AudioInput elts) = C.Node go
  where
  C.InitializeBandpass i = Common.toInitializeBandpass i'
  go parent di@(C.AudioInterpret { ids, scope, makeBandpass, setFrequency, setQ }) = keepLatest
    ( (sample_ ids (bang unit)) <#> tmpIdentity \me ->
        bang
          ( makeBandpass
              { id: useMeIfMe parent me, parent: useParentIfParent parent, scope: just scope, frequency: i.frequency, q: i.q }
          )
          <|> map
            ( \(C.Bandpass e) -> match
                { frequency: \g -> setFrequency { id: useMeIfMe parent me, frequency: g }
                , q: \g -> setQ { id: useMeIfMe parent me, q: g }
                }
                e
            )
            atts
          <|> oneOf
            ( NEA.toArray
                (map (\elt -> ((\y -> let C.Node x = y in x) elt) (Parent me) di) elts)
            )
    )

bandpass
  :: forall i outputChannels produced consumed ord event payload
   . IsEvent event
  => Common.InitialBandpass i
  => Sym.Compare "" produced ord
  => TLOrd ord produced produced "" produced
  => Nub consumed consumed
  => i
  -> event C.Bandpass
  -> Array (C.Node outputChannels produced consumed event payload)
  -> C.Node outputChannels produced consumed event payload
bandpass i e a = case uncons a of
  DM.Nothing -> bandpassx i e (constant 0.0 empty :* ([] :: Array (C.Node outputChannels produced consumed event payload)))
  DM.Just { head, tail } -> bandpassx i e (head :::* tail)

bandpass_ i a = bandpass i empty a

bandpassx
  :: forall i outputChannels produced consumed event payload
   . IsEvent event
  => Common.InitialBandpass i
  => i
  -> event C.Bandpass
  -> C.AudioInput outputChannels produced consumed event payload
  -> C.Node outputChannels produced consumed event payload
bandpassx = __bandpass

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
      ( (sample_ ids (bang unit)) <#> tmpIdentity \me ->
          bang
            ( makeConstant
                { id: useMeIfMe parent me
                , parent: useParentIfParent parent
                , scope: just scope
                , offset: i.offset
                }
            )
            <|> map
              ( \(C.Constant e) -> match
                  { offset: \offset -> setOffset
                      { id: useMeIfMe parent me, offset }
                  , onOff: \onOff -> setOnOff { id: useMeIfMe parent me, onOff }
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
  -> C.Node outputChannels "" () event payload
constant = __constant

constant_ i = constant i empty

-- convolver

__convolver
  :: forall i outputChannels producedI consumedI producedO consumedO event
       payload
   . IsEvent event
  => Common.InitialConvolver i
  => i
  -> C.AudioInput outputChannels producedI consumedI event payload
  -> C.Node outputChannels producedO consumedO event payload
__convolver i' (C.AudioInput elts) = C.Node go
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
      ( (sample_ ids (bang unit)) <#> tmpIdentity \me ->
          bang
            ( makeConvolver
                { id: useMeIfMe parent me
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

convolver
  :: forall i outputChannels produced consumed ord event payload
   . IsEvent event
  => Common.InitialConvolver i
  => Sym.Compare "" produced ord
  => TLOrd ord produced produced "" produced
  => Nub consumed consumed
  => i
  -> Array (C.Node outputChannels produced consumed event payload)
  -> C.Node outputChannels produced consumed event payload
convolver i a = case uncons a of
  DM.Nothing -> convolverx i (constant 0.0 empty :* ([] :: Array (C.Node outputChannels produced consumed event payload)))
  DM.Just { head, tail } -> convolverx i (head :::* tail)

convolverx
  :: forall i outputChannels produced consumed event payload
   . IsEvent event
  => Common.InitialConvolver i
  => i
  -> C.AudioInput outputChannels produced consumed event payload
  -> C.Node outputChannels produced consumed event payload
convolverx = __convolver

-- delay
__delay
  :: forall i outputChannels producedI consumedI producedO consumedO event
       payload
   . IsEvent event
  => Common.InitialDelay i
  => i
  -> event C.Delay
  -> C.AudioInput outputChannels producedI consumedI event payload
  -> C.Node outputChannels producedO consumedO event payload
__delay i' atts (C.AudioInput elts) = C.Node go
  where
  C.InitializeDelay i = Common.toInitializeDelay i'
  go parent di@(C.AudioInterpret { ids, scope, makeDelay, setDelay }) = keepLatest
    ( (sample_ ids (bang unit)) <#> tmpIdentity \me ->
        bang
          ( makeDelay
              { id: useMeIfMe parent me, parent: useParentIfParent parent, scope: just scope, delayTime: i.delayTime }
          )
          <|> map
            ( \(C.Delay e) -> match
                { delayTime: \g -> setDelay { id: useMeIfMe parent me, delayTime: g }
                }
                e
            )
            atts
          <|> oneOf
            ( NEA.toArray
                (map (\elt -> ((\y -> let C.Node x = y in x) elt) (Parent me) di) elts)
            )
    )

delay
  :: forall i outputChannels produced consumed ord event payload
   . IsEvent event
  => Common.InitialDelay i
  => Sym.Compare "" produced ord
  => TLOrd ord produced produced "" produced
  => Nub consumed consumed
  => i
  -> event C.Delay
  -> Array (C.Node outputChannels produced consumed event payload)
  -> C.Node outputChannels produced consumed event payload
delay i e a = case uncons a of
  DM.Nothing -> delayx i e (constant 0.0 empty :* ([] :: Array (C.Node outputChannels produced consumed event payload)))
  DM.Just { head, tail } -> delayx i e (head :::* tail)

delay_ i a = delay i empty a

delayx
  :: forall i outputChannels produced consumed event payload
   . IsEvent event
  => Common.InitialDelay i
  => i
  -> event C.Delay
  -> C.AudioInput outputChannels produced consumed event payload
  -> C.Node outputChannels produced consumed event payload
delayx = __delay

-- dynamics compressor

__dynamicsCompressor
  :: forall i outputChannels producedI consumedI producedO consumedO event
       payload
   . IsEvent event
  => Common.InitialDynamicsCompressor i
  => i
  -> event C.DynamicsCompressor
  -> C.AudioInput outputChannels producedI consumedI event payload
  -> C.Node outputChannels producedO consumedO event payload
__dynamicsCompressor i' atts (C.AudioInput elts) = C.Node go
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
      ( (sample_ ids (bang unit)) <#> tmpIdentity \me ->
          bang
            ( makeDynamicsCompressor
                { id: useMeIfMe parent me
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
                      { id: useMeIfMe parent me, threshold }
                  , ratio: \ratio -> setRatio
                      { id: useMeIfMe parent me, ratio }
                  , knee: \knee -> setKnee
                      { id: useMeIfMe parent me, knee }
                  , attack: \attack -> setAttack
                      { id: useMeIfMe parent me, attack }
                  , release: \release -> setRelease
                      { id: useMeIfMe parent me, release }
                  }
                  e
              )
              atts
          <|> oneOf
            ( NEA.toArray
                (map (\elt -> ((\y -> let C.Node x = y in x) elt) (Parent me) di) elts)
            )
    )

dynamicsCompressor
  :: forall i outputChannels produced consumed ord event payload
   . IsEvent event
  => Common.InitialDynamicsCompressor i
  => Sym.Compare "" produced ord
  => TLOrd ord produced produced "" produced
  => Nub consumed consumed
  => i
  -> event C.DynamicsCompressor
  -> Array (C.Node outputChannels produced consumed event payload)
  -> C.Node outputChannels produced consumed event payload
dynamicsCompressor i e a = case uncons a of
  DM.Nothing -> dynamicsCompressorx i e (constant 0.0 empty :* ([] :: Array (C.Node outputChannels produced consumed event payload)))
  DM.Just { head, tail } -> dynamicsCompressorx i e (head :::* tail)

dynamicsCompressor_ i a = dynamicsCompressor i empty a

dynamicsCompressorx
  :: forall i outputChannels produced consumed event payload
   . IsEvent event
  => Common.InitialDynamicsCompressor i
  => i
  -> event C.DynamicsCompressor
  -> C.AudioInput outputChannels produced consumed event payload
  -> C.Node outputChannels produced consumed event payload
dynamicsCompressorx = __dynamicsCompressor

-- gain
__gain
  :: forall i outputChannels producedI consumedI producedO consumedO event
       payload
   . IsEvent event
  => Common.InitialGain i
  => i
  -> event C.Gain
  -> C.AudioInput outputChannels producedI consumedI event payload
  -> C.Node outputChannels producedO consumedO event payload
__gain i' atts (C.AudioInput elts) = C.Node go
  where
  C.InitializeGain i = Common.toInitializeGain i'
  go parent di@(C.AudioInterpret { ids, scope, makeGain, setGain }) = keepLatest
    ( (sample_ ids (bang unit)) <#> tmpIdentity \me ->
        bang
          ( makeGain
              { id: useMeIfMe parent me, parent: useParentIfParent parent, scope: just scope, gain: i.gain }
          )
          <|> map
            ( \(C.Gain e) -> match
                { gain: \g -> setGain { id: useMeIfMe parent me, gain: g }
                }
                e
            )
            atts
          <|> oneOf
            ( NEA.toArray
                (map (\elt -> ((\y -> let C.Node x = y in x) elt) (Parent me) di) elts)
            )
    )

gain
  :: forall i outputChannels produced consumed ord event payload
   . IsEvent event
  => Common.InitialGain i
  => Sym.Compare "" produced ord
  => TLOrd ord produced produced "" produced
  => Nub consumed consumed
  => i
  -> event C.Gain
  -> Array (C.Node outputChannels produced consumed event payload)
  -> C.Node outputChannels produced consumed event payload
gain i e a = case uncons a of
  DM.Nothing -> gainx i e (constant 0.0 empty :* ([] :: Array (C.Node outputChannels produced consumed event payload)))
  DM.Just { head, tail } -> gainx i e (head :::* tail)

gain_ i a = gain i empty a

gainx
  :: forall i outputChannels produced consumed event payload
   . IsEvent event
  => Common.InitialGain i
  => i
  -> event C.Gain
  -> C.AudioInput outputChannels produced consumed event payload
  -> C.Node outputChannels produced consumed event payload
gainx = __gain

-- highpass

-- highpass
__highpass
  :: forall i outputChannels producedI consumedI producedO consumedO event
       payload
   . IsEvent event
  => Common.InitialHighpass i
  => i
  -> event C.Highpass
  -> C.AudioInput outputChannels producedI consumedI event payload
  -> C.Node outputChannels producedO consumedO event payload
__highpass i' atts (C.AudioInput elts) = C.Node go
  where
  C.InitializeHighpass i = Common.toInitializeHighpass i'
  go parent di@(C.AudioInterpret { ids, scope, makeHighpass, setFrequency, setQ }) = keepLatest
    ( (sample_ ids (bang unit)) <#> tmpIdentity \me ->
        bang
          ( makeHighpass
              { id: useMeIfMe parent me, parent: useParentIfParent parent, scope: just scope, frequency: i.frequency, q: i.q }
          )
          <|> map
            ( \(C.Highpass e) -> match
                { frequency: \g -> setFrequency { id: useMeIfMe parent me, frequency: g }
                , q: \g -> setQ { id: useMeIfMe parent me, q: g }
                }
                e
            )
            atts
          <|> oneOf
            ( NEA.toArray
                (map (\elt -> ((\y -> let C.Node x = y in x) elt) (Parent me) di) elts)
            )
    )

highpass
  :: forall i outputChannels produced consumed ord event payload
   . IsEvent event
  => Common.InitialHighpass i
  => Sym.Compare "" produced ord
  => TLOrd ord produced produced "" produced
  => Nub consumed consumed
  => i
  -> event C.Highpass
  -> Array (C.Node outputChannels produced consumed event payload)
  -> C.Node outputChannels produced consumed event payload
highpass i e a = case uncons a of
  DM.Nothing -> highpassx i e (constant 0.0 empty :* ([] :: Array (C.Node outputChannels produced consumed event payload)))
  DM.Just { head, tail } -> highpassx i e (head :::* tail)

highpass_ i a = highpass i empty a

highpassx
  :: forall i outputChannels produced consumed event payload
   . IsEvent event
  => Common.InitialHighpass i
  => i
  -> event C.Highpass
  -> C.AudioInput outputChannels produced consumed event payload
  -> C.Node outputChannels produced consumed event payload
highpassx = __highpass

-- highshelf
__highshelf
  :: forall i outputChannels producedI consumedI producedO consumedO event
       payload
   . IsEvent event
  => Common.InitialHighshelf i
  => i
  -> event C.Highshelf
  -> C.AudioInput outputChannels producedI consumedI event payload
  -> C.Node outputChannels producedO consumedO event payload
__highshelf i' atts (C.AudioInput elts) = C.Node go
  where
  C.InitializeHighshelf i = Common.toInitializeHighshelf i'
  go parent di@(C.AudioInterpret { ids, scope, makeHighshelf, setFrequency, setGain }) = keepLatest
    ( (sample_ ids (bang unit)) <#> tmpIdentity \me ->
        bang
          ( makeHighshelf
              { id: useMeIfMe parent me, parent: useParentIfParent parent, scope: just scope, frequency: i.frequency, gain: i.gain }
          )
          <|> map
            ( \(C.Highshelf e) -> match
                { frequency: \g -> setFrequency { id: useMeIfMe parent me, frequency: g }
                , gain: \g -> setGain { id: useMeIfMe parent me, gain: g }
                }
                e
            )
            atts
          <|> oneOf
            ( NEA.toArray
                (map (\elt -> ((\y -> let C.Node x = y in x) elt) (Parent me) di) elts)
            )
    )

highshelf
  :: forall i outputChannels produced consumed ord event payload
   . IsEvent event
  => Common.InitialHighshelf i
  => Sym.Compare "" produced ord
  => TLOrd ord produced produced "" produced
  => Nub consumed consumed
  => i
  -> event C.Highshelf
  -> Array (C.Node outputChannels produced consumed event payload)
  -> C.Node outputChannels produced consumed event payload
highshelf i e a = case uncons a of
  DM.Nothing -> highshelfx i e (constant 0.0 empty :* ([] :: Array (C.Node outputChannels produced consumed event payload)))
  DM.Just { head, tail } -> highshelfx i e (head :::* tail)

highshelf_ i a = highshelf i empty a

highshelfx
  :: forall i outputChannels produced consumed event payload
   . IsEvent event
  => Common.InitialHighshelf i
  => i
  -> event C.Highshelf
  -> C.AudioInput outputChannels produced consumed event payload
  -> C.Node outputChannels produced consumed event payload
highshelfx = __highshelf

-- iir filter
-- iirFilter

__iirFilter
  :: forall i proxy feedforward feedback outputChannels producedI consumedI producedO consumedO event
       payload
   . IsEvent event
  => Lt D2 feedforward
  => Lt D2 feedback
  => Common.InitialIIRFilter i feedforward feedback
  => proxy feedforward
  -> proxy feedback
  -> i
  -> C.AudioInput outputChannels producedI consumedI event payload
  -> C.Node outputChannels producedO consumedO event payload
__iirFilter _ _ i' (C.AudioInput elts) = C.Node go
  where
  C.InitializeIIRFilter i = (Common.toInitializeIIRFilter :: i -> C.InitializeIIRFilter feedforward feedback) i'
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
      ( (sample_ ids (bang unit)) <#> tmpIdentity \me ->
          bang
            ( makeIIRFilter
                { id: useMeIfMe parent me
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

iirFilter
  :: forall i feedforward feedback outputChannels produced consumed ord event payload
   . IsEvent event
  => Lt D2 feedforward
  => Lt D2 feedback
  => Common.InitialIIRFilter i feedforward feedback
  => Sym.Compare "" produced ord
  => TLOrd ord produced produced "" produced
  => Nub consumed consumed
  => i
  -> Array (C.Node outputChannels produced consumed event payload)
  -> C.Node outputChannels produced consumed event payload
iirFilter i a = case uncons a of
  DM.Nothing -> __iirFilter (Proxy :: _ feedforward) (Proxy :: _ feedback) i (constant 0.0 empty :* ([] :: Array (C.Node outputChannels produced consumed event payload)))
  DM.Just { head, tail } -> __iirFilter (Proxy :: _ feedforward) (Proxy :: _ feedback) i (head :::* tail)

iirFilterx
  :: forall i feedforward feedback outputChannels produced consumed event payload
   . IsEvent event
  => Lt D2 feedforward
  => Lt D2 feedback
  => Common.InitialIIRFilter i feedforward feedback
  => i
  -> C.AudioInput outputChannels produced consumed event payload
  -> C.Node outputChannels produced consumed event payload
iirFilterx = __iirFilter (Proxy :: _ feedforward) (Proxy :: _ feedback)

-- lowpass
__lowpass
  :: forall i outputChannels producedI consumedI producedO consumedO event
       payload
   . IsEvent event
  => Common.InitialLowpass i
  => i
  -> event C.Lowpass
  -> C.AudioInput outputChannels producedI consumedI event payload
  -> C.Node outputChannels producedO consumedO event payload
__lowpass i' atts (C.AudioInput elts) = C.Node go
  where
  C.InitializeLowpass i = Common.toInitializeLowpass i'
  go parent di@(C.AudioInterpret { ids, scope, makeLowpass, setFrequency, setQ }) = keepLatest
    ( (sample_ ids (bang unit)) <#> tmpIdentity \me ->
        bang
          ( makeLowpass
              { id: useMeIfMe parent me, parent: useParentIfParent parent, scope: just scope, frequency: i.frequency, q: i.q }
          )
          <|> map
            ( \(C.Lowpass e) -> match
                { frequency: \g -> setFrequency { id: useMeIfMe parent me, frequency: g }
                , q: \g -> setQ { id: useMeIfMe parent me, q: g }
                }
                e
            )
            atts
          <|> oneOf
            ( NEA.toArray
                (map (\elt -> ((\y -> let C.Node x = y in x) elt) (Parent me) di) elts)
            )
    )

lowpass
  :: forall i outputChannels produced consumed ord event payload
   . IsEvent event
  => Common.InitialLowpass i
  => Sym.Compare "" produced ord
  => TLOrd ord produced produced "" produced
  => Nub consumed consumed
  => i
  -> event C.Lowpass
  -> Array (C.Node outputChannels produced consumed event payload)
  -> C.Node outputChannels produced consumed event payload
lowpass i e a = case uncons a of
  DM.Nothing -> lowpassx i e (constant 0.0 empty :* ([] :: Array (C.Node outputChannels produced consumed event payload)))
  DM.Just { head, tail } -> lowpassx i e (head :::* tail)

lowpass_ i a = lowpass i empty a

lowpassx
  :: forall i outputChannels produced consumed event payload
   . IsEvent event
  => Common.InitialLowpass i
  => i
  -> event C.Lowpass
  -> C.AudioInput outputChannels produced consumed event payload
  -> C.Node outputChannels produced consumed event payload
lowpassx = __lowpass

-- lowshelf
__lowshelf
  :: forall i outputChannels producedI consumedI producedO consumedO event
       payload
   . IsEvent event
  => Common.InitialLowshelf i
  => i
  -> event C.Lowshelf
  -> C.AudioInput outputChannels producedI consumedI event payload
  -> C.Node outputChannels producedO consumedO event payload
__lowshelf i' atts (C.AudioInput elts) = C.Node go
  where
  C.InitializeLowshelf i = Common.toInitializeLowshelf i'
  go parent di@(C.AudioInterpret { ids, scope, makeLowshelf, setFrequency, setGain }) = keepLatest
    ( (sample_ ids (bang unit)) <#> tmpIdentity \me ->
        bang
          ( makeLowshelf
              { id: useMeIfMe parent me, parent: useParentIfParent parent, scope: just scope, frequency: i.frequency, gain: i.gain }
          )
          <|> map
            ( \(C.Lowshelf e) -> match
                { frequency: \g -> setFrequency { id: useMeIfMe parent me, frequency: g }
                , gain: \g -> setGain { id: useMeIfMe parent me, gain: g }
                }
                e
            )
            atts
          <|> oneOf
            ( NEA.toArray
                (map (\elt -> ((\y -> let C.Node x = y in x) elt) (Parent me) di) elts)
            )
    )

lowshelf
  :: forall i outputChannels produced consumed ord event payload
   . IsEvent event
  => Common.InitialLowshelf i
  => Sym.Compare "" produced ord
  => TLOrd ord produced produced "" produced
  => Nub consumed consumed
  => i
  -> event C.Lowshelf
  -> Array (C.Node outputChannels produced consumed event payload)
  -> C.Node outputChannels produced consumed event payload
lowshelf i e a = case uncons a of
  DM.Nothing -> lowshelfx i e (constant 0.0 empty :* ([] :: Array (C.Node outputChannels produced consumed event payload)))
  DM.Just { head, tail } -> lowshelfx i e (head :::* tail)

lowshelf_ i a = lowshelf i empty a

lowshelfx
  :: forall i outputChannels produced consumed event payload
   . IsEvent event
  => Common.InitialLowshelf i
  => i
  -> event C.Lowshelf
  -> C.AudioInput outputChannels produced consumed event payload
  -> C.Node outputChannels produced consumed event payload
lowshelfx = __lowshelf

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
      ( (sample_ ids (bang unit)) <#> tmpIdentity \me ->
          bang
            ( makeLoopBuf
                { id: useMeIfMe parent me
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
                  { buffer: \buffer -> setBuffer { id: useMeIfMe parent me, buffer }
                  , playbackRate: \playbackRate -> setPlaybackRate
                      { id: useMeIfMe parent me, playbackRate }
                  , loopStart: \loopStart -> setLoopStart { id: useMeIfMe parent me, loopStart }
                  , loopEnd: \loopEnd -> setLoopEnd { id: useMeIfMe parent me, loopEnd }
                  , onOff: \onOff -> setOnOff { id: useMeIfMe parent me, onOff }
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
  -> C.Node outputChannels "" () event payload
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
      ( (sample_ ids (bang unit)) <#> tmpIdentity \me ->
          bang
            ( makeMediaElement
                { id: useMeIfMe parent me
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
  -> C.Node outputChannels "" () event payload
mediaElement = __mediaElement

-- microphone

__microphone
  :: forall outputChannels produced consumed event payload
   . IsEvent event
  => C.InitializeMicrophone
  -> C.Node outputChannels produced consumed event payload
__microphone (C.InitializeMicrophone i) = C.Node go
  where
  go parent (C.AudioInterpret { ids, scope, makeMicrophone }) =
    keepLatest
      ( (sample_ ids (bang unit)) <#> tmpIdentity \me ->
          bang
            ( makeMicrophone
                { id: useMeIfMe parent me
                , parent: useParentIfParent parent
                , scope: just scope
                , microphone: i.microphone
                }
            )
      )

microphone
  :: forall outputChannels event payload
   . IsEvent event
  => C.InitializeMicrophone
  -> C.Node outputChannels "" () event payload
microphone = __microphone

-- notch
__notch
  :: forall i outputChannels producedI consumedI producedO consumedO event
       payload
   . IsEvent event
  => Common.InitialNotch i
  => i
  -> event C.Notch
  -> C.AudioInput outputChannels producedI consumedI event payload
  -> C.Node outputChannels producedO consumedO event payload
__notch i' atts (C.AudioInput elts) = C.Node go
  where
  C.InitializeNotch i = Common.toInitializeNotch i'
  go parent di@(C.AudioInterpret { ids, scope, makeNotch, setFrequency, setQ }) = keepLatest
    ( (sample_ ids (bang unit)) <#> tmpIdentity \me ->
        bang
          ( makeNotch
              { id: useMeIfMe parent me, parent: useParentIfParent parent, scope: just scope, frequency: i.frequency, q: i.q }
          )
          <|> map
            ( \(C.Notch e) -> match
                { frequency: \g -> setFrequency { id: useMeIfMe parent me, frequency: g }
                , q: \g -> setQ { id: useMeIfMe parent me, q: g }
                }
                e
            )
            atts
          <|> oneOf
            ( NEA.toArray
                (map (\elt -> ((\y -> let C.Node x = y in x) elt) (Parent me) di) elts)
            )
    )

notch
  :: forall i outputChannels produced consumed ord event payload
   . IsEvent event
  => Common.InitialNotch i
  => Sym.Compare "" produced ord
  => TLOrd ord produced produced "" produced
  => Nub consumed consumed
  => i
  -> event C.Notch
  -> Array (C.Node outputChannels produced consumed event payload)
  -> C.Node outputChannels produced consumed event payload
notch i e a = case uncons a of
  DM.Nothing -> notchx i e (constant 0.0 empty :* ([] :: Array (C.Node outputChannels produced consumed event payload)))
  DM.Just { head, tail } -> notchx i e (head :::* tail)

notch_ i a = notch i empty a

notchx
  :: forall i outputChannels produced consumed event payload
   . IsEvent event
  => Common.InitialNotch i
  => i
  -> event C.Notch
  -> C.AudioInput outputChannels produced consumed event payload
  -> C.Node outputChannels produced consumed event payload
notchx = __notch

-- peaking
__peaking
  :: forall i outputChannels producedI consumedI producedO consumedO event
       payload
   . IsEvent event
  => Common.InitialPeaking i
  => i
  -> event C.Peaking
  -> C.AudioInput outputChannels producedI consumedI event payload
  -> C.Node outputChannels producedO consumedO event payload
__peaking i' atts (C.AudioInput elts) = C.Node go
  where
  C.InitializePeaking i = Common.toInitializePeaking i'
  go parent di@(C.AudioInterpret { ids, scope, makePeaking, setFrequency, setQ, setGain }) = keepLatest
    ( (sample_ ids (bang unit)) <#> tmpIdentity \me ->
        bang
          ( makePeaking
              { id: useMeIfMe parent me, parent: useParentIfParent parent, scope: just scope, frequency: i.frequency, gain: i.gain, q: i.q }
          )
          <|> map
            ( \(C.Peaking e) -> match
                { frequency: \g -> setFrequency { id: useMeIfMe parent me, frequency: g }
                , q: \g -> setQ { id: useMeIfMe parent me, q: g }
                , gain: \g -> setGain { id: useMeIfMe parent me, gain: g }
                }
                e
            )
            atts
          <|> oneOf
            ( NEA.toArray
                (map (\elt -> ((\y -> let C.Node x = y in x) elt) (Parent me) di) elts)
            )
    )

peaking
  :: forall i outputChannels produced consumed ord event payload
   . IsEvent event
  => Common.InitialPeaking i
  => Sym.Compare "" produced ord
  => TLOrd ord produced produced "" produced
  => Nub consumed consumed
  => i
  -> event C.Peaking
  -> Array (C.Node outputChannels produced consumed event payload)
  -> C.Node outputChannels produced consumed event payload
peaking i e a = case uncons a of
  DM.Nothing -> peakingx i e (constant 0.0 empty :* ([] :: Array (C.Node outputChannels produced consumed event payload)))
  DM.Just { head, tail } -> peakingx i e (head :::* tail)

peaking_ i a = peaking i empty a

peakingx
  :: forall i outputChannels produced consumed event payload
   . IsEvent event
  => Common.InitialPeaking i
  => i
  -> event C.Peaking
  -> C.AudioInput outputChannels produced consumed event payload
  -> C.Node outputChannels produced consumed event payload
peakingx = __peaking

-- periodicOsc

__periodicOsc
  :: forall outputChannels produced consumed event payload
   . IsEvent event
  => C.InitializePeriodicOsc
  -> event C.PeriodicOsc
  -> C.Node outputChannels produced consumed event payload
__periodicOsc (C.InitializePeriodicOsc i) atts = C.Node go
  where
  go
    parent
    ( C.AudioInterpret
        { ids, scope, makePeriodicOsc, setFrequency, setOnOff, setPeriodicOsc }
    ) =
    keepLatest
      ( (sample_ ids (bang unit)) <#> tmpIdentity \me ->
          bang
            ( makePeriodicOsc
                { id: useMeIfMe parent me
                , parent: useParentIfParent parent
                , scope: just scope
                , frequency: i.frequency
                , spec: i.spec
                }
            )
            <|> map
              ( \(C.PeriodicOsc e) -> match
                  { frequency: \frequency -> setFrequency
                      { id: useMeIfMe parent me, frequency }
                  , onOff: \onOff -> setOnOff { id: useMeIfMe parent me, onOff }
                  , spec: \spec -> setPeriodicOsc { id: useMeIfMe parent me, spec }
                  }
                  e
              )
              atts
      )

periodicOsc
  :: forall outputChannels event payload
   . IsEvent event
  => C.InitializePeriodicOsc
  -> event C.PeriodicOsc
  -> C.Node outputChannels "" () event payload
periodicOsc = __periodicOsc

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
      ( (sample_ ids (bang unit)) <#> tmpIdentity \me ->
          bang
            ( makePlayBuf
                { id: useMeIfMe parent me
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
                  { buffer: \buffer -> setBuffer { id: useMeIfMe parent me, buffer }
                  , playbackRate: \playbackRate -> setPlaybackRate
                      { id: useMeIfMe parent me, playbackRate }
                  , bufferOffset: \bufferOffset -> setBufferOffset
                      { id: useMeIfMe parent me, bufferOffset }
                  , onOff: \onOff -> setOnOff { id: useMeIfMe parent me, onOff }
                  , duration: \duration -> setDuration { id: useMeIfMe parent me, duration }
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
  -> C.Node outputChannels "" () event payload
playBuf = __playBuf

-- recorder
recorder
  :: forall outputChannels produced consumed event payload
   . IsEvent event
  => C.InitializeRecorder
  -> C.Node outputChannels produced consumed event payload
  -> C.Node outputChannels produced consumed event payload
recorder (C.InitializeRecorder i) elt = C.Node go
  where
  go parent di@(C.AudioInterpret { ids, scope, makeRecorder }) =
    keepLatest
      ( (sample_ ids (bang unit)) <#> \me ->
          bang
            ( makeRecorder
                { id: useMeIfMe parent me, parent: useParentIfParent parent, scope: just scope, cb: i.cb }
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
      ( (sample_ ids (bang unit)) <#> tmpIdentity \me ->
          bang
            ( makeSawtoothOsc
                { id: useMeIfMe parent me
                , parent: useParentIfParent parent
                , scope: just scope
                , frequency: i.frequency
                }
            )
            <|> map
              ( \(C.SawtoothOsc e) -> match
                  { frequency: \frequency -> setFrequency
                      { id: useMeIfMe parent me, frequency }
                  , onOff: \onOff -> setOnOff { id: useMeIfMe parent me, onOff }
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
  -> C.Node outputChannels "" () event payload
sawtoothOsc = __sawtoothOsc

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
      ( (sample_ ids (bang unit)) <#> tmpIdentity \me ->
          bang
            ( makeSinOsc
                { id: useMeIfMe parent me
                , parent: useParentIfParent parent
                , scope: just scope
                , frequency: i.frequency
                }
            )
            <|> map
              ( \(C.SinOsc e) -> match
                  { frequency: \frequency -> setFrequency
                      { id: useMeIfMe parent me, frequency }
                  , onOff: \onOff -> setOnOff { id: useMeIfMe parent me, onOff }
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
  -> C.Node outputChannels "" () event payload
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
      ( (sample_ ids (bang unit)) <#> tmpIdentity \me ->
          bang
            ( makeSquareOsc
                { id: useMeIfMe parent me
                , parent: useParentIfParent parent
                , scope: just scope
                , frequency: i.frequency
                }
            )
            <|> map
              ( \(C.SquareOsc e) -> match
                  { frequency: \frequency -> setFrequency
                      { id: useMeIfMe parent me, frequency }
                  , onOff: \onOff -> setOnOff { id: useMeIfMe parent me, onOff }
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
  -> C.Node outputChannels "" () event payload
squareOsc = __squareOsc

-- speaker
speaker
  :: forall outputChannels event payload
   . IsEvent event
  => C.AudioInput outputChannels "" () event payload
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
  => C.AudioInput D2 "" () event payload
  -> C.AudioInterpret event payload
  -> event payload
speaker2 = speaker

-- pan

__pan
  :: forall i outputChannels producedI consumedI producedO consumedO event
       payload
   . IsEvent event
  => Common.InitialStereoPanner i
  => i
  -> event C.StereoPanner
  -> C.Node outputChannels producedI consumedI event payload
  -> C.Node outputChannels producedO consumedO event payload
__pan i' atts elt = C.Node go
  where
  C.InitializeStereoPanner i = Comomn.toInitializeStereoPanner i'
  go parent di@(C.AudioInterpret { ids, scope, makeStereoPanner, setPan }) =
    keepLatest
      ( (sample_ ids (bang unit)) <#> tmpIdentity \me ->
          bang
            ( makeStereoPanner
                { id: useMeIfMe parent me, parent: useParentIfParent parent, scope: just scope, pan: i.pan }
            )
            <|> map
              ( \(C.StereoPanner e) -> match
                  { pan: \pn -> setPan
                      { id: useMeIfMe parent me, pan: pn }
                  }
                  e
              )
              atts
            <|> ((\y -> let C.Node x = y in x) elt) (Parent me) di
      )

pan
  :: forall i outputChannels produced consumed event payload
   . IsEvent event
  => Common.InitialStereoPanner i
  => i
  -> event C.StereoPanner
  -> C.Node outputChannels produced consumed event payload
  -> C.Node outputChannels produced consumed event payload
pan = __pan

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
      ( (sample_ ids (bang unit)) <#> tmpIdentity \me ->
          bang
            ( makeTriangleOsc
                { id: useMeIfMe parent me
                , parent: useParentIfParent parent
                , scope: just scope
                , frequency: i.frequency
                }
            )
            <|> map
              ( \(C.TriangleOsc e) -> match
                  { frequency: \frequency -> setFrequency
                      { id: useMeIfMe parent me, frequency }
                  , onOff: \onOff -> setOnOff { id: useMeIfMe parent me, onOff }
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
  -> C.Node outputChannels "" () event payload
triangleOsc = __triangleOsc

-- waveshaper

__waveshaper
  :: forall outputChannels producedI consumedI producedO consumedO event
       payload
   . IsEvent event
  => C.InitializeWaveshaper
  -> C.Node outputChannels producedI consumedI event payload
  -> C.Node outputChannels producedO consumedO event payload
__waveshaper (C.InitializeWaveshaper i) elt = C.Node go
  where
  go parent di@(C.AudioInterpret { ids, scope, makeWaveShaper }) =
    keepLatest
      ( (sample_ ids (bang unit)) <#> tmpIdentity \me ->
          bang
            ( makeWaveShaper
                { id: useMeIfMe parent me
                , parent: useParentIfParent parent
                , scope: just scope
                , curve: i.curve
                , oversample: i.oversample
                }
            ) <|> ((\y -> let C.Node x = y in x) elt) (Parent me) di
      )

waveshaper
  :: forall outputChannels produced consumed event payload
   . IsEvent event
  => C.InitializeWaveshaper
  -> C.Node outputChannels produced consumed event payload
  -> C.Node outputChannels produced consumed event payload
waveshaper = __waveshaper
