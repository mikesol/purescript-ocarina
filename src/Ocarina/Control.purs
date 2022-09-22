module Ocarina.Control where

import Prelude

import Bolson.Control as Bolson
import Bolson.Core (Element(..), Entity(..), PSR, Scope(..), fixed)
import Control.Alt ((<|>))
import Control.Comonad (extract)
import Control.Monad.ST.Internal as RRef
import Control.Plus (empty)
import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Data.FastVect.FastVect (Vect, singleton, toArray, index)
import Data.Foldable (oneOf)
import Data.Homogeneous (class HomogeneousRowLabels)
import Data.Homogeneous.Variant (homogeneous)
import Data.Int (pow)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Profunctor (lcmap)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Typelevel.Num (class Nat, class Pos, class Pred, D1, D2, pred, toInt)
import Data.Variant (Unvariant(..), inj, match, unvariant)
import FRP.Event (Event, keepLatest, makeLemmingEvent)
import Foreign.Object (fromHomogeneous)
import Ocarina.Common as Common
import Ocarina.Core (ChannelCountMode(..), ChannelInterpretation(..), Po2(..))
import Ocarina.Core as C
import Ocarina.WebAPI (AnalyserNodeCb(..), BrowserAudioBuffer)
import Prim.Int (class Compare)
import Prim.Ordering (GT, LT)
import Safe.Coerce (coerce)
import Simple.JSON as JSON
import Type.Proxy (Proxy(..))
import Type.Row.Homogeneous (class Homogeneous)

scopeToMaybe :: Scope -> Maybe String
scopeToMaybe Global = Nothing
scopeToMaybe (Local s) = Just s

-- allpass

allpass
  :: forall i (outputChannels :: Type) lock payload
   . Common.InitialAllpass i
  => i
  -> Event (C.Allpass lock payload)
  -> Array (C.Audible outputChannels lock payload)
  -> C.Audible outputChannels lock payload
allpass i' atts elts = Element' $ C.Node go
  where
  C.InitializeAllpass i = Common.toInitializeAllpass i'
  go parent di@(C.AudioInterpret { ids, deleteFromCache, makeAllpass, setFrequency, setQ }) = makeLemmingEvent \mySub k -> do
    me <- ids
    parent.raiseId me
    unsub <- mySub
      ( oneOf
          [ pure
              ( makeAllpass
                  { id: me, parent: parent.parent, scope: scopeToMaybe parent.scope, frequency: i.frequency, q: i.q }
              )
          , keepLatest $ map
              ( \(C.Allpass e) -> match
                  { frequency: tmpResolveAU parent.scope di (setFrequency <<< { id: me, frequency: _ })
                  , q: tmpResolveAU parent.scope di (setQ <<< { id: me, q: _ })
                  }
                  e
              )
              atts
          , __internalOcarinaFlatten { parent: Just me, scope: parent.scope, raiseId: \_ -> pure unit } di (fixed elts)
          ]
      )
      k
    pure do
      k (deleteFromCache { id: me })
      unsub

allpass_
  :: forall i (outputChannels :: Type) lock payload
   . Common.InitialAllpass i
  => i
  -> Array (C.Audible outputChannels lock payload)
  -> C.Audible outputChannels lock payload
allpass_ i a = allpass i empty a

-- analyser

data AnalyserOptions = AnalyserOptions

instance
  ConvertOption AnalyserOptions
    "playbackRate"
    C.InitialAudioParameter
    C.InitialAudioParameter where
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
  :: forall i outputChannels lock payload
   . InitialAnalyser i
  => i
  -> Event C.Analyser
  -> Array (C.Audible outputChannels lock payload)
  -> C.Audible outputChannels lock payload
analyser i' atts elts = Element' $ C.Node go
  where
  C.InitializeAnalyser i = toInitializeAnalyser i'
  go
    parent
    di@(C.AudioInterpret { ids, deleteFromCache, makeAnalyser, setAnalyserNodeCb }) =
    makeLemmingEvent \mySub k -> do
      me <- ids
      parent.raiseId me
      unsub <- mySub
        ( oneOf
            [ pure
                ( makeAnalyser
                    { id: me
                    , parent: parent.parent
                    , scope: scopeToMaybe parent.scope
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
            , map
                ( \(C.Analyser e) -> match
                    { cb: \cb -> setAnalyserNodeCb { id: me, cb }
                    }
                    e
                )
                atts
            , __internalOcarinaFlatten { parent: Just me, scope: parent.scope, raiseId: \_ -> pure unit } di (fixed elts)
            ]
        )
        k
      pure do
        k (deleteFromCache { id: me })
        unsub

analyser_
  :: forall i outputChannels lock payload
   . InitialAnalyser i
  => i
  -> Array (C.Audible outputChannels lock payload)
  -> C.Audible outputChannels lock payload
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
  => Homogeneous parameterData C.InitialAudioParameter
  => HomogeneousRowLabels parameterData (C.AudioParameter lock payload) parameterDataRL
  => JSON.WriteForeign { | processorOptions }
  => C.InitializeAudioWorkletNode name numberOfInputs numberOfOutputs
       outputChannelCount
       parameterData
       processorOptions
  -> Event (C.AudioWorkletNode parameterData)
  -> C.Audible numberOfOutputs lock payload
  -> C.Audible numberOfOutputs lock payload
__audioWorklet (C.InitializeAudioWorkletNode i) atts elt = Element' $ C.Node go
  where
  go
    parent
    di@
      ( C.AudioInterpret
          { ids, deleteFromCache, makeAudioWorkletNode, setAudioWorkletParameter }
      ) =
    makeLemmingEvent \mySub k -> do
      me <- ids
      parent.raiseId me
      unsub <- mySub
        ( oneOf
            [ pure
                ( makeAudioWorkletNode
                    { id: me
                    , parent: parent.parent
                    , scope: scopeToMaybe parent.scope
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
            , keepLatest $ map
                ( \(C.AudioWorkletNode e) -> tmpResolveAU parent.scope di
                    ( \paramValue -> setAudioWorkletParameter
                        { id: me
                        , paramName: (let Unvariant e' = unvariant e in e')
                            (\sym _ -> reflectSymbol sym)
                        , paramValue
                        }
                    )
                    (extract (homogeneous e))
                )
                atts
            , __internalOcarinaFlatten { parent: Just me, scope: parent.scope, raiseId: \_ -> pure unit } di elt
            ]
        )
        k
      pure do
        k (deleteFromCache { id: me })
        unsub

audioWorklet
  :: forall name numberOfInputs numberOfOutputs outputChannelCount parameterData
       parameterDataRL
       processorOptions lock payload
   . IsSymbol name
  => Nat numberOfInputs
  => Pos numberOfOutputs
  => ValidateOutputChannelCount numberOfOutputs outputChannelCount
  => Homogeneous parameterData C.InitialAudioParameter
  => HomogeneousRowLabels parameterData (C.AudioParameter lock payload) parameterDataRL
  => JSON.WriteForeign { | processorOptions }
  => C.InitializeAudioWorkletNode name numberOfInputs numberOfOutputs
       outputChannelCount
       parameterData
       processorOptions
  -> Event (C.AudioWorkletNode parameterData)
  -> C.Audible numberOfOutputs lock payload
  -> C.Audible numberOfOutputs lock payload
audioWorklet = __audioWorklet

-- bandpass
bandpass
  :: forall i (outputChannels :: Type) lock payload
   . Common.InitialBandpass i
  => i
  -> Event (C.Bandpass lock payload)
  -> Array (C.Audible outputChannels lock payload)
  -> C.Audible outputChannels lock payload
bandpass i' atts elts = Element' $ C.Node go
  where
  C.InitializeBandpass i = Common.toInitializeBandpass i'
  go parent di@(C.AudioInterpret { ids, deleteFromCache, makeBandpass, setFrequency, setQ }) = makeLemmingEvent \mySub k -> do
    me <- ids
    parent.raiseId me
    unsub <- mySub
      ( oneOf
          [ pure
              ( makeBandpass
                  { id: me, parent: parent.parent, scope: scopeToMaybe parent.scope, frequency: i.frequency, q: i.q }
              )
          , keepLatest $ map
              ( \(C.Bandpass e) -> match
                  { frequency: tmpResolveAU parent.scope di (setFrequency <<< { id: me, frequency: _ })
                  , q: tmpResolveAU parent.scope di (setQ <<< { id: me, q: _ })
                  }
                  e
              )
              atts
          , __internalOcarinaFlatten { parent: Just me, scope: parent.scope, raiseId: \_ -> pure unit } di (fixed elts)
          ]
      )
      k
    pure do
      k (deleteFromCache { id: me })
      unsub

bandpass_
  :: forall i (outputChannels :: Type) lock payload
   . Common.InitialBandpass i
  => i
  -> Array (C.Audible outputChannels lock payload)
  -> C.Audible outputChannels lock payload
bandpass_ i a = bandpass i empty a

-- constant

__constant
  :: forall i outputChannels lock payload
   . Common.InitialConstant i
  => i
  -> Event (C.Constant lock payload)
  -> C.Audible outputChannels lock payload
__constant i' atts = Element' $ C.Node go
  where
  C.InitializeConstant i = Common.toInitializeConstant i'
  go parent di@(C.AudioInterpret { ids, deleteFromCache, makeConstant, setOffset, setOnOff }) =
    makeLemmingEvent \mySub k -> do
      me <- ids
      parent.raiseId me
      unsub <- mySub
        ( oneOf
            [ pure
                ( makeConstant
                    { id: me
                    , parent: parent.parent
                    , scope: scopeToMaybe parent.scope
                    , offset: i.offset
                    }
                )
            , keepLatest $ map
                ( \(C.Constant e) -> match
                    { offset: tmpResolveAU parent.scope di (setOffset <<< { id: me, offset: _ })
                    , onOff: \onOff -> pure $ setOnOff { id: me, onOff }
                    }
                    e
                )
                atts
            ]
        )
        k
      pure do
        k (deleteFromCache { id: me })
        unsub

constant
  :: forall i outputChannels lock payload
   . Common.InitialConstant i
  => i
  -> Event (C.Constant lock payload)
  -> C.Audible outputChannels lock payload
constant = __constant

constant_
  :: forall i outputChannels lock payload
   . Common.InitialConstant i
  => i
  -> C.Audible outputChannels lock payload
constant_ i = constant i empty

-- convolver

convolver
  :: forall i (outputChannels :: Type) lock payload
   . Common.InitialConvolver i
  => i
  -> Array (C.Audible outputChannels lock payload)
  -> C.Audible outputChannels lock payload
convolver i' elts = Element' $ C.Node go
  where
  C.InitializeConvolver i = Common.toInitializeConvolver i'
  go parent di@(C.AudioInterpret { ids, deleteFromCache, makeConvolver }) =
    makeLemmingEvent \mySub k -> do
      me <- ids
      parent.raiseId me
      unsub <- mySub
        ( oneOf
            [ pure
                ( makeConvolver
                    { id: me
                    , parent: parent.parent
                    , scope: scopeToMaybe parent.scope
                    , buffer: i.buffer
                    }
                )
            , __internalOcarinaFlatten { parent: Just me, scope: parent.scope, raiseId: \_ -> pure unit } di (fixed elts)
            ]
        )
        k
      pure do
        k (deleteFromCache { id: me })
        unsub

-- delay
delay
  :: forall i (outputChannels :: Type) lock payload
   . Common.InitialDelay i
  => i
  -> Event (C.Delay lock payload)
  -> Array (C.Audible outputChannels lock payload)
  -> C.Audible outputChannels lock payload
delay i' atts elts = Element' $ C.Node go
  where
  C.InitializeDelay i = Common.toInitializeDelay i'
  go parent di@(C.AudioInterpret { ids, deleteFromCache, makeDelay, setDelay }) = makeLemmingEvent \mySub k -> do
    me <- ids
    parent.raiseId me
    unsub <- mySub
      ( oneOf
          [ pure
              ( makeDelay
                  { id: me, parent: parent.parent, scope: scopeToMaybe parent.scope, delayTime: i.delayTime, maxDelayTime: i.maxDelayTime }
              )
          , ( keepLatest $ map
                ( \(C.Delay e) -> match
                    { delayTime: tmpResolveAU parent.scope di (setDelay <<< { id: me, delayTime: _ })
                    }
                    e
                )
                atts
            )
          , __internalOcarinaFlatten { parent: Just me, scope: parent.scope, raiseId: \_ -> pure unit } di (fixed elts)
          ]
      )
      k
    pure do
      k (deleteFromCache { id: me })
      unsub

delay_
  :: forall i (outputChannels :: Type) lock payload
   . Common.InitialDelay i
  => i
  -> Array (C.Audible outputChannels lock payload)
  -> C.Audible outputChannels lock payload
delay_ i a = delay i empty a

-- dynamics compressor
dynamicsCompressor
  :: forall i (outputChannels :: Type) lock payload
   . Common.InitialDynamicsCompressor i
  => i
  -> Event (C.DynamicsCompressor lock payload)
  -> Array (C.Audible outputChannels lock payload)
  -> C.Audible outputChannels lock payload
dynamicsCompressor i' atts elts = Element' $ C.Node go
  where
  C.InitializeDynamicsCompressor i = Common.toInitializeDynamicsCompressor i'
  go
    parent
    di@
      ( C.AudioInterpret
          { ids
          , deleteFromCache
          , makeDynamicsCompressor
          , setThreshold
          , setRatio
          , setKnee
          , setAttack
          , setRelease
          }
      ) =
    makeLemmingEvent \mySub k -> do
      me <- ids
      parent.raiseId me
      unsub <- mySub
        ( oneOf
            [ pure
                ( makeDynamicsCompressor
                    { id: me
                    , parent: parent.parent
                    , scope: scopeToMaybe parent.scope
                    , threshold: i.threshold
                    , ratio: i.ratio
                    , knee: i.knee
                    , attack: i.attack
                    , release: i.release
                    }
                )
            , keepLatest $ map
                ( \(C.DynamicsCompressor e) -> match
                    { threshold: tmpResolveAU parent.scope di
                        ( setThreshold <<<
                            { id: me, threshold: _ }
                        )
                    , ratio: tmpResolveAU parent.scope di
                        ( setRatio <<<
                            { id: me, ratio: _ }
                        )
                    , knee: tmpResolveAU parent.scope di
                        ( setKnee <<<
                            { id: me, knee: _ }
                        )
                    , attack: tmpResolveAU parent.scope di
                        ( setAttack <<<
                            { id: me, attack: _ }
                        )
                    , release: tmpResolveAU parent.scope di
                        ( setRelease <<<
                            { id: me, release: _ }
                        )
                    }
                    e
                )
                atts
            , __internalOcarinaFlatten { parent: Just me, scope: parent.scope, raiseId: \_ -> pure unit } di (fixed elts)
            ]
        )
        k
      pure do
        k (deleteFromCache { id: me })
        unsub

dynamicsCompressor_
  :: forall i (outputChannels :: Type) lock payload
   . Common.InitialDynamicsCompressor i
  => i
  -> Array (C.Audible outputChannels lock payload)
  -> C.Audible outputChannels lock payload
dynamicsCompressor_ i = dynamicsCompressor i empty

-- gain
gain
  :: forall i (outputChannels :: Type) lock payload
   . Common.InitialGain i
  => i
  -> Event (C.Gain lock payload)
  -> Array (C.Audible outputChannels lock payload)
  -> C.Audible outputChannels lock payload
gain i' atts elts = Element' $ C.Node go
  where
  C.InitializeGain i = Common.toInitializeGain i'
  go parent di@(C.AudioInterpret { ids, deleteFromCache, makeGain, setGain }) = makeLemmingEvent \mySub k -> do
    me <- ids
    parent.raiseId me
    unsub <- mySub
      ( oneOf
          [ pure
              ( makeGain
                  { id: me, parent: parent.parent, scope: scopeToMaybe parent.scope, gain: i.gain }
              )
          , ( keepLatest $ map
                ( \(C.Gain e) -> match
                    { gain: tmpResolveAU parent.scope di (setGain <<< { id: me, gain: _ })
                    }
                    e
                )
                atts
            )
          , __internalOcarinaFlatten { parent: Just me, scope: parent.scope, raiseId: \_ -> pure unit } di (fixed elts)
          ]
      )
      k
    pure do
      k (deleteFromCache { id: me })
      unsub

gain_
  :: forall i (outputChannels :: Type) lock payload
   . Common.InitialGain i
  => i
  -> Array (C.Audible outputChannels lock payload)
  -> C.Audible outputChannels lock payload
gain_ i a = gain i empty a

-- highpass
highpass
  :: forall i (outputChannels :: Type) lock payload
   . Common.InitialHighpass i
  => i
  -> Event (C.Highpass lock payload)
  -> Array (C.Audible outputChannels lock payload)
  -> C.Audible outputChannels lock payload
highpass i' atts elts = Element' $ C.Node go
  where
  C.InitializeHighpass i = Common.toInitializeHighpass i'
  go parent di@(C.AudioInterpret { ids, deleteFromCache, makeHighpass, setFrequency, setQ }) = makeLemmingEvent \mySub k -> do
    me <- ids
    parent.raiseId me
    unsub <- mySub
      ( oneOf
          [ pure
              ( makeHighpass
                  { id: me, parent: parent.parent, scope: scopeToMaybe parent.scope, frequency: i.frequency, q: i.q }
              )
          , keepLatest $ map
              ( \(C.Highpass e) -> match
                  { frequency: tmpResolveAU parent.scope di (setFrequency <<< { id: me, frequency: _ })
                  , q: tmpResolveAU parent.scope di (setQ <<< { id: me, q: _ })
                  }
                  e
              )
              atts
          , __internalOcarinaFlatten { parent: Just me, scope: parent.scope, raiseId: \_ -> pure unit } di (fixed elts)
          ]
      )
      k
    pure do
      k (deleteFromCache { id: me })
      unsub

highpass_
  :: forall i (outputChannels :: Type) lock payload
   . Common.InitialHighpass i
  => i
  -> Array (C.Audible outputChannels lock payload)
  -> C.Audible outputChannels lock payload
highpass_ i a = highpass i empty a

-- highshelf
highshelf
  :: forall i (outputChannels :: Type) lock payload
   . Common.InitialHighshelf i
  => i
  -> Event (C.Highshelf lock payload)
  -> Array (C.Audible outputChannels lock payload)
  -> C.Audible outputChannels lock payload
highshelf i' atts elts = Element' $ C.Node go
  where
  C.InitializeHighshelf i = Common.toInitializeHighshelf i'
  go parent di@(C.AudioInterpret { ids, deleteFromCache, makeHighshelf, setFrequency, setGain }) = makeLemmingEvent \mySub k -> do
    me <- ids
    parent.raiseId me
    unsub <- mySub
      ( oneOf
          [ pure
              ( makeHighshelf
                  { id: me, parent: parent.parent, scope: scopeToMaybe parent.scope, frequency: i.frequency, gain: i.gain }
              )
          , keepLatest $ map
              ( \(C.Highshelf e) -> match
                  { frequency: tmpResolveAU parent.scope di (setFrequency <<< { id: me, frequency: _ })
                  , gain: tmpResolveAU parent.scope di (setGain <<< { id: me, gain: _ })
                  }
                  e
              )
              atts
          , __internalOcarinaFlatten { parent: Just me, scope: parent.scope, raiseId: \_ -> pure unit } di (fixed elts)
          ]
      )
      k
    pure do
      k (deleteFromCache { id: me })
      unsub

highshelf_
  :: forall i (outputChannels :: Type) lock payload
   . Common.InitialHighshelf i
  => i
  -> Array (C.Audible outputChannels lock payload)
  -> C.Audible outputChannels lock payload
highshelf_ i a = highshelf i empty a

-- iirFilter

iirFilter
  :: forall i (feedforward :: Int) (feedback :: Int) (outputChannels :: Type) lock
       payload
   . Compare 2 feedforward LT
  => Compare 2 feedback LT
  => Common.InitialIIRFilter i feedforward feedback
  => i
  -> Array (C.Audible outputChannels lock payload)
  -> C.Audible outputChannels lock payload
iirFilter = iirFilter' (Proxy :: _ feedforward) (Proxy :: _ feedback)

iirFilter'
  :: forall i (feedforward :: Int) (feedback :: Int) (outputChannels :: Type) lock
       payload
   . Compare 2 feedforward LT
  => Compare 2 feedback LT
  => Common.InitialIIRFilter i feedforward feedback
  => Proxy feedforward
  -> Proxy feedback
  -> i
  -> Array (C.Audible outputChannels lock payload)
  -> C.Audible outputChannels lock payload
iirFilter' fwd bk i' elts = Element' $ C.Node go
  where
  C.InitializeIIRFilter i = Common.toInitializeIIRFilter i' fwd bk
  go
    parent
    di@
      ( C.AudioInterpret
          { ids
          , deleteFromCache
          , makeIIRFilter
          }
      ) =
    makeLemmingEvent \mySub k -> do
      me <- ids
      parent.raiseId me
      unsub <- mySub
        ( oneOf
            [ pure
                ( makeIIRFilter
                    { id: me
                    , parent: parent.parent
                    , scope: scopeToMaybe parent.scope
                    , feedforward: toArray i.feedforward
                    , feedback: toArray i.feedback
                    }
                )
            , __internalOcarinaFlatten { parent: Just me, scope: parent.scope, raiseId: \_ -> pure unit } di (fixed elts)
            ]
        )
        k
      pure do
        k (deleteFromCache { id: me })
        unsub

-- lowpass
lowpass
  :: forall i (outputChannels :: Type) lock payload
   . Common.InitialLowpass i
  => i
  -> Event (C.Lowpass lock payload)
  -> Array (C.Audible outputChannels lock payload)
  -> C.Audible outputChannels lock payload
lowpass i' atts elts = Element' $ C.Node go
  where
  C.InitializeLowpass i = Common.toInitializeLowpass i'
  go parent di@(C.AudioInterpret { ids, deleteFromCache, makeLowpass, setFrequency, setQ }) = makeLemmingEvent \mySub k -> do
    me <- ids
    parent.raiseId me
    unsub <- mySub
      ( oneOf
          [ pure
              ( makeLowpass
                  { id: me, parent: parent.parent, scope: scopeToMaybe parent.scope, frequency: i.frequency, q: i.q }
              )
          , keepLatest $ map
              ( \(C.Lowpass e) -> match
                  { frequency: tmpResolveAU parent.scope di (setFrequency <<< { id: me, frequency: _ })
                  , q: tmpResolveAU parent.scope di (setQ <<< { id: me, q: _ })
                  }
                  e
              )
              atts
          , __internalOcarinaFlatten { parent: Just me, scope: parent.scope, raiseId: \_ -> pure unit } di (fixed elts)
          ]
      )
      k
    pure do
      k (deleteFromCache { id: me })
      unsub

lowpass_
  :: forall i (outputChannels :: Type) lock payload
   . Common.InitialLowpass i
  => i
  -> Array (C.Audible outputChannels lock payload)
  -> C.Audible outputChannels lock payload
lowpass_ i a = lowpass i empty a

-- lowshelf
lowshelf
  :: forall i (outputChannels :: Type) lock payload
   . Common.InitialLowshelf i
  => i
  -> Event (C.Lowshelf lock payload)
  -> Array (C.Audible outputChannels lock payload)
  -> C.Audible outputChannels lock payload
lowshelf i' atts elts = Element' $ C.Node go
  where
  C.InitializeLowshelf i = Common.toInitializeLowshelf i'
  go parent di@(C.AudioInterpret { ids, deleteFromCache, makeLowshelf, setFrequency, setGain }) = makeLemmingEvent \mySub k -> do
    me <- ids
    parent.raiseId me
    unsub <- mySub
      ( oneOf
          [ pure
              ( makeLowshelf
                  { id: me, parent: parent.parent, scope: scopeToMaybe parent.scope, frequency: i.frequency, gain: i.gain }
              )
          , keepLatest $ map
              ( \(C.Lowshelf e) -> match
                  { frequency: tmpResolveAU parent.scope di (setFrequency <<< { id: me, frequency: _ })
                  , gain: tmpResolveAU parent.scope di (setGain <<< { id: me, gain: _ })
                  }
                  e
              )
              atts
          , __internalOcarinaFlatten { parent: Just me, scope: parent.scope, raiseId: \_ -> pure unit } di (fixed elts)
          ]
      )
      k
    pure do
      k (deleteFromCache { id: me })
      unsub

lowshelf_
  :: forall i (outputChannels :: Type) lock payload
   . Common.InitialLowshelf i
  => i
  -> Array (C.Audible outputChannels lock payload)
  -> C.Audible outputChannels lock payload
lowshelf_ i a = lowshelf i empty a

-- loopBuf

__loopBuf
  :: forall i outputChannels lock payload
   . Common.InitialLoopBuf i
  => i
  -> Event (C.LoopBuf lock payload)
  -> C.Audible outputChannels lock payload
__loopBuf i' atts = Element' $ C.Node go
  where
  C.InitializeLoopBuf i = Common.toInitializeLoopBuf i'
  go
    parent
    di@
      ( C.AudioInterpret
          { ids
          , deleteFromCache
          , makeLoopBuf
          , setBuffer
          , setOnOff
          , setPlaybackRate
          , setLoopStart
          , setLoopEnd
          }
      ) =
    makeLemmingEvent \mySub k -> do
      me <- ids
      parent.raiseId me
      unsub <- mySub
        ( oneOf
            [ pure
                ( makeLoopBuf
                    { id: me
                    , parent: parent.parent
                    , scope: scopeToMaybe parent.scope
                    , buffer: i.buffer
                    , playbackRate: i.playbackRate
                    , loopStart: i.loopStart
                    , loopEnd: i.loopEnd
                    , duration: i.duration
                    }
                )
            , keepLatest $ map
                ( \(C.LoopBuf e) -> match
                    { buffer: \buffer -> pure $ setBuffer { id: me, buffer }
                    , playbackRate: tmpResolveAU parent.scope di (setPlaybackRate <<< { id: me, playbackRate: _ })
                    , loopStart: \loopStart -> pure $ setLoopStart { id: me, loopStart }
                    , loopEnd: \loopEnd -> pure $ setLoopEnd { id: me, loopEnd }
                    , onOff: \onOff -> pure $ setOnOff { id: me, onOff }
                    }
                    e
                )
                atts
            ]
        )
        k
      pure do
        k (deleteFromCache { id: me })
        unsub

loopBuf
  :: forall i outputChannels lock payload
   . Common.InitialLoopBuf i
  => i
  -> Event (C.LoopBuf lock payload)
  -> C.Audible outputChannels lock payload
loopBuf = __loopBuf

loopBuf_
  :: forall i outputChannels lock payload
   . Common.InitialLoopBuf i
  => i
  -> C.Audible outputChannels lock payload
loopBuf_ i = loopBuf i empty

-- mediaElement

__mediaElement
  :: forall outputChannels lock payload
   . C.InitializeMediaElement
  -> C.Audible outputChannels lock payload
__mediaElement (C.InitializeMediaElement i) = Element' $ C.Node go
  where
  go parent (C.AudioInterpret { ids, deleteFromCache, makeMediaElement }) =
    makeLemmingEvent \mySub k -> do
      me <- ids
      parent.raiseId me
      unsub <- mySub
        ( pure
            ( makeMediaElement
                { id: me
                , parent: parent.parent
                , scope: scopeToMaybe parent.scope
                , element: i.element
                }
            )
        )
        k
      pure do
        k (deleteFromCache { id: me })
        unsub

mediaElement
  :: forall outputChannels lock payload
   . C.InitializeMediaElement
  -> C.Audible outputChannels lock payload
mediaElement = __mediaElement

-- microphone

__microphone
  :: forall i outputChannels lock payload
   . Common.InitialMicrophone i
  => i
  -> C.Audible outputChannels lock payload
__microphone i' = Element' $ C.Node go
  where
  C.InitializeMicrophone i = Common.toInitializeMicrophone i'
  go parent (C.AudioInterpret { ids, deleteFromCache, makeMicrophone }) =
    makeLemmingEvent \mySub k -> do
      me <- ids
      parent.raiseId me
      unsub <- mySub
        ( pure
            ( makeMicrophone
                { id: me
                , parent: parent.parent
                , scope: scopeToMaybe parent.scope
                , microphone: i.microphone
                }
            )
        )
        k
      pure do
        k (deleteFromCache { id: me })
        unsub

microphone
  :: forall i outputChannels lock payload
   . Common.InitialMicrophone i
  => i
  -> C.Audible outputChannels lock payload
microphone = __microphone

-- notch
notch
  :: forall i (outputChannels :: Type) lock payload
   . Common.InitialNotch i
  => i
  -> Event (C.Notch lock payload)
  -> Array (C.Audible outputChannels lock payload)
  -> C.Audible outputChannels lock payload
notch i' atts elts = Element' $ C.Node go
  where
  C.InitializeNotch i = Common.toInitializeNotch i'
  go parent di@(C.AudioInterpret { ids, deleteFromCache, makeNotch, setFrequency, setQ }) = makeLemmingEvent \mySub k -> do
    me <- ids
    parent.raiseId me
    unsub <- mySub
      ( oneOf
          [ pure
              ( makeNotch
                  { id: me, parent: parent.parent, scope: scopeToMaybe parent.scope, frequency: i.frequency, q: i.q }
              )
          , keepLatest $ map
              ( \(C.Notch e) -> match
                  { frequency: tmpResolveAU parent.scope di (setFrequency <<< { id: me, frequency: _ })
                  , q: tmpResolveAU parent.scope di (setQ <<< { id: me, q: _ })
                  }
                  e
              )
              atts
          , __internalOcarinaFlatten { parent: Just me, scope: parent.scope, raiseId: \_ -> pure unit } di (fixed elts)
          ]
      )
      k
    pure do
      k (deleteFromCache { id: me })
      unsub

notch_
  :: forall i (outputChannels :: Type) lock payload
   . Common.InitialNotch i
  => i
  -> Array (C.Audible outputChannels lock payload)
  -> C.Audible outputChannels lock payload
notch_ i a = notch i empty a

-- peaking
peaking
  :: forall i (outputChannels :: Type) lock payload
   . Common.InitialPeaking i
  => i
  -> Event (C.Peaking lock payload)
  -> Array (C.Audible outputChannels lock payload)
  -> C.Audible outputChannels lock payload
peaking i' atts elts = Element' $ C.Node go
  where
  C.InitializePeaking i = Common.toInitializePeaking i'
  go parent di@(C.AudioInterpret { ids, deleteFromCache, makePeaking, setFrequency, setQ, setGain }) = makeLemmingEvent \mySub k -> do
    me <- ids
    parent.raiseId me
    unsub <- mySub
      ( oneOf
          [ pure $ makePeaking
              { id: me, parent: parent.parent, scope: scopeToMaybe parent.scope, frequency: i.frequency, q: i.q, gain: i.gain }
          , keepLatest $ map
              ( \(C.Peaking e) -> match
                  { frequency: tmpResolveAU parent.scope di (setFrequency <<< { id: me, frequency: _ })
                  , q: tmpResolveAU parent.scope di (setQ <<< { id: me, q: _ })
                  , gain: tmpResolveAU parent.scope di (setGain <<< { id: me, gain: _ })
                  }
                  e
              )
              atts
          , __internalOcarinaFlatten { parent: Just me, scope: parent.scope, raiseId: \_ -> pure unit } di (fixed elts)
          ]
      )
      k
    pure do
      k (deleteFromCache { id: me })
      unsub

peaking_
  :: forall i (outputChannels :: Type) lock payload
   . Common.InitialPeaking i
  => i
  -> Array (C.Audible outputChannels lock payload)
  -> C.Audible outputChannels lock payload
peaking_ i a = peaking i empty a

-- periodicOsc

__periodicOsc
  :: forall i outputChannels lock payload
   . Common.InitialPeriodicOsc i
  => i
  -> Event (C.PeriodicOsc lock payload)
  -> C.Audible outputChannels lock payload
__periodicOsc i' atts = Element' $ C.Node go
  where
  C.InitializePeriodicOsc i = Common.toInitializePeriodicOsc i'
  go
    parent
    di@
      ( C.AudioInterpret
          { ids, deleteFromCache, makePeriodicOsc, setFrequency, setOnOff, setPeriodicOsc }
      ) =
    makeLemmingEvent \mySub k -> do
      me <- ids
      parent.raiseId me
      unsub <- mySub
        ( oneOf
            [ pure
                ( makePeriodicOsc
                    { id: me
                    , parent: parent.parent
                    , scope: scopeToMaybe parent.scope
                    , frequency: i.frequency
                    , spec: i.spec
                    }
                )
            , keepLatest $ map
                ( \(C.PeriodicOsc e) -> match
                    { frequency: tmpResolveAU parent.scope di (setFrequency <<< { id: me, frequency: _ })
                    , onOff: \onOff -> pure $ setOnOff { id: me, onOff }
                    , spec: \spec -> pure $ setPeriodicOsc { id: me, spec }
                    }
                    e
                )
                atts
            ]
        )
        k
      pure do
        k (deleteFromCache { id: me })
        unsub

periodicOsc
  :: forall i outputChannels lock payload
   . Common.InitialPeriodicOsc i
  => i
  -> Event (C.PeriodicOsc lock payload)
  -> C.Audible outputChannels lock payload
periodicOsc = __periodicOsc

periodicOsc_
  :: forall i outputChannels lock payload
   . Common.InitialPeriodicOsc i
  => i
  -> C.Audible outputChannels lock payload
periodicOsc_ i = periodicOsc i empty

-- playBuf

data PlayBufOptions = PlayBufOptions

instance
  ConvertOption PlayBufOptions
    "playbackRate"
    C.InitialAudioParameter
    C.InitialAudioParameter where
  convertOption _ _ = identity

instance ConvertOption PlayBufOptions "duration" Number (Maybe Number) where
  convertOption _ _ = Just

instance ConvertOption PlayBufOptions "bufferOffset" Number Number where
  convertOption _ _ = identity

instance
  ConvertOption PlayBufOptions "buffer" BrowserAudioBuffer BrowserAudioBuffer where
  convertOption _ _ = identity

type PlayBufOptional =
  ( bufferOffset :: Number
  , playbackRate :: C.InitialAudioParameter
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
  , duration: Nothing
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
  -> Event (C.PlayBuf lock payload)
  -> C.Audible outputChannels lock payload
__playBuf i' atts = Element' $ C.Node go
  where
  C.InitializePlayBuf i = Common.toInitializePlayBuf i'
  go
    parent
    di@
      ( C.AudioInterpret
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
    makeLemmingEvent \mySub k -> do
      me <- ids
      parent.raiseId me
      unsub <- mySub
        ( oneOf
            [ pure
                ( makePlayBuf
                    { id: me
                    , parent: parent.parent
                    , scope: scopeToMaybe parent.scope
                    , buffer: i.buffer
                    , playbackRate: i.playbackRate
                    , bufferOffset: i.bufferOffset
                    , duration: i.duration
                    }
                )
            , keepLatest $ map
                ( \(C.PlayBuf e) -> match
                    { buffer: \buffer -> pure $ setBuffer { id: me, buffer }
                    , playbackRate: tmpResolveAU parent.scope di
                        ( setPlaybackRate <<<
                            { id: me, playbackRate: _ }
                        )
                    , bufferOffset: \bufferOffset -> pure $ setBufferOffset
                        { id: me, bufferOffset }
                    , onOff: \onOff -> pure $ setOnOff { id: me, onOff }
                    , duration: \duration -> pure $ setDuration { id: me, duration }
                    }
                    e
                )
                atts
            ]
        )
        k
      pure do
        k (deleteFromCache { id: me })
        unsub

playBuf
  :: forall i outputChannels lock payload
   . Common.InitialPlayBuf i
  => i
  -> Event (C.PlayBuf lock payload)
  -> C.Audible outputChannels lock payload
playBuf = __playBuf

playBuf_
  :: forall i outputChannels lock payload
   . Common.InitialPlayBuf i
  => i
  -> C.Audible outputChannels lock payload
playBuf_ i = playBuf i empty

-- recorder
recorder
  :: forall i outputChannels lock payload
   . Common.InitialRecorder i
  => i
  -> C.Audible outputChannels lock payload
  -> C.Audible outputChannels lock payload
recorder i' elt = Element' $ C.Node go
  where
  C.InitializeRecorder i = Common.toInitializeRecorder i'
  go parent di@(C.AudioInterpret { ids, deleteFromCache, makeRecorder }) =
    makeLemmingEvent \mySub k -> do
      me <- ids
      parent.raiseId me
      unsub <- mySub
        ( oneOf
            [ pure
                ( makeRecorder
                    { id: me, parent: parent.parent, scope: scopeToMaybe parent.scope, cb: i.cb }
                )
            , __internalOcarinaFlatten { parent: Just me, scope: parent.scope, raiseId: \_ -> pure unit } di elt
            ]
        )
        k
      pure do
        k (deleteFromCache { id: me })
        unsub

-- sawtoothOsc

__sawtoothOsc
  :: forall i outputChannels lock payload
   . Common.InitialSawtoothOsc i
  => i
  -> Event (C.SawtoothOsc lock payload)
  -> C.Audible outputChannels lock payload
__sawtoothOsc i' atts = Element' $ C.Node go
  where
  C.InitializeSawtoothOsc i = Common.toInitializeSawtoothOsc i'
  go
    parent
    di@(C.AudioInterpret { ids, deleteFromCache, makeSawtoothOsc, setFrequency, setOnOff }) =
    makeLemmingEvent \mySub k -> do
      me <- ids
      parent.raiseId me
      unsub <- mySub
        ( oneOf
            [ pure
                ( makeSawtoothOsc
                    { id: me
                    , parent: parent.parent
                    , scope: scopeToMaybe parent.scope
                    , frequency: i.frequency
                    }
                )
            , keepLatest $ map
                ( \(C.SawtoothOsc e) -> match
                    { frequency: tmpResolveAU parent.scope di (setFrequency <<< { id: me, frequency: _ })
                    , onOff: \onOff -> pure $ setOnOff { id: me, onOff }
                    }
                    e
                )
                atts
            ]
        )
        k
      pure do
        k (deleteFromCache { id: me })
        unsub

sawtoothOsc
  :: forall i outputChannels lock payload
   . Common.InitialSawtoothOsc i
  => i
  -> Event (C.SawtoothOsc lock payload)
  -> C.Audible outputChannels lock payload
sawtoothOsc = __sawtoothOsc

sawtoothOsc_
  :: forall i outputChannels lock payload
   . Common.InitialSawtoothOsc i
  => i
  -> C.Audible outputChannels lock payload
sawtoothOsc_ i = sawtoothOsc i empty

-- sinOsc

__sinOsc
  :: forall i outputChannels lock payload
   . Common.InitialSinOsc i
  => i
  -> Event (C.SinOsc lock payload)
  -> C.Audible outputChannels lock payload
__sinOsc i' atts = Element' $ C.Node go
  where
  C.InitializeSinOsc i = Common.toInitializeSinOsc i'
  go
    parent
    di@(C.AudioInterpret { ids, deleteFromCache, makeSinOsc, setFrequency, setOnOff }) =
    makeLemmingEvent \mySub k -> do
      me <- ids
      parent.raiseId me
      unsub <- mySub
        ( oneOf
            [ pure
                ( makeSinOsc
                    { id: me
                    , parent: parent.parent
                    , scope: scopeToMaybe parent.scope
                    , frequency: i.frequency
                    }
                )
            , keepLatest $ map
                ( \(C.SinOsc e) -> match
                    { frequency: tmpResolveAU parent.scope di (setFrequency <<< { id: me, frequency: _ })
                    , onOff: \onOff -> pure $ setOnOff { id: me, onOff }
                    }
                    e
                )
                atts
            ]
        )
        k
      pure do
        k (deleteFromCache { id: me })
        unsub

sinOsc
  :: forall i outputChannels lock payload
   . Common.InitialSinOsc i
  => i
  -> Event (C.SinOsc lock payload)
  -> C.Audible outputChannels lock payload
sinOsc = __sinOsc

sinOsc_
  :: forall i outputChannels lock payload
   . Common.InitialSinOsc i
  => i
  -> C.Audible outputChannels lock payload
sinOsc_ a = sinOsc a empty

-- squareOsc

__squareOsc
  :: forall i outputChannels lock payload
   . Common.InitialSquareOsc i
  => i
  -> Event (C.SquareOsc lock payload)
  -> C.Audible outputChannels lock payload
__squareOsc i' atts = Element' $ C.Node go
  where
  C.InitializeSquareOsc i = Common.toInitializeSquareOsc i'
  go
    parent
    di@(C.AudioInterpret { ids, deleteFromCache, makeSquareOsc, setFrequency, setOnOff }) =
    makeLemmingEvent \mySub k -> do
      me <- ids
      parent.raiseId me
      unsub <- mySub
        ( oneOf
            [ pure
                ( makeSquareOsc
                    { id: me
                    , parent: parent.parent
                    , scope: scopeToMaybe parent.scope
                    , frequency: i.frequency
                    }
                )
            , keepLatest $ map
                ( \(C.SquareOsc e) -> match
                    { frequency: tmpResolveAU parent.scope di (setFrequency <<< { id: me, frequency: _ })
                    , onOff: \onOff -> pure $ setOnOff { id: me, onOff }
                    }
                    e
                )
                atts
            ]
        )
        k
      pure do
        k (deleteFromCache { id: me })
        unsub

squareOsc
  :: forall i outputChannels lock payload
   . Common.InitialSquareOsc i
  => i
  -> Event (C.SquareOsc lock payload)
  -> C.Audible outputChannels lock payload
squareOsc = __squareOsc

squareOsc_
  :: forall i outputChannels lock payload
   . Common.InitialSquareOsc i
  => i
  -> C.Audible outputChannels lock payload
squareOsc_ i = squareOsc i empty

-- speaker
speaker
  :: forall (outputChannels :: Type) lock payload
   . Array (C.Audible outputChannels lock payload)
  -> C.AudioInterpret payload
  -> Event payload
speaker elts di@(C.AudioInterpret { ids, makeSpeaker }) = makeLemmingEvent \mySub k -> do
  id <- ids
  k (makeSpeaker { id })
  mySub (__internalOcarinaFlatten { parent: Just id, scope: Local "toplevel", raiseId: \_ -> pure unit } di (fixed elts)) k

speaker2
  :: forall lock payload
   . Array (C.Audible D2 lock payload)
  -> C.AudioInterpret payload
  -> Event payload
speaker2 = speaker

-- pan
pan
  :: forall i (outputChannels :: Type) lock payload
   . Common.InitialStereoPanner i
  => i
  -> Event (C.StereoPanner lock payload)
  -> Array (C.Audible outputChannels lock payload)
  -> C.Audible outputChannels lock payload
pan i' atts elts = Element' $ C.Node go
  where
  C.InitializeStereoPanner i = Common.toInitializeStereoPanner i'
  go parent di@(C.AudioInterpret { ids, deleteFromCache, makeStereoPanner, setPan }) = makeLemmingEvent \mySub k -> do
    me <- ids
    parent.raiseId me
    unsub <- mySub
      ( oneOf
          [ pure
              ( makeStereoPanner
                  { id: me, parent: parent.parent, scope: scopeToMaybe parent.scope, pan: i.pan }
              )
          , keepLatest $ map
              ( \(C.StereoPanner e) -> match
                  { pan: tmpResolveAU parent.scope di (setPan <<< { id: me, pan: _ })
                  }
                  e
              )
              atts
          , __internalOcarinaFlatten { parent: Just me, scope: parent.scope, raiseId: \_ -> pure unit } di (fixed elts)
          ]
      )
      k
    pure do
      k (deleteFromCache { id: me })
      unsub

pan_
  :: forall i (outputChannels :: Type) lock payload
   . Common.InitialStereoPanner i
  => i
  -> Array (C.Audible outputChannels lock payload)
  -> C.Audible outputChannels lock payload
pan_ i = pan i empty

-- triangleOsc

__triangleOsc
  :: forall i outputChannels lock payload
   . Common.InitialTriangleOsc i
  => i
  -> Event (C.TriangleOsc lock payload)
  -> C.Audible outputChannels lock payload
__triangleOsc i' atts = Element' $ C.Node go
  where
  C.InitializeTriangleOsc i = Common.toInitializeTriangleOsc i'
  go
    parent
    di@(C.AudioInterpret { ids, deleteFromCache, makeTriangleOsc, setFrequency, setOnOff }) =
    makeLemmingEvent \mySub k -> do
      me <- ids
      parent.raiseId me
      unsub <- mySub
        ( oneOf
            [ pure
                ( makeTriangleOsc
                    { id: me
                    , parent: parent.parent
                    , scope: scopeToMaybe parent.scope
                    , frequency: i.frequency
                    }
                )
            , ( keepLatest $ map
                  ( \(C.TriangleOsc e) -> match
                      { frequency: tmpResolveAU parent.scope di (setFrequency <<< { id: me, frequency: _ })
                      , onOff: \onOff -> pure $ setOnOff { id: me, onOff }
                      }
                      e
                  )
                  atts
              )
            ]
        )
        k
      pure do
        k (deleteFromCache { id: me })
        unsub

triangleOsc
  :: forall i outputChannels lock payload
   . Common.InitialTriangleOsc i
  => i
  -> Event (C.TriangleOsc lock payload)
  -> C.Audible outputChannels lock payload
triangleOsc = __triangleOsc

triangleOsc_
  :: forall i outputChannels lock payload
   . Common.InitialTriangleOsc i
  => i
  -> C.Audible outputChannels lock payload
triangleOsc_ i = triangleOsc i empty

-- waveShaper

waveShaper
  :: forall i (outputChannels :: Type) lock payload
   . Common.InitialWaveShaper i
  => i
  -> Array (C.Audible outputChannels lock payload)
  -> C.Audible outputChannels lock payload
waveShaper i' elts = Element' $ C.Node go
  where
  C.InitializeWaveShaper i = Common.toInitializeWaveShaper i'
  go parent di@(C.AudioInterpret { ids, deleteFromCache, makeWaveShaper }) =
    makeLemmingEvent \mySub k -> do
      me <- ids
      parent.raiseId me
      unsub <- mySub
        ( oneOf
            [ pure
                ( makeWaveShaper
                    { id: me
                    , parent: parent.parent
                    , scope: scopeToMaybe parent.scope
                    , curve: i.curve
                    , oversample: i.oversample
                    }
                )
            , __internalOcarinaFlatten { parent: Just me, scope: parent.scope, raiseId: \_ -> pure unit } di (fixed elts)
            ]
        )
        k
      pure do
        k (deleteFromCache { id: me })
        unsub

----------
globalFan
  :: forall n o lock payload
   . Compare n (-1) GT
  => Vect n (C.Audible o lock payload)
  -> (Vect n (C.Audible o lock payload) -> C.Audible o lock payload)
  -> C.Audible o lock payload
globalFan a b = Bolson.globalPortalComplexComplex
  { doLogic: absurd
  , ids: unwrap >>> _.ids
  , disconnectElement: \(C.AudioInterpret { disconnectXFromY }) { id, parent } -> disconnectXFromY { from: id, to: parent }
  , toElt: \(C.Node e) -> Element e
  }
  { fromEltO1: coerce
  , fromEltO2: coerce
  , toElt: coerce
  , wrapElt: \e -> gain_ 1.0 [ e ]
  , giveNewParent: \(C.AudioInterpret { connectXToY }) { id, parent } _ -> connectXToY { from: id, to: parent }
  , deleteFromCache: unwrap >>> _.deleteFromCache
  }
  a
  (lcmap (map (_ $ unit)) b)

fan
  :: forall o n lock0 payload
   . Compare n (-1) GT
  => Vect n (C.Audible o lock0 payload)
  -> ( forall lock1
        . Vect n (C.Audible o lock1 payload)
       -> (C.Audible o lock0 payload -> C.Audible o lock1 payload)
       -> C.Audible o lock1 payload
     )
  -> C.Audible o lock0 payload
fan a b = Bolson.portalComplexComplex
  { doLogic: absurd
  , ids: unwrap >>> _.ids
  , disconnectElement: \(C.AudioInterpret { disconnectXFromY }) { id, parent } -> disconnectXFromY { from: id, to: parent }
  , toElt: \(C.Node e) -> Element e
  }
  { fromEltO1: coerce
  , fromEltO2: coerce
  , toElt: coerce
  , wrapElt: \e -> gain_ 1.0 [ e ]
  , giveNewParent: \(C.AudioInterpret { connectXToY }) { id, parent } _ -> connectXToY { from: id, to: parent }
  , deleteFromCache: unwrap >>> _.deleteFromCache
  }
  a
  (lcmap (map (_ $ unit)) (coerce b))

globalFan1
  :: forall o lock payload
   . C.Audible o lock payload
  -> ( C.Audible o lock payload
       -> C.Audible o lock payload
     )
  -> C.Audible o lock payload
globalFan1 a b = globalFan (singleton a) (lcmap (index (Proxy :: _ 0)) b)

fan1
  :: forall o lock0 payload
   . C.Audible o lock0 payload
  -> ( forall lock1
        . C.Audible o lock1 payload
       -> (C.Audible o lock0 payload -> C.Audible o lock1 payload)
       -> C.Audible o lock1 payload
     )
  -> C.Audible o lock0 payload
fan1 a b = fan (singleton a) (lcmap (index (Proxy :: _ 0)) b)

---- fix
fix
  :: forall outputChannels lock payload
   . (C.Audible outputChannels lock payload -> C.Audible outputChannels lock payload)
  -> C.Audible outputChannels lock payload
fix = Bolson.fixComplexComplex
  { doLogic: absurd
  , ids: unwrap >>> _.ids
  , disconnectElement: \(C.AudioInterpret { disconnectXFromY }) { id, parent } -> disconnectXFromY { from: id, to: parent }
  , toElt: \(C.Node e) -> Element e
  }
  { fromElt: coerce
  , connectToParent: \(C.AudioInterpret { connectXToY }) { id, parent } -> connectXToY { from: id, to: parent }
  }

silence
  :: forall outputChannels lock payload
   . C.Audible outputChannels lock payload
silence = fix identity

-----
-- starts work on merge
-- merge
--   :: forall i n lock payload
--    . IsEvent event
--   => Pos n
--   => Vec n (C.Node D1 lock payload)
--   -> C.Node n lock payload
-- merge elts = Element' $ C.Node go
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
--       ( (sample_ ids (pure unit)) <#> \me ->
--           pure
--             ( makeMerger
--                 { id: me
--                 , parent: parent.parent
--                 , scope: scopeToMaybe parent.scope
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

-- TODO
-- this function is copied between two files
-- with the sole difference that this version wraps its argument in a gain node
-- the reason for this is that, otherwise, we'd have to write additional machinery
-- for all generators (ie sine wave oscillators) to listen to when they turn on and off and reconnect fresh generators whenever something turns on again
-- by doing it this way, all generators go to a gain node, so we can use code we've already written
-- the downside is that we have an extra gain node for every audio parameter
-- which can add up
-- so we definitely want to delete this and use Common.resolveAU
-- as soon as we can correctly attach and detach generators
tmpResolveAU :: forall lock payload. Scope -> C.AudioInterpret payload -> (C.FFIAudioParameter -> payload) -> C.AudioParameter lock payload -> Event payload
tmpResolveAU = go
  where
  cncl = C.FFIAudioParameter <<< inj (Proxy :: _ "cancel")
  ev = C.FFIAudioParameter <<< inj (Proxy :: _ "envelope")
  nmc = C.FFIAudioParameter <<< inj (Proxy :: _ "numeric")
  sdn = C.FFIAudioParameter <<< inj (Proxy :: _ "sudden")
  ut = C.FFIAudioParameter <<< inj (Proxy :: _ "unit")
  go scope di f (C.AudioParameter a) = match
    { numeric: pure <<< f <<< nmc
    , envelope: pure <<< f <<< ev
    , cancel: pure <<< f <<< cncl
    , sudden: pure <<< f <<< sdn
    , unit: \(C.AudioUnit { u }) ->
        let
          n = gain_ 1.0 [ u ]
        in
          makeLemmingEvent \mySub k -> do
            av <- RRef.new Nothing
            mySub
              ( __internalOcarinaFlatten { parent: Nothing, scope: scope, raiseId: \x -> void $ RRef.write (Just x) av } di n <|> makeLemmingEvent \mySub k2 ->
                  do
                    RRef.read av >>= case _ of
                      Nothing -> pure unit -- ugh, fails silently
                      Just i -> k2 (f (ut (C.FFIAudioUnit { i })))
                    pure (pure unit)
              )
              k
    }
    a

__internalOcarinaFlatten
  :: forall o lock payload
   . PSR ()
  -> C.AudioInterpret payload
  -> C.Audible o lock payload
  -> Event payload
__internalOcarinaFlatten = Bolson.flatten
  { doLogic: absurd
  , ids: unwrap >>> _.ids
  , disconnectElement: \(C.AudioInterpret { disconnectXFromY }) { id, parent } -> disconnectXFromY { from: id, to: parent }
  , toElt: \(C.Node e) -> Element e
  }