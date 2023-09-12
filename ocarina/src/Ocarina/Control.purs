module Ocarina.Control where

import Prelude

import Bolson.Control (behaving)
import Bolson.Control as Bolson
import Bolson.Core (Element(..), Entity(..), PSR, Scope(..), fixed)
import Control.Alt ((<|>))
import Control.Comonad (extract)
import Control.Monad.ST.Internal as RRef
import Control.Plus (empty)
import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Data.FastVect.FastVect (Vect, singleton, toArray, index)
import Data.Homogeneous (class HomogeneousRowLabels)
import Data.Homogeneous.Variant (homogeneous)
import Data.Int (pow)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Profunctor (lcmap)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Typelevel.Num (class Nat, class Pos, class Pred, D1, D2, pred, toInt)
import Data.Variant (Unvariant(..), inj, match, unvariant)
import Effect.Console (error)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Event (justNone, justOne, keepLatest, makeEvent)
import FRP.Poll (Poll, poll, sample)
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

subscribeChildren subscribe me deferralPath scope di elts ee =
  subscribe (sample (__internalOcarinaFlatten { parent: Just (show me), deferralPath: deferralPath, scope: scope, raiseId: \_ -> pure unit } di elts) ee)

subscribeAtts subscribe ee atts = subscribe (sample atts ee)

scopeToMaybe :: Scope -> Maybe String
scopeToMaybe Global = Nothing
scopeToMaybe (Local s) = Just s

-- allpass

allpass
  :: forall i (outputChannels :: Type) payload
   . Common.InitialAllpass i
  => i
  -> Poll (C.Allpass payload)
  -> Array (C.Audible outputChannels payload)
  -> C.Audible outputChannels payload
allpass i' atts elts = Element' $ C.Node go
  where
  C.InitializeAllpass i = Common.toInitializeAllpass i'
  go parent di@(C.AudioInterpret { ids, deferPayload, deleteFromCache, makeAllpass, setFrequency, setQ }) = behaving \ee kx subscribe -> do
    me <- justNone ids
    justNone $ parent.raiseId $ show me
    kx $ makeAllpass
      { id: show me, parent: parent.parent, scope: scopeToMaybe parent.scope, frequency: i.frequency, q: i.q }
    kx $ deferPayload parent.deferralPath $ deleteFromCache { id: show me }
    subscribeChildren subscribe me parent.deferralPath parent.scope di (fixed elts) ee
    subscribeAtts subscribe ee
      ( keepLatest $ map
          ( \(C.Allpass e) -> match
              { frequency: tmpResolveAU parent.scope parent.deferralPath di (setFrequency <<< { id: show me, frequency: _ })
              , q: tmpResolveAU parent.scope parent.deferralPath di (setQ <<< { id: show me, q: _ })
              }
              e
          )
          atts
      )

allpass_
  :: forall i (outputChannels :: Type) payload
   . Common.InitialAllpass i
  => i
  -> Array (C.Audible outputChannels payload)
  -> C.Audible outputChannels payload
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
  :: forall i outputChannels payload
   . InitialAnalyser i
  => i
  -> Poll C.Analyser
  -> Array (C.Audible outputChannels payload)
  -> C.Audible outputChannels payload
analyser i' atts elts = Element' $ C.Node go
  where
  C.InitializeAnalyser i = toInitializeAnalyser i'
  go
    parent
    di@(C.AudioInterpret { ids, deferPayload, deleteFromCache, makeAnalyser, setAnalyserNodeCb }) =
    behaving \ee kx subscribe -> do
      me <- justNone ids
      justNone $ parent.raiseId $ show me
      kx $ makeAnalyser
        { id: show me
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
      kx $ deferPayload parent.deferralPath $ deleteFromCache { id: show me }
      subscribeChildren subscribe me parent.deferralPath parent.scope di (fixed elts) ee
      subscribeAtts subscribe ee
        ( map
            ( \(C.Analyser e) -> match
                { cb: \cb -> setAnalyserNodeCb { id: show me, cb }
                }
                e
            )
            atts
        )

analyser_
  :: forall i outputChannels payload
   . InitialAnalyser i
  => i
  -> Array (C.Audible outputChannels payload)
  -> C.Audible outputChannels payload
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
       processorOptions payload
   . IsSymbol name
  => Nat numberOfInputs
  => Pos numberOfOutputs
  => ValidateOutputChannelCount numberOfOutputs outputChannelCount
  => Homogeneous parameterData C.InitialAudioParameter
  => HomogeneousRowLabels parameterData (C.AudioParameter payload) parameterDataRL
  => JSON.WriteForeign { | processorOptions }
  => C.InitializeAudioWorkletNode name numberOfInputs numberOfOutputs
       outputChannelCount
       parameterData
       processorOptions
  -> Poll (C.AudioWorkletNode parameterData)
  -> C.Audible numberOfOutputs payload
  -> C.Audible numberOfOutputs payload
__audioWorklet (C.InitializeAudioWorkletNode i) atts elt = Element' $ C.Node go
  where
  go
    parent
    di@
      ( C.AudioInterpret
          { ids, deferPayload, deleteFromCache, makeAudioWorkletNode, setAudioWorkletParameter }
      ) =
    behaving \ee kx subscribe -> do
      me <- justNone ids
      justNone $ parent.raiseId $ show me
      kx $ makeAudioWorkletNode
        { id: show me
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
      kx $ deferPayload parent.deferralPath $ deleteFromCache { id: show me }
      subscribeChildren subscribe me parent.deferralPath parent.scope di elt ee
      subscribeAtts subscribe ee
        ( keepLatest $ map
            ( \(C.AudioWorkletNode e) -> tmpResolveAU parent.scope parent.deferralPath di
                ( \paramValue -> setAudioWorkletParameter
                    { id: show me
                    , paramName: (let Unvariant e' = unvariant e in e')
                        (\sym _ -> reflectSymbol sym)
                    , paramValue
                    }
                )
                (extract (homogeneous e))
            )
            atts
        )

audioWorklet
  :: forall name numberOfInputs numberOfOutputs outputChannelCount parameterData
       parameterDataRL
       processorOptions payload
   . IsSymbol name
  => Nat numberOfInputs
  => Pos numberOfOutputs
  => ValidateOutputChannelCount numberOfOutputs outputChannelCount
  => Homogeneous parameterData C.InitialAudioParameter
  => HomogeneousRowLabels parameterData (C.AudioParameter payload) parameterDataRL
  => JSON.WriteForeign { | processorOptions }
  => C.InitializeAudioWorkletNode name numberOfInputs numberOfOutputs
       outputChannelCount
       parameterData
       processorOptions
  -> Poll (C.AudioWorkletNode parameterData)
  -> C.Audible numberOfOutputs payload
  -> C.Audible numberOfOutputs payload
audioWorklet = __audioWorklet

-- bandpass
bandpass
  :: forall i (outputChannels :: Type) payload
   . Common.InitialBandpass i
  => i
  -> Poll (C.Bandpass payload)
  -> Array (C.Audible outputChannels payload)
  -> C.Audible outputChannels payload
bandpass i' atts elts = Element' $ C.Node go
  where
  C.InitializeBandpass i = Common.toInitializeBandpass i'
  go parent di@(C.AudioInterpret { ids, deferPayload, deleteFromCache, makeBandpass, setFrequency, setQ }) = behaving \ee kx subscribe -> do
    me <- justNone ids
    justNone $ parent.raiseId $ show me
    kx $ makeBandpass
      { id: show me, parent: parent.parent, scope: scopeToMaybe parent.scope, frequency: i.frequency, q: i.q }
    kx $ deferPayload parent.deferralPath $ deleteFromCache { id: show me }
    subscribeChildren subscribe me parent.deferralPath parent.scope di (fixed elts) ee
    subscribeAtts subscribe ee
      ( keepLatest $ map
          ( \(C.Bandpass e) -> match
              { frequency: tmpResolveAU parent.scope parent.deferralPath di (setFrequency <<< { id: show me, frequency: _ })
              , q: tmpResolveAU parent.scope parent.deferralPath di (setQ <<< { id: show me, q: _ })
              }
              e
          )
          atts
      )

bandpass_
  :: forall i (outputChannels :: Type) payload
   . Common.InitialBandpass i
  => i
  -> Array (C.Audible outputChannels payload)
  -> C.Audible outputChannels payload
bandpass_ i a = bandpass i empty a

-- constant

__constant
  :: forall i outputChannels payload
   . Common.InitialConstant i
  => i
  -> Poll (C.Constant payload)
  -> C.Audible outputChannels payload
__constant i' atts = Element' $ C.Node go
  where
  C.InitializeConstant i = Common.toInitializeConstant i'
  go parent di@(C.AudioInterpret { ids, deferPayload, deleteFromCache, makeConstant, setOffset, setOnOff }) =
    behaving \ee kx subscribe -> do
      me <- justNone ids
      justNone $ parent.raiseId $ show me
      kx $ makeConstant
        { id: show me
        , parent: parent.parent
        , scope: scopeToMaybe parent.scope
        , offset: i.offset
        }
      kx $ deferPayload parent.deferralPath $ deleteFromCache { id: show me }
      subscribeAtts subscribe ee
        ( keepLatest $ map
            ( \(C.Constant e) -> match
                { offset: tmpResolveAU parent.scope parent.deferralPath di (setOffset <<< { id: show me, offset: _ })
                , onOff: \onOff -> pure $ setOnOff { id: show me, onOff }
                }
                e
            )
            atts
        )

constant
  :: forall i outputChannels payload
   . Common.InitialConstant i
  => i
  -> Poll (C.Constant payload)
  -> C.Audible outputChannels payload
constant = __constant

constant_
  :: forall i outputChannels payload
   . Common.InitialConstant i
  => i
  -> C.Audible outputChannels payload
constant_ i = constant i empty

-- convolver

convolver
  :: forall i (outputChannels :: Type) payload
   . Common.InitialConvolver i
  => i
  -> Array (C.Audible outputChannels payload)
  -> C.Audible outputChannels payload
convolver i' elts = Element' $ C.Node go
  where
  C.InitializeConvolver i = Common.toInitializeConvolver i'
  go parent di@(C.AudioInterpret { ids, deferPayload, deleteFromCache, makeConvolver }) =
    behaving \ee kx subscribe -> do
      me <- justNone ids
      justNone $ parent.raiseId $ show me
      kx $ makeConvolver
        { id: show me
        , parent: parent.parent
        , scope: scopeToMaybe parent.scope
        , buffer: i.buffer
        }
      kx $ deferPayload parent.deferralPath $ deleteFromCache { id: show me }
      subscribeChildren subscribe me parent.deferralPath parent.scope di (fixed elts) ee

-- delay
delay
  :: forall i (outputChannels :: Type) payload
   . Common.InitialDelay i
  => i
  -> Poll (C.Delay payload)
  -> Array (C.Audible outputChannels payload)
  -> C.Audible outputChannels payload
delay i' atts elts = Element' $ C.Node go
  where
  C.InitializeDelay i = Common.toInitializeDelay i'
  go parent di@(C.AudioInterpret { ids, deferPayload, deleteFromCache, makeDelay, setDelay }) = behaving \ee kx subscribe -> do
    me <- justNone ids
    justNone $ parent.raiseId $ show me
    kx $ makeDelay
      { id: show me, parent: parent.parent, scope: scopeToMaybe parent.scope, delayTime: i.delayTime, maxDelayTime: i.maxDelayTime }
    kx $ deferPayload parent.deferralPath $ deleteFromCache { id: show me }
    subscribeChildren subscribe me parent.deferralPath parent.scope di (fixed elts) ee
    subscribeAtts subscribe ee
      ( keepLatest $ map
          ( \(C.Delay e) -> match
              { delayTime: tmpResolveAU parent.scope parent.deferralPath di (setDelay <<< { id: show me, delayTime: _ })
              }
              e
          )
          atts
      )

delay_
  :: forall i (outputChannels :: Type) payload
   . Common.InitialDelay i
  => i
  -> Array (C.Audible outputChannels payload)
  -> C.Audible outputChannels payload
delay_ i a = delay i empty a

-- dynamics compressor
dynamicsCompressor
  :: forall i (outputChannels :: Type) payload
   . Common.InitialDynamicsCompressor i
  => i
  -> Poll (C.DynamicsCompressor payload)
  -> Array (C.Audible outputChannels payload)
  -> C.Audible outputChannels payload
dynamicsCompressor i' atts elts = Element' $ C.Node go
  where
  C.InitializeDynamicsCompressor i = Common.toInitializeDynamicsCompressor i'
  go
    parent
    di@
      ( C.AudioInterpret
          { ids
          , deferPayload
          , deleteFromCache
          , makeDynamicsCompressor
          , setThreshold
          , setRatio
          , setKnee
          , setAttack
          , setRelease
          }
      ) =
    behaving \ee kx subscribe -> do
      me <- justNone ids
      justNone $ parent.raiseId $ show me
      kx $ makeDynamicsCompressor
        { id: show me
        , parent: parent.parent
        , scope: scopeToMaybe parent.scope
        , threshold: i.threshold
        , ratio: i.ratio
        , knee: i.knee
        , attack: i.attack
        , release: i.release
        }
      kx $ deferPayload parent.deferralPath $ deleteFromCache { id: show me }
      subscribeChildren subscribe me parent.deferralPath parent.scope di (fixed elts) ee
      subscribeAtts subscribe ee
        ( keepLatest $ map
            ( \(C.DynamicsCompressor e) -> match
                { threshold: tmpResolveAU parent.scope parent.deferralPath di
                    ( setThreshold <<<
                        { id: show me, threshold: _ }
                    )
                , ratio: tmpResolveAU parent.scope parent.deferralPath di
                    ( setRatio <<<
                        { id: show me, ratio: _ }
                    )
                , knee: tmpResolveAU parent.scope parent.deferralPath di
                    ( setKnee <<<
                        { id: show me, knee: _ }
                    )
                , attack: tmpResolveAU parent.scope parent.deferralPath di
                    ( setAttack <<<
                        { id: show me, attack: _ }
                    )
                , release: tmpResolveAU parent.scope parent.deferralPath di
                    ( setRelease <<<
                        { id: show me, release: _ }
                    )
                }
                e
            )
            atts
        )

dynamicsCompressor_
  :: forall i (outputChannels :: Type) payload
   . Common.InitialDynamicsCompressor i
  => i
  -> Array (C.Audible outputChannels payload)
  -> C.Audible outputChannels payload
dynamicsCompressor_ i = dynamicsCompressor i empty

-- gain
gain
  :: forall i (outputChannels :: Type) payload
   . Common.InitialGain i
  => i
  -> Poll (C.Gain payload)
  -> Array (C.Audible outputChannels payload)
  -> C.Audible outputChannels payload
gain i' atts elts = Element' $ C.Node go
  where
  C.InitializeGain i = Common.toInitializeGain i'
  go parent di@(C.AudioInterpret { ids, deferPayload, deleteFromCache, makeGain, setGain }) = behaving \ee kx subscribe -> do
    me <- justNone ids
    justNone $ parent.raiseId $ show me
    kx $ makeGain
      { id: show me, parent: parent.parent, scope: scopeToMaybe parent.scope, gain: i.gain }
    kx $ deferPayload parent.deferralPath $ deleteFromCache { id: show me }
    subscribeChildren subscribe me parent.deferralPath parent.scope di (fixed elts) ee
    subscribeAtts subscribe ee
      ( keepLatest $ map
          ( \(C.Gain e) -> match
              { gain: tmpResolveAU parent.scope parent.deferralPath di (setGain <<< { id: show me, gain: _ })
              }
              e
          )
          atts
      )

gain_
  :: forall i (outputChannels :: Type) payload
   . Common.InitialGain i
  => i
  -> Array (C.Audible outputChannels payload)
  -> C.Audible outputChannels payload
gain_ i a = gain i empty a

-- highpass
highpass
  :: forall i (outputChannels :: Type) payload
   . Common.InitialHighpass i
  => i
  -> Poll (C.Highpass payload)
  -> Array (C.Audible outputChannels payload)
  -> C.Audible outputChannels payload
highpass i' atts elts = Element' $ C.Node go
  where
  C.InitializeHighpass i = Common.toInitializeHighpass i'
  go parent di@(C.AudioInterpret { ids, deferPayload, deleteFromCache, makeHighpass, setFrequency, setQ }) = behaving \ee kx subscribe -> do
    me <- justNone ids
    justNone $ parent.raiseId $ show me
    kx $ makeHighpass
      { id: show me, parent: parent.parent, scope: scopeToMaybe parent.scope, frequency: i.frequency, q: i.q }
    kx $ deferPayload parent.deferralPath $ deleteFromCache { id: show me }
    subscribeChildren subscribe me parent.deferralPath parent.scope di (fixed elts) ee
    subscribeAtts subscribe ee
      ( keepLatest $ map
          ( \(C.Highpass e) -> match
              { frequency: tmpResolveAU parent.scope parent.deferralPath di (setFrequency <<< { id: show me, frequency: _ })
              , q: tmpResolveAU parent.scope parent.deferralPath di (setQ <<< { id: show me, q: _ })
              }
              e
          )
          atts
      )

highpass_
  :: forall i (outputChannels :: Type) payload
   . Common.InitialHighpass i
  => i
  -> Array (C.Audible outputChannels payload)
  -> C.Audible outputChannels payload
highpass_ i a = highpass i empty a

-- highshelf
highshelf
  :: forall i (outputChannels :: Type) payload
   . Common.InitialHighshelf i
  => i
  -> Poll (C.Highshelf payload)
  -> Array (C.Audible outputChannels payload)
  -> C.Audible outputChannels payload
highshelf i' atts elts = Element' $ C.Node go
  where
  C.InitializeHighshelf i = Common.toInitializeHighshelf i'
  go parent di@(C.AudioInterpret { ids, deferPayload, deleteFromCache, makeHighshelf, setFrequency, setGain }) = behaving \ee kx subscribe -> do
    me <- justNone ids
    justNone $ parent.raiseId $ show me
    kx $ makeHighshelf
      { id: show me, parent: parent.parent, scope: scopeToMaybe parent.scope, frequency: i.frequency, gain: i.gain }
    kx $ deferPayload parent.deferralPath $ deleteFromCache { id: show me }
    subscribeChildren subscribe me parent.deferralPath parent.scope di (fixed elts) ee
    subscribeAtts subscribe ee
      ( keepLatest $ map
          ( \(C.Highshelf e) -> match
              { frequency: tmpResolveAU parent.scope parent.deferralPath di (setFrequency <<< { id: show me, frequency: _ })
              , gain: tmpResolveAU parent.scope parent.deferralPath di (setGain <<< { id: show me, gain: _ })
              }
              e
          )
          atts
      )

highshelf_
  :: forall i (outputChannels :: Type) payload
   . Common.InitialHighshelf i
  => i
  -> Array (C.Audible outputChannels payload)
  -> C.Audible outputChannels payload
highshelf_ i a = highshelf i empty a

-- iirFilter

iirFilter
  :: forall i (feedforward :: Int) (feedback :: Int) (outputChannels :: Type)
       payload
   . Compare 2 feedforward LT
  => Compare 2 feedback LT
  => Common.InitialIIRFilter i feedforward feedback
  => i
  -> Array (C.Audible outputChannels payload)
  -> C.Audible outputChannels payload
iirFilter = iirFilter' (Proxy :: _ feedforward) (Proxy :: _ feedback)

iirFilter'
  :: forall i (feedforward :: Int) (feedback :: Int) (outputChannels :: Type)
       payload
   . Compare 2 feedforward LT
  => Compare 2 feedback LT
  => Common.InitialIIRFilter i feedforward feedback
  => Proxy feedforward
  -> Proxy feedback
  -> i
  -> Array (C.Audible outputChannels payload)
  -> C.Audible outputChannels payload
iirFilter' fwd bk i' elts = Element' $ C.Node go
  where
  C.InitializeIIRFilter i = Common.toInitializeIIRFilter i' fwd bk
  go
    parent
    di@
      ( C.AudioInterpret
          { ids
          , deferPayload
          , deleteFromCache
          , makeIIRFilter
          }
      ) =
    behaving \ee kx subscribe -> do
      me <- justNone ids
      justNone $ parent.raiseId $ show me
      kx $ makeIIRFilter
        { id: show me
        , parent: parent.parent
        , scope: scopeToMaybe parent.scope
        , feedforward: toArray i.feedforward
        , feedback: toArray i.feedback
        }
      kx $ deferPayload parent.deferralPath $ deleteFromCache { id: show me }
      subscribeChildren subscribe me parent.deferralPath parent.scope di (fixed elts) ee

-- lowpass
lowpass
  :: forall i (outputChannels :: Type) payload
   . Common.InitialLowpass i
  => i
  -> Poll (C.Lowpass payload)
  -> Array (C.Audible outputChannels payload)
  -> C.Audible outputChannels payload
lowpass i' atts elts = Element' $ C.Node go
  where
  C.InitializeLowpass i = Common.toInitializeLowpass i'
  go parent di@(C.AudioInterpret { ids, deferPayload, deleteFromCache, makeLowpass, setFrequency, setQ }) = behaving \ee kx subscribe -> do
    me <- justNone ids
    justNone $ parent.raiseId $ show me
    kx $ makeLowpass
      { id: show me, parent: parent.parent, scope: scopeToMaybe parent.scope, frequency: i.frequency, q: i.q }
    kx $ deferPayload parent.deferralPath $ deleteFromCache { id: show me }
    subscribeChildren subscribe me parent.deferralPath parent.scope di (fixed elts) ee
    subscribeAtts subscribe ee
      ( keepLatest $ map
          ( \(C.Lowpass e) -> match
              { frequency: tmpResolveAU parent.scope parent.deferralPath di (setFrequency <<< { id: show me, frequency: _ })
              , q: tmpResolveAU parent.scope parent.deferralPath di (setQ <<< { id: show me, q: _ })
              }
              e
          )
          atts
      )

lowpass_
  :: forall i (outputChannels :: Type) payload
   . Common.InitialLowpass i
  => i
  -> Array (C.Audible outputChannels payload)
  -> C.Audible outputChannels payload
lowpass_ i a = lowpass i empty a

-- lowshelf
lowshelf
  :: forall i (outputChannels :: Type) payload
   . Common.InitialLowshelf i
  => i
  -> Poll (C.Lowshelf payload)
  -> Array (C.Audible outputChannels payload)
  -> C.Audible outputChannels payload
lowshelf i' atts elts = Element' $ C.Node go
  where
  C.InitializeLowshelf i = Common.toInitializeLowshelf i'
  go parent di@(C.AudioInterpret { ids, deferPayload, deleteFromCache, makeLowshelf, setFrequency, setGain }) = behaving \ee kx subscribe -> do
    me <- justNone ids
    justNone $ parent.raiseId $ show me
    kx $ makeLowshelf
      { id: show me, parent: parent.parent, scope: scopeToMaybe parent.scope, frequency: i.frequency, gain: i.gain }
    kx $ deferPayload parent.deferralPath $ deleteFromCache { id: show me }
    subscribeChildren subscribe me parent.deferralPath parent.scope di (fixed elts) ee
    subscribeAtts subscribe ee
      ( keepLatest $ map
          ( \(C.Lowshelf e) -> match
              { frequency: tmpResolveAU parent.scope parent.deferralPath di (setFrequency <<< { id: show me, frequency: _ })
              , gain: tmpResolveAU parent.scope parent.deferralPath di (setGain <<< { id: show me, gain: _ })
              }
              e
          )
          atts
      )

lowshelf_
  :: forall i (outputChannels :: Type) payload
   . Common.InitialLowshelf i
  => i
  -> Array (C.Audible outputChannels payload)
  -> C.Audible outputChannels payload
lowshelf_ i a = lowshelf i empty a

-- loopBuf

__loopBuf
  :: forall i outputChannels payload
   . Common.InitialLoopBuf i
  => i
  -> Poll (C.LoopBuf payload)
  -> C.Audible outputChannels payload
__loopBuf i' atts = Element' $ C.Node go
  where
  C.InitializeLoopBuf i = Common.toInitializeLoopBuf i'
  go
    parent
    di@
      ( C.AudioInterpret
          { ids
          , deleteFromCache
          , deferPayload
          , makeLoopBuf
          , setBuffer
          , setOnOff
          , setPlaybackRate
          , setLoopStart
          , setLoopEnd
          }
      ) =
    behaving \ee kx subscribe -> do
      me <- justNone ids
      justNone $ parent.raiseId $ show me
      kx $ makeLoopBuf
        { id: show me
        , parent: parent.parent
        , scope: scopeToMaybe parent.scope
        , buffer: i.buffer
        , playbackRate: i.playbackRate
        , loopStart: i.loopStart
        , loopEnd: i.loopEnd
        , duration: i.duration
        }
      kx $ deferPayload parent.deferralPath $ deleteFromCache { id: show me }
      subscribeAtts subscribe ee
        ( keepLatest $ map
            ( \(C.LoopBuf e) -> match
                { buffer: \buffer -> pure $ setBuffer { id: show me, buffer }
                , playbackRate: tmpResolveAU parent.scope parent.deferralPath di (setPlaybackRate <<< { id: show me, playbackRate: _ })
                , loopStart: \loopStart -> pure $ setLoopStart { id: show me, loopStart }
                , loopEnd: \loopEnd -> pure $ setLoopEnd { id: show me, loopEnd }
                , onOff: \onOff -> pure $ setOnOff { id: show me, onOff }
                }
                e
            )
            atts
        )

loopBuf
  :: forall i outputChannels payload
   . Common.InitialLoopBuf i
  => i
  -> Poll (C.LoopBuf payload)
  -> C.Audible outputChannels payload
loopBuf = __loopBuf

loopBuf_
  :: forall i outputChannels payload
   . Common.InitialLoopBuf i
  => i
  -> C.Audible outputChannels payload
loopBuf_ i = loopBuf i empty

-- mediaElement

__mediaElement
  :: forall outputChannels payload
   . C.InitializeMediaElement
  -> C.Audible outputChannels payload
__mediaElement (C.InitializeMediaElement i) = Element' $ C.Node go
  where
  go parent (C.AudioInterpret { ids, deferPayload, deleteFromCache, makeMediaElement }) =
    behaving \_ kx _ -> do
      me <- justNone ids
      justNone $ parent.raiseId $ show me
      kx $ makeMediaElement
        { id: show me
        , parent: parent.parent
        , scope: scopeToMaybe parent.scope
        , element: i.element
        }
      kx $ deferPayload parent.deferralPath $ deleteFromCache { id: show me }

mediaElement
  :: forall outputChannels payload
   . C.InitializeMediaElement
  -> C.Audible outputChannels payload
mediaElement = __mediaElement

-- microphone

__microphone
  :: forall i outputChannels payload
   . Common.InitialMicrophone i
  => i
  -> C.Audible outputChannels payload
__microphone i' = Element' $ C.Node go
  where
  C.InitializeMicrophone i = Common.toInitializeMicrophone i'
  go parent (C.AudioInterpret { ids, deferPayload, deleteFromCache, makeMicrophone }) =
    behaving \_ kx _ -> do
      me <- justNone ids
      justNone $ parent.raiseId $ show me
      kx
        ( makeMicrophone
            { id: show me
            , parent: parent.parent
            , scope: scopeToMaybe parent.scope
            , microphone: i.microphone
            }
        )
      kx $ deferPayload parent.deferralPath $ deleteFromCache { id: show me }

microphone
  :: forall i outputChannels payload
   . Common.InitialMicrophone i
  => i
  -> C.Audible outputChannels payload
microphone = __microphone

-- notch
notch
  :: forall i (outputChannels :: Type) payload
   . Common.InitialNotch i
  => i
  -> Poll (C.Notch payload)
  -> Array (C.Audible outputChannels payload)
  -> C.Audible outputChannels payload
notch i' atts elts = Element' $ C.Node go
  where
  C.InitializeNotch i = Common.toInitializeNotch i'
  go parent di@(C.AudioInterpret { ids, deferPayload, deleteFromCache, makeNotch, setFrequency, setQ }) = behaving \ee kx subscribe -> do
    me <- justNone ids
    justNone $ parent.raiseId $ show me
    kx $ makeNotch { id: show me, parent: parent.parent, scope: scopeToMaybe parent.scope, frequency: i.frequency, q: i.q }
    kx $ deferPayload parent.deferralPath $ deleteFromCache { id: show me }
    subscribeChildren subscribe me parent.deferralPath parent.scope di (fixed elts) ee
    subscribeAtts subscribe ee
      ( keepLatest $ map
          ( \(C.Notch e) -> match
              { frequency: tmpResolveAU parent.scope parent.deferralPath di (setFrequency <<< { id: show me, frequency: _ })
              , q: tmpResolveAU parent.scope parent.deferralPath di (setQ <<< { id: show me, q: _ })
              }
              e
          )
          atts
      )

notch_
  :: forall i (outputChannels :: Type) payload
   . Common.InitialNotch i
  => i
  -> Array (C.Audible outputChannels payload)
  -> C.Audible outputChannels payload
notch_ i a = notch i empty a

-- peaking
peaking
  :: forall i (outputChannels :: Type) payload
   . Common.InitialPeaking i
  => i
  -> Poll (C.Peaking payload)
  -> Array (C.Audible outputChannels payload)
  -> C.Audible outputChannels payload
peaking i' atts elts = Element' $ C.Node go
  where
  C.InitializePeaking i = Common.toInitializePeaking i'
  go parent di@(C.AudioInterpret { ids, deferPayload, deleteFromCache, makePeaking, setFrequency, setQ, setGain }) = behaving \ee kx subscribe -> do
    me <- justNone ids
    justNone $ parent.raiseId $ show me
    kx $ makePeaking
      { id: show me, parent: parent.parent, scope: scopeToMaybe parent.scope, frequency: i.frequency, q: i.q, gain: i.gain }
    kx $ deferPayload parent.deferralPath $ deleteFromCache { id: show me }
    subscribeChildren subscribe me parent.deferralPath parent.scope di (fixed elts) ee
    subscribeAtts subscribe ee
      ( keepLatest $ map
          ( \(C.Peaking e) -> match
              { frequency: tmpResolveAU parent.scope parent.deferralPath di (setFrequency <<< { id: show me, frequency: _ })
              , q: tmpResolveAU parent.scope parent.deferralPath di (setQ <<< { id: show me, q: _ })
              , gain: tmpResolveAU parent.scope parent.deferralPath di (setGain <<< { id: show me, gain: _ })
              }
              e
          )
          atts
      )

peaking_
  :: forall i (outputChannels :: Type) payload
   . Common.InitialPeaking i
  => i
  -> Array (C.Audible outputChannels payload)
  -> C.Audible outputChannels payload
peaking_ i a = peaking i empty a

-- periodicOsc

__periodicOsc
  :: forall i outputChannels payload
   . Common.InitialPeriodicOsc i
  => i
  -> Poll (C.PeriodicOsc payload)
  -> C.Audible outputChannels payload
__periodicOsc i' atts = Element' $ C.Node go
  where
  C.InitializePeriodicOsc i = Common.toInitializePeriodicOsc i'
  go
    parent
    di@
      ( C.AudioInterpret
          { ids, deferPayload, deleteFromCache, makePeriodicOsc, setFrequency, setOnOff, setPeriodicOsc }
      ) =
    behaving \ee kx subscribe -> do
      me <- justNone ids
      justNone $ parent.raiseId $ show me
      kx $ makePeriodicOsc
        { id: show me
        , parent: parent.parent
        , scope: scopeToMaybe parent.scope
        , frequency: i.frequency
        , spec: i.spec
        }
      kx $ deferPayload parent.deferralPath $ deleteFromCache { id: show me }
      subscribeAtts subscribe ee
        ( keepLatest $ map
            ( \(C.PeriodicOsc e) -> match
                { frequency: tmpResolveAU parent.scope parent.deferralPath di (setFrequency <<< { id: show me, frequency: _ })
                , onOff: \onOff -> pure $ setOnOff { id: show me, onOff }
                , spec: \spec -> pure $ setPeriodicOsc { id: show me, spec }
                }
                e
            )
            atts
        )

periodicOsc
  :: forall i outputChannels payload
   . Common.InitialPeriodicOsc i
  => i
  -> Poll (C.PeriodicOsc payload)
  -> C.Audible outputChannels payload
periodicOsc = __periodicOsc

periodicOsc_
  :: forall i outputChannels payload
   . Common.InitialPeriodicOsc i
  => i
  -> C.Audible outputChannels payload
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
  :: forall i outputChannels payload
   . Common.InitialPlayBuf i
  => i
  -> Poll (C.PlayBuf payload)
  -> C.Audible outputChannels payload
__playBuf i' atts = Element' $ C.Node go
  where
  C.InitializePlayBuf i = Common.toInitializePlayBuf i'
  go
    parent
    di@
      ( C.AudioInterpret
          { ids
          , deleteFromCache
          , deferPayload
          , makePlayBuf
          , setBuffer
          , setOnOff
          , setDuration
          , setPlaybackRate
          , setBufferOffset
          }
      ) =
    behaving \ee kx subscribe -> do
      me <- justNone ids
      justNone $ parent.raiseId $ show me
      kx $ makePlayBuf
        { id: show me
        , parent: parent.parent
        , scope: scopeToMaybe parent.scope
        , buffer: i.buffer
        , playbackRate: i.playbackRate
        , bufferOffset: i.bufferOffset
        , duration: i.duration
        }
      kx $ deferPayload parent.deferralPath $ deleteFromCache { id: show me }
      subscribeAtts subscribe ee
        ( keepLatest $ map
            ( \(C.PlayBuf e) -> match
                { buffer: \buffer -> pure $ setBuffer { id: show me, buffer }
                , playbackRate: tmpResolveAU parent.scope parent.deferralPath di
                    ( setPlaybackRate <<<
                        { id: show me, playbackRate: _ }
                    )
                , bufferOffset: \bufferOffset -> pure $ setBufferOffset
                    { id: show me, bufferOffset }
                , onOff: \onOff -> pure $ setOnOff { id: show me, onOff }
                , duration: \duration -> pure $ setDuration { id: show me, duration }
                }
                e
            )
            atts
        )

playBuf
  :: forall i outputChannels payload
   . Common.InitialPlayBuf i
  => i
  -> Poll (C.PlayBuf payload)
  -> C.Audible outputChannels payload
playBuf = __playBuf

playBuf_
  :: forall i outputChannels payload
   . Common.InitialPlayBuf i
  => i
  -> C.Audible outputChannels payload
playBuf_ i = playBuf i empty

-- recorder
recorder
  :: forall i outputChannels payload
   . Common.InitialRecorder i
  => i
  -> C.Audible outputChannels payload
  -> C.Audible outputChannels payload
recorder i' elt = Element' $ C.Node go
  where
  C.InitializeRecorder i = Common.toInitializeRecorder i'
  go parent di@(C.AudioInterpret { ids, deferPayload, deleteFromCache, makeRecorder }) =
    behaving \ee kx subscribe -> do
      me <- justNone ids
      justNone $ parent.raiseId $ show me
      kx $ makeRecorder
        { id: show me, parent: parent.parent, scope: scopeToMaybe parent.scope, cb: i.cb }
      kx $ deferPayload parent.deferralPath $ deleteFromCache { id: show me }
      subscribeChildren subscribe me parent.deferralPath parent.scope di elt ee

-- sawtoothOsc

__sawtoothOsc
  :: forall i outputChannels payload
   . Common.InitialSawtoothOsc i
  => i
  -> Poll (C.SawtoothOsc payload)
  -> C.Audible outputChannels payload
__sawtoothOsc i' atts = Element' $ C.Node go
  where
  C.InitializeSawtoothOsc i = Common.toInitializeSawtoothOsc i'
  go
    parent
    di@(C.AudioInterpret { ids, deferPayload, deleteFromCache, makeSawtoothOsc, setFrequency, setOnOff }) =
    behaving \ee kx subscribe -> do
      me <- justNone ids
      justNone $ parent.raiseId $ show me
      kx $ makeSawtoothOsc
        { id: show me
        , parent: parent.parent
        , scope: scopeToMaybe parent.scope
        , frequency: i.frequency
        }
      kx $ deferPayload parent.deferralPath $ deleteFromCache { id: show me }
      subscribeAtts subscribe ee
        ( keepLatest $ map
            ( \(C.SawtoothOsc e) -> match
                { frequency: tmpResolveAU parent.scope parent.deferralPath di (setFrequency <<< { id: show me, frequency: _ })
                , onOff: \onOff -> pure $ setOnOff { id: show me, onOff }
                }
                e
            )
            atts
        )

sawtoothOsc
  :: forall i outputChannels payload
   . Common.InitialSawtoothOsc i
  => i
  -> Poll (C.SawtoothOsc payload)
  -> C.Audible outputChannels payload
sawtoothOsc = __sawtoothOsc

sawtoothOsc_
  :: forall i outputChannels payload
   . Common.InitialSawtoothOsc i
  => i
  -> C.Audible outputChannels payload
sawtoothOsc_ i = sawtoothOsc i empty

-- sinOsc

__sinOsc
  :: forall i outputChannels payload
   . Common.InitialSinOsc i
  => i
  -> Poll (C.SinOsc payload)
  -> C.Audible outputChannels payload
__sinOsc i' atts = Element' $ C.Node go
  where
  C.InitializeSinOsc i = Common.toInitializeSinOsc i'
  go
    parent
    di@(C.AudioInterpret { ids, deferPayload, deleteFromCache, makeSinOsc, setFrequency, setOnOff }) =
    behaving \ee kx subscribe -> do
      me <- justNone ids
      justNone $ parent.raiseId $ show me
      kx $ makeSinOsc
        { id: show me
        , parent: parent.parent
        , scope: scopeToMaybe parent.scope
        , frequency: i.frequency
        }
      kx $ deferPayload parent.deferralPath $ deleteFromCache { id: show me }
      subscribeAtts subscribe ee
        ( keepLatest $ map
            ( \(C.SinOsc e) -> match
                { frequency: tmpResolveAU parent.scope parent.deferralPath di (setFrequency <<< { id: show me, frequency: _ })
                , onOff: \onOff -> pure $ setOnOff { id: show me, onOff }
                }
                e
            )
            atts
        )

sinOsc
  :: forall i outputChannels payload
   . Common.InitialSinOsc i
  => i
  -> Poll (C.SinOsc payload)
  -> C.Audible outputChannels payload
sinOsc = __sinOsc

sinOsc_
  :: forall i outputChannels payload
   . Common.InitialSinOsc i
  => i
  -> C.Audible outputChannels payload
sinOsc_ a = sinOsc a empty

-- squareOsc

__squareOsc
  :: forall i outputChannels payload
   . Common.InitialSquareOsc i
  => i
  -> Poll (C.SquareOsc payload)
  -> C.Audible outputChannels payload
__squareOsc i' atts = Element' $ C.Node go
  where
  C.InitializeSquareOsc i = Common.toInitializeSquareOsc i'
  go
    parent
    di@(C.AudioInterpret { ids, deferPayload, deleteFromCache, makeSquareOsc, setFrequency, setOnOff }) =
    behaving \ee kx subscribe -> do
      me <- justNone ids
      justNone $ parent.raiseId $ show me
      kx $ makeSquareOsc
        { id: show me
        , parent: parent.parent
        , scope: scopeToMaybe parent.scope
        , frequency: i.frequency
        }
      kx $ deferPayload parent.deferralPath $ deleteFromCache { id: show me }
      subscribeAtts subscribe ee
        ( keepLatest $ map
            ( \(C.SquareOsc e) -> match
                { frequency: tmpResolveAU parent.scope parent.deferralPath di (setFrequency <<< { id: show me, frequency: _ })
                , onOff: \onOff -> pure $ setOnOff { id: show me, onOff }
                }
                e
            )
            atts
        )

squareOsc
  :: forall i outputChannels payload
   . Common.InitialSquareOsc i
  => i
  -> Poll (C.SquareOsc payload)
  -> C.Audible outputChannels payload
squareOsc = __squareOsc

squareOsc_
  :: forall i outputChannels payload
   . Common.InitialSquareOsc i
  => i
  -> C.Audible outputChannels payload
squareOsc_ i = squareOsc i empty

-- speaker
speaker
  :: forall (outputChannels :: Type) payload
   . Array (C.Audible outputChannels payload)
  -> C.AudioInterpret payload
  -> Poll payload
speaker elts di@(C.AudioInterpret { ids, makeSpeaker }) = behaving \ee kx subscribe -> do
  me <- justNone ids
  kx $ makeSpeaker { id: show me }
  subscribeChildren subscribe me List.Nil (Local "toplevel") di (fixed elts) ee

speaker2
  :: forall payload
   . Array (C.Audible D2 payload)
  -> C.AudioInterpret payload
  -> Poll payload
speaker2 = speaker

-- pan
pan
  :: forall i (outputChannels :: Type) payload
   . Common.InitialStereoPanner i
  => i
  -> Poll (C.StereoPanner payload)
  -> Array (C.Audible outputChannels payload)
  -> C.Audible outputChannels payload
pan i' atts elts = Element' $ C.Node go
  where
  C.InitializeStereoPanner i = Common.toInitializeStereoPanner i'
  go parent di@(C.AudioInterpret { ids, deferPayload, deleteFromCache, makeStereoPanner, setPan }) = behaving \ee kx subscribe -> do
    me <- justNone ids
    justNone $ parent.raiseId $ show me
    kx $ makeStereoPanner
      { id: show me, parent: parent.parent, scope: scopeToMaybe parent.scope, pan: i.pan }
    kx $ deferPayload parent.deferralPath $ deleteFromCache { id: show me }
    subscribeChildren subscribe me parent.deferralPath parent.scope di (fixed elts) ee
    subscribeAtts subscribe ee
      ( keepLatest $ map
          ( \(C.StereoPanner e) -> match
              { pan: tmpResolveAU parent.scope parent.deferralPath di (setPan <<< { id: show me, pan: _ })
              }
              e
          )
          atts
      )

pan_
  :: forall i (outputChannels :: Type) payload
   . Common.InitialStereoPanner i
  => i
  -> Array (C.Audible outputChannels payload)
  -> C.Audible outputChannels payload
pan_ i = pan i empty

-- triangleOsc

__triangleOsc
  :: forall i outputChannels payload
   . Common.InitialTriangleOsc i
  => i
  -> Poll (C.TriangleOsc payload)
  -> C.Audible outputChannels payload
__triangleOsc i' atts = Element' $ C.Node go
  where
  C.InitializeTriangleOsc i = Common.toInitializeTriangleOsc i'
  go
    parent
    di@(C.AudioInterpret { ids, deferPayload, deleteFromCache, makeTriangleOsc, setFrequency, setOnOff }) =
    behaving \ee kx subscribe -> do
      me <- justNone ids
      justNone $ parent.raiseId $ show me
      kx $ makeTriangleOsc
        { id: show me
        , parent: parent.parent
        , scope: scopeToMaybe parent.scope
        , frequency: i.frequency
        }
      kx $ deferPayload parent.deferralPath $ deleteFromCache { id: show me }
      subscribeAtts subscribe ee
        ( keepLatest $ map
            ( \(C.TriangleOsc e) -> match
                { frequency: tmpResolveAU parent.scope parent.deferralPath di (setFrequency <<< { id: show me, frequency: _ })
                , onOff: \onOff -> pure $ setOnOff { id: show me, onOff }
                }
                e
            )
            atts
        )

triangleOsc
  :: forall i outputChannels payload
   . Common.InitialTriangleOsc i
  => i
  -> Poll (C.TriangleOsc payload)
  -> C.Audible outputChannels payload
triangleOsc = __triangleOsc

triangleOsc_
  :: forall i outputChannels payload
   . Common.InitialTriangleOsc i
  => i
  -> C.Audible outputChannels payload
triangleOsc_ i = triangleOsc i empty

-- waveShaper

waveShaper
  :: forall i (outputChannels :: Type) payload
   . Common.InitialWaveShaper i
  => i
  -> Array (C.Audible outputChannels payload)
  -> C.Audible outputChannels payload
waveShaper i' elts = Element' $ C.Node go
  where
  C.InitializeWaveShaper i = Common.toInitializeWaveShaper i'
  go parent di@(C.AudioInterpret { ids, deferPayload, deleteFromCache, makeWaveShaper }) =
    behaving \ee kx subscribe -> do
      me <- justNone ids
      justNone $ parent.raiseId $ show me
      kx $ makeWaveShaper
        { id: show me
        , parent: parent.parent
        , scope: scopeToMaybe parent.scope
        , curve: i.curve
        , oversample: i.oversample
        }
      kx $ deferPayload parent.deferralPath $ deleteFromCache { id: show me }
      subscribeChildren subscribe me parent.deferralPath parent.scope di (fixed elts) ee

----------
globalFan
  :: forall n o payload
   . Compare n (-1) GT
  => Vect n (C.Audible o payload)
  -> (Vect n (C.Audible o payload) -> C.Audible o payload)
  -> C.Audible o payload
globalFan a b = Bolson.globalPortalComplexComplex
  { doLogic: absurd
  , deferPayload: \(C.AudioInterpret { deferPayload }) -> deferPayload
  , forcePayload: \(C.AudioInterpret { forcePayload }) -> forcePayload
  , ids: unwrap >>> _.ids
  , disconnectElement: \(C.AudioInterpret { disconnectXFromY }) { id, parent } -> disconnectXFromY { from: id, to: parent }
  , toElt: \(C.Node e) -> Element e
  }
  { fromEltO1: coerce
  , fromEltO2: coerce
  , toElt: coerce
  , wrapElt: \e -> gain_ 1.0 [ e ]
  , giveNewParent: \(C.AudioInterpret { connectXToY }) { id, parent } _ _ -> connectXToY { from: id, to: parent }
  , deleteFromCache: unwrap >>> _.deleteFromCache
  }
  a
  (lcmap (map (_ $ unit)) b)

fan
  :: forall o n payload
   . Compare n (-1) GT
  => Vect n (C.Audible o payload)
  -> ( Vect n (C.Audible o payload)
       -> C.Audible o payload
     )
  -> C.Audible o payload
fan a b = Bolson.portalComplexComplex
  { doLogic: absurd
  , deferPayload: \(C.AudioInterpret { deferPayload }) -> deferPayload
  , forcePayload: \(C.AudioInterpret { forcePayload }) -> forcePayload
  , ids: unwrap >>> _.ids
  , disconnectElement: \(C.AudioInterpret { disconnectXFromY }) { id, parent } -> disconnectXFromY { from: id, to: parent }
  , toElt: \(C.Node e) -> Element e
  }
  { fromEltO1: coerce
  , fromEltO2: coerce
  , toElt: coerce
  , wrapElt: \e -> gain_ 1.0 [ e ]
  , giveNewParent: \(C.AudioInterpret { connectXToY }) { id, parent } _ _ -> connectXToY { from: id, to: parent }
  , deleteFromCache: unwrap >>> _.deleteFromCache
  }
  a
  (lcmap (map (_ $ unit)) (coerce b))

globalFan1
  :: forall o payload
   . C.Audible o payload
  -> ( C.Audible o payload
       -> C.Audible o payload
     )
  -> C.Audible o payload
globalFan1 a b = globalFan (singleton a) (lcmap (index (Proxy :: _ 0)) b)

fan1
  :: forall o payload
   . C.Audible o payload
  -> ( C.Audible o payload
       -> C.Audible o payload
     )
  -> C.Audible o payload
fan1 a b = fan (singleton a) (lcmap (index (Proxy :: _ 0)) b)

---- fix
fix
  :: forall outputChannels payload
   . (C.Audible outputChannels payload -> C.Audible outputChannels payload)
  -> C.Audible outputChannels payload
fix = Bolson.fixComplexComplex
  { doLogic: absurd
  , ids: unwrap >>> _.ids
  , deferPayload: \(C.AudioInterpret { deferPayload }) -> deferPayload
  , forcePayload: \(C.AudioInterpret { forcePayload }) -> forcePayload
  , disconnectElement: \(C.AudioInterpret { disconnectXFromY }) { id, parent } -> disconnectXFromY { from: id, to: parent }
  , toElt: \(C.Node e) -> Element e
  }
  { fromElt: coerce
  , connectToParent: \(C.AudioInterpret { connectXToY }) { id, parent } -> connectXToY { from: id, to: parent }
  }

silence
  :: forall outputChannels payload
   . C.Audible outputChannels payload
silence = fix identity

-----
-- starts work on merge
-- merge
--   :: forall i n payload
--    . IsEvent event
--   => Pos n
--   => Vec n (C.Node D1 payload)
--   -> C.Node n payload
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
--                 { id: show me
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
tmpResolveAU
  :: forall payload. Scope -> List.List Int -> C.AudioInterpret payload -> (C.FFIAudioParameter -> payload) -> C.AudioParameter payload -> Poll payload
tmpResolveAU = go
  where
  cncl = C.FFIAudioParameter <<< inj (Proxy :: _ "cancel")
  ev = C.FFIAudioParameter <<< inj (Proxy :: _ "envelope")
  nmc = C.FFIAudioParameter <<< inj (Proxy :: _ "numeric")
  sdn = C.FFIAudioParameter <<< inj (Proxy :: _ "sudden")
  ut = C.FFIAudioParameter <<< inj (Proxy :: _ "unit")
  go scope deferralPath di f (C.AudioParameter a) = match
    { numeric: pure <<< f <<< nmc
    , envelope: pure <<< f <<< ev
    , cancel: pure <<< f <<< cncl
    , sudden: pure <<< f <<< sdn
    , unit: \(C.AudioUnit { u }) -> do
        let wrappingGain = gain_ 1.0 [ u ]
        behaving \ee _ subscribe -> do
          av <- justNone $ RRef.new Nothing
          -- todo: make sure the ordering is correct here
          -- in the old ocarina, it was easier to reason that the event
          -- on the left would happen before the one on the right
          -- however, with polls, it's tougher to think about an l-to-r relationship
          -- if we see an error message, deal with it then
          subscribe
            ( sample
                ( __internalOcarinaFlatten { parent: Nothing, scope: scope, deferralPath: deferralPath, raiseId: \x -> void $ RRef.write (Just x) av } di
                    wrappingGain <|>
                    ( poll \e ->
                        ( makeEvent \sub ->
                            sub e \f0 -> do
                              justNone (RRef.read av) >>= case _ of
                                Nothing -> pure $ unsafePerformEffect (error "Wrapped audio unit failed!")
                                Just i -> justOne (f0 (f (ut (C.FFIAudioUnit { i }))))
                        )
                    )
                )
                ee
            )
    }
    a

__internalOcarinaFlatten
  :: forall o payload
   . PSR ()
  -> C.AudioInterpret payload
  -> C.Audible o payload
  -> Poll payload
__internalOcarinaFlatten psr ai au = Bolson.flatten
  { doLogic: absurd
  , deferPayload: \(C.AudioInterpret { deferPayload }) -> deferPayload
  , forcePayload: \(C.AudioInterpret { forcePayload }) -> forcePayload
  , ids: unwrap >>> _.ids
  , disconnectElement: \(C.AudioInterpret { disconnectXFromY }) { id, parent } -> disconnectXFromY { from: id, to: parent }
  , toElt: \(C.Node e) -> Element e
  }
  au
  psr
  ai