module WAGS.Control where

import Prelude

import Control.Alt ((<|>))
import Control.Comonad (extract)
import Control.Plus (empty)
import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Data.Either (Either(..))
import Data.Foldable (for_, oneOf)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Homogeneous (class HomogeneousRowLabels)
import Data.Homogeneous.Variant (homogeneous)
import Data.Int (pow)
import Data.Profunctor (lcmap)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Typelevel.Num (class Lt, class Nat, class Pos, class Pred, D1, D2, d0, pred, toInt)
import Data.Variant (Unvariant(..), inj, match, unvariant)
import Data.Variant.Maybe (Maybe, just, nothing)
import Data.Vec (Vec, index, singleton, toArray)
import Effect (Effect, foreachE)
import Effect.AVar (tryPut)
import Effect.AVar as AVar
import Effect.Exception (throwException)
import FRP.Event (Event, bang, keepLatest, makeEvent, subscribe)
import Foreign.Object (fromHomogeneous)
import Simple.JSON as JSON
import Type.Proxy (Proxy(..))
import Type.Row.Homogeneous (class Homogeneous)
import Unsafe.Coerce (unsafeCoerce)
import WAGS.Common as Common
import WAGS.Core (ChannelCountMode(..), ChannelInterpretation(..), Po2(..), __internalWagsFlatten, mix)
import WAGS.Core as Core
import WAGS.WebAPI (BrowserAudioBuffer)

-- -- audio worklet

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

-- __audioWorklet
--   :: forall name numberOfInputs numberOfOutputs outputChannelCount parameterData
--        parameterDataRL
--        processorOptions lock payload
--    . IsSymbol name
--   => Nat numberOfInputs
--   => Pos numberOfOutputs
--   => ValidateOutputChannelCount numberOfOutputs outputChannelCount
--   => Homogeneous parameterData Core.InitialAudioParameter
--   => HomogeneousRowLabels parameterData (Core.AudioParameter lock payload) parameterDataRL
--   => JSON.WriteForeign { | processorOptions }
--   => Core.InitializeAudioWorkletNode name numberOfInputs numberOfOutputs
--        outputChannelCount
--        parameterData
--        processorOptions
--   -> Event (Core.AudioWorkletNode parameterData)
--   -> Core.Node numberOfOutputs lock payload
--   -> Core.Node numberOfOutputs lock payload
-- __audioWorklet (Core.InitializeAudioWorkletNode i) atts elt = Core.Node go
--   where
--   go
--     parent
--     di@
--       ( Core.AudioInterpret
--           { ids, deleteFromCache, makeAudioWorkletNode, setAudioWorkletParameter }
--       ) =
--     makeEvent \k -> do
--       me <- ids
--       parent.raiseId me
--       map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
--         bang
--           ( makeAudioWorkletNode
--               { id: me
--               , parent: parent.parent
--               , scope: parent.scope
--               , options:
--                   Core.AudioWorkletNodeOptions_
--                     { name: reflectSymbol (Proxy :: _ name)
--                     , numberOfInputs: toInt i.numberOfInputs
--                     , numberOfOutputs: toInt i.numberOfOutputs
--                     , outputChannelCount: toOutputChannelCount
--                         i.numberOfOutputs
--                         i.outputChannelCount
--                     , parameterData: fromHomogeneous i.parameterData
--                     , processorOptions: JSON.writeImpl i.processorOptions
--                     }
--               }
--           )
--           <|>
--             ( keepLatest $ map
--                 ( \(Core.AudioWorkletNode e) -> tmpResolveAU parent.scope di
--                     ( \paramValue -> setAudioWorkletParameter
--                         { id: me
--                         , paramName: (let Unvariant e' = unvariant e in e')
--                             (\sym _ -> reflectSymbol sym)
--                         , paramValue
--                         }
--                     )
--                     (extract (homogeneous e))
--                 )
--                 atts
--             )
--           <|> __internalWagsFlatten (just me) parent.scope di (mix elt)

-- audioWorklet
--   :: forall name numberOfInputs numberOfOutputs outputChannelCount parameterData
--        parameterDataRL
--        processorOptions lock payload
--    . IsSymbol name
--   => Nat numberOfInputs
--   => Pos numberOfOutputs
--   => ValidateOutputChannelCount numberOfOutputs outputChannelCount
--   => Homogeneous parameterData Core.InitialAudioParameter
--   => HomogeneousRowLabels parameterData (Core.AudioParameter lock payload) parameterDataRL
--   => JSON.WriteForeign { | processorOptions }
--   => Core.InitializeAudioWorkletNode name numberOfInputs numberOfOutputs
--        outputChannelCount
--        parameterData
--        processorOptions
--   -> Event (Core.AudioWorkletNode parameterData)
--   -> Core.Node numberOfOutputs lock payload
--   -> Core.Node numberOfOutputs lock payload
-- audioWorklet = __audioWorklet

-- -- gain

-- -- highpass
-- highpass
--   :: forall i aud (outputChannels :: Type) lock payload
--    . Common.InitialHighpass i
--   => Core.Mix aud (Core.Audible outputChannels lock payload)
--   => i
--   -> Event (Core.Highpass lock payload)
--   -> aud
--   -> Core.Node outputChannels lock payload
-- highpass i' atts elts = Core.Node go
--   where
--   Core.InitializeHighpass i = Common.toInitializeHighpass i'
--   go parent di@(Core.AudioInterpret { ids, deleteFromCache, makeHighpass, setFrequency, setQ }) = makeEvent \k -> do
--     me <- ids
--     parent.raiseId me
--     map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
--       bang
--         ( makeHighpass
--             { id: me, parent: parent.parent, scope: parent.scope, frequency: i.frequency, q: i.q }
--         )
--         <|>
--           ( keepLatest $ map
--               ( \(Core.Highpass e) -> match
--                   { frequency: tmpResolveAU parent.scope di (setFrequency <<< { id: me, frequency: _ })
--                   , q: tmpResolveAU parent.scope di (setQ <<< { id: me, q: _ })
--                   }
--                   e
--               )
--               atts
--           )
--         <|> __internalWagsFlatten (just me) parent.scope di (mix elts)

-- highpass_
--   :: forall i aud (outputChannels :: Type) lock payload
--    . Common.InitialHighpass i
--   => Core.Mix aud (Core.Audible outputChannels lock payload)
--   => i
--   -> aud
--   -> Core.Node outputChannels lock payload
-- highpass_ i a = highpass i empty a

-- -- highshelf
-- highshelf
--   :: forall i aud (outputChannels :: Type) lock payload
--    . Common.InitialHighshelf i
--   => Core.Mix aud (Core.Audible outputChannels lock payload)
--   => i
--   -> Event (Core.Highshelf lock payload)
--   -> aud
--   -> Core.Node outputChannels lock payload
-- highshelf i' atts elts = Core.Node go
--   where
--   Core.InitializeHighshelf i = Common.toInitializeHighshelf i'
--   go parent di@(Core.AudioInterpret { ids, deleteFromCache, makeHighshelf, setFrequency, setGain }) = makeEvent \k -> do
--     me <- ids
--     parent.raiseId me
--     map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
--       bang
--         ( makeHighshelf
--             { id: me, parent: parent.parent, scope: parent.scope, frequency: i.frequency, gain: i.gain }
--         )
--         <|>
--           ( keepLatest $ map
--               ( \(Core.Highshelf e) -> match
--                   { frequency: tmpResolveAU parent.scope di (setFrequency <<< { id: me, frequency: _ })
--                   , gain: tmpResolveAU parent.scope di (setGain <<< { id: me, gain: _ })
--                   }
--                   e
--               )
--               atts
--           )
--         <|> __internalWagsFlatten (just me) parent.scope di (mix elts)

-- highshelf_
--   :: forall i aud (outputChannels :: Type) lock payload
--    . Common.InitialHighshelf i
--   => Core.Mix aud (Core.Audible outputChannels lock payload)
--   => i
--   -> aud
--   -> Core.Node outputChannels lock payload
-- highshelf_ i a = highshelf i empty a

-- -- iirFilter

-- iirFilter
--   :: forall i aud (feedforward :: Type) (feedback :: Type) (outputChannels :: Type) lock
--        payload
--    . Lt D2 feedforward
--   => Lt D2 feedback
--   => Core.Mix aud (Core.Audible outputChannels lock payload)
--   => Common.InitialIIRFilter i feedforward feedback
--   => Core.Mix aud (Core.Audible outputChannels lock payload)
--   => i
--   -> aud
--   -> Core.Node outputChannels lock payload
-- iirFilter = iirFilter' (Proxy :: _ feedforward) (Proxy :: _ feedback)

-- iirFilter'
--   :: forall i aud proxy (feedforward :: Type) (feedback :: Type) (outputChannels :: Type) lock
--        payload
--    . Lt D2 feedforward
--   => Lt D2 feedback
--   => Common.InitialIIRFilter i feedforward feedback
--   => Core.Mix aud (Core.Audible outputChannels lock payload)
--   => proxy feedforward
--   -> proxy feedback
--   -> i
--   -> aud
--   -> Core.Node outputChannels lock payload
-- iirFilter' fwd bk i' elts = Core.Node go
--   where
--   Core.InitializeIIRFilter i = Common.toInitializeIIRFilter i' fwd bk
--   go
--     parent
--     di@
--       ( Core.AudioInterpret
--           { ids
--           , deleteFromCache
--           , makeIIRFilter
--           }
--       ) =
--     makeEvent \k -> do
--       me <- ids
--       parent.raiseId me
--       map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
--         bang
--           ( makeIIRFilter
--               { id: me
--               , parent: parent.parent
--               , scope: parent.scope
--               , feedforward: toArray i.feedforward
--               , feedback: toArray i.feedback
--               }
--           )
--           <|> __internalWagsFlatten (just me) parent.scope di (mix elts)

-- -- lowpass
-- lowpass
--   :: forall i aud (outputChannels :: Type) lock payload
--    . Common.InitialLowpass i
--   => Core.Mix aud (Core.Audible outputChannels lock payload)
--   => i
--   -> Event (Core.Lowpass lock payload)
--   -> aud
--   -> Core.Node outputChannels lock payload
-- lowpass i' atts elts = Core.Node go
--   where
--   Core.InitializeLowpass i = Common.toInitializeLowpass i'
--   go parent di@(Core.AudioInterpret { ids, deleteFromCache, makeLowpass, setFrequency, setQ }) = makeEvent \k -> do
--     me <- ids
--     parent.raiseId me
--     map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
--       bang
--         ( makeLowpass
--             { id: me, parent: parent.parent, scope: parent.scope, frequency: i.frequency, q: i.q }
--         )
--         <|>
--           ( keepLatest $ map
--               ( \(Core.Lowpass e) -> match
--                   { frequency: tmpResolveAU parent.scope di (setFrequency <<< { id: me, frequency: _ })
--                   , q: tmpResolveAU parent.scope di (setQ <<< { id: me, q: _ })
--                   }
--                   e
--               )
--               atts
--           )
--         <|> __internalWagsFlatten (just me) parent.scope di (mix elts)

-- lowpass_
--   :: forall i aud (outputChannels :: Type) lock payload
--    . Common.InitialLowpass i
--   => Core.Mix aud (Core.Audible outputChannels lock payload)
--   => i
--   -> aud
--   -> Core.Node outputChannels lock payload
-- lowpass_ i a = lowpass i empty a

-- -- lowshelf
-- lowshelf
--   :: forall i aud (outputChannels :: Type) lock payload
--    . Common.InitialLowshelf i
--   => Core.Mix aud (Core.Audible outputChannels lock payload)
--   => i
--   -> Event (Core.Lowshelf lock payload)
--   -> aud
--   -> Core.Node outputChannels lock payload
-- lowshelf i' atts elts = Core.Node go
--   where
--   Core.InitializeLowshelf i = Common.toInitializeLowshelf i'
--   go parent di@(Core.AudioInterpret { ids, deleteFromCache, makeLowshelf, setFrequency, setGain }) = makeEvent \k -> do
--     me <- ids
--     parent.raiseId me
--     map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
--       bang
--         ( makeLowshelf
--             { id: me, parent: parent.parent, scope: parent.scope, frequency: i.frequency, gain: i.gain }
--         )
--         <|>
--           ( keepLatest $ map
--               ( \(Core.Lowshelf e) -> match
--                   { frequency: tmpResolveAU parent.scope di (setFrequency <<< { id: me, frequency: _ })
--                   , gain: tmpResolveAU parent.scope di (setGain <<< { id: me, gain: _ })
--                   }
--                   e
--               )
--               atts
--           )
--         <|> __internalWagsFlatten (just me) parent.scope di (mix elts)

-- lowshelf_
--   :: forall i aud (outputChannels :: Type) lock payload
--    . Common.InitialLowshelf i
--   => Core.Mix aud (Core.Audible outputChannels lock payload)
--   => i
--   -> aud
--   -> Core.Node outputChannels lock payload
-- lowshelf_ i a = lowshelf i empty a

-- -- loopBuf

-- __loopBuf
--   :: forall i outputChannels lock payload
--    . Common.InitialLoopBuf i
--   => i
--   -> Event (Core.LoopBuf lock payload)
--   -> Core.Node outputChannels lock payload
-- __loopBuf i' atts = Core.Node go
--   where
--   Core.InitializeLoopBuf i = Common.toInitializeLoopBuf i'
--   go
--     parent
--     di@
--       ( Core.AudioInterpret
--           { ids
--           , deleteFromCache
--           , makeLoopBuf
--           , setBuffer
--           , setOnOff
--           , setPlaybackRate
--           , setLoopStart
--           , setLoopEnd
--           }
--       ) =
--     makeEvent \k -> do
--       me <- ids
--       parent.raiseId me
--       map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
--         bang
--           ( makeLoopBuf
--               { id: me
--               , parent: parent.parent
--               , scope: parent.scope
--               , buffer: i.buffer
--               , playbackRate: i.playbackRate
--               , loopStart: i.loopStart
--               , loopEnd: i.loopEnd
--               , duration: i.duration
--               }
--           )
--           <|>
--             ( keepLatest $ map
--                 ( \(Core.LoopBuf e) -> match
--                     { buffer: \buffer -> bang $ setBuffer { id: me, buffer }
--                     , playbackRate: tmpResolveAU parent.scope di (setPlaybackRate <<< { id: me, playbackRate: _ })
--                     , loopStart: \loopStart -> bang $ setLoopStart { id: me, loopStart }
--                     , loopEnd: \loopEnd -> bang $ setLoopEnd { id: me, loopEnd }
--                     , onOff: \onOff -> bang $ setOnOff { id: me, onOff }
--                     }
--                     e
--                 )
--                 atts
--             )

-- loopBuf
--   :: forall i outputChannels lock payload
--    . Common.InitialLoopBuf i
--   => i
--   -> Event (Core.LoopBuf lock payload)
--   -> Core.Node outputChannels lock payload
-- loopBuf = __loopBuf

-- loopBuf_
--   :: forall i outputChannels lock payload
--    . Common.InitialLoopBuf i
--   => i
--   -> Core.Node outputChannels lock payload
-- loopBuf_ i = loopBuf i empty

-- -- mediaElement

-- __mediaElement
--   :: forall outputChannels lock payload
--    . Core.InitializeMediaElement
--   -> Core.Node outputChannels lock payload
-- __mediaElement (Core.InitializeMediaElement i) = Core.Node go
--   where
--   go parent (Core.AudioInterpret { ids, deleteFromCache, makeMediaElement }) =
--     makeEvent \k -> do
--       me <- ids
--       parent.raiseId me
--       map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
--         bang
--           ( makeMediaElement
--               { id: me
--               , parent: parent.parent
--               , scope: parent.scope
--               , element: i.element
--               }
--           )

-- mediaElement
--   :: forall outputChannels lock payload
--    . Core.InitializeMediaElement
--   -> Core.Node outputChannels lock payload
-- mediaElement = __mediaElement

-- -- microphone

-- __microphone
--   :: forall i outputChannels lock payload
--    . Common.InitialMicrophone i
--   => i
--   -> Core.Node outputChannels lock payload
-- __microphone i' = Core.Node go
--   where
--   Core.InitializeMicrophone i = Common.toInitializeMicrophone i'
--   go parent (Core.AudioInterpret { ids, deleteFromCache, makeMicrophone }) =
--     makeEvent \k -> do
--       me <- ids
--       parent.raiseId me
--       map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
--         bang
--           ( makeMicrophone
--               { id: me
--               , parent: parent.parent
--               , scope: parent.scope
--               , microphone: i.microphone
--               }
--           )

-- microphone
--   :: forall i outputChannels lock payload
--    . Common.InitialMicrophone i
--   => i
--   -> Core.Node outputChannels lock payload
-- microphone = __microphone

-- -- notch
-- notch
--   :: forall i aud (outputChannels :: Type) lock payload
--    . Common.InitialNotch i
--   => Core.Mix aud (Core.Audible outputChannels lock payload)
--   => i
--   -> Event (Core.Notch lock payload)
--   -> aud
--   -> Core.Node outputChannels lock payload
-- notch i' atts elts = Core.Node go
--   where
--   Core.InitializeNotch i = Common.toInitializeNotch i'
--   go parent di@(Core.AudioInterpret { ids, deleteFromCache, makeNotch, setFrequency, setQ }) = makeEvent \k -> do
--     me <- ids
--     parent.raiseId me
--     map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
--       bang
--         ( makeNotch
--             { id: me, parent: parent.parent, scope: parent.scope, frequency: i.frequency, q: i.q }
--         )
--         <|>
--           ( keepLatest $ map
--               ( \(Core.Notch e) -> match
--                   { frequency: tmpResolveAU parent.scope di (setFrequency <<< { id: me, frequency: _ })
--                   , q: tmpResolveAU parent.scope di (setQ <<< { id: me, q: _ })
--                   }
--                   e
--               )
--               atts
--           )
--         <|> __internalWagsFlatten (just me) parent.scope di (mix elts)

-- notch_
--   :: forall i aud (outputChannels :: Type) lock payload
--    . Common.InitialNotch i
--   => Core.Mix aud (Core.Audible outputChannels lock payload)
--   => i
--   -> aud
--   -> Core.Node outputChannels lock payload
-- notch_ i a = notch i empty a

-- -- peaking
-- peaking
--   :: forall i aud (outputChannels :: Type) lock payload
--    . Common.InitialPeaking i
--   => Core.Mix aud (Core.Audible outputChannels lock payload)
--   => i
--   -> Event (Core.Peaking lock payload)
--   -> aud
--   -> Core.Node outputChannels lock payload
-- peaking i' atts elts = Core.Node go
--   where
--   Core.InitializePeaking i = Common.toInitializePeaking i'
--   go parent di@(Core.AudioInterpret { ids, deleteFromCache, makePeaking, setFrequency, setQ, setGain }) = makeEvent \k -> do
--     me <- ids
--     parent.raiseId me
--     map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
--       bang
--         ( makePeaking
--             { id: me, parent: parent.parent, scope: parent.scope, frequency: i.frequency, q: i.q, gain: i.gain }
--         )
--         <|>
--           ( keepLatest $ map
--               ( \(Core.Peaking e) -> match
--                   { frequency: tmpResolveAU parent.scope di (setFrequency <<< { id: me, frequency: _ })
--                   , q: tmpResolveAU parent.scope di (setQ <<< { id: me, q: _ })
--                   , gain: tmpResolveAU parent.scope di (setGain <<< { id: me, gain: _ })
--                   }
--                   e
--               )
--               atts
--           )
--         <|> __internalWagsFlatten (just me) parent.scope di (mix elts)

-- peaking_
--   :: forall i aud (outputChannels :: Type) lock payload
--    . Common.InitialPeaking i
--   => Core.Mix aud (Core.Audible outputChannels lock payload)
--   => i
--   -> aud
--   -> Core.Node outputChannels lock payload
-- peaking_ i a = peaking i empty a

-- -- periodicOsc

-- __periodicOsc
--   :: forall i outputChannels lock payload
--    . Common.InitialPeriodicOsc i
--   => i
--   -> Event (Core.PeriodicOsc lock payload)
--   -> Core.Node outputChannels lock payload
-- __periodicOsc i' atts = Core.Node go
--   where
--   Core.InitializePeriodicOsc i = Common.toInitializePeriodicOsc i'
--   go
--     parent
--     di@
--       ( Core.AudioInterpret
--           { ids, deleteFromCache, makePeriodicOsc, setFrequency, setOnOff, setPeriodicOsc }
--       ) =
--     makeEvent \k -> do
--       me <- ids
--       parent.raiseId me
--       map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
--         bang
--           ( makePeriodicOsc
--               { id: me
--               , parent: parent.parent
--               , scope: parent.scope
--               , frequency: i.frequency
--               , spec: i.spec
--               }
--           )
--           <|>
--             ( keepLatest $ map
--                 ( \(Core.PeriodicOsc e) -> match
--                     { frequency: tmpResolveAU parent.scope di (setFrequency <<< { id: me, frequency: _ })
--                     , onOff: \onOff -> bang $ setOnOff { id: me, onOff }
--                     , spec: \spec -> bang $ setPeriodicOsc { id: me, spec }
--                     }
--                     e
--                 )
--                 atts
--             )

-- periodicOsc
--   :: forall i outputChannels lock payload
--    . Common.InitialPeriodicOsc i
--   => i
--   -> Event (Core.PeriodicOsc lock payload)
--   -> Core.Node outputChannels lock payload
-- periodicOsc = __periodicOsc

-- periodicOsc_
--   :: forall i outputChannels lock payload
--    . Common.InitialPeriodicOsc i
--   => i
--   -> Core.Node outputChannels lock payload
-- periodicOsc_ i = periodicOsc i empty

-- -- playBuf

-- data PlayBufOptions = PlayBufOptions

-- instance
--   ConvertOption PlayBufOptions
--     "playbackRate"
--     Core.InitialAudioParameter
--     Core.InitialAudioParameter where
--   convertOption _ _ = identity

-- instance ConvertOption PlayBufOptions "duration" Number (Maybe Number) where
--   convertOption _ _ = just

-- instance ConvertOption PlayBufOptions "bufferOffset" Number Number where
--   convertOption _ _ = identity

-- instance
--   ConvertOption PlayBufOptions "buffer" BrowserAudioBuffer BrowserAudioBuffer where
--   convertOption _ _ = identity

-- type PlayBufOptional =
--   ( bufferOffset :: Number
--   , playbackRate :: Core.InitialAudioParameter
--   , duration :: Maybe Number
--   )

-- type PlayBufAll =
--   ( buffer :: BrowserAudioBuffer
--   | PlayBufOptional
--   )

-- defaultPlayBuf :: { | PlayBufOptional }
-- defaultPlayBuf =
--   { bufferOffset: 0.0
--   , playbackRate: 1.0
--   , duration: nothing
--   }

-- class InitialPlayBuf i where
--   toInitializePlayBuf :: i -> Core.InitializePlayBuf

-- instance InitialPlayBuf Core.InitializePlayBuf where
--   toInitializePlayBuf = identity

-- instance InitialPlayBuf BrowserAudioBuffer where
--   toInitializePlayBuf = toInitializePlayBuf <<< { buffer: _ }

-- instance
--   ConvertOptionsWithDefaults PlayBufOptions { | PlayBufOptional } { | provided }
--     { | PlayBufAll } =>
--   InitialPlayBuf { | provided } where
--   toInitializePlayBuf provided = Core.InitializePlayBuf
--     (convertOptionsWithDefaults PlayBufOptions defaultPlayBuf provided)

-- -- recorder
-- recorder
--   :: forall i outputChannels lock payload
--    . Common.InitialRecorder i
--   => i
--   -> Core.Node outputChannels lock payload
--   -> Core.Node outputChannels lock payload
-- recorder i' elt = Core.Node go
--   where
--   Core.InitializeRecorder i = Common.toInitializeRecorder i'
--   go parent di@(Core.AudioInterpret { ids, deleteFromCache, makeRecorder }) =
--     makeEvent \k -> do
--       me <- ids
--       parent.raiseId me
--       map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
--         bang
--           ( makeRecorder
--               { id: me, parent: parent.parent, scope: parent.scope, cb: i.cb }
--           )
--           <|> __internalWagsFlatten (just me) parent.scope di (mix elt)

-- -- sawtoothOsc

-- __sawtoothOsc
--   :: forall i outputChannels lock payload
--    . Common.InitialSawtoothOsc i
--   => i
--   -> Event (Core.SawtoothOsc lock payload)
--   -> Core.Node outputChannels lock payload
-- __sawtoothOsc i' atts = Core.Node go
--   where
--   Core.InitializeSawtoothOsc i = Common.toInitializeSawtoothOsc i'
--   go
--     parent
--     di@(Core.AudioInterpret { ids, deleteFromCache, makeSawtoothOsc, setFrequency, setOnOff }) =
--     makeEvent \k -> do
--       me <- ids
--       parent.raiseId me
--       map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
--         bang
--           ( makeSawtoothOsc
--               { id: me
--               , parent: parent.parent
--               , scope: parent.scope
--               , frequency: i.frequency
--               }
--           )
--           <|>
--             ( keepLatest $ map
--                 ( \(Core.SawtoothOsc e) -> match
--                     { frequency: tmpResolveAU parent.scope di (setFrequency <<< { id: me, frequency: _ })
--                     , onOff: \onOff -> bang $ setOnOff { id: me, onOff }
--                     }
--                     e
--                 )
--                 atts
--             )

-- sawtoothOsc
--   :: forall i outputChannels lock payload
--    . Common.InitialSawtoothOsc i
--   => i
--   -> Event (Core.SawtoothOsc lock payload)
--   -> Core.Node outputChannels lock payload
-- sawtoothOsc = __sawtoothOsc

-- sawtoothOsc_
--   :: forall i outputChannels lock payload
--    . Common.InitialSawtoothOsc i
--   => i
--   -> Core.Node outputChannels lock payload
-- sawtoothOsc_ i = sawtoothOsc i empty

-- -- sinOsc

-- __sinOsc
--   :: forall i outputChannels lock payload
--    . Common.InitialSinOsc i
--   => i
--   -> Event (Core.SinOsc lock payload)
--   -> Core.Node outputChannels lock payload
-- __sinOsc i' atts = Core.Node go
--   where
--   Core.InitializeSinOsc i = Common.toInitializeSinOsc i'
--   go
--     parent
--     di@(Core.AudioInterpret { ids, deleteFromCache, makeSinOsc, setFrequency, setOnOff }) =
--     makeEvent \k -> do
--       me <- ids
--       parent.raiseId me
--       map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
--         bang
--           ( makeSinOsc
--               { id: me
--               , parent: parent.parent
--               , scope: parent.scope
--               , frequency: i.frequency
--               }
--           )
--           <|>
--             ( keepLatest $ map
--                 ( \(Core.SinOsc e) -> match
--                     { frequency: tmpResolveAU parent.scope di (setFrequency <<< { id: me, frequency: _ })
--                     , onOff: \onOff -> bang $ setOnOff { id: me, onOff }
--                     }
--                     e
--                 )
--                 atts
--             )

-- sinOsc
--   :: forall i outputChannels lock payload
--    . Common.InitialSinOsc i
--   => i
--   -> Event (Core.SinOsc lock payload)
--   -> Core.Node outputChannels lock payload
-- sinOsc = __sinOsc

-- sinOsc_
--   :: forall i outputChannels lock payload
--    . Common.InitialSinOsc i
--   => i
--   -> Core.Node outputChannels lock payload
-- sinOsc_ a = sinOsc a empty

-- -- squareOsc

-- __squareOsc
--   :: forall i outputChannels lock payload
--    . Common.InitialSquareOsc i
--   => i
--   -> Event (Core.SquareOsc lock payload)
--   -> Core.Node outputChannels lock payload
-- __squareOsc i' atts = Core.Node go
--   where
--   Core.InitializeSquareOsc i = Common.toInitializeSquareOsc i'
--   go
--     parent
--     di@(Core.AudioInterpret { ids, deleteFromCache, makeSquareOsc, setFrequency, setOnOff }) =
--     makeEvent \k -> do
--       me <- ids
--       parent.raiseId me
--       map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
--         bang
--           ( makeSquareOsc
--               { id: me
--               , parent: parent.parent
--               , scope: parent.scope
--               , frequency: i.frequency
--               }
--           )
--           <|>
--             ( keepLatest $ map
--                 ( \(Core.SquareOsc e) -> match
--                     { frequency: tmpResolveAU parent.scope di (setFrequency <<< { id: me, frequency: _ })
--                     , onOff: \onOff -> bang $ setOnOff { id: me, onOff }
--                     }
--                     e
--                 )
--                 atts
--             )

-- squareOsc
--   :: forall i outputChannels lock payload
--    . Common.InitialSquareOsc i
--   => i
--   -> Event (Core.SquareOsc lock payload)
--   -> Core.Node outputChannels lock payload
-- squareOsc = __squareOsc

-- squareOsc_
--   :: forall i outputChannels lock payload
--    . Common.InitialSquareOsc i
--   => i
--   -> Core.Node outputChannels lock payload
-- squareOsc_ i = squareOsc i empty

-- speaker
speaker
  :: forall aud (outputChannels :: Type) lock payload
   . Core.Mix aud (Core.Audible outputChannels lock payload)
  => aud
  -> Core.AudioInterpret payload
  -> Event payload
speaker elts di@(Core.AudioInterpret { ids, makeSpeaker }) = makeEvent \k -> do
  id <- ids
  k (makeSpeaker { id })
  subscribe (__internalWagsFlatten (just id) "toplevel" di (mix elts)) k

speaker2
  :: forall aud lock payload
   . Core.Mix aud (Core.Audible D2 lock payload)
  => aud
  -> Core.AudioInterpret payload
  -> Event payload
speaker2 = speaker

-- -- pan
-- pan
--   :: forall i aud (outputChannels :: Type) lock payload
--    . Common.InitialStereoPanner i
--   => Core.Mix aud (Core.Audible outputChannels lock payload)
--   => i
--   -> Event (Core.StereoPanner lock payload)
--   -> aud
--   -> Core.Node outputChannels lock payload
-- pan i' atts elts = Core.Node go
--   where
--   Core.InitializeStereoPanner i = Common.toInitializeStereoPanner i'
--   go parent di@(Core.AudioInterpret { ids, deleteFromCache, makeStereoPanner, setPan }) = makeEvent \k -> do
--     me <- ids
--     parent.raiseId me
--     map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
--       bang
--         ( makeStereoPanner
--             { id: me, parent: parent.parent, scope: parent.scope, pan: i.pan }
--         )
--         <|>
--           ( keepLatest $ map
--               ( \(Core.StereoPanner e) -> match
--                   { pan: tmpResolveAU parent.scope di (setPan <<< { id: me, pan: _ })
--                   }
--                   e
--               )
--               atts
--           )
--         <|> __internalWagsFlatten (just me) parent.scope di (mix elts)

-- pan_
--   :: forall i aud (outputChannels :: Type) lock payload
--    . Common.InitialStereoPanner i
--   => Core.Mix aud (Core.Audible outputChannels lock payload)
--   => i
--   -> aud
--   -> Core.Node outputChannels lock payload
-- pan_ i = pan i empty

-- -- triangleOsc

-- __triangleOsc
--   :: forall i outputChannels lock payload
--    . Common.InitialTriangleOsc i
--   => i
--   -> Event (Core.TriangleOsc lock payload)
--   -> Core.Node outputChannels lock payload
-- __triangleOsc i' atts = Core.Node go
--   where
--   Core.InitializeTriangleOsc i = Common.toInitializeTriangleOsc i'
--   go
--     parent
--     di@(Core.AudioInterpret { ids, deleteFromCache, makeTriangleOsc, setFrequency, setOnOff }) =
--     makeEvent \k -> do
--       me <- ids
--       parent.raiseId me
--       map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
--         bang
--           ( makeTriangleOsc
--               { id: me
--               , parent: parent.parent
--               , scope: parent.scope
--               , frequency: i.frequency
--               }
--           )
--           <|>
--             ( keepLatest $ map
--                 ( \(Core.TriangleOsc e) -> match
--                     { frequency: tmpResolveAU parent.scope di (setFrequency <<< { id: me, frequency: _ })
--                     , onOff: \onOff -> bang $ setOnOff { id: me, onOff }
--                     }
--                     e
--                 )
--                 atts
--             )

-- triangleOsc
--   :: forall i outputChannels lock payload
--    . Common.InitialTriangleOsc i
--   => i
--   -> Event (Core.TriangleOsc lock payload)
--   -> Core.Node outputChannels lock payload
-- triangleOsc = __triangleOsc

-- triangleOsc_
--   :: forall i outputChannels lock payload
--    . Common.InitialTriangleOsc i
--   => i
--   -> Core.Node outputChannels lock payload
-- triangleOsc_ i = triangleOsc i empty

-- -- waveShaper

-- waveShaper
--   :: forall i aud (outputChannels :: Type) lock payload
--    . Common.InitialWaveShaper i
--   => Core.Mix aud (Core.Audible outputChannels lock payload)
--   => i
--   -> aud
--   -> Core.Node outputChannels lock payload
-- waveShaper i' elts = Core.Node go
--   where
--   Core.InitializeWaveShaper i = Common.toInitializeWaveShaper i'
--   go parent di@(Core.AudioInterpret { ids, deleteFromCache, makeWaveShaper }) =
--     makeEvent \k -> do
--       me <- ids
--       parent.raiseId me
--       map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
--         bang
--           ( makeWaveShaper
--               { id: me
--               , parent: parent.parent
--               , scope: parent.scope
--               , curve: i.curve
--               , oversample: i.oversample
--               }
--           ) <|> __internalWagsFlatten (just me) parent.scope di (mix elts)

-- ----------

-- newtype MutAr a = MutAr (Array a)

-- foreign import mutAr :: forall a. Array a -> Effect (MutAr a)
-- foreign import unsafeUpdateMutAr :: forall a. Int -> a -> MutAr a -> Effect Unit
-- foreign import readAr :: forall a. MutAr a -> Effect (Array a)

-- -- todo: this is almost literally a copy-paste now of Deku
-- -- are the libraries close enough now where we can merge the two functions into one?
-- -- the only differences I can see are:
-- -- - the deconstruction and reconstruction of Node
-- -- - the name of the connection function
-- internalFan
--   :: forall n outputChannels lock0 lock1 payload
--    . Boolean
--   -> (String -> String)
--   -> Vec n (Core.Node outputChannels lock0 payload)
--   -> ( Vec n (Core.Node outputChannels lock1 payload)
--        -> (Core.Node outputChannels lock0 payload -> Core.Node outputChannels lock1 payload)
--        -> Core.Audible outputChannels lock1 payload
--      )
--   -> Core.Node outputChannels lock0 payload
-- internalFan isGlobal scopeF gaga closure = Core.Node go
--   where
--   go psr di@(Core.AudioInterpret { deleteFromCache }) = makeEvent \k -> do
--     av <- mutAr (map (const "") $ toArray gaga)
--     let
--       actualized = oneOf $ mapWithIndex
--         ( \ix (Core.Node gogo) ->
--             gogo
--               { parent: just "@fan@"
--               , scope: scopeF psr.scope
--               , raiseId: \id -> unsafeUpdateMutAr ix id av
--               }
--               di
--         )
--         gaga
--     u0 <- subscribe actualized k
--     av2 <- AVar.empty
--     let
--       asIds :: Array String -> Vec n String
--       asIds = unsafeCoerce
--     idz <- asIds <$> readAr av
--     let
--       -- we never connect or disconnect the referentially opaque node
--       -- instead, it is always managed inside a referentially transparent node
--       -- that can be properly connected and disconnected
--       injectable = map
--         ( \id -> Core.Node
--             \{ parent, raiseId } (Core.AudioInterpret { connectXToY }) ->
--               makeEvent \k2 -> do
--                 raiseId id
--                 for_ parent \p' ->
--                   k2 (connectXToY { from: id, to: p' })
--                 pure (pure unit)
--         )
--         idz
--       realized = __internalWagsFlatten psr.parent psr.scope di
--         ((unsafeCoerce :: Core.Audible _ _ _ -> Core.Audible _ _ _) (closure injectable (\(Core.Node q) -> Core.Node q)))
--     u <- subscribe realized k
--     void $ tryPut u av2
--     -- cancel immediately, as it should be run synchronously
--     -- so if this actually does something then we have a problem
--     pure do
--       u0
--       when (not isGlobal) $ foreachE (toArray idz) \id -> k
--         (deleteFromCache { id })
--       cncl2 <- AVar.take av2 \q -> case q of
--         Right usu -> usu
--         Left e -> throwException e
--       -- cancel immediately, as it should be run synchronously
--       -- so if this actually does something then we have a problem
--       cncl2

-- globalFan
--   :: forall n outputChannels lock payload
--    . Vec n (Core.Node outputChannels lock payload)
--   -> (Vec n (Core.Node outputChannels lock payload) -> Core.Audible outputChannels lock payload)
--   -> Core.Node outputChannels lock payload
-- globalFan e f = internalFan true (const "@fan@") e (\x _ -> f x)

-- globalFan1
--   :: forall outputChannels lock payload
--    . Core.Node outputChannels lock payload
--   -> (Core.Node outputChannels lock payload -> Core.Audible outputChannels lock payload)
--   -> Core.Node outputChannels lock payload
-- globalFan1 e f = globalFan (singleton e) (lcmap (flip index d0) f)

-- fan
--   :: forall n outputChannels lock0 payload
--    . Vec n (Core.Node outputChannels lock0 payload)
--   -> ( forall lock1
--         . Vec n (Core.Node outputChannels lock1 payload)
--        -> (Core.Node outputChannels lock0 payload -> Core.Node outputChannels lock1 payload)
--        -> Core.Audible outputChannels lock1 payload
--      )
--   -> Core.Node outputChannels lock0 payload
-- fan e = internalFan false identity e

-- fan1
--   :: forall outputChannels lock0 payload
--    . Core.Node outputChannels lock0 payload
--   -> ( forall lock1
--         . Core.Node outputChannels lock1 payload
--        -> (Core.Node outputChannels lock0 payload -> Core.Node outputChannels lock1 payload)
--        -> Core.Audible outputChannels lock1 payload
--      )
--   -> Core.Node outputChannels lock0 payload
-- fan1 e f = fan (singleton e) (lcmap (flip index d0) f)

-- ---- fix
-- fix
--   :: forall outputChannels lock payload
--    . (Core.Node outputChannels lock payload -> Core.Node outputChannels lock payload)
--   -> Core.Node outputChannels lock payload
-- fix f = Core.Node go
--   where
--   go i di@(Core.AudioInterpret { connectXToY }) = makeEvent \k -> do
--     av <- AVar.empty
--     let
--       Core.Node nn = f $ Core.Node \ii _ -> makeEvent \k -> do
--         void $ AVar.read av case _ of
--           Left e -> throwException e
--           -- only do the connection if not silence
--           Right r -> for_ ii.parent \p' ->
--             when (r /= p') (ii.raiseId r *> k (connectXToY { from: r, to: p' }))
--         pure (pure unit)
--     subscribe
--       ( nn
--           { parent: i.parent
--           , scope: i.scope
--           , raiseId: \s -> do
--               i.raiseId s
--               void $ tryPut s av
--           }
--           di
--       )
--       k

-- silence
--   :: forall outputChannels lock payload
--    . Core.Node outputChannels lock payload
-- silence = fix identity

-- -----
-- -- starts work on merge
-- -- merge
-- --   :: forall i n lock payload
-- --    . IsEvent event
-- --   => Pos n
-- --   => Vec n (Core.Node D1 lock payload)
-- --   -> Core.Node n lock payload
-- -- merge elts = Core.Node go
-- --   where
-- --   go
-- --     parent
-- --     di@
-- --       ( Core.AudioInterpret
-- --           { ids
-- --           , scope
-- --           , makeMerger
-- --           }
-- --       ) =
-- --     keepLatest
-- --       ( (sample_ ids (bang unit)) <#> \me ->
-- --           bang
-- --             ( makeMerger
-- --                 { id: me
-- --                 , parent: parent.parent
-- --                 , scope: parent.scope
-- --                 }
-- --             )
-- --             <|> oneOf
-- --               ( ( mapWithIndex
-- --                     -- parent needs to accept an ix for this to work
-- --                     ( \ix (id /\ elt) -> (((\y -> let Core.Node x = y in x) elt) (Parent me ix) di)
-- --                     )
-- --                     elts
-- --                 )
-- --               )
-- --       )

-- -- TODO
-- -- this function is copied between two files
-- -- with the sole difference that this version wraps its argument in a gain node
-- -- the reason for this is that, otherwise, we'd have to write additional machinery
-- -- for all generators (ie sine wave oscillators) to listen to when they turn on and off and reconnect fresh generators whenever something turns on again
-- -- by doing it this way, all generators go to a gain node, so we can use code we've already written
-- -- the downside is that we have an extra gain node for every audio parameter
-- -- which can add up
-- -- so we definitely want to delete this and use Common.resolveAU
-- -- as soon as we can correctly attach and detach generators
-- tmpResolveAU :: forall lock payload. String -> Core.AudioInterpret payload -> (Core.FFIAudioParameter -> payload) -> Core.AudioParameter lock payload -> Event payload
-- tmpResolveAU = go
--   where
--   cncl = Core.FFIAudioParameter <<< inj (Proxy :: _ "cancel")
--   ev = Core.FFIAudioParameter <<< inj (Proxy :: _ "envelope")
--   nmc = Core.FFIAudioParameter <<< inj (Proxy :: _ "numeric")
--   sdn = Core.FFIAudioParameter <<< inj (Proxy :: _ "sudden")
--   ut = Core.FFIAudioParameter <<< inj (Proxy :: _ "unit")
--   go scope di f (Core.AudioParameter a) = match
--     { numeric: bang <<< f <<< nmc
--     , envelope: bang <<< f <<< ev
--     , cancel: bang <<< f <<< cncl
--     , sudden: bang <<< f <<< sdn
--     , unit: \(Core.AudioUnit { u }) ->
--         let
--           Core.Node n = gain_ 1.0 u
--         in
--           makeEvent \k -> do
--             av <- AVar.empty
--             subscribe
--               ( n { parent: nothing, scope: scope, raiseId: \x -> void $ AVar.tryPut x av } di <|> makeEvent \k2 -> do
--                   void $ AVar.take av case _ of
--                     Left e -> throwException e
--                     -- only do the connection if not silence
--                     Right i -> k2 (f (ut (Core.FFIAudioUnit { i })))
--                   pure (pure unit)
--               )
--               k
--     }
--     a
