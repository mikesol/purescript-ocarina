module WAGS.Control where

import Prelude

import Control.Alt ((<|>))
import Control.Comonad (extract)
import Data.Array.NonEmpty (fromNonEmpty)
import Data.Array.NonEmpty as NEA
import Data.Foldable (oneOf)
import Data.Homogeneous (class HomogeneousRowLabels)
import Data.Homogeneous.Variant (homogeneous)
import Data.NonEmpty ((:|))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Typelevel.Num (class Nat, class Pos, class Pred, D1, D2, pred, toInt)
import Data.Variant (Unvariant(..), match, unvariant)
import WAGS.Core (AudioParameter, AudioWorkletNodeOptions_(..), InitialAudioParameter)
import WAGS.Core as C
import FRP.Behavior (sample_)
import FRP.Event (class IsEvent, keepLatest)
import Foreign.Object (fromHomogeneous)
import Prim.Row (class Cons, class Nub, class Union)
import Safe.Coerce (coerce)
import Simple.JSON as JSON
import Type.Proxy (Proxy(..))
import Type.Row.Homogeneous (class Homogeneous)

-- gain input
singleton
  :: forall outputChannels produced consumed event payload
   . IsEvent event
  => C.Node outputChannels produced consumed event payload
  -> C.GainInput outputChannels produced consumed event payload
singleton a = C.GainInput (NEA.singleton a)

gainInputCons
  :: forall outputChannels produced0 produced1 produced2 consumed0 consumed1
       consumed2 event
       payload
   . IsEvent event
  => Union produced0 produced1 produced2
  => Union consumed0 consumed1 consumed2
  => C.Node outputChannels produced0 consumed0 event payload
  -> Array (C.Node outputChannels produced1 consumed1 event payload)
  -> C.GainInput outputChannels produced2 consumed2 event payload
gainInputCons a b = C.GainInput (fromNonEmpty (coerce a :| coerce b))

infixr 6 gainInputCons as :*

gainInputCons2
  :: forall outputChannels produced0 produced1 produced2 consumed0 consumed1
       consumed2 event
       payload
   . IsEvent event
  => Union produced0 produced1 produced2
  => Union consumed0 consumed1 consumed2
  => C.Node outputChannels produced0 consumed0 event payload
  -> C.GainInput outputChannels produced1 consumed1 event payload
  -> C.GainInput outputChannels produced2 consumed2 event payload
gainInputCons2 a b = C.GainInput (NEA.cons (coerce a) (coerce b))

infixr 6 gainInputCons2 as ::*

gainInputAppend
  :: forall outputChannels produced0 produced1 produced2 consumed0 consumed1
       consumed2 event
       payload
   . IsEvent event
  => Union produced0 produced1 produced2
  => Union consumed0 consumed1 consumed2
  => C.GainInput outputChannels produced0 consumed0 event payload
  -> C.GainInput outputChannels produced1 consumed1 event payload
  -> C.GainInput outputChannels produced2 consumed2 event payload
gainInputAppend a b = C.GainInput (coerce a <> coerce b)

infixr 6 gainInputAppend as <>*

-- allpass

allpass
  :: forall outputChannels produced consumed event payload
   . IsEvent event
  => C.InitializeAllpass
  -> event C.Allpass
  -> C.Node outputChannels produced consumed event payload
  -> C.Node outputChannels produced consumed event payload
allpass i atts elt = C.Node go
  where
  go parent di@(C.AudioInterpret { ids, makeAllpass, setFrequency, setQ }) =
    keepLatest
      ( (sample_ ids (pure unit)) <#> \me ->
          ( pure
              ( makeAllpass
                  { id: me, parent: parent, frequency: i.frequency, q: i.q }
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

allpass'
  :: forall proxy sym outputChannels produced produced' consumed event payload
   . IsEvent event
  => Cons sym C.Input produced' produced
  => proxy sym
  -> C.InitializeAllpass
  -> event C.Allpass
  -> C.Node outputChannels produced' consumed event payload
  -> C.Node outputChannels produced consumed event payload
allpass' _ i atts elts = let C.Node n = allpass i atts elts in C.Node n

-- analyser
analyser
  :: forall outputChannels produced consumed event payload
   . IsEvent event
  => C.InitializeAnalyser
  -> event C.Analyser
  -> C.Node outputChannels produced consumed event payload
  -> C.Node outputChannels produced consumed event payload
analyser i atts elt = C.Node go
  where
  go parent di@(C.AudioInterpret { ids, makeAnalyser, setAnalyserNodeCb }) =
    keepLatest
      ( (sample_ ids (pure unit)) <#> \me ->
          pure (makeAnalyser { id: me, parent: parent, cb: i.cb })
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
audioWorklet i atts elt = C.Node go
  where
  go
    parent
    di@
      (C.AudioInterpret { ids, makeAudioWorkletNode, setAudioWorkletParameter }) =
    keepLatest
      ( (sample_ ids (pure unit)) <#> \me ->
          pure
            ( makeAudioWorkletNode
                { id: me
                , parent: parent
                , options:
                    AudioWorkletNodeOptions_
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

audioWorklet'
  :: forall proxy sym name numberOfInputs numberOfOutputs outputChannelCount
       parameterData parameterDataRL processorOptions produced consumed event payload
   . IsSymbol name
  => Nat numberOfInputs
  => Pos numberOfOutputs
  => ValidateOutputChannelCount numberOfOutputs outputChannelCount
  => Homogeneous parameterData InitialAudioParameter
  => HomogeneousRowLabels parameterData AudioParameter parameterDataRL
  => JSON.WriteForeign { | processorOptions }
  => IsEvent event
  => IsSymbol sym
  => proxy sym
  -> C.InitializeAudioWorkletNode name numberOfInputs numberOfOutputs
       outputChannelCount
       parameterData
       processorOptions
  -> event (C.AudioWorkletNode parameterData)
  -> C.Node numberOfOutputs produced consumed event payload
  -> C.Node numberOfOutputs produced consumed event payload
audioWorklet' _ i atts elts =
  let C.Node n = audioWorklet i atts elts in C.Node n

-- bandpass

bandpass
  :: forall outputChannels produced consumed event payload
   . IsEvent event
  => C.InitializeBandpass
  -> event C.Bandpass
  -> C.Node outputChannels produced consumed event payload
  -> C.Node outputChannels produced consumed event payload
bandpass i atts elt = C.Node go
  where
  go parent di@(C.AudioInterpret { ids, makeBandpass, setFrequency, setQ }) =
    keepLatest
      ( (sample_ ids (pure unit)) <#> \me ->
          pure
            ( makeBandpass
                { id: me, parent: parent, frequency: i.frequency, q: i.q }
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

bandpass'
  :: forall proxy sym outputChannels produced produced' consumed event payload
   . IsEvent event
  => Cons sym C.Input produced' produced
  => proxy sym
  -> C.InitializeBandpass
  -> event C.Bandpass
  -> C.Node outputChannels produced' consumed event payload
  -> C.Node outputChannels produced consumed event payload
bandpass' _ i atts elts = let C.Node n = bandpass i atts elts in C.Node n

-- constant

constant
  :: forall outputChannels event payload
   . IsEvent event
  => C.InitializeConstant
  -> event C.Constant
  -> C.Node outputChannels () () event payload
constant i atts = C.Node go
  where
  go parent (C.AudioInterpret { ids, makeConstant, setOffset, setOnOff }) =
    keepLatest
      ( (sample_ ids (pure unit)) <#> \me ->
          pure
            ( makeConstant
                { id: me
                , parent: parent
                , offset: i.offset
                , onOff: i.onOff
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

constant'
  :: forall proxy sym outputChannels produced event payload
   . IsEvent event
  => Cons sym C.Input () produced
  => proxy sym
  -> C.InitializeConstant
  -> event C.Constant
  -> C.Node outputChannels produced () event payload
constant' _ i atts = let C.Node n = constant i atts in C.Node n

-- convolver

convolver
  :: forall outputChannels event payload
   . IsEvent event
  => C.InitializeConvolver
  -> C.Node outputChannels () () event payload
convolver i = C.Node go
  where
  go parent (C.AudioInterpret { ids, makeConvolver }) =
    keepLatest
      ( (sample_ ids (pure unit)) <#> \me ->
          pure
            ( makeConvolver
                { id: me
                , parent: parent
                , buffer: i.buffer
                }
            )
      )

convolver'
  :: forall proxy sym outputChannels produced event payload
   . IsEvent event
  => Cons sym C.Input () produced
  => proxy sym
  -> C.InitializeConvolver
  -> C.Node outputChannels produced () event payload
convolver' _ i = let C.Node n = convolver i in C.Node n

-- delay

delay
  :: forall outputChannels produced consumed event payload
   . IsEvent event
  => C.InitializeDelay
  -> event C.Delay
  -> C.Node outputChannels produced consumed event payload
  -> C.Node outputChannels produced consumed event payload
delay i atts elt = C.Node go
  where
  go parent di@(C.AudioInterpret { ids, makeDelay, setDelay }) =
    keepLatest
      ( (sample_ ids (pure unit)) <#> \me ->
          pure
            ( makeDelay
                { id: me, parent: parent, delayTime: i.delayTime }
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

delay'
  :: forall proxy sym outputChannels produced produced' consumed event payload
   . IsEvent event
  => Cons sym C.Input produced' produced
  => proxy sym
  -> C.InitializeDelay
  -> event C.Delay
  -> C.Node outputChannels produced' consumed event payload
  -> C.Node outputChannels produced consumed event payload
delay' _ i atts elts = let C.Node n = delay i atts elts in C.Node n

-- dynamics compressor

dynamicsCompressor
  :: forall outputChannels produced consumed event payload
   . IsEvent event
  => C.InitializeDynamicsCompressor
  -> event C.DynamicsCompressor
  -> C.Node outputChannels produced consumed event payload
  -> C.Node outputChannels produced consumed event payload
dynamicsCompressor i atts elt = C.Node go
  where
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
    keepLatest
      ( (sample_ ids (pure unit)) <#> \me ->
          pure
            ( makeDynamicsCompressor
                { id: me
                , parent: parent
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

dynamicsCompressor'
  :: forall proxy sym outputChannels produced produced' consumed event payload
   . IsEvent event
  => Cons sym C.Input produced' produced
  => proxy sym
  -> C.InitializeDynamicsCompressor
  -> event C.DynamicsCompressor
  -> C.Node outputChannels produced' consumed event payload
  -> C.Node outputChannels produced consumed event payload
dynamicsCompressor' _ i atts elts =
  let C.Node n = dynamicsCompressor i atts elts in C.Node n

-- gain

gain_
  :: forall outputChannels produced consumed event payload
   . IsEvent event
  => C.InitializeGain
  -> event C.Gain
  -> C.Node outputChannels produced consumed event payload
  -> Array (C.Node outputChannels produced consumed event payload)
  -> C.Node outputChannels produced consumed event payload
gain_ i atts h t = gain i atts (C.GainInput (NEA.fromNonEmpty (h :| t)))

gain
  :: forall outputChannels produced consumed event payload
   . IsEvent event
  => C.InitializeGain
  -> event C.Gain
  -> C.GainInput outputChannels produced consumed event payload
  -> C.Node outputChannels produced consumed event payload
gain i atts (C.GainInput elts) = C.Node go
  where
  go parent di@(C.AudioInterpret { ids, makeGain, setGain }) = keepLatest
    ( (sample_ ids (pure unit)) <#> \me ->
        pure (makeGain { id: me, parent: parent, gain: i.gain })
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

gain'
  :: forall proxy sym outputChannels produced produced' consumed event payload
   . IsEvent event
  => Cons sym C.Input produced' produced
  => proxy sym
  -> C.InitializeGain
  -> event C.Gain
  -> C.GainInput outputChannels produced' consumed event payload
  -> C.Node outputChannels produced consumed event payload
gain' _ i atts elts = let C.Node n = gain i atts elts in C.Node n

-- highpass

highpass
  :: forall outputChannels produced consumed event payload
   . IsEvent event
  => C.InitializeHighpass
  -> event C.Highpass
  -> C.Node outputChannels produced consumed event payload
  -> C.Node outputChannels produced consumed event payload
highpass i atts elt = C.Node go
  where
  go parent di@(C.AudioInterpret { ids, makeHighpass, setFrequency, setQ }) =
    keepLatest
      ( (sample_ ids (pure unit)) <#> \me ->
          pure
            ( makeHighpass
                { id: me, parent: parent, frequency: i.frequency, q: i.q }
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

highpass'
  :: forall proxy sym outputChannels produced produced' consumed event payload
   . IsEvent event
  => Cons sym C.Input produced' produced
  => proxy sym
  -> C.InitializeHighpass
  -> event C.Highpass
  -> C.Node outputChannels produced' consumed event payload
  -> C.Node outputChannels produced consumed event payload
highpass' _ i atts elts = let C.Node n = highpass i atts elts in C.Node n

-- highshelf

highshelf
  :: forall outputChannels produced consumed event payload
   . IsEvent event
  => C.InitializeHighshelf
  -> event C.Highshelf
  -> C.Node outputChannels produced consumed event payload
  -> C.Node outputChannels produced consumed event payload
highshelf i atts elt = C.Node go
  where
  go parent di@(C.AudioInterpret { ids, makeHighshelf, setFrequency, setGain }) =
    keepLatest
      ( (sample_ ids (pure unit)) <#> \me ->
          pure
            ( makeHighshelf
                { id: me, parent: parent, frequency: i.frequency, gain: i.gain }
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

highshelf'
  :: forall proxy sym outputChannels produced produced' consumed event payload
   . IsEvent event
  => Cons sym C.Input produced' produced
  => proxy sym
  -> C.InitializeHighshelf
  -> event C.Highshelf
  -> C.Node outputChannels produced' consumed event payload
  -> C.Node outputChannels produced consumed event payload
highshelf' _ i atts elts = let C.Node n = highshelf i atts elts in C.Node n

-- input

input
  :: forall outputChannels produced consumed event payload
   . IsEvent event
  => C.Input
  -> C.Node outputChannels produced consumed event payload
input (C.Input me) = C.Node go
  where
  go parent (C.AudioInterpret { makeInput }) = pure
    ( makeInput
        { id: me, parent: parent }
    )

-- lowpass

lowpass
  :: forall outputChannels produced consumed event payload
   . IsEvent event
  => C.InitializeLowpass
  -> event C.Lowpass
  -> C.Node outputChannels produced consumed event payload
  -> C.Node outputChannels produced consumed event payload
lowpass i atts elt = C.Node go
  where
  go parent di@(C.AudioInterpret { ids, makeLowpass, setFrequency, setQ }) =
    keepLatest
      ( (sample_ ids (pure unit)) <#> \me ->
          pure
            ( makeLowpass
                { id: me, parent: parent, frequency: i.frequency, q: i.q }
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

lowpass'
  :: forall proxy sym outputChannels produced produced' consumed event payload
   . IsEvent event
  => Cons sym C.Input produced' produced
  => proxy sym
  -> C.InitializeLowpass
  -> event C.Lowpass
  -> C.Node outputChannels produced' consumed event payload
  -> C.Node outputChannels produced consumed event payload
lowpass' _ i atts elts = let C.Node n = lowpass i atts elts in C.Node n

-- lowshelf

lowshelf
  :: forall outputChannels produced consumed event payload
   . IsEvent event
  => C.InitializeLowshelf
  -> event C.Lowshelf
  -> C.Node outputChannels produced consumed event payload
  -> C.Node outputChannels produced consumed event payload
lowshelf i atts elt = C.Node go
  where
  go parent di@(C.AudioInterpret { ids, makeLowshelf, setFrequency, setGain }) =
    keepLatest
      ( (sample_ ids (pure unit)) <#> \me ->
          pure
            ( makeLowshelf
                { id: me, parent: parent, frequency: i.frequency, gain: i.gain }
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

lowshelf'
  :: forall proxy sym outputChannels produced produced' consumed event payload
   . IsEvent event
  => Cons sym C.Input produced' produced
  => proxy sym
  -> C.InitializeLowshelf
  -> event C.Lowshelf
  -> C.Node outputChannels produced' consumed event payload
  -> C.Node outputChannels produced consumed event payload
lowshelf' _ i atts elts = let C.Node n = lowshelf i atts elts in C.Node n

-- loopBuf

loopBuf
  :: forall outputChannels event payload
   . IsEvent event
  => C.InitializeLoopBuf
  -> event C.LoopBuf
  -> C.Node outputChannels () () event payload
loopBuf i atts = C.Node go
  where
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
    keepLatest
      ( (sample_ ids (pure unit)) <#> \me ->
          pure
            ( makeLoopBuf
                { id: me
                , parent: parent
                , buffer: i.buffer
                , onOff: i.onOff
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

loopBuf'
  :: forall proxy sym outputChannels produced event payload
   . IsEvent event
  => Cons sym C.Input () produced
  => proxy sym
  -> C.InitializeLoopBuf
  -> event C.LoopBuf
  -> C.Node outputChannels produced () event payload
loopBuf' _ i atts = let C.Node n = loopBuf i atts in C.Node n

-- mediaElement

mediaElement
  :: forall outputChannels event payload
   . IsEvent event
  => C.InitializeMediaElement
  -> C.Node outputChannels () () event payload
mediaElement i = C.Node go
  where
  go parent (C.AudioInterpret { ids, makeMediaElement }) =
    keepLatest
      ( (sample_ ids (pure unit)) <#> \me ->
          pure
            ( makeMediaElement
                { id: me
                , parent: parent
                , element: i.element
                }
            )
      )

mediaElement'
  :: forall proxy sym outputChannels produced event payload
   . IsEvent event
  => Cons sym C.Input () produced
  => proxy sym
  -> C.InitializeMediaElement
  -> C.Node outputChannels produced () event payload
mediaElement' _ i = let C.Node n = mediaElement i in C.Node n

-- microphone

microphone
  :: forall outputChannels event payload
   . IsEvent event
  => C.InitializeMicrophone
  -> C.Node outputChannels () () event payload
microphone i = C.Node go
  where
  go parent (C.AudioInterpret { ids, makeMicrophone }) =
    keepLatest
      ( (sample_ ids (pure unit)) <#> \me ->
          pure
            ( makeMicrophone
                { id: me
                , parent: parent
                , microphone: i.microphone
                }
            )
      )

microphone'
  :: forall proxy sym outputChannels produced event payload
   . IsEvent event
  => Cons sym C.Input () produced
  => proxy sym
  -> C.InitializeMicrophone
  -> C.Node outputChannels produced () event payload
microphone' _ i = let C.Node n = microphone i in C.Node n

-- notch

notch
  :: forall outputChannels produced consumed event payload
   . IsEvent event
  => C.InitializeNotch
  -> event C.Notch
  -> C.Node outputChannels produced consumed event payload
  -> C.Node outputChannels produced consumed event payload
notch i atts elt = C.Node go
  where
  go parent di@(C.AudioInterpret { ids, makeNotch, setFrequency, setQ }) =
    keepLatest
      ( (sample_ ids (pure unit)) <#> \me ->
          pure
            ( makeNotch
                { id: me, parent: parent, frequency: i.frequency, q: i.q }
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

notch'
  :: forall proxy sym outputChannels produced produced' consumed event payload
   . IsEvent event
  => Cons sym C.Input produced' produced
  => proxy sym
  -> C.InitializeNotch
  -> event C.Notch
  -> C.Node outputChannels produced' consumed event payload
  -> C.Node outputChannels produced consumed event payload
notch' _ i atts elts = let C.Node n = notch i atts elts in C.Node n

-- peaking

peaking
  :: forall outputChannels produced consumed event payload
   . IsEvent event
  => C.InitializePeaking
  -> event C.Peaking
  -> C.Node outputChannels produced consumed event payload
  -> C.Node outputChannels produced consumed event payload
peaking i atts elt = C.Node go
  where
  go
    parent
    di@(C.AudioInterpret { ids, makePeaking, setFrequency, setQ, setGain }) =
    keepLatest
      ( (sample_ ids (pure unit)) <#> \me ->
          pure
            ( makePeaking
                { id: me
                , parent: parent
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

peaking'
  :: forall proxy sym outputChannels produced produced' consumed event payload
   . IsEvent event
  => Cons sym C.Input produced' produced
  => proxy sym
  -> C.InitializePeaking
  -> event C.Peaking
  -> C.Node outputChannels produced' consumed event payload
  -> C.Node outputChannels produced consumed event payload
peaking' _ i atts elts = let C.Node n = peaking i atts elts in C.Node n

-- periodicOsc

periodicOsc
  :: forall outputChannels event payload
   . IsEvent event
  => C.InitializePeriodicOsc
  -> event C.PeriodicOsc
  -> C.Node outputChannels () () event payload
periodicOsc i atts = C.Node go
  where
  go parent (C.AudioInterpret { ids, makePeriodicOsc, setFrequency, setOnOff, setPeriodicOsc }) =
    keepLatest
      ( (sample_ ids (pure unit)) <#> \me ->
          pure
            ( makePeriodicOsc
                { id: me
                , parent: parent
                , frequency: i.frequency
                , onOff: i.onOff
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

periodicOsc'
  :: forall proxy sym outputChannels produced event payload
   . IsEvent event
  => Cons sym C.Input () produced
  => proxy sym
  -> C.InitializePeriodicOsc
  -> event C.PeriodicOsc
  -> C.Node outputChannels produced () event payload
periodicOsc' _ i atts = let C.Node n = periodicOsc i atts in C.Node n

-- playBuf

playBuf
  :: forall outputChannels event payload
   . IsEvent event
  => C.InitializePlayBuf
  -> event C.PlayBuf
  -> C.Node outputChannels () () event payload
playBuf i atts = C.Node go
  where
  go
    parent
    ( C.AudioInterpret
        { ids
        , makePlayBuf
        , setBuffer
        , setOnOff
        , setPlaybackRate
        , setBufferOffset
        }
    ) =
    keepLatest
      ( (sample_ ids (pure unit)) <#> \me ->
          pure
            ( makePlayBuf
                { id: me
                , parent: parent
                , buffer: i.buffer
                , onOff: i.onOff
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
                  , bufferOffset: \bufferOffset -> setBufferOffset { id: me, bufferOffset }
                  , onOff: \onOff -> setOnOff { id: me, onOff }
                  }
                  e
              )
              atts
      )

playBuf'
  :: forall proxy sym outputChannels produced event payload
   . IsEvent event
  => Cons sym C.Input () produced
  => proxy sym
  -> C.InitializePlayBuf
  -> event C.PlayBuf
  -> C.Node outputChannels produced () event payload
playBuf' _ i atts = let C.Node n = playBuf i atts in C.Node n

-- recorder
recorder
  :: forall outputChannels produced consumed event payload
   . IsEvent event
  => C.InitializeRecorder
  -> C.Node outputChannels produced consumed event payload
  -> C.Node outputChannels produced consumed event payload
recorder i elt = C.Node go
  where
  go parent di@(C.AudioInterpret { ids, makeRecorder }) =
    keepLatest
      ( (sample_ ids (pure unit)) <#> \me ->
          pure (makeRecorder { id: me, parent: parent, cb: i.cb })
            <|> ((\y -> let C.Node x = y in x) elt) me di

      )
-- ref

ref
  :: forall proxy sym outputChannels consumed event payload
   . IsEvent event
  => IsSymbol sym
  => Cons sym C.Input () consumed
  => proxy sym
  -> C.Node outputChannels () consumed event payload
ref px = C.Node go
  where
  go parent (C.AudioInterpret { connectXToY }) =
    pure (connectXToY { from: reflectSymbol px, to: parent })

-- sawtoothOsc

sawtoothOsc
  :: forall outputChannels event payload
   . IsEvent event
  => C.InitializeSawtoothOsc
  -> event C.SawtoothOsc
  -> C.Node outputChannels () () event payload
sawtoothOsc i atts = C.Node go
  where
  go parent (C.AudioInterpret { ids, makeSawtoothOsc, setFrequency, setOnOff }) =
    keepLatest
      ( (sample_ ids (pure unit)) <#> \me ->
          pure
            ( makeSawtoothOsc
                { id: me
                , parent: parent
                , frequency: i.frequency
                , onOff: i.onOff
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

sawtoothOsc'
  :: forall proxy sym outputChannels produced event payload
   . IsEvent event
  => Cons sym C.Input () produced
  => proxy sym
  -> C.InitializeSawtoothOsc
  -> event C.SawtoothOsc
  -> C.Node outputChannels produced () event payload
sawtoothOsc' _ i atts = let C.Node n = sawtoothOsc i atts in C.Node n

-- sinOsc

sinOsc
  :: forall outputChannels event payload
   . IsEvent event
  => C.InitializeSinOsc
  -> event C.SinOsc
  -> C.Node outputChannels () () event payload
sinOsc i atts = C.Node go
  where
  go parent (C.AudioInterpret { ids, makeSinOsc, setFrequency, setOnOff }) =
    keepLatest
      ( (sample_ ids (pure unit)) <#> \me ->
          pure
            ( makeSinOsc
                { id: me
                , parent: parent
                , frequency: i.frequency
                , onOff: i.onOff
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

sinOsc'
  :: forall proxy sym outputChannels produced event payload
   . IsEvent event
  => Cons sym C.Input () produced
  => proxy sym
  -> C.InitializeSinOsc
  -> event C.SinOsc
  -> C.Node outputChannels produced () event payload
sinOsc' _ i atts = let C.Node n = sinOsc i atts in C.Node n

-- squareOsc

squareOsc
  :: forall outputChannels event payload
   . IsEvent event
  => C.InitializeSquareOsc
  -> event C.SquareOsc
  -> C.Node outputChannels () () event payload
squareOsc i atts = C.Node go
  where
  go parent (C.AudioInterpret { ids, makeSquareOsc, setFrequency, setOnOff }) =
    keepLatest
      ( (sample_ ids (pure unit)) <#> \me ->
          pure
            ( makeSquareOsc
                { id: me
                , parent: parent
                , frequency: i.frequency
                , onOff: i.onOff
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

squareOsc'
  :: forall proxy sym outputChannels produced event payload
   . IsEvent event
  => Cons sym C.Input () produced
  => proxy sym
  -> C.InitializeSquareOsc
  -> event C.SquareOsc
  -> C.Node outputChannels produced () event payload
squareOsc' _ i atts = let C.Node n = squareOsc i atts in C.Node n
-- speaker
speaker
  :: forall outputChannels produced produced' consumed event payload
   . IsEvent event
  => Nub produced produced
  => Union produced consumed produced'
  => Nub produced' produced
  => C.GainInput outputChannels produced consumed event payload
  -> C.AudioInterpret event payload
  -> event payload
speaker (C.GainInput elts) di@(C.AudioInterpret { ids, makeSpeaker }) =
  keepLatest
    ( (sample_ ids (pure unit)) <#> \me ->
        pure (makeSpeaker { id: me })
          <|> oneOf
            (map (\elt -> ((\y -> let C.Node x = y in x) elt) me di) elts)
    )

speaker2
  :: forall produced produced' consumed event payload
   . IsEvent event
  => Nub produced produced
  => Union produced consumed produced'
  => Nub produced' produced
  => C.GainInput D2 produced consumed event payload
  -> C.AudioInterpret event payload
  -> event payload
speaker2 = speaker

-- pan

pan
  :: forall outputChannels produced consumed event payload
   . IsEvent event
  => C.InitializeStereoPanner
  -> event C.StereoPanner
  -> C.Node outputChannels produced consumed event payload
  -> C.Node outputChannels produced consumed event payload
pan i atts elt = C.Node go
  where
  go parent di@(C.AudioInterpret { ids, makeStereoPanner, setPan }) =
    keepLatest
      ( (sample_ ids (pure unit)) <#> \me ->
          pure
            ( makeStereoPanner
                { id: me, parent: parent, pan: i.pan }
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

pan'
  :: forall proxy sym outputChannels produced produced' consumed event payload
   . IsEvent event
  => Cons sym C.Input produced' produced
  => proxy sym
  -> C.InitializeStereoPanner
  -> event C.StereoPanner
  -> C.Node outputChannels produced' consumed event payload
  -> C.Node outputChannels produced consumed event payload
pan' _ i atts elts = let C.Node n = pan i atts elts in C.Node n

-- triangleOsc

triangleOsc
  :: forall outputChannels event payload
   . IsEvent event
  => C.InitializeTriangleOsc
  -> event C.TriangleOsc
  -> C.Node outputChannels () () event payload
triangleOsc i atts = C.Node go
  where
  go parent (C.AudioInterpret { ids, makeTriangleOsc, setFrequency, setOnOff }) =
    keepLatest
      ( (sample_ ids (pure unit)) <#> \me ->
          pure
            ( makeTriangleOsc
                { id: me
                , parent: parent
                , frequency: i.frequency
                , onOff: i.onOff
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

triangleOsc'
  :: forall proxy sym outputChannels produced event payload
   . IsEvent event
  => Cons sym C.Input () produced
  => proxy sym
  -> C.InitializeTriangleOsc
  -> event C.TriangleOsc
  -> C.Node outputChannels produced () event payload
triangleOsc' _ i atts = let C.Node n = triangleOsc i atts in C.Node n

-- waveshaper

waveshaper
  :: forall outputChannels event payload
   . IsEvent event
  => C.InitializeWaveshaper
  -> C.Node outputChannels () () event payload
waveshaper i = C.Node go
  where
  go parent (C.AudioInterpret { ids, makeWaveShaper }) =
    keepLatest
      ( (sample_ ids (pure unit)) <#> \me ->
          pure
            ( makeWaveShaper
                { id: me
                , parent: parent
                , curve: i.curve
                , oversample: i.oversample
                }
            )
      )

waveshaper'
  :: forall proxy sym outputChannels produced event payload
   . IsEvent event
  => Cons sym C.Input () produced
  => proxy sym
  -> C.InitializeWaveshaper
  -> C.Node outputChannels produced () event payload
waveshaper' _ i = let C.Node n = waveshaper i in C.Node n

-- todo: tumult