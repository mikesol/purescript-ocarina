module WAGS.Change where

import Prelude

import Control.Comonad (extract)
import Data.Homogeneous.Row.Options (homogeneous, objFromHomogeneous)
import Data.Newtype (over)
import Data.Row.Options (asOptions, options, megamap)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Typelevel.Num (class Lt, class Nat, class Pos, D1)
import Data.Variant.Maybe (Maybe)
import Data.Vec as V
import Foreign.Object (values)
import Prim.Row as R
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Prim.Symbol as Sym
import Record as Record
import Type.Proxy (Proxy(..))
import WAGS.ConstructEdges (class ConstructEdges, constructEdges)
import WAGS.Control.Indexed (IxWAG(..))
import WAGS.Control.Types (WAG, unsafeUnWAG, unsafeWAG)
import WAGS.Graph.AudioUnit (AudioWorkletNodeOptions(..))
import WAGS.Graph.AudioUnit as CTOR
import WAGS.Graph.Graph (Graph)
import WAGS.Graph.Node (NodeC)
import WAGS.Graph.Oversample (class IsOversample)
import WAGS.Graph.Paramable (onOffIze, paramize)
import WAGS.Graph.Parameter (class MM, AudioEnvelope, AudioOnOff, AudioParameter, AudioParameterCancellation, AudioSingleNumber, OnOff, MultiPlayBufOnOff)
import WAGS.Interpret (class AudioInterpret, setAnalyserNodeCb, setAttack, setAudioWorkletParameter, setBuffer, setBufferOffset, setConvolverBuffer, setDelay, setFrequency, setGain, setInput, setKnee, setLoopEnd, setLoopStart, setMediaRecorderCb, setOffset, setOnOff, setMultiPlayBufOnOff, setPan, setPeriodicOsc, setPeriodicOscV, setPlaybackRate, setQ, setRatio, setRelease, setSingleSubgraph, setSubgraph, setThreshold, setTumult, setWaveShaperCurve)
import WAGS.Rendered (RealImg(..))
import WAGS.Tumult (Tumultuous, safeUntumult)
import WAGS.Util (class MakePrefixIfNeeded, class CoercePrefixToString)
import WAGS.WebAPI (AnalyserNodeCb, BrowserAudioBuffer, BrowserFloatArray, BrowserMicrophone, BrowserPeriodicWave, MediaRecorderCb)

type Change'Type (ptr :: Symbol) (a :: Type) (graph :: Graph) =
  forall proxy audio engine proof res
   . AudioInterpret audio engine
  => proxy ptr
  -> WAG audio engine proof res graph a
  -> WAG audio engine proof res graph Unit

-- | Change an audio unit `node` in `igraph` with index `ptr`, outputting the changed node.
class Change' (ptr :: Symbol) (a :: Type) (graph :: Graph) where
  change' :: Change'Type ptr a graph

type CanBeChangedType
  (sym :: Symbol)
  (val :: Type)
  (ptr :: Symbol)
  (graph :: Graph) =
  forall proxy audio engine proof res
   . AudioInterpret audio engine
  => proxy sym
  -> val
  -> proxy ptr
  -> WAG audio engine proof res graph Unit
  -> WAG audio engine proof res graph Unit

class
  CanBeChanged (sym :: Symbol) (val :: Type) (ptr :: Symbol) (graph :: Graph) where
  canBeChanged :: CanBeChangedType sym val ptr graph

type Change''Type
  (rl :: RL.RowList Type)
  (ptr :: Symbol)
  (a :: Row Type)
  (graph :: Graph) =
  forall proxyA proxyB audio engine proof res
   . AudioInterpret audio engine
  => proxyA rl
  -> proxyB ptr
  -> WAG audio engine proof res graph { | a }
  -> WAG audio engine proof res graph Unit

-- | Change an audio unit `node` in `igraph` with index `ptr`, outputting the changed node.
class
  Change''
    (rl :: RL.RowList Type)
    (ptr :: Symbol)
    (a :: Row Type)
    (graph :: Graph) where
  change'' :: Change''Type rl ptr a graph

instance change''Nil :: Change'' RL.Nil ptr a graph where
  change'' _ _ = (<$) unit

instance change''Cons ::
  ( IsSymbol sym
  , IsSymbol ptr
  , R.Cons sym val ignore a
  , CanBeChanged sym val ptr graph
  , Change'' rest ptr a graph
  ) =>
  Change'' (RL.Cons sym val rest) ptr a graph where
  change'' _ a w =
    let
      psym = Proxy :: _ sym

      ew = extract w

      res = canBeChanged psym (Record.get psym ew) (Proxy :: _ ptr) (w $> unit)
    in
      change'' (Proxy :: _ rest) a (res $> ew)

ichange'
  :: forall proxy ptr a audio engine proof res i
   . AudioInterpret audio engine
  => Change' ptr a i
  => proxy ptr
  -> a
  -> IxWAG audio engine proof res i i Unit
ichange' ptr a = IxWAG (change' ptr <<< (<$) a)

-- | Similar to `change'`, but accepts a record with multiple units to change.
class Change (r :: Row Type) (graph :: Graph) where
  change
    :: forall audio engine proof res
     . AudioInterpret audio engine
    => WAG audio engine proof res graph { | r }
    -> WAG audio engine proof res graph Unit

type ChangeInternalSig
  (prefix :: Type)
  (map :: Type)
  (r :: Row Type)
  (graph :: Graph) =
  forall proxyPrefix proxyMap audio engine proof res
   . AudioInterpret audio engine
  => proxyPrefix prefix
  -> proxyMap map
  -> WAG audio engine proof res graph { | r }
  -> WAG audio engine proof res graph Unit

class
  ChangeInternal (prefix :: Type) (map :: Type) (r :: Row Type) (graph :: Graph) where
  changeInternal :: ChangeInternalSig prefix map r graph

class
  ChangeRL
    (rl :: RL.RowList Type)
    (prefix :: Type)
    (map :: Type)
    (r :: Row Type)
    (graph :: Graph) where
  changeRL
    :: forall proxyPrefix proxyMap proxy audio engine proof res
     . AudioInterpret audio engine
    => proxy rl
    -> proxyPrefix prefix
    -> proxyMap map
    -> WAG audio engine proof res graph { | r }
    -> WAG audio engine proof res graph Unit

instance changeInternalAll ::
  ( RL.RowToList r rl
  , ChangeRL rl prefix map r graph
  ) =>
  ChangeInternal prefix map r graph where
  changeInternal = changeRL (Proxy :: _ rl)

instance changeAll :: ChangeInternal Unit Unit r graph => Change r graph where
  change = changeInternal (Proxy :: _ Unit) (Proxy :: _ Unit)

instance changeRLNil :: ChangeRL RL.Nil prefix map r graph where
  changeRL _ _ _ r = r $> unit

instance changeRLConsU :: ChangeRL (RL.Cons key Unit rest) prefix map r graph where
  changeRL _ _ _ r = r $> unit
else instance changeRLCons ::
  ( IsSymbol key
  , R.Cons key val ignore r
  , MakePrefixIfNeeded key prefix prefix'
  , ConstructEdges prefix' map val newPrefix newMap (node /\ { | edges })
  , CoercePrefixToString prefix realPrefix
  , Sym.Append realPrefix key newKey
  , Change' newKey node graph
  , ChangeInternal newPrefix newMap edges graph
  , ChangeRL rest prefix map r graph
  ) =>
  ChangeRL (RL.Cons key val rest) prefix map r graph where
  changeRL _ _ _ r = step3
    where
    rx = extract r

    _ /\ _ /\ (node /\ edges) = constructEdges (Proxy :: _ prefix')
      (Proxy :: _ map)
      (Record.get (Proxy :: _ key) rx)

    step1 = change' (Proxy :: _ newKey) (r $> node)

    step2 =
      (changeInternal :: ChangeInternalSig newPrefix newMap edges graph) Proxy
        Proxy
        (step1 $> edges)

    step3 = changeRL (Proxy :: _ rest) (Proxy :: _ prefix) (Proxy :: _ map)
      (step2 $> rx)

ichange
  :: forall r audio engine proof res inGraph
   . AudioInterpret audio engine
  => Change r inGraph
  => { | r }
  -> IxWAG audio engine proof res inGraph inGraph Unit
ichange r = IxWAG (change <<< (<$) r)

class PushAudioOnOffToEnd (i :: RL.RowList Type) (o :: RL.RowList Type) | i -> o

instance pushAudioOnOffToEndNil :: PushAudioOnOffToEnd RL.Nil RL.Nil
else instance pushAudioOnOffToEndOO ::
  PushAudioOnOffToEnd (RL.Cons "onOff" a RL.Nil) (RL.Cons "onOff" a RL.Nil)
else instance pushAudioOnOffToEndOOC ::
  PushAudioOnOffToEnd (RL.Cons x y (RL.Cons "onOff" a z)) o =>
  PushAudioOnOffToEnd (RL.Cons "onOff" a (RL.Cons x y z)) o
else instance pushAudioOnOffToEndRest ::
  PushAudioOnOffToEnd c o =>
  PushAudioOnOffToEnd (RL.Cons a b c) (RL.Cons a b o)

class Detup (a :: Type) (b :: Type) | a -> b

instance detupT :: Detup (a /\ b) a
else instance detupOther :: Detup a a

class
  Monoid tau <=
  OneShotChange tau p au
  | tau p -> au where
  oneShotChange :: tau -> p -> au

instance changeNumber ::
  Change' ptr AudioParameter graph =>
  Change' ptr Number graph where
  change' px w = change' px (map paramize w)

instance changeSingleNumber ::
  Change' ptr AudioParameter graph =>
  Change' ptr AudioSingleNumber graph where
  change' px w = change' px (map paramize w)

instance changeOnOff ::
  Change' ptr AudioOnOff graph =>
  Change' ptr OnOff graph where
  change' px w = change' px (map onOffIze w)

instance changeBrowserAudioBuffer ::
  ( R.Cons ptr tau' ignore graph
  , Detup tau' tau
  , Monoid tau
  , OneShotChange tau BrowserAudioBuffer au
  , Change' ptr au graph
  ) =>
  Change' ptr BrowserAudioBuffer graph where
  change' px w = change' px (oneShotChange (mempty :: tau) <$> w)

instance changeTumultC ::
  ( R.Cons ptr tau' ignore graph
  , Detup tau' tau
  , Monoid tau
  , OneShotChange tau (Tumultuous n terminus inputs) au
  , Change' ptr au graph
  ) =>
  Change' ptr (Tumultuous n terminus inputs) graph where
  change' px w = change' px (oneShotChange (mempty :: tau) <$> w)

instance changeAudioOnOff ::
  ( R.Cons ptr tau' ignore graph
  , Detup tau' tau
  , Monoid tau
  , OneShotChange tau AudioOnOff au
  , Change' ptr au graph
  ) =>
  Change' ptr AudioOnOff graph where
  change' px w = change' px (oneShotChange (mempty :: tau) <$> w)

instance changeEnvelope ::
  Change' ptr AudioParameter graph =>
  Change' ptr AudioEnvelope graph where
  change' px w = change' px (map paramize w)

instance changeCancellation ::
  Change' ptr AudioParameter graph =>
  Change' ptr AudioParameterCancellation graph where
  change' px w = change' px (map paramize w)

instance changeAudioParameter ::
  ( R.Cons ptr tau' ignore graph
  , Detup tau' tau
  , Monoid tau
  , OneShotChange tau AudioParameter au
  , Change' ptr au graph
  ) =>
  Change' ptr AudioParameter graph where
  change' px w = change' px (oneShotChange (mempty :: tau) <$> w)

instance changeBrowerPeriodicWave ::
  ( R.Cons ptr tau' ignore graph
  , Detup tau' tau
  , Monoid tau
  , OneShotChange tau BrowserPeriodicWave au
  , Change' ptr au graph
  ) =>
  Change' ptr BrowserPeriodicWave graph where
  change' px w = change' px (oneShotChange (mempty :: tau) <$> w)

instance changeBrowserFloatArray ::
  ( R.Cons ptr tau' ignore graph
  , Detup tau' tau
  , Monoid tau
  , OneShotChange tau BrowserFloatArray au
  , Change' ptr au graph
  ) =>
  Change' ptr BrowserFloatArray graph where
  change' px w = change' px (oneShotChange (mempty :: tau) <$> w)

instance changeBrowserMicrophone ::
  ( R.Cons ptr tau' ignore graph
  , Detup tau' tau
  , Monoid tau
  , OneShotChange tau BrowserMicrophone au
  , Change' ptr au graph
  ) =>
  Change' ptr BrowserMicrophone graph where
  change' px w = change' px (oneShotChange (mempty :: tau) <$> w)

instance changeVec ::
  ( R.Cons ptr tau' ignore graph
  , Detup tau' tau
  , Monoid tau
  , Lt D1 size
  , OneShotChange tau (V.Vec size Number /\ V.Vec size Number) au
  , Change' ptr au graph
  ) =>
  Change' ptr (V.Vec size Number /\ V.Vec size Number) graph where
  change' px w = change' px (oneShotChange (mempty :: tau) <$> w)

instance changeRec ::
  ( RL.RowToList r rl'
  , PushAudioOnOffToEnd rl' rl
  , Change'' rl ptr r graph
  ) =>
  Change' ptr { | r } graph where
  change' px w = change'' (Proxy :: _ rl) px w

instance changeUnit ::
  Change' ptr Unit graph where
  change' _ w = w $> unit

class Freqable (tau :: Type)

instance freqableAllpass :: Freqable CTOR.TAllpass

instance freqableBandpass :: Freqable CTOR.TBandpass

instance freqableNotch :: Freqable CTOR.TNotch

instance freqablePeaking :: Freqable CTOR.TPeaking

instance freqableHighshelf :: Freqable CTOR.THighshelf

instance freqableLowshelf :: Freqable CTOR.TLowshelf

instance freqableHighpass :: Freqable CTOR.THighpass

instance freqableLowpass :: Freqable CTOR.TLowpass

instance freqableSinOsc :: Freqable CTOR.TSinOsc

instance freqableSawtoothOsc :: Freqable CTOR.TSawtoothOsc

instance freqableSquareOsc :: Freqable CTOR.TSquareOsc

instance freqablePeriodicOsc :: Freqable CTOR.TPeriodicOsc

instance freqableTriangleOsc :: Freqable CTOR.TTriangleOsc

instance canBeChangedFreqN ::
  CanBeChanged "freq" AudioParameter ptr graph =>
  CanBeChanged "freq" Number ptr graph where
  canBeChanged sym val ptr w = canBeChanged sym (paramize val) ptr w

instance canBeChangedFreq ::
  ( IsSymbol ptr
  , R.Cons ptr tau' ignore graph
  , Detup tau' tau
  , Freqable tau
  ) =>
  CanBeChanged "freq" AudioParameter ptr graph where
  canBeChanged _ val ptr w = o
    where
    { context: i } = unsafeUnWAG w

    id = reflectSymbol ptr

    argA_Changes = [ setFrequency { id, frequency: val } ]

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> argA_Changes
              }
        , value: unit
        }

class AudioOnOffable (tau :: Type)

instance onOffableConstant :: AudioOnOffable CTOR.TConstant

instance onOffablePlayBuf :: AudioOnOffable CTOR.TPlayBuf

instance AudioOnOffable CTOR.TMultiPlayBuf

instance onOffableLoopBuf :: AudioOnOffable CTOR.TLoopBuf

instance onOffableSinOsc :: AudioOnOffable CTOR.TSinOsc

instance onOffableSawtoothOsc :: AudioOnOffable CTOR.TSawtoothOsc

instance onOffableSquareOsc :: AudioOnOffable CTOR.TSquareOsc

instance onOffablePeriodicOsc :: AudioOnOffable CTOR.TPeriodicOsc

instance onOffableTriangleOsc :: AudioOnOffable CTOR.TTriangleOsc

instance canBeChangedOnOffSimple ::
  ( CanBeChanged "onOff" AudioOnOff ptr graph
  ) =>
  CanBeChanged "onOff" OnOff ptr graph where
  canBeChanged sym val ptr w = canBeChanged sym (onOffIze val) ptr w

instance canBeChangedAudioOnOff ::
  ( IsSymbol ptr
  , R.Cons ptr tau' ignore graph
  , Detup tau' tau
  , AudioOnOffable tau
  ) =>
  CanBeChanged "onOff" AudioOnOff ptr graph where
  canBeChanged _ onOff ptr w = o
    where
    { context: i } = unsafeUnWAG w

    id = reflectSymbol ptr

    argA_Changes = [ setOnOff { id, onOff } ]

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> argA_Changes
              }
        , value: unit
        }

instance canBeChangedAudioMultiPlayBufOnOff ::
  ( IsSymbol ptr
  , R.Cons ptr tau' ignore graph
  , Detup tau' tau
  , AudioOnOffable tau
  ) =>
  CanBeChanged "onOff" MultiPlayBufOnOff ptr graph where
  canBeChanged _ onOff ptr w = o
    where
    { context: i } = unsafeUnWAG w

    id = reflectSymbol ptr

    argA_Changes = [ setMultiPlayBufOnOff { id, onOff } ]

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> argA_Changes
              }
        , value: unit
        }

class Qable (tau :: Type)

instance qableAllpass :: Qable CTOR.TAllpass

instance qableBandpass :: Qable CTOR.TBandpass

instance qableNotch :: Qable CTOR.TNotch

instance qablePeaking :: Qable CTOR.TPeaking

instance qableHighpass :: Qable CTOR.THighpass

instance qableLowpass :: Qable CTOR.TLowpass

instance canBeChangedQN ::
  ( CanBeChanged "q" AudioParameter ptr graph
  ) =>
  CanBeChanged "q" Number ptr graph where
  canBeChanged sym val ptr w = canBeChanged sym (paramize val) ptr w

instance canBeChangedQ ::
  ( IsSymbol ptr
  , R.Cons ptr tau' ignore graph
  , Detup tau' tau
  , Qable tau
  ) =>
  CanBeChanged "q" AudioParameter ptr graph where
  canBeChanged _ q ptr w = o
    where
    { context: i } = unsafeUnWAG w

    id = reflectSymbol ptr

    argA_Changes = [ setQ { id, q } ]

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> argA_Changes
              }
        , value: unit
        }

class Gainable (tau :: Type)

instance gainablePeaking :: Gainable CTOR.TPeaking

instance gainableHighshelf :: Gainable CTOR.THighshelf

instance gainableLowshelf :: Gainable CTOR.TLowshelf

instance canBeChangedGainN ::
  ( CanBeChanged "gain" AudioParameter ptr graph
  ) =>
  CanBeChanged "gain" Number ptr graph where
  canBeChanged sym val ptr w = canBeChanged sym (paramize val) ptr w

instance canBeChangedGain ::
  ( IsSymbol ptr
  , R.Cons ptr tau' ignore graph
  , Detup tau' tau
  , Gainable tau
  ) =>
  CanBeChanged "gain" AudioParameter ptr graph where
  canBeChanged _ gain ptr w = o
    where
    { context: i } = unsafeUnWAG w

    id = reflectSymbol ptr

    argA_Changes = [ setGain { id, gain } ]

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> argA_Changes
              }
        , value: unit
        }

class Offsetable (tau :: Type)

instance offsetableConstant :: Offsetable CTOR.TConstant

instance canBeChangedOffsetN ::
  ( CanBeChanged "offset" AudioParameter ptr graph
  ) =>
  CanBeChanged "offset" Number ptr graph where
  canBeChanged sym val ptr w = canBeChanged sym (paramize val) ptr w

instance canBeChangedOffset ::
  ( IsSymbol ptr
  , R.Cons ptr tau' ignore graph
  , Detup tau' tau
  , Offsetable tau
  ) =>
  CanBeChanged "offset" AudioParameter ptr graph where
  canBeChanged _ offset ptr w = o
    where
    { context: i } = unsafeUnWAG w

    id = reflectSymbol ptr

    argA_Changes = [ setOffset { id, offset } ]

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> argA_Changes
              }
        , value: unit
        }

class LoopStartable (tau :: Type)

instance loopStartableLoopBuf :: LoopStartable CTOR.TLoopBuf

instance canBeChangedLoopStart ::
  ( IsSymbol ptr
  , R.Cons ptr tau' ignore graph
  , Detup tau' tau
  , LoopStartable tau
  ) =>
  CanBeChanged "loopStart" Number ptr graph where
  canBeChanged _ val ptr w = o
    where
    { context: i } = unsafeUnWAG w

    id = reflectSymbol ptr

    argA_Changes = [ setLoopStart { id, loopStart: val } ]

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> argA_Changes
              }
        , value: unit
        }

class LoopEndable (tau :: Type)

instance loopEndableLoopBuf :: LoopEndable CTOR.TLoopBuf

instance canBeChangedLoopEnd ::
  ( IsSymbol ptr
  , R.Cons ptr tau' ignore graph
  , Detup tau' tau
  , LoopEndable tau
  ) =>
  CanBeChanged "loopEnd" Number ptr graph where
  canBeChanged _ val ptr w = o
    where
    { context: i } = unsafeUnWAG w

    id = reflectSymbol ptr

    argA_Changes = [ setLoopEnd { id, loopEnd: val } ]

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> argA_Changes
              }
        , value: unit
        }

class BufferOffsetable (tau :: Type)

instance bufferOffsetablePlayBuf :: BufferOffsetable CTOR.TPlayBuf

instance canBeChangedBufferOffset ::
  ( IsSymbol ptr
  , R.Cons ptr tau' ignore graph
  , Detup tau' tau
  , BufferOffsetable tau
  ) =>
  CanBeChanged "bufferOffset" Number ptr graph where
  canBeChanged _ val ptr w = o
    where
    { context: i } = unsafeUnWAG w

    id = reflectSymbol ptr

    argA_Changes = [ setBufferOffset { id, bufferOffset: val } ]

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> argA_Changes
              }
        , value: unit
        }

class WebAudioInputable (tau :: Type)

instance webAudioInputableInput :: WebAudioInputable (CTOR.TInput input)

instance canBeChangedWebAudioInput ::
  ( IsSymbol ptr
  , IsSymbol input
  , R.Cons ptr tau' ignore graph
  , Detup tau' tau
  , WebAudioInputable tau
  ) =>
  CanBeChanged "input" (Proxy input) ptr graph where
  canBeChanged _ val ptr w = o
    where
    { context: i } = unsafeUnWAG w

    id = reflectSymbol ptr

    argA_Changes = [ setInput { id, source: (reflectSymbol val) } ]

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> argA_Changes
              }
        , value: unit
        }

class PlaybackRateable (tau :: Type)

instance playbackRateableLoopBuf :: PlaybackRateable CTOR.TLoopBuf

instance playbackRateablePlayBuf :: PlaybackRateable CTOR.TPlayBuf

instance playbackRateableMultiPlayBuf :: PlaybackRateable CTOR.TMultiPlayBuf

instance canBeChangedPlaybackRateN ::
  ( CanBeChanged "playbackRate" AudioParameter ptr graph
  ) =>
  CanBeChanged "playbackRate" Number ptr graph where
  canBeChanged sym val ptr w = canBeChanged sym (paramize val) ptr w

instance canBeChangedPlaybackRate ::
  ( IsSymbol ptr
  , R.Cons ptr tau' ignore graph
  , Detup tau' tau
  , PlaybackRateable tau
  ) =>
  CanBeChanged "playbackRate" AudioParameter ptr graph where
  canBeChanged _ val ptr w = o
    where
    { context: i } = unsafeUnWAG w

    id = reflectSymbol ptr

    argA_Changes = [ setPlaybackRate { id, playbackRate: val } ]

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> argA_Changes
              }
        , value: unit
        }

class Bufferable (tau :: Type)

instance bufferableLoopBuf :: Bufferable CTOR.TLoopBuf

instance bufferablePlayBuf :: Bufferable CTOR.TPlayBuf

instance canBeChangedBuffer ::
  ( IsSymbol ptr
  , R.Cons ptr tau' ignore graph
  , Detup tau' tau
  , Bufferable tau
  ) =>
  CanBeChanged "buffer" BrowserAudioBuffer ptr graph where
  canBeChanged _ val ptr w = o
    where
    { context: i } = unsafeUnWAG w

    id = reflectSymbol ptr

    argA_Changes = [ setBuffer { id, buffer: val } ]

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> argA_Changes
              }
        , value: unit
        }

class Waveformable (tau :: Type)

instance waveformablePeriodicOsc :: Waveformable CTOR.TPeriodicOsc

instance canBeChangedWaveform ::
  ( IsSymbol ptr
  , R.Cons ptr tau' ignore graph
  , Detup tau' tau
  , Waveformable tau
  ) =>
  CanBeChanged "waveform" BrowserPeriodicWave ptr graph where
  canBeChanged _ val ptr w = o
    where
    { context: i } = unsafeUnWAG w

    id = reflectSymbol ptr

    argA_Changes = [ setPeriodicOsc { id, wave: val } ]

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> argA_Changes
              }
        , value: unit
        }

instance canBeChangedWaveformV ::
  ( IsSymbol ptr
  , R.Cons ptr tau' ignore graph
  , Detup tau' tau
  , Lt D1 size
  , Waveformable tau
  ) =>
  CanBeChanged "waveform" (V.Vec size Number /\ V.Vec size Number) ptr graph where
  canBeChanged _ val ptr w = o
    where
    { context: i } = unsafeUnWAG w

    id = reflectSymbol ptr

    argA_Changes =
      [ setPeriodicOscV
          { id
          , realImg: val # \(real /\ img) -> RealImg
              { real: V.toArray real, img: V.toArray img }
          }
      ]

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> argA_Changes
              }
        , value: unit
        }

class Thresholdable (tau :: Type)

instance thresholdableDynamicsCompressor ::
  Thresholdable CTOR.TDynamicsCompressor

instance canBeChangedThresholdN ::
  ( CanBeChanged "threshold" AudioParameter ptr graph
  ) =>
  CanBeChanged "threshold" Number ptr graph where
  canBeChanged sym val ptr w = canBeChanged sym (paramize val) ptr w

instance canBeChangedThreshold ::
  ( IsSymbol ptr
  , R.Cons ptr tau' ignore graph
  , Detup tau' tau
  , Thresholdable tau
  ) =>
  CanBeChanged "threshold" AudioParameter ptr graph where
  canBeChanged _ val ptr w = o
    where
    { context: i } = unsafeUnWAG w

    id = reflectSymbol ptr

    argA_Changes = [ setThreshold { id, threshold: val } ]

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> argA_Changes
              }
        , value: unit
        }

class Ratioable (tau :: Type)

instance ratioableDynamicsCompressor :: Ratioable CTOR.TDynamicsCompressor

instance canBeChangedRatioN ::
  ( CanBeChanged "ratio" AudioParameter ptr graph
  ) =>
  CanBeChanged "ratio" Number ptr graph where
  canBeChanged sym val ptr w = canBeChanged sym (paramize val) ptr w

instance canBeChangedRatio ::
  ( IsSymbol ptr
  , R.Cons ptr tau' ignore graph
  , Detup tau' tau
  , Ratioable tau
  ) =>
  CanBeChanged "ratio" AudioParameter ptr graph where
  canBeChanged _ val ptr w = o
    where
    { context: i } = unsafeUnWAG w

    id = reflectSymbol ptr

    argA_Changes = [ setRatio { id, ratio: val } ]

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> argA_Changes
              }
        , value: unit
        }

class Kneeable (tau :: Type)

instance kneeableDynamicsCompressor :: Kneeable CTOR.TDynamicsCompressor

instance canBeChangedKneeN ::
  ( CanBeChanged "knee" AudioParameter ptr graph
  ) =>
  CanBeChanged "knee" Number ptr graph where
  canBeChanged sym val ptr w = canBeChanged sym (paramize val) ptr w

instance canBeChangedKnee ::
  ( IsSymbol ptr
  , R.Cons ptr tau' ignore graph
  , Detup tau' tau
  , Kneeable tau
  ) =>
  CanBeChanged "knee" AudioParameter ptr graph where
  canBeChanged _ val ptr w = o
    where
    { context: i } = unsafeUnWAG w

    id = reflectSymbol ptr

    argA_Changes = [ setKnee { id, knee: val } ]

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> argA_Changes
              }
        , value: unit
        }

class Attackable (tau :: Type)

instance attackableDynamicsCompressor :: Attackable CTOR.TDynamicsCompressor

instance canBeChangedAttackN ::
  ( CanBeChanged "attack" AudioParameter ptr graph
  ) =>
  CanBeChanged "attack" Number ptr graph where
  canBeChanged sym val ptr w = canBeChanged sym (paramize val) ptr w

instance canBeChangedAttack ::
  ( IsSymbol ptr
  , R.Cons ptr tau' ignore graph
  , Detup tau' tau
  , Attackable tau
  ) =>
  CanBeChanged "attack" AudioParameter ptr graph where
  canBeChanged _ val ptr w = o
    where
    { context: i } = unsafeUnWAG w

    id = reflectSymbol ptr

    argA_Changes = [ setAttack { id, attack: val } ]

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> argA_Changes
              }
        , value: unit
        }

class Releaseable (tau :: Type)

instance releaseableDynamicsCompressor :: Releaseable CTOR.TDynamicsCompressor

instance canBeChangedReleaseN ::
  ( CanBeChanged "release" AudioParameter ptr graph
  ) =>
  CanBeChanged "release" Number ptr graph where
  canBeChanged sym val ptr w = canBeChanged sym (paramize val) ptr w

instance canBeChangedRelease ::
  ( IsSymbol ptr
  , R.Cons ptr tau' ignore graph
  , Detup tau' tau
  , Releaseable tau
  ) =>
  CanBeChanged "release" AudioParameter ptr graph where
  canBeChanged _ val ptr w = o
    where
    { context: i } = unsafeUnWAG w

    id = reflectSymbol ptr

    argA_Changes = [ setRelease { id, release: val } ]

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> argA_Changes
              }
        , value: unit
        }

instance changeAnalyser ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.TAnalyser edges) ignore graph
  ) =>
  Change' ptr (CTOR.Analyser AnalyserNodeCb) graph where
  change' ptr w = o
    where
    { context: i, value: (CTOR.Analyser cb) } = unsafeUnWAG w

    id = reflectSymbol ptr

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <>
                  [ setAnalyserNodeCb { id, cb } ]
              }
        , value: unit
        }

instance oneShotChangeAllpass ::
  OneShotChange CTOR.TAllpass AudioParameter CTOR.XAllpass where
  oneShotChange = const (CTOR.XAllpass <<< options <<< { freq: _ })

instance changeAllpass ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.TAllpass edges) ignore graph
  ) =>
  Change' ptr CTOR.Allpass graph where
  change' ptr = change' ptr
    <<< map (over CTOR.Allpass asOptions :: _ -> CTOR.XAllpass)

instance changeXAllpass ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.TAllpass edges) ignore graph
  ) =>
  Change' ptr CTOR.XAllpass graph where
  change' ptr w = o
    where
    { context: i, value: (CTOR.XAllpass x) } = unsafeUnWAG w

    id = reflectSymbol ptr
    changes = values
      $ objFromHomogeneous
      $ homogeneous
      $ megamap x
          { freq: setFrequency <<< { id, frequency: _ }
          , q: setQ <<< { id, q: _ }
          }

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> changes
              }
        , value: unit
        }

class
  InterpretParameters
    (parameterData :: Row Type)
    (parameterRL :: RowList Type)
    (parameters :: Row Type) where
  interpretParameters
    :: forall proxyR proxyRL audio engine
     . AudioInterpret audio engine
    => String
    -> proxyR parameterData
    -> proxyRL parameterRL
    -> { | parameters }
    -> Array (audio -> engine)

instance interpretParametersNil ::
  InterpretParameters parameterData RL.Nil parameters where
  interpretParameters _ _ _ _ = []

instance interpretParametersCons ::
  ( IsSymbol key
  , R.Cons key val parameterData' parameterData
  , R.Cons key AudioParameter parameters' parameters
  , InterpretParameters parameterData rest parameters
  ) =>
  InterpretParameters parameterData (RL.Cons key AudioParameter rest) parameters where
  interpretParameters id pd _ p =
    let
      px = Proxy :: _ key
    in
      [ setAudioWorkletParameter
          { id, paramName: (reflectSymbol px), paramValue: (Record.get px p) }
      ]
        <> interpretParameters id pd (Proxy :: _ rest) p

instance changeAudioWorkletNode ::
  ( IsSymbol ptr
  , RowToList parameterData parameterDataRL
  , InterpretParameters optionsParamaterData parameterDataRL parameterData
  , R.Cons ptr
      ( NodeC
          ( CTOR.TAudioWorkletNode node numberOfInputs numberOfOutputs
              outputChannelCount
              parameterData
              processorOptions
          )
          edges
      )
      ignore
      graph
  ) =>
  Change' ptr
    ( CTOR.AudioWorkletNode node numberOfInputs numberOfOutputs
        outputChannelCount
        parameterData
        processorOptions
    )
    graph where
  change' ptr w = o
    where
    { context: i
    , value: (CTOR.AudioWorkletNode _ (AudioWorkletNodeOptions options))
    } = unsafeUnWAG w

    id = reflectSymbol ptr

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <>
                  ( interpretParameters id (Proxy :: _ optionsParamaterData)
                      (Proxy :: _ parameterDataRL)
                      options.parameterData
                  )
              }
        , value: unit
        }

instance oneShotChangeBandpass ::
  OneShotChange CTOR.TBandpass AudioParameter CTOR.XBandpass where
  oneShotChange = const (CTOR.XBandpass <<< options <<< { freq: _ })

instance changeBandpass ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.TBandpass edges) ignore graph
  ) =>
  Change' ptr CTOR.Bandpass graph where
  change' ptr = change' ptr
    <<< map (over CTOR.Bandpass asOptions :: _ -> CTOR.XBandpass)

instance changeXBandpass ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.TBandpass edges) ignore graph
  ) =>
  Change' ptr CTOR.XBandpass graph where
  change' ptr w = o
    where
    { context: i, value: (CTOR.XBandpass x) } = unsafeUnWAG w

    id = reflectSymbol ptr
    changes = values
      $ objFromHomogeneous
      $ homogeneous
      $ megamap x
          { freq: setFrequency <<< { id, frequency: _ }
          , q: setQ <<< { id, q: _ }
          }

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> changes
              }
        , value: unit
        }

instance oneShotChangeConstantOffset ::
  OneShotChange CTOR.TConstant AudioParameter CTOR.XConstant where
  oneShotChange = const (CTOR.XConstant <<< options <<< { offset: _ })

instance oneShotChangeConstantOnOff ::
  OneShotChange CTOR.TConstant AudioOnOff CTOR.XConstant where
  oneShotChange = const (CTOR.XConstant <<< options <<< { onOff: _ })

instance changeConstant ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.TConstant edges) ignore graph
  ) =>
  Change' ptr CTOR.Constant graph where
  change' ptr = change' ptr
    <<< map (over CTOR.Constant asOptions :: _ -> CTOR.XConstant)

instance changeXConstant ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.TConstant edges) ignore graph
  ) =>
  Change' ptr CTOR.XConstant graph where
  change' ptr w = o
    where
    { context: i, value: (CTOR.XConstant x) } = unsafeUnWAG w

    id = reflectSymbol ptr
    changes = values
      $ objFromHomogeneous
      $ homogeneous
      $ megamap x
          { offset: setOffset <<< { id, offset: _ }
          , onOff: setOnOff <<< { id, onOff: _ }
          }

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> changes
              }
        , value: unit
        }

instance oneShotChangeConvolver ::
  OneShotChange CTOR.TConvolver BrowserAudioBuffer (CTOR.XConvolver) where
  oneShotChange = const (CTOR.XConvolver <<< options <<< { buffer: _ })

instance changeConvolver ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.TConvolver edges) ignore graph
  ) =>
  Change' ptr (CTOR.Convolver) graph where
  change' ptr = change' ptr
    <<< map (over CTOR.Convolver asOptions :: _ -> CTOR.XConvolver)

instance changeXConvolver ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.TConvolver edges) ignore graph
  ) =>
  Change' ptr (CTOR.XConvolver) graph where
  change' ptr w = o
    where
    { context: i, value: (CTOR.XConvolver x) } = unsafeUnWAG w

    id = reflectSymbol ptr
    changes = values
      $ objFromHomogeneous
      $ homogeneous
      $ megamap x
          { buffer: setConvolverBuffer <<< { id, buffer: _ }
          }

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> changes
              }
        , value: unit
        }

instance oneShotChangeDelay ::
  OneShotChange CTOR.TDelay AudioParameter CTOR.XDelay where
  oneShotChange = const (CTOR.XDelay <<< options <<< { delayTime: _ })

instance changeDelay ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.TDelay edges) ignore graph
  ) =>
  Change' ptr CTOR.Delay graph where
  change' ptr = change' ptr <<< map
    (over CTOR.Delay asOptions :: _ -> CTOR.XDelay)

instance changeXDelay ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.TDelay edges) ignore graph
  ) =>
  Change' ptr CTOR.XDelay graph where
  change' ptr w = o
    where
    { context: i, value: (CTOR.XDelay x) } = unsafeUnWAG w

    id = reflectSymbol ptr
    changes = values
      $ objFromHomogeneous
      $ homogeneous
      $ megamap x { delayTime: setDelay <<< { id, delay: _ } }

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> changes
              }
        , value: unit
        }

instance changeDynamicsCompressor ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.TDynamicsCompressor edges) ignore graph
  ) =>
  Change' ptr CTOR.DynamicsCompressor graph where
  change' ptr = change' ptr
    <<< map
      (over CTOR.DynamicsCompressor asOptions :: _ -> CTOR.XDynamicsCompressor)

instance changeXDynamicsCompressor ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.TDynamicsCompressor edges) ignore graph
  ) =>
  Change' ptr CTOR.XDynamicsCompressor graph where
  change' ptr w = o
    where
    { context: i, value: (CTOR.XDynamicsCompressor x) } = unsafeUnWAG w

    id = reflectSymbol ptr
    changes = values
      $ objFromHomogeneous
      $ homogeneous
      $ megamap x
          { knee: setKnee <<< { id, knee: _ }
          , threshold: setThreshold <<< { id, threshold: _ }
          , ratio: setRatio <<< { id, ratio: _ }
          , attack: setAttack <<< { id, attack: _ }
          , release: setRelease <<< { id, release: _ }
          }

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> changes
              }
        , value: unit
        }

instance oneShotChangeGain :: OneShotChange CTOR.TGain AudioParameter CTOR.XGain where
  oneShotChange = const (CTOR.XGain <<< options <<< { gain: _ })

instance changeGain ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.TGain edges) ignore graph
  ) =>
  Change' ptr CTOR.Gain graph where
  change' ptr = change' ptr
    <<< map (over CTOR.Gain asOptions :: _ -> CTOR.XGain)

instance changeXGain ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.TGain edges) ignore graph
  ) =>
  Change' ptr CTOR.XGain graph where
  change' ptr w = o
    where
    { context: i, value: (CTOR.XGain x) } = unsafeUnWAG w

    id = reflectSymbol ptr
    changes = values
      $ objFromHomogeneous
      $ homogeneous
      $ megamap x
          { gain: setGain <<< { id, gain: _ }
          }

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> changes
              }
        , value: unit
        }

instance oneShotChangeHighpass ::
  OneShotChange CTOR.THighpass AudioParameter CTOR.XHighpass where
  oneShotChange = const (CTOR.XHighpass <<< options <<< { freq: _ })

instance changeHighpass ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.THighpass edges) ignore graph
  ) =>
  Change' ptr CTOR.Highpass graph where
  change' ptr = change' ptr
    <<< map (over CTOR.Highpass asOptions :: _ -> CTOR.XHighpass)

instance changeXHighpass ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.THighpass edges) ignore graph
  ) =>
  Change' ptr CTOR.XHighpass graph where
  change' ptr w = o
    where
    { context: i, value: (CTOR.XHighpass x) } = unsafeUnWAG w

    id = reflectSymbol ptr
    changes = values
      $ objFromHomogeneous
      $ homogeneous
      $ megamap x
          { freq: setFrequency <<< { id, frequency: _ }
          , q: setQ <<< { id, q: _ }
          }

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> changes
              }
        , value: unit
        }

instance oneShotChangeHighshelf ::
  OneShotChange CTOR.THighshelf AudioParameter CTOR.XHighshelf where
  oneShotChange = const (CTOR.XHighshelf <<< options <<< { freq: _ })

instance changeHighshelf ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.THighshelf edges) ignore graph
  ) =>
  Change' ptr CTOR.Highshelf graph where
  change' ptr = change' ptr
    <<< map (over CTOR.Highshelf asOptions :: _ -> CTOR.XHighshelf)

instance changeXHighshelf ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.THighshelf edges) ignore graph
  ) =>
  Change' ptr CTOR.XHighshelf graph where
  change' ptr w = o
    where
    { context: i, value: (CTOR.XHighshelf x) } = unsafeUnWAG w

    id = reflectSymbol ptr
    changes = values
      $ objFromHomogeneous
      $ homogeneous
      $ megamap x
          { freq: setFrequency <<< { id, frequency: _ }
          , gain: setGain <<< { id, gain: _ }
          }

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> changes
              }
        , value: unit
        }

instance oneShotChangeInput ::
  OneShotChange (CTOR.TInput input) (Proxy input) (CTOR.Input input) where
  oneShotChange _ _ = CTOR.Input

instance changeInput ::
  ( IsSymbol ptr
  , MM (Proxy input) (Maybe (Proxy input))
  , R.Cons ptr (NodeC (CTOR.TInput input) edges) ignore graph
  ) =>
  Change' ptr (CTOR.Input input) graph where
  change' _ w = w $> unit

instance oneShotChangeLoopBuf ::
  OneShotChange CTOR.TLoopBuf AudioParameter CTOR.XLoopBuf where
  oneShotChange = const
    ( CTOR.XLoopBuf
        <<< options
        <<< { playbackRate: _ }
    )

instance oneShotChangeLoopBufOO ::
  OneShotChange CTOR.TLoopBuf AudioOnOff CTOR.XLoopBuf where
  oneShotChange = const (CTOR.XLoopBuf <<< options <<< { onOff: _ })

instance oneShotChangeLoopBufProxy ::
  OneShotChange CTOR.TLoopBuf BrowserAudioBuffer CTOR.XLoopBuf where
  oneShotChange = const (CTOR.XLoopBuf <<< options <<< { buffer: _ })

instance changeLoopBuf ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.TLoopBuf edges) ignore graph
  ) =>
  Change' ptr CTOR.LoopBuf graph where
  change' ptr = change' ptr
    <<< map (over CTOR.LoopBuf asOptions :: _ -> CTOR.XLoopBuf)

instance changeXLoopBuf ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.TLoopBuf edges) ignore graph
  ) =>
  Change' ptr CTOR.XLoopBuf graph where
  change' ptr w = o
    where
    { context: i, value: (CTOR.XLoopBuf x) } = unsafeUnWAG w

    id = reflectSymbol ptr
    changes = values
      $ objFromHomogeneous
      $ homogeneous
      $ megamap x
          { buffer: setBuffer <<< { id, buffer: _ }
          , onOff: setOnOff <<< { id, onOff: _ }
          , playbackRate: setPlaybackRate <<< { id, playbackRate: _ }
          , loopStart: setLoopStart <<< { id, loopStart: _ }
          , loopEnd: setLoopEnd <<< { id, loopEnd: _ }
          }

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> changes
              }
        , value: unit
        }

instance oneShotChangeLowpass ::
  OneShotChange CTOR.TLowpass AudioParameter CTOR.XLowpass where
  oneShotChange = const (CTOR.XLowpass <<< options <<< { freq: _ })

instance changeLowpass ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.TLowpass edges) ignore graph
  ) =>
  Change' ptr CTOR.Lowpass graph where
  change' ptr = change' ptr
    <<< map (over CTOR.Lowpass asOptions :: _ -> CTOR.XLowpass)

instance changeXLowpass ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.TLowpass edges) ignore graph
  ) =>
  Change' ptr CTOR.XLowpass graph where
  change' ptr w = o
    where
    { context: i, value: (CTOR.XLowpass x) } = unsafeUnWAG w

    id = reflectSymbol ptr
    changes = values
      $ objFromHomogeneous
      $ homogeneous
      $ megamap x
          { freq: setFrequency <<< { id, frequency: _ }
          , q: setQ <<< { id, q: _ }
          }

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> changes
              }
        , value: unit
        }

instance oneShotChangeLowshelf ::
  OneShotChange CTOR.TLowshelf AudioParameter CTOR.XLowshelf where
  oneShotChange = const (CTOR.XLowshelf <<< options <<< { freq: _ })

instance changeLowshelf ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.TLowshelf edges) ignore graph
  ) =>
  Change' ptr CTOR.Lowshelf graph where
  change' ptr = change' ptr
    <<< map (over CTOR.Lowshelf asOptions :: _ -> CTOR.XLowshelf)

instance changeXLowshelf ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.TLowshelf edges) ignore graph
  ) =>
  Change' ptr CTOR.XLowshelf graph where
  change' ptr w = o
    where
    { context: i, value: (CTOR.XLowshelf x) } = unsafeUnWAG w

    id = reflectSymbol ptr
    changes = values
      $ objFromHomogeneous
      $ homogeneous
      $ megamap x
          { freq: setFrequency <<< { id, frequency: _ }
          , gain: setGain <<< { id, gain: _ }
          }

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> changes
              }
        , value: unit
        }

instance changeMediaElement ::
  ( R.Cons iSym (NodeC CTOR.TMediaElement edges) ignore graph
  ) =>
  Change' iSym (CTOR.MediaElement) graph where
  -- for now, we make this a no-op as it does not make sense in the
  -- web api to change a media element
  change' _ w = w $> unit

instance changeMicrophone ::
  ( R.Cons "microphone" (NodeC CTOR.TMicrophone edges) ignore graph
  ) =>
  Change' "microphone" (CTOR.Microphone) graph where
  -- for now, we make this a no-op as it does not make sense in the
  -- web api to change the microphone
  -- in future iterations, we can look into expanding this once changing
  -- the microphone has semantic meaning
  change' _ w = w $> unit

instance oneShotChangeNotch ::
  OneShotChange CTOR.TNotch AudioParameter CTOR.XNotch where
  oneShotChange = const (CTOR.XNotch <<< options <<< { freq: _ })

instance changeNotch ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.TNotch edges) ignore graph
  ) =>
  Change' ptr CTOR.Notch graph where
  change' ptr = change' ptr
    <<< map (over CTOR.Notch asOptions :: _ -> CTOR.XNotch)

instance changeXNotch ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.TNotch edges) ignore graph
  ) =>
  Change' ptr CTOR.XNotch graph where
  change' ptr w = o
    where
    { context: i, value: (CTOR.XNotch x) } = unsafeUnWAG w

    id = reflectSymbol ptr
    changes = values
      $ objFromHomogeneous
      $ homogeneous
      $ megamap x
          { freq: setFrequency <<< { id, frequency: _ }
          , q: setQ <<< { id, q: _ }
          }

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> changes
              }
        , value: unit
        }

instance oneShotChangePeaking ::
  OneShotChange CTOR.TPeaking AudioParameter CTOR.XPeaking where
  oneShotChange = const (CTOR.XPeaking <<< options <<< { freq: _ })

instance changePeaking ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.TPeaking edges) ignore graph
  ) =>
  Change' ptr CTOR.Peaking graph where
  change' ptr = change' ptr
    <<< map (over CTOR.Peaking asOptions :: _ -> CTOR.XPeaking)

instance changeXPeaking ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.TPeaking edges) ignore graph
  ) =>
  Change' ptr CTOR.XPeaking graph where
  change' ptr w = o
    where
    { context: i, value: (CTOR.XPeaking x) } = unsafeUnWAG w

    id = reflectSymbol ptr
    changes = values
      $ objFromHomogeneous
      $ homogeneous
      $ megamap x
          { freq: setFrequency <<< { id, frequency: _ }
          , q: setQ <<< { id, q: _ }
          , gain: setGain <<< { id, gain: _ }
          }

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> changes
              }
        , value: unit
        }

instance oneShotChangePeriodicOsc ::
  OneShotChange CTOR.TPeriodicOsc
    BrowserPeriodicWave
    (CTOR.XPeriodicOsc BrowserPeriodicWave) where
  oneShotChange = const (CTOR.XPeriodicOsc <<< options <<< { wave: _ })

instance oneShotChangePeriodicOscOO ::
  OneShotChange CTOR.TPeriodicOsc AudioOnOff (CTOR.XPeriodicOsc periodicOsc) where
  oneShotChange = const (CTOR.XPeriodicOsc <<< options <<< { onOff: _ })

instance oneShotChangePeriodicOscFreq ::
  OneShotChange CTOR.TPeriodicOsc AudioParameter (CTOR.XPeriodicOsc periodicOsc) where
  oneShotChange = const (CTOR.XPeriodicOsc <<< options <<< { freq: _ })

instance oneShotChangePeriodicOscVec ::
  Lt D1 size =>
  OneShotChange CTOR.TPeriodicOsc
    (V.Vec size Number /\ V.Vec size Number)
    (CTOR.XPeriodicOsc (V.Vec size Number /\ V.Vec size Number)) where
  oneShotChange = const (CTOR.XPeriodicOsc <<< options <<< { wave: _ })

instance changePeriodicOsc ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.TPeriodicOsc edges) ignore graph
  , Change' ptr (CTOR.XPeriodicOsc periodicOsc) graph
  ) =>
  Change' ptr (CTOR.PeriodicOsc periodicOsc) graph where
  change' ptr = change' ptr
    <<< map
      (over CTOR.PeriodicOsc asOptions :: _ -> CTOR.XPeriodicOsc periodicOsc)

instance changePeriodicXOsc ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.TPeriodicOsc edges) ignore graph
  ) =>
  Change' ptr (CTOR.XPeriodicOsc BrowserPeriodicWave) graph where
  change' ptr w = o
    where
    { context: i, value: (CTOR.XPeriodicOsc x) } = unsafeUnWAG w

    id = reflectSymbol ptr
    changes = values
      $ objFromHomogeneous
      $ homogeneous
      $ megamap x
          { freq: setFrequency <<< { id, frequency: _ }
          , onOff: setOnOff <<< { id, onOff: _ }
          , wave: setPeriodicOsc <<< { id, wave: _ }
          }

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> changes
              }
        , value: unit
        }

instance changePeriodicOscV ::
  ( IsSymbol ptr
  , Lt D1 size
  , R.Cons ptr (NodeC CTOR.TPeriodicOsc edges) ignore graph
  ) =>
  Change' ptr (CTOR.XPeriodicOsc (V.Vec size Number /\ V.Vec size Number)) graph where
  change' ptr w = o
    where
    { context: i, value: (CTOR.XPeriodicOsc x) } = unsafeUnWAG w

    id = reflectSymbol ptr
    changes = values
      $ objFromHomogeneous
      $ homogeneous
      $ megamap x
          { freq: setFrequency <<< { id, frequency: _ }
          , onOff: setOnOff <<< { id, onOff: _ }
          , wave:
              setPeriodicOscV
                <<< { id, realImg: _ }
                <<< RealImg <$>
                ( { real: _, img: _ }
                    <$> V.toArray <<< fst
                    <*> V.toArray <<< snd
                )
          }

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> changes
              }
        , value: unit
        }

instance oneShotChangePlayBuf ::
  OneShotChange CTOR.TPlayBuf AudioParameter (CTOR.XPlayBuf) where
  oneShotChange = const (CTOR.XPlayBuf <<< options <<< { playbackRate: _ })

instance oneShotChangePlayBufOO ::
  OneShotChange CTOR.TPlayBuf AudioOnOff (CTOR.XPlayBuf) where
  oneShotChange = const (CTOR.XPlayBuf <<< options <<< { onOff: _ })

instance oneShotChangePlayBufBuffer ::
  OneShotChange CTOR.TPlayBuf BrowserAudioBuffer (CTOR.XPlayBuf) where
  oneShotChange = const (CTOR.XPlayBuf <<< options <<< { buffer: _ })

instance changePlayBuf ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.TPlayBuf edges) ignore graph
  ) =>
  Change' ptr CTOR.PlayBuf graph where
  change' ptr = change' ptr
    <<< map (over CTOR.PlayBuf asOptions :: _ -> CTOR.XPlayBuf)

instance changeXPlayBuf ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.TPlayBuf edges) ignore graph
  ) =>
  Change' ptr CTOR.XPlayBuf graph where
  change' ptr w = o
    where
    { context: i, value: (CTOR.XPlayBuf x) } = unsafeUnWAG w

    id = reflectSymbol ptr
    changes = values
      $ objFromHomogeneous
      $ homogeneous
      $ megamap x
          { buffer: setBuffer <<< { id, buffer: _ }
          , onOff: setOnOff <<< { id, onOff: _ }
          , playbackRate: setPlaybackRate <<< { id, playbackRate: _ }
          , bufferOffset: setBufferOffset <<< { id, bufferOffset: _ }
          }

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> changes
              }
        , value: unit
        }

instance changeMultiPlayBuf ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.TMultiPlayBuf edges) ignore graph
  ) =>
  Change' ptr CTOR.MultiPlayBuf graph where
  change' ptr = change' ptr
    <<< map (over CTOR.MultiPlayBuf asOptions :: _ -> CTOR.XMultiPlayBuf)

instance changeXMultiPlayBuf ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.TMultiPlayBuf edges) ignore graph
  ) =>
  Change' ptr CTOR.XMultiPlayBuf graph where
  change' ptr w = o
    where
    { context: i, value: (CTOR.XMultiPlayBuf x) } = unsafeUnWAG w

    id = reflectSymbol ptr
    changes = values
      $ objFromHomogeneous
      $ homogeneous
      $ megamap x
          { onOff: setMultiPlayBufOnOff <<< { id, onOff: _ }
          , playbackRate: setPlaybackRate <<< { id, playbackRate: _ }
          }

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> changes
              }
        , value: unit
        }

instance oneShotChangeRecorder ::
  OneShotChange CTOR.TRecorder MediaRecorderCb (CTOR.XRecorder) where
  oneShotChange = const (CTOR.XRecorder <<< options <<< { cb: _ })

instance changeRecorder ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.TRecorder edges) ignore graph
  ) =>
  Change' ptr CTOR.Recorder graph where
  change' ptr = change' ptr
    <<< map (over CTOR.Recorder asOptions :: _ -> CTOR.XRecorder)

instance changeXRecorder ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.TRecorder edges) ignore graph
  ) =>
  Change' ptr (CTOR.XRecorder) graph where
  change' ptr w = o
    where
    { context: i, value: (CTOR.XRecorder x) } = unsafeUnWAG w

    id = reflectSymbol ptr
    changes = values
      $ objFromHomogeneous
      $ homogeneous
      $ megamap x
          { cb: setMediaRecorderCb <<< { id, cb: _ }
          }

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> changes
              }
        , value: unit
        }

instance oneShotChangeSawtoothOscOffset ::
  OneShotChange CTOR.TSawtoothOsc AudioParameter CTOR.XSawtoothOsc where
  oneShotChange = const (CTOR.XSawtoothOsc <<< options <<< { freq: _ })

instance oneShotChangeSawtoothOscOnOff ::
  OneShotChange CTOR.TSawtoothOsc AudioOnOff CTOR.XSawtoothOsc where
  oneShotChange = const (CTOR.XSawtoothOsc <<< options <<< { onOff: _ })

instance changeSawtoothOsc ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.TSawtoothOsc edges) ignore graph
  ) =>
  Change' ptr CTOR.SawtoothOsc graph where
  change' ptr = change' ptr
    <<< map (over CTOR.SawtoothOsc asOptions :: _ -> CTOR.XSawtoothOsc)

instance changeXSawtoothOsc ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.TSawtoothOsc edges) ignore graph
  ) =>
  Change' ptr CTOR.XSawtoothOsc graph where
  change' ptr w = o
    where
    { context: i, value: (CTOR.XSawtoothOsc x) } = unsafeUnWAG w

    id = reflectSymbol ptr
    changes = values
      $ objFromHomogeneous
      $ homogeneous
      $ megamap x
          { freq: setFrequency <<< { id, frequency: _ }
          , onOff: setOnOff <<< { id, onOff: _ }
          }

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> changes
              }
        , value: unit
        }

instance oneShotChangeSinOscOffset ::
  OneShotChange CTOR.TSinOsc AudioParameter CTOR.XSinOsc where
  oneShotChange = const (CTOR.XSinOsc <<< options <<< { freq: _ })

instance oneShotChangeSinOscOnOff ::
  OneShotChange CTOR.TSinOsc AudioOnOff CTOR.XSinOsc where
  oneShotChange = const (CTOR.XSinOsc <<< options <<< { onOff: _ })

instance changeSinOsc ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.TSinOsc edges) ignore graph
  ) =>
  Change' ptr CTOR.SinOsc graph where
  change' ptr = change' ptr
    <<< map (over CTOR.SinOsc asOptions :: _ -> CTOR.XSinOsc)

instance changeXSinOsc ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.TSinOsc edges) ignore graph
  ) =>
  Change' ptr CTOR.XSinOsc graph where
  change' ptr w = o
    where
    { context: i, value: (CTOR.XSinOsc x) } = unsafeUnWAG w

    id = reflectSymbol ptr
    changes = values
      $ objFromHomogeneous
      $ homogeneous
      $ megamap x
          { freq: setFrequency <<< { id, frequency: _ }
          , onOff: setOnOff <<< { id, onOff: _ }
          }

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> changes
              }
        , value: unit
        }

instance changeSpeaker ::
  ( R.Cons "speaker" (NodeC (CTOR.TSpeaker) edges) ignore graph
  ) =>
  Change' "speaker" (CTOR.Speaker) graph where
  change' _ w = w $> unit

instance oneShotChangeSquareOscOffset ::
  OneShotChange CTOR.TSquareOsc AudioParameter CTOR.XSquareOsc where
  oneShotChange = const (CTOR.XSquareOsc <<< options <<< { freq: _ })

instance oneShotChangeSquareOscOnOff ::
  OneShotChange CTOR.TSquareOsc AudioOnOff CTOR.XSquareOsc where
  oneShotChange = const (CTOR.XSquareOsc <<< options <<< { onOff: _ })

instance changeSquareOsc ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.TSquareOsc edges) ignore graph
  ) =>
  Change' ptr CTOR.SquareOsc graph where
  change' ptr = change' ptr
    <<< map (over CTOR.SquareOsc asOptions :: _ -> CTOR.XSquareOsc)

instance changeXSquareOsc ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.TSquareOsc edges) ignore graph
  ) =>
  Change' ptr CTOR.XSquareOsc graph where
  change' ptr w = o
    where
    { context: i, value: (CTOR.XSquareOsc x) } = unsafeUnWAG w

    id = reflectSymbol ptr
    changes = values
      $ objFromHomogeneous
      $ homogeneous
      $ megamap x
          { freq: setFrequency <<< { id, frequency: _ }
          , onOff: setOnOff <<< { id, onOff: _ }
          }

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> changes
              }
        , value: unit
        }

instance oneShotChangeStereoPanner ::
  OneShotChange CTOR.TStereoPanner AudioParameter CTOR.XStereoPanner where
  oneShotChange = const (CTOR.XStereoPanner <<< options <<< { pan: _ })

instance changeStereoPanner ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.TStereoPanner edges) ignore graph
  ) =>
  Change' ptr CTOR.StereoPanner graph where
  change' ptr = change' ptr <<< map
    (over CTOR.StereoPanner asOptions :: _ -> CTOR.XStereoPanner)

instance changeXStereoPanner ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.TStereoPanner edges) ignore graph
  ) =>
  Change' ptr CTOR.XStereoPanner graph where
  change' ptr w = o
    where
    { context: i, value: (CTOR.XStereoPanner x) } = unsafeUnWAG w

    id = reflectSymbol ptr
    changes = values
      $ objFromHomogeneous
      $ homogeneous
      $ megamap x { pan: setPan <<< { id, pan: _ } }

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> changes
              }
        , value: unit
        }

instance changeSubgraph0 ::
  ( IsSymbol ptr
  , IsSymbol terminus
  , R.Cons ptr (NodeC (CTOR.TSubgraph n terminus inputs env) edges) ignore graph
  , Pos n
  ) =>
  Change' ptr (CTOR.Subgraph inputs notImportant (V.Vec n env)) graph where
  change' ptr w = o
    where
    { context: i, value: (CTOR.Subgraph { envs }) } = unsafeUnWAG w

    id = reflectSymbol ptr

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <>
                  [ setSubgraph { id, envs } ]
              }
        , value: unit
        }

instance changeSubgraph1 ::
  ( IsSymbol ptr
  , IsSymbol terminus
  , R.Cons ptr (NodeC (CTOR.TSubgraph n terminus inputs env) edges) ignore graph
  , Pos n
  , Nat i
  , Lt i n
  ) =>
  Change' ptr (CTOR.XSubgraph i env) graph where
  change' ptr w = o
    where
    { context: i, value: (CTOR.XSubgraph { index, env }) } = unsafeUnWAG w

    id = reflectSymbol ptr

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <>
                  [ setSingleSubgraph { id, index, env } ]
              }
        , value: unit
        }

instance oneShotChangeTriangleOscOffset ::
  OneShotChange CTOR.TTriangleOsc AudioParameter CTOR.XTriangleOsc where
  oneShotChange = const (CTOR.XTriangleOsc <<< options <<< { freq: _ })

instance oneShotChangeTriangleOscOnOff ::
  OneShotChange CTOR.TTriangleOsc AudioOnOff CTOR.XTriangleOsc where
  oneShotChange = const (CTOR.XTriangleOsc <<< options <<< { onOff: _ })

instance changeTriangleOsc ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.TTriangleOsc edges) ignore graph
  ) =>
  Change' ptr CTOR.TriangleOsc graph where
  change' ptr = change' ptr
    <<< map (over CTOR.TriangleOsc asOptions :: _ -> CTOR.XTriangleOsc)

instance changeXTriangleOsc ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.TTriangleOsc edges) ignore graph
  ) =>
  Change' ptr CTOR.XTriangleOsc graph where
  change' ptr w = o
    where
    { context: i, value: (CTOR.XTriangleOsc x) } = unsafeUnWAG w

    id = reflectSymbol ptr
    changes = values
      $ objFromHomogeneous
      $ homogeneous
      $ megamap x
          { freq: setFrequency <<< { id, frequency: _ }
          , onOff: setOnOff <<< { id, onOff: _ }
          }

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> changes
              }
        , value: unit
        }

instance oneShotChangeTumult ::
  ( IsOversample oversample
  , Monoid oversample
  ) =>
  OneShotChange (CTOR.TTumult nSubgraphs terminus inputs)
    (Tumultuous nSubgraphs terminus inputs)
    (CTOR.Tumult nSubgraphs terminus inputs) where
  oneShotChange = const (CTOR.Tumult <<< { tumult: _ })

instance changeTumult ::
  ( IsSymbol ptr
  , IsSymbol terminus
  , Pos n
  , R.Cons ptr (NodeC (CTOR.TTumult n terminus inputs) edges) ignore graph
  ) =>
  Change' ptr (CTOR.Tumult n terminus inputs) graph where
  change' ptr w = o
    where
    { context: i, value: (CTOR.Tumult { tumult }) } = unsafeUnWAG w
    id = reflectSymbol ptr
    tms = reflectSymbol (Proxy :: _ terminus)
    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <>
                  [ setTumult
                      { id, terminus: tms, instructions: (safeUntumult tumult) }
                  ]
              }
        , value: unit
        }

instance oneShotChangeWaveshaper ::
  ( IsOversample oversample
  , Monoid oversample
  ) =>
  OneShotChange (CTOR.TWaveShaper oversample)
    BrowserFloatArray
    (CTOR.XWaveShaper) where
  oneShotChange _ floatArray = CTOR.XWaveShaper { floatArray }

instance changeWaveShaper ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC (CTOR.TWaveShaper a) edges) ignore graph
  ) =>
  Change' ptr (CTOR.WaveShaper oversample) graph where
  change' ptr w = change' ptr
    ( map
        ( over CTOR.WaveShaper (\{ floatArray } -> { floatArray })
            :: _ -> CTOR.XWaveShaper
        )
        w
    )

instance changeXWaveShaper ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC (CTOR.TWaveShaper a) edges) ignore graph
  ) =>
  Change' ptr (CTOR.XWaveShaper) graph where
  change' ptr w = o
    where
    { context: i, value: (CTOR.XWaveShaper { floatArray }) } = unsafeUnWAG w

    id = reflectSymbol ptr

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <>
                  [ setWaveShaperCurve { id, curve: floatArray } ]
              }
        , value: unit
        }
