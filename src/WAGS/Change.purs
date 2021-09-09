module WAGS.Change where

import Prelude

import Control.Comonad (extract)
import Data.Maybe (Maybe(..), maybe)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Vec as V
import Prim.Row as R
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Prim.Symbol as Sym
import Record as Record
import Type.Proxy (Proxy(..))
import WAGS.ConstructEdges (class ConstructEdges, constructEdges)
import WAGS.Control.Indexed (IxWAG(..))
import WAGS.Control.Types (WAG, unsafeUnWAG, unsafeWAG)
import WAGS.Graph.AudioUnit (APOnOff, AudioWorkletNodeOptions(..), OnOff)
import WAGS.Graph.AudioUnit as CTOR
import WAGS.Graph.Graph (Graph)
import WAGS.Graph.Node (NodeC)
import WAGS.Graph.Oversample (class IsOversample, reflectOversample)
import WAGS.Graph.Paramable (class Paramable, paramize, class OnOffable, onOffIze)
import WAGS.Graph.Parameter (class MM, AudioParameter_, AudioParameter, mm)
import WAGS.Interpret (class AudioInterpret, setAnalyserNodeCb, setAttack, setAudioWorkletParameter, setBuffer, setBufferOffset, setConvolverBuffer, setDelay, setFrequency, setGain, setKnee, setLoopEnd, setLoopStart, setMediaRecorderCb, setOffset, setOnOff, setPan, setPeriodicOsc, setPeriodicOscV, setPlaybackRate, setQ, setRatio, setRelease, setThreshold, setWaveShaperCurve)
import WAGS.Rendered (Oversample)
import WAGS.Util (class MakePrefixIfNeeded, class CoercePrefixToString)
import WAGS.WebAPI (AnalyserNodeCb, BrowserAudioBuffer, BrowserFloatArray, BrowserMicrophone, BrowserPeriodicWave, MediaRecorderCb)

apure = pure :: forall a. a -> AudioParameter_ a

type Change'Type (ptr :: Symbol) (a :: Type) (graph :: Graph)
  =
  forall proxy audio engine proof res
   . AudioInterpret audio engine
  => proxy ptr
  -> WAG audio engine proof res graph a
  -> WAG audio engine proof res graph Unit

-- | Change an audio unit `node` in `igraph` with index `ptr`, outputting the changed node.
class Change' (ptr :: Symbol) (a :: Type) (graph :: Graph) where
  change' :: Change'Type ptr a graph

type CanBeChangedType (sym :: Symbol) (val :: Type) (ptr :: Symbol) (graph :: Graph)
  =
  forall proxy audio engine proof res
   . AudioInterpret audio engine
  => proxy sym
  -> val
  -> proxy ptr
  -> WAG audio engine proof res graph Unit
  -> WAG audio engine proof res graph Unit

class CanBeChanged (sym :: Symbol) (val :: Type) (ptr :: Symbol) (graph :: Graph) where
  canBeChanged :: CanBeChangedType sym val ptr graph

type Change''Type (rl :: RL.RowList Type) (ptr :: Symbol) (a :: Row Type) (graph :: Graph)
  =
  forall proxyA proxyB audio engine proof res
   . AudioInterpret audio engine
  => proxyA rl
  -> proxyB ptr
  -> WAG audio engine proof res graph { | a }
  -> WAG audio engine proof res graph Unit

-- | Change an audio unit `node` in `igraph` with index `ptr`, outputting the changed node.
class Change'' (rl :: RL.RowList Type) (ptr :: Symbol) (a :: Row Type) (graph :: Graph) where
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

type ChangeInternalSig (prefix :: Type) (map :: Type) (r :: Row Type) (graph :: Graph)
  =
  forall proxyPrefix proxyMap audio engine proof res
   . AudioInterpret audio engine
  => proxyPrefix prefix
  -> proxyMap map
  -> WAG audio engine proof res graph { | r }
  -> WAG audio engine proof res graph Unit

class ChangeInternal (prefix :: Type) (map :: Type) (r :: Row Type) (graph :: Graph) where
  changeInternal :: ChangeInternalSig prefix map r graph

class ChangeRL (rl :: RL.RowList Type) (prefix :: Type) (map :: Type) (r :: Row Type) (graph :: Graph) where
  changeRL
    :: forall proxyPrefix proxyMap proxy audio engine proof res
     . AudioInterpret audio engine
    => proxy rl
    -> proxyPrefix prefix
    -> proxyMap map
    -> WAG audio engine proof res graph { | r }
    -> WAG audio engine proof res graph Unit

instance changeInternalAll :: (RL.RowToList r rl, ChangeRL rl prefix map r graph) => ChangeInternal prefix map r graph where
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

    _ /\ _ /\ (node /\ edges) = constructEdges (Proxy :: _ prefix') (Proxy :: _ map) (Record.get (Proxy :: _ key) rx)

    step1 = change' (Proxy :: _ newKey) (r $> node)

    step2 =
      (changeInternal :: ChangeInternalSig newPrefix newMap edges graph) Proxy Proxy
        (step1 $> edges)

    step3 = changeRL (Proxy :: _ rest) (Proxy :: _ prefix) (Proxy :: _ map) (step2 $> rx)

ichange
  :: forall r audio engine proof res inGraph
   . AudioInterpret audio engine
  => Change r inGraph
  => { | r }
  -> IxWAG audio engine proof res inGraph inGraph Unit
ichange r = IxWAG (change <<< (<$) r)

class PushAPOnOffToEnd (i :: RL.RowList Type) (o :: RL.RowList Type) | i -> o

instance pushAPOnOffToEndNil :: PushAPOnOffToEnd RL.Nil RL.Nil
else instance pushAPOnOffToEndOO :: PushAPOnOffToEnd (RL.Cons "onOff" a RL.Nil) (RL.Cons "onOff" a RL.Nil)
else instance pushAPOnOffToEndOOC :: PushAPOnOffToEnd (RL.Cons x y (RL.Cons "onOff" a z)) o => PushAPOnOffToEnd (RL.Cons "onOff" a (RL.Cons x y z)) o
else instance pushAPOnOffToEndRest :: PushAPOnOffToEnd c o => PushAPOnOffToEnd (RL.Cons a b c) (RL.Cons a b o)

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
  change' px w = change' px (map apure w)

instance changeOnOff ::
  Change' ptr APOnOff graph =>
  Change' ptr OnOff graph where
  change' px w = change' px (map apure w)

instance changeBrowserAudioBuffer ::
  ( R.Cons ptr tau' ignore graph
  , Detup tau' tau
  , Monoid tau
  , OneShotChange tau BrowserAudioBuffer au
  , Change' ptr au graph
  ) =>
  Change' ptr BrowserAudioBuffer graph where
  change' px w = change' px (oneShotChange (mempty :: tau) <$> w)

instance changeAudioParameter ::
  ( R.Cons ptr tau' ignore graph
  , Detup tau' tau
  , Monoid tau
  , OneShotChange tau (AudioParameter_ param) au
  , Change' ptr au graph
  ) =>
  Change' ptr (AudioParameter_ param) graph where
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
  , OneShotChange tau (V.Vec size Number /\ V.Vec size Number) au
  , Change' ptr au graph
  ) =>
  Change' ptr (V.Vec size Number /\ V.Vec size Number) graph where
  change' px w = change' px (oneShotChange (mempty :: tau) <$> w)

instance changeRec ::
  ( RL.RowToList r rl'
  , PushAPOnOffToEnd rl' rl
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
  canBeChanged sym val ptr w = canBeChanged sym (apure val) ptr w

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

    nn = reflectSymbol ptr

    argA_Changes = [ setFrequency nn val ]

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> argA_Changes
              }
        , value: unit
        }

class APOnOffable (tau :: Type)

instance onOffableConstant :: APOnOffable CTOR.TConstant

instance onOffablePlayBuf :: APOnOffable CTOR.TPlayBuf

instance onOffableLoopBuf :: APOnOffable CTOR.TLoopBuf

instance onOffableSinOsc :: APOnOffable CTOR.TSinOsc

instance onOffableSawtoothOsc :: APOnOffable CTOR.TSawtoothOsc

instance onOffableSquareOsc :: APOnOffable CTOR.TSquareOsc

instance onOffablePeriodicOsc :: APOnOffable CTOR.TPeriodicOsc

instance onOffableTriangleOsc :: APOnOffable CTOR.TTriangleOsc

instance canBeChangedOnOffSimple ::
  ( CanBeChanged "onOff" APOnOff ptr graph
  ) =>
  CanBeChanged "onOff" OnOff ptr graph where
  canBeChanged sym val ptr w = canBeChanged sym (apure val) ptr w

instance canBeChangedAPOnOff ::
  ( IsSymbol ptr
  , R.Cons ptr tau' ignore graph
  , Detup tau' tau
  , APOnOffable tau
  ) =>
  CanBeChanged "onOff" APOnOff ptr graph where
  canBeChanged _ val ptr w = o
    where
    { context: i } = unsafeUnWAG w

    nn = reflectSymbol ptr

    argA_Changes = [ setOnOff nn val ]

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
  canBeChanged sym val ptr w = canBeChanged sym (apure val) ptr w

instance canBeChangedQ ::
  ( IsSymbol ptr
  , R.Cons ptr tau' ignore graph
  , Detup tau' tau
  , Qable tau
  ) =>
  CanBeChanged "q" AudioParameter ptr graph where
  canBeChanged _ val ptr w = o
    where
    { context: i } = unsafeUnWAG w

    nn = reflectSymbol ptr

    argA_Changes = [ setQ nn val ]

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
  canBeChanged sym val ptr w = canBeChanged sym (apure val) ptr w

instance canBeChangedGain ::
  ( IsSymbol ptr
  , R.Cons ptr tau' ignore graph
  , Detup tau' tau
  , Gainable tau
  ) =>
  CanBeChanged "gain" AudioParameter ptr graph where
  canBeChanged _ val ptr w = o
    where
    { context: i } = unsafeUnWAG w

    nn = reflectSymbol ptr

    argA_Changes = [ setGain nn val ]

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
  canBeChanged sym val ptr w = canBeChanged sym (apure val) ptr w

instance canBeChangedOffset ::
  ( IsSymbol ptr
  , R.Cons ptr tau' ignore graph
  , Detup tau' tau
  , Offsetable tau
  ) =>
  CanBeChanged "offset" AudioParameter ptr graph where
  canBeChanged _ val ptr w = o
    where
    { context: i } = unsafeUnWAG w

    nn = reflectSymbol ptr

    argA_Changes = [ setOffset nn val ]

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

    nn = reflectSymbol ptr

    argA_Changes = [ setLoopStart nn val ]

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

    nn = reflectSymbol ptr

    argA_Changes = [ setLoopEnd nn val ]

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> argA_Changes
              }
        , value: unit
        }

class BufferOffsetable (tau :: Type)

instance bufferOffsetableLoopBuf :: BufferOffsetable CTOR.TLoopBuf

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

    nn = reflectSymbol ptr

    argA_Changes = [ setBufferOffset nn val ]

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

instance canBeChangedPlaybackRateN ::
  ( CanBeChanged "playbackRate" AudioParameter ptr graph
  ) =>
  CanBeChanged "playbackRate" Number ptr graph where
  canBeChanged sym val ptr w = canBeChanged sym ((pure :: forall a. a -> AudioParameter_ a) val) ptr w

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

    nn = reflectSymbol ptr

    argA_Changes = [ setPlaybackRate nn val ]

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

    nn = reflectSymbol ptr

    argA_Changes = [ setBuffer nn val ]

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

    nn = reflectSymbol ptr

    argA_Changes = [ setPeriodicOsc nn val ]

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
  , Waveformable tau
  ) =>
  CanBeChanged "waveform" (V.Vec size Number /\ V.Vec size Number) ptr graph where
  canBeChanged _ val ptr w = o
    where
    { context: i } = unsafeUnWAG w

    nn = reflectSymbol ptr

    argA_Changes = [ setPeriodicOscV nn val ]

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> argA_Changes
              }
        , value: unit
        }

class Thresholdable (tau :: Type)

instance thresholdableDynamicsCompressor :: Thresholdable CTOR.TDynamicsCompressor

instance canBeChangedThresholdN ::
  ( CanBeChanged "threshold" AudioParameter ptr graph
  ) =>
  CanBeChanged "threshold" Number ptr graph where
  canBeChanged sym val ptr w = canBeChanged sym (apure val) ptr w

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

    nn = reflectSymbol ptr

    argA_Changes = [ setThreshold nn val ]

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
  canBeChanged sym val ptr w = canBeChanged sym (apure val) ptr w

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

    nn = reflectSymbol ptr

    argA_Changes = [ setRatio nn val ]

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
  canBeChanged sym val ptr w = canBeChanged sym (apure val) ptr w

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

    nn = reflectSymbol ptr

    argA_Changes = [ setKnee nn val ]

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
  canBeChanged sym val ptr w = canBeChanged sym (apure val) ptr w

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

    nn = reflectSymbol ptr

    argA_Changes = [ setAttack nn val ]

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
  canBeChanged sym val ptr w = canBeChanged sym (apure val) ptr w

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

    nn = reflectSymbol ptr

    argA_Changes = [ setRelease nn val ]

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

    nn = reflectSymbol ptr

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> [ setAnalyserNodeCb nn cb ]
              }
        , value: unit
        }

instance oneShotChangeAllpass :: OneShotChange CTOR.TAllpass AudioParameter (CTOR.Allpass (Maybe AudioParameter) (Maybe AudioParameter)) where
  oneShotChange _ freq = CTOR.Allpass (Just freq) Nothing

instance changeAllpass ::
  ( IsSymbol ptr
  , MM mArgA (Maybe argA)
  , Paramable argA
  , MM mArgB (Maybe argB)
  , Paramable argB
  , R.Cons ptr (NodeC CTOR.TAllpass edges) ignore graph
  ) =>
  Change' ptr (CTOR.Allpass mArgA mArgB) graph where
  change' ptr w = o
    where
    { context: i, value: (CTOR.Allpass argA argB) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    argA_iv' = paramize <$> (mm argA)

    argA_Changes = maybe [] (\argA_iv'' -> [ setFrequency nn argA_iv'' ]) argA_iv'

    argB_iv' = paramize <$> (mm argB)

    argB_Changes = maybe [] (\argB_iv'' -> [ setQ nn argB_iv'' ]) argB_iv'

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> argA_Changes <> argB_Changes
              }
        , value: unit
        }

class InterpretParameters (parameterData :: Row Type) (parameterRL :: RowList Type) (parameters :: Row Type) where
  interpretParameters
    :: forall proxyR proxyRL audio engine
     . AudioInterpret audio engine
    => String
    -> proxyR parameterData
    -> proxyRL parameterRL
    -> { | parameters }
    -> Array (audio -> engine)

instance interpretParametersNil :: InterpretParameters parameterData RL.Nil parameters where
  interpretParameters _ _ _ _ = []

instance interpretParametersCons ::
  ( IsSymbol key
  , R.Cons key val parameterData' parameterData
  , R.Cons key AudioParameter parameters' parameters
  , InterpretParameters parameterData rest parameters
  ) =>
  InterpretParameters parameterData (RL.Cons key AudioParameter rest) parameters where
  interpretParameters nn pd _ p =
    let
      px = Proxy :: _ key
    in
      [ setAudioWorkletParameter nn (reflectSymbol px) (Record.get px p) ]
        <> interpretParameters nn pd (Proxy :: _ rest) p

instance changeAudioWorkletNode ::
  ( IsSymbol ptr
  , RowToList parameterData parameterDataRL
  , InterpretParameters optionsParamaterData parameterDataRL parameterData
  , R.Cons ptr (NodeC (CTOR.TAudioWorkletNode node numberOfInputs numberOfOutputs outputChannelCount parameterData processorOptions) edges) ignore graph
  ) =>
  Change' ptr (CTOR.AudioWorkletNode node numberOfInputs numberOfOutputs outputChannelCount parameterData processorOptions) graph where
  change' ptr w = o
    where
    { context: i, value: (CTOR.AudioWorkletNode _ (AudioWorkletNodeOptions options)) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <>
                  ( interpretParameters nn (Proxy :: _ optionsParamaterData)
                      (Proxy :: _ parameterDataRL)
                      options.parameterData
                  )
              }
        , value: unit
        }

instance oneShotChangeBandpass :: OneShotChange CTOR.TBandpass AudioParameter (CTOR.Bandpass (Maybe AudioParameter) (Maybe AudioParameter)) where
  oneShotChange _ freq = CTOR.Bandpass (Just freq) Nothing

instance changeBandpass ::
  ( IsSymbol ptr
  , MM mArgA (Maybe argA)
  , Paramable argA
  , MM mArgB (Maybe argB)
  , Paramable argB
  , R.Cons ptr (NodeC CTOR.TBandpass edges) ignore graph
  ) =>
  Change' ptr (CTOR.Bandpass mArgA mArgB) graph where
  change' ptr w = o
    where
    { context: i, value: (CTOR.Bandpass argA argB) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    argA_iv' = paramize <$> (mm argA)

    argA_Changes = maybe [] (\argA_iv'' -> [ setFrequency nn argA_iv'' ]) argA_iv'

    argB_iv' = paramize <$> (mm argB)

    argB_Changes = maybe [] (\argB_iv'' -> [ setQ nn argB_iv'' ]) argB_iv'

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> argA_Changes <> argB_Changes
              }
        , value: unit
        }

instance oneShotChangeConstant :: OneShotChange CTOR.TConstant AudioParameter (CTOR.Constant (Maybe APOnOff) (Maybe AudioParameter)) where
  oneShotChange _ offset = CTOR.Constant Nothing (Just offset)

instance oneShotChangeConstantOO :: OneShotChange CTOR.TConstant APOnOff (CTOR.Constant (Maybe APOnOff) (Maybe AudioParameter)) where
  oneShotChange _ oo = CTOR.Constant (Just oo) Nothing

instance changeConstant ::
  ( IsSymbol ptr
  , MM mAPOnOff (Maybe onOff)
  , OnOffable onOff
  , MM mArgA (Maybe argA)
  , Paramable argA
  , R.Cons ptr (NodeC CTOR.TConstant edges) ignore graph
  ) =>
  Change' ptr (CTOR.Constant mAPOnOff mArgA) graph where
  change' ptr w = o
    where
    { context: i, value: (CTOR.Constant onOff argA) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    oo_Changes = maybe [] (\onOff' -> [ setOnOff nn (onOffIze onOff') ]) (mm onOff)

    argA_iv' = paramize <$> (mm argA)

    argA_Changes = maybe [] (\argA_iv'' -> [ setOffset nn argA_iv'' ]) argA_iv'

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> (argA_Changes <> oo_Changes)
              }
        , value: unit
        }

instance oneShotChangeConvolver :: OneShotChange CTOR.TConvolver BrowserAudioBuffer (CTOR.Convolver BrowserAudioBuffer) where
  oneShotChange _ bab = CTOR.Convolver bab

instance changeConvolver ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.TConvolver edges) ignore graph
  ) =>
  Change' ptr (CTOR.Convolver BrowserAudioBuffer) graph where
  change' ptr w = o
    where
    { context: i, value: (CTOR.Convolver buffer) } = unsafeUnWAG w

    nn = reflectSymbol ptr
    o =
      unsafeWAG
        { context: i { instructions = i.instructions <> [ setConvolverBuffer nn buffer ] }
        , value: unit
        }

instance oneShotChangeDelay :: OneShotChange CTOR.TDelay AudioParameter (CTOR.Delay (Maybe AudioParameter)) where
  oneShotChange _ delay = CTOR.Delay (Just delay)

instance changeDelay ::
  ( IsSymbol ptr
  , MM mArgA (Maybe argA)
  , Paramable argA
  , R.Cons ptr (NodeC CTOR.TDelay edges) ignore graph
  ) =>
  Change' ptr (CTOR.Delay mArgA) graph where
  change' ptr w = o
    where
    { context: i, value: (CTOR.Delay argA) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    argA_iv' = paramize <$> (mm argA)

    argA_Changes = maybe [] (\argA_iv'' -> [ setDelay nn argA_iv'' ]) argA_iv'

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> argA_Changes
              }
        , value: unit
        }

instance changeDynamicsCompressor ::
  ( IsSymbol ptr
  , MM mArgA (Maybe argA)
  , Paramable argA
  , MM mArgB (Maybe argB)
  , Paramable argB
  , MM mArgC (Maybe argC)
  , Paramable argC
  , MM mArgD (Maybe argD)
  , Paramable argD
  , MM mArgE (Maybe argE)
  , Paramable argE
  , R.Cons ptr (NodeC CTOR.TDynamicsCompressor edges) ignore graph
  ) =>
  Change' ptr (CTOR.DynamicsCompressor mArgA mArgB mArgC mArgD mArgE) graph where
  change' ptr w = o
    where
    { context: i, value: (CTOR.DynamicsCompressor argA argB argC argD argE) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    argA_iv' = paramize <$> (mm argA)

    argA_Changes = maybe [] (\argA_iv'' -> [ setThreshold nn argA_iv'' ]) argA_iv'

    argB_iv' = paramize <$> (mm argB)

    argB_Changes = maybe [] (\argB_iv'' -> [ setKnee nn argB_iv'' ]) argB_iv'

    argC_iv' = paramize <$> (mm argC)

    argC_Changes = maybe [] (\argC_iv'' -> [ setRatio nn argC_iv'' ]) argC_iv'

    argD_iv' = paramize <$> (mm argD)

    argD_Changes = maybe [] (\argD_iv'' -> [ setAttack nn argD_iv'' ]) argD_iv'

    argE_iv' = paramize <$> (mm argE)

    argE_Changes = maybe [] (\argE_iv'' -> [ setRelease nn argE_iv'' ]) argE_iv'

    o =
      unsafeWAG
        { context:
            i
              { instructions =
                  i.instructions
                    <> argA_Changes
                    <> argB_Changes
                    <> argC_Changes
                    <> argD_Changes
                    <> argE_Changes
              }
        , value: unit
        }

instance oneShotChangeGain :: OneShotChange CTOR.TGain AudioParameter (CTOR.Gain (Maybe AudioParameter)) where
  oneShotChange _ gain = CTOR.Gain (Just gain)

instance changeGain ::
  ( IsSymbol ptr
  , MM mArgA (Maybe argA)
  , Paramable argA
  , R.Cons ptr (NodeC CTOR.TGain edges) ignore graph
  ) =>
  Change' ptr (CTOR.Gain mArgA) graph where
  change' ptr w = o
    where
    { context: i, value: (CTOR.Gain argA) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    argA_iv' = paramize <$> (mm argA)

    argA_Changes = maybe [] (\argA_iv'' -> [ setGain nn argA_iv'' ]) argA_iv'

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> argA_Changes
              }
        , value: unit
        }

instance oneShotChangeHighpass :: OneShotChange CTOR.THighpass AudioParameter (CTOR.Highpass (Maybe AudioParameter) (Maybe AudioParameter)) where
  oneShotChange _ freq = CTOR.Highpass (Just freq) Nothing

instance changeHighpass ::
  ( IsSymbol ptr
  , MM mArgA (Maybe argA)
  , Paramable argA
  , MM mArgB (Maybe argB)
  , Paramable argB
  , R.Cons ptr (NodeC CTOR.THighpass edges) ignore graph
  ) =>
  Change' ptr (CTOR.Highpass mArgA mArgB) graph where
  change' ptr w = o
    where
    { context: i, value: (CTOR.Highpass argA argB) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    argA_iv' = paramize <$> (mm argA)

    argA_Changes = maybe [] (\argA_iv'' -> [ setFrequency nn argA_iv'' ]) argA_iv'

    argB_iv' = paramize <$> (mm argB)

    argB_Changes = maybe [] (\argB_iv'' -> [ setQ nn argB_iv'' ]) argB_iv'

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> argA_Changes <> argB_Changes
              }
        , value: unit
        }

instance oneShotChangeHighshelf :: OneShotChange CTOR.THighshelf AudioParameter (CTOR.Highshelf (Maybe AudioParameter) (Maybe AudioParameter)) where
  oneShotChange _ freq = CTOR.Highshelf (Just freq) Nothing

instance changeHighshelf ::
  ( IsSymbol ptr
  , MM mArgA (Maybe argA)
  , Paramable argA
  , MM mArgB (Maybe argB)
  , Paramable argB
  , R.Cons ptr (NodeC CTOR.THighshelf edges) ignore graph
  ) =>
  Change' ptr (CTOR.Highshelf mArgA mArgB) graph where
  change' ptr w = o
    where
    { context: i, value: (CTOR.Highshelf argA argB) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    argA_iv' = paramize <$> (mm argA)

    argA_Changes = maybe [] (\argA_iv'' -> [ setFrequency nn argA_iv'' ]) argA_iv'

    argB_iv' = paramize <$> (mm argB)

    argB_Changes = maybe [] (\argB_iv'' -> [ setGain nn argB_iv'' ]) argB_iv'

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> argA_Changes <> argB_Changes
              }
        , value: unit
        }

instance oneShotChangeLoopBuf :: OneShotChange CTOR.TLoopBuf AudioParameter (CTOR.LoopBuf (Maybe BrowserAudioBuffer) (Maybe APOnOff) (Maybe AudioParameter) (Maybe Number) (Maybe Number)) where
  oneShotChange _ rate = CTOR.LoopBuf Nothing Nothing (Just rate) Nothing Nothing

instance oneShotChangeLoopBufOO :: OneShotChange CTOR.TLoopBuf APOnOff (CTOR.LoopBuf (Maybe BrowserAudioBuffer) (Maybe APOnOff) (Maybe AudioParameter) (Maybe Number) (Maybe Number)) where
  oneShotChange _ onOff = CTOR.LoopBuf Nothing (Just onOff) Nothing Nothing Nothing

instance oneShotChangeLoopBufProxy :: OneShotChange CTOR.TLoopBuf BrowserAudioBuffer (CTOR.LoopBuf (Maybe BrowserAudioBuffer) (Maybe APOnOff) (Maybe AudioParameter) (Maybe Number) (Maybe Number)) where
  oneShotChange _ buffer = CTOR.LoopBuf (Just buffer) Nothing Nothing Nothing Nothing

instance changeLoopBuf ::
  ( IsSymbol ptr
  , MM mBuffer (Maybe BrowserAudioBuffer)
  , MM mAPOnOff (Maybe onOff)
  , OnOffable onOff
  , MM mArgA (Maybe argA)
  , MM mLoopStart (Maybe Number)
  , MM mLoopEnd (Maybe Number)
  , Paramable argA
  , R.Cons ptr (NodeC CTOR.TLoopBuf edges) ignore graph
  ) =>
  Change' ptr (CTOR.LoopBuf mBuffer mAPOnOff mArgA mLoopStart mLoopEnd) graph where
  change' ptr w = o
    where
    { context: i, value: (CTOR.LoopBuf buffer onOff argA loopStart loopEnd) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    buffer_Changes = maybe [] (\buffer' -> [ setBuffer nn buffer' ]) (mm buffer)

    oo_Changes = maybe [] (\onOff' -> [ setOnOff nn (onOffIze onOff') ]) (mm onOff)

    argA_iv' = paramize <$> (mm argA)

    argA_Changes = maybe [] (\argA_iv'' -> [ setPlaybackRate nn argA_iv'' ]) argA_iv'

    loopStart_Changes = maybe [] (\loopStart' -> [ setLoopStart nn loopStart' ]) (mm loopStart)

    loopEnd_Changes = maybe [] (\loopEnd' -> [ setLoopEnd nn loopEnd' ]) (mm loopEnd)

    o =
      unsafeWAG
        { context:
            i
              { instructions =
                  i.instructions
                    <> buffer_Changes
                    <> oo_Changes
                    <> argA_Changes
                    <> loopStart_Changes
                    <> loopEnd_Changes
              }
        , value: unit
        }

instance oneShotChangeLowpass :: OneShotChange CTOR.TLowpass AudioParameter (CTOR.Lowpass (Maybe AudioParameter) (Maybe AudioParameter)) where
  oneShotChange _ freq = CTOR.Lowpass (Just freq) Nothing

instance changeLowpass ::
  ( IsSymbol ptr
  , MM mArgA (Maybe argA)
  , Paramable argA
  , MM mArgB (Maybe argB)
  , Paramable argB
  , R.Cons ptr (NodeC CTOR.TLowpass edges) ignore graph
  ) =>
  Change' ptr (CTOR.Lowpass mArgA mArgB) graph where
  change' ptr w = o
    where
    { context: i, value: (CTOR.Lowpass argA argB) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    argA_iv' = paramize <$> (mm argA)

    argA_Changes = maybe [] (\argA_iv'' -> [ setFrequency nn argA_iv'' ]) argA_iv'

    argB_iv' = paramize <$> (mm argB)

    argB_Changes = maybe [] (\argB_iv'' -> [ setQ nn argB_iv'' ]) argB_iv'

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> argA_Changes <> argB_Changes
              }
        , value: unit
        }

instance oneShotChangeLowshelf :: OneShotChange CTOR.TLowshelf AudioParameter (CTOR.Lowshelf (Maybe AudioParameter) (Maybe AudioParameter)) where
  oneShotChange _ freq = CTOR.Lowshelf (Just freq) Nothing

instance changeLowshelf ::
  ( IsSymbol ptr
  , MM mArgA (Maybe argA)
  , Paramable argA
  , MM mArgB (Maybe argB)
  , Paramable argB
  , R.Cons ptr (NodeC CTOR.TLowshelf edges) ignore graph
  ) =>
  Change' ptr (CTOR.Lowshelf mArgA mArgB) graph where
  change' ptr w = o
    where
    { context: i, value: (CTOR.Lowshelf argA argB) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    argA_iv' = paramize <$> (mm argA)

    argA_Changes = maybe [] (\argA_iv'' -> [ setFrequency nn argA_iv'' ]) argA_iv'

    argB_iv' = paramize <$> (mm argB)

    argB_Changes = maybe [] (\argB_iv'' -> [ setGain nn argB_iv'' ]) argB_iv'

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> argA_Changes <> argB_Changes
              }
        , value: unit
        }

instance changeMicrophone ::
  ( R.Cons "microphone" (NodeC CTOR.TMicrophone edges) ignore graph
  ) =>
  Change' "microphone" (CTOR.Microphone BrowserMicrophone) graph where
  -- for now, we make this a no-op as it does not make sense in the
  -- web api to change the microphone
  -- in future iterations, we can look into expanding this once changing
  -- the microphone has semantic meaning
  change' _ w = w $> unit

instance oneShotChangeNotch :: OneShotChange CTOR.TNotch AudioParameter (CTOR.Notch (Maybe AudioParameter) (Maybe AudioParameter)) where
  oneShotChange _ freq = CTOR.Notch (Just freq) Nothing

instance changeNotch ::
  ( IsSymbol ptr
  , MM mArgA (Maybe argA)
  , Paramable argA
  , MM mArgB (Maybe argB)
  , Paramable argB
  , R.Cons ptr (NodeC CTOR.TNotch edges) ignore graph
  ) =>
  Change' ptr (CTOR.Notch mArgA mArgB) graph where
  change' ptr w = o
    where
    { context: i, value: (CTOR.Notch argA argB) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    argA_iv' = paramize <$> (mm argA)

    argA_Changes = maybe [] (\argA_iv'' -> [ setFrequency nn argA_iv'' ]) argA_iv'

    argB_iv' = paramize <$> (mm argB)

    argB_Changes = maybe [] (\argB_iv'' -> [ setQ nn argB_iv'' ]) argB_iv'

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> argA_Changes <> argB_Changes
              }
        , value: unit
        }

instance oneShotChangePeaking :: OneShotChange CTOR.TPeaking AudioParameter (CTOR.Peaking (Maybe AudioParameter) (Maybe AudioParameter) (Maybe AudioParameter)) where
  oneShotChange _ freq = CTOR.Peaking (Just freq) Nothing Nothing

instance changePeaking ::
  ( IsSymbol ptr
  , MM mArgA (Maybe argA)
  , Paramable argA
  , MM mArgB (Maybe argB)
  , Paramable argB
  , MM mArgC (Maybe argC)
  , Paramable argC
  , R.Cons ptr (NodeC CTOR.TPeaking edges) ignore graph
  ) =>
  Change' ptr (CTOR.Peaking mArgA mArgB mArgC) graph where
  change' ptr w = o
    where
    { context: i, value: (CTOR.Peaking argA argB argC) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    argA_iv' = paramize <$> (mm argA)

    argA_Changes = maybe [] (\argA_iv'' -> [ setFrequency nn argA_iv'' ]) argA_iv'

    argB_iv' = paramize <$> (mm argB)

    argB_Changes = maybe [] (\argB_iv'' -> [ setQ nn argB_iv'' ]) argB_iv'

    argC_iv' = paramize <$> (mm argC)

    argC_Changes = maybe [] (\argC_iv'' -> [ setGain nn argC_iv'' ]) argC_iv'

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> argA_Changes <> argB_Changes <> argC_Changes
              }
        , value: unit
        }

instance oneShotChangePeriodicOsc :: OneShotChange CTOR.TPeriodicOsc AudioParameter (CTOR.PeriodicOsc (Maybe BrowserPeriodicWave) (Maybe APOnOff) (Maybe AudioParameter)) where
  oneShotChange _ freq = CTOR.PeriodicOsc Nothing Nothing (Just freq)

instance oneShotChangePeriodicOscOO :: OneShotChange CTOR.TPeriodicOsc APOnOff (CTOR.PeriodicOsc (Maybe BrowserPeriodicWave) (Maybe APOnOff) (Maybe AudioParameter)) where
  oneShotChange _ oo = CTOR.PeriodicOsc Nothing (Just oo) Nothing

instance oneShotChangePeriodicOscProxy :: OneShotChange CTOR.TPeriodicOsc BrowserPeriodicWave (CTOR.PeriodicOsc (Maybe BrowserPeriodicWave) (Maybe APOnOff) (Maybe AudioParameter)) where
  oneShotChange _ osc = CTOR.PeriodicOsc (Just osc) Nothing Nothing

instance oneShotChangePeriodicOscVec :: OneShotChange CTOR.TPeriodicOsc (V.Vec size Number /\ V.Vec size Number) (CTOR.PeriodicOsc (Maybe (V.Vec size Number /\ V.Vec size Number)) (Maybe APOnOff) (Maybe AudioParameter)) where
  oneShotChange _ osc = CTOR.PeriodicOsc (Just osc) Nothing Nothing

class ChangePeriodicOsc a where
  setPosc :: forall audio engine. AudioInterpret audio engine => String -> a -> audio -> engine

instance changePeriodicOscV :: ChangePeriodicOsc (V.Vec size Number /\ V.Vec size Number) where
  setPosc = setPeriodicOscV

instance changePeriodicOscS :: ChangePeriodicOsc BrowserPeriodicWave where
  setPosc = setPeriodicOsc

instance changePeriodicOsc ::
  ( IsSymbol ptr
  , MM mOsc (Maybe osc)
  , MM mAPOnOff (Maybe onOff)
  , OnOffable onOff
  , ChangePeriodicOsc osc
  , MM mArgA (Maybe argA)
  , Paramable argA
  , R.Cons ptr (NodeC CTOR.TPeriodicOsc edges) ignore graph
  ) =>
  Change' ptr (CTOR.PeriodicOsc mOsc mAPOnOff mArgA) graph where
  change' ptr w = o
    where
    { context: i, value: (CTOR.PeriodicOsc periodicWave onOff argA) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    pw_Changes = maybe [] (\periodicWave' -> [ setPosc nn periodicWave' ]) (mm periodicWave)

    oo_Changes = maybe [] (\onOff' -> [ setOnOff nn (onOffIze onOff') ]) (mm onOff)

    argA_iv' = paramize <$> (mm argA)

    argA_Changes = maybe [] (\argA_iv'' -> [ setFrequency nn argA_iv'' ]) argA_iv'

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> pw_Changes <> oo_Changes <> argA_Changes
              }
        , value: unit
        }

instance oneShotChangePlayBuf :: OneShotChange CTOR.TPlayBuf AudioParameter (CTOR.PlayBuf (Maybe BrowserAudioBuffer) (Maybe Number) (Maybe APOnOff) (Maybe AudioParameter)) where
  oneShotChange _ rate = CTOR.PlayBuf Nothing Nothing Nothing (Just rate)

instance oneShotChangePlayBufOO :: OneShotChange CTOR.TPlayBuf APOnOff (CTOR.PlayBuf (Maybe BrowserAudioBuffer) (Maybe Number) (Maybe APOnOff) (Maybe AudioParameter)) where
  oneShotChange _ oo = CTOR.PlayBuf Nothing Nothing (Just oo) Nothing

instance oneShotChangePlayBufProxy :: OneShotChange CTOR.TPlayBuf BrowserAudioBuffer (CTOR.PlayBuf (Maybe BrowserAudioBuffer) (Maybe Number) (Maybe APOnOff) (Maybe AudioParameter)) where
  oneShotChange _ buffer = CTOR.PlayBuf (Just buffer) Nothing Nothing Nothing

instance changePlayBuf ::
  ( IsSymbol ptr
  , MM mBuffer (Maybe BrowserAudioBuffer)
  , MM mOffset (Maybe Number)
  , MM mAPOnOff (Maybe onOff)
  , OnOffable onOff
  , MM mArgA (Maybe argA)
  , Paramable argA
  , R.Cons ptr (NodeC CTOR.TPlayBuf edges) ignore graph
  ) =>
  Change' ptr (CTOR.PlayBuf mBuffer mOffset mAPOnOff mArgA) graph where
  change' ptr w = o
    where
    { context: i, value: (CTOR.PlayBuf buffer offset onOff argA) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    buffer_Changes = maybe [] (\buffer' -> [ setBuffer nn buffer' ]) (mm buffer)

    offset_Changes = maybe [] (\offset' -> [ setBufferOffset nn offset' ]) (mm offset)

    oo_Changes = maybe [] (\onOff' -> [ setOnOff nn (onOffIze onOff') ]) (mm onOff)

    argA_iv' = paramize <$> (mm argA)

    argA_Changes = maybe [] (\argA_iv'' -> [ setPlaybackRate nn argA_iv'' ]) argA_iv'

    o =
      unsafeWAG
        { context:
            i
              { instructions =
                  i.instructions
                    <> buffer_Changes
                    <> offset_Changes
                    <> oo_Changes
                    <> argA_Changes
              }
        , value: unit
        }

instance changeRecorder ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.TRecorder edges) ignore graph
  ) =>
  Change' ptr (CTOR.Recorder MediaRecorderCb) graph where
  change' ptr w = o
    where
    { context: i, value: (CTOR.Recorder cb) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> [ setMediaRecorderCb nn cb ]
              }
        , value: unit
        }

instance oneShotChangeSawtoothOsc :: OneShotChange CTOR.TSawtoothOsc AudioParameter (CTOR.SawtoothOsc (Maybe APOnOff) (Maybe AudioParameter)) where
  oneShotChange _ freq = CTOR.SawtoothOsc Nothing (Just freq)

instance oneShotChangeSawtoothOscOO :: OneShotChange CTOR.TSawtoothOsc APOnOff (CTOR.SawtoothOsc (Maybe APOnOff) (Maybe AudioParameter)) where
  oneShotChange _ oo = CTOR.SawtoothOsc (Just oo) Nothing

instance changeSawtoothOsc ::
  ( IsSymbol ptr
  , MM mAPOnOff (Maybe onOff)
  , OnOffable onOff
  , MM mArgA (Maybe argA)
  , Paramable argA
  , R.Cons ptr (NodeC CTOR.TSawtoothOsc edges) ignore graph
  ) =>
  Change' ptr (CTOR.SawtoothOsc mAPOnOff mArgA) graph where
  change' ptr w = o
    where
    { context: i, value: (CTOR.SawtoothOsc onOff argA) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    oo_Changes = maybe [] (\onOff' -> [ setOnOff nn (onOffIze onOff') ]) (mm onOff)

    argA_iv' = paramize <$> (mm argA)

    argA_Changes = maybe [] (\argA_iv'' -> [ setFrequency nn argA_iv'' ]) argA_iv'

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> oo_Changes <> argA_Changes
              }
        , value: unit
        }

instance oneShotChangeSinOsc :: OneShotChange CTOR.TSinOsc AudioParameter (CTOR.SinOsc (Maybe APOnOff) (Maybe AudioParameter)) where
  oneShotChange _ freq = CTOR.SinOsc Nothing (Just freq)

instance oneShotChangeSinOscOO :: OneShotChange CTOR.TSinOsc APOnOff (CTOR.SinOsc (Maybe APOnOff) (Maybe AudioParameter)) where
  oneShotChange _ oo = CTOR.SinOsc (Just oo) Nothing

instance changeSinOsc ::
  ( IsSymbol ptr
  , MM mAPOnOff (Maybe onOff)
  , OnOffable onOff
  , MM mArgA (Maybe argA)
  , Paramable argA
  , R.Cons ptr (NodeC CTOR.TSinOsc edges) ignore graph
  ) =>
  Change' ptr (CTOR.SinOsc mAPOnOff mArgA) graph where
  change' ptr w = o
    where
    { context: i, value: (CTOR.SinOsc onOff argA) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    oo_Changes = maybe [] (\onOff' -> [ setOnOff nn (onOffIze onOff') ]) (mm onOff)

    argA_iv' = paramize <$> (mm argA)

    argA_Changes = maybe [] (\argA_iv'' -> [ setFrequency nn argA_iv'' ]) argA_iv'

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> oo_Changes <> argA_Changes
              }
        , value: unit
        }

instance changeSpeaker ::
  ( R.Cons "speaker" (NodeC (CTOR.TSpeaker) edges) ignore graph
  ) =>
  Change' "speaker" (CTOR.Speaker) graph where
  change' _ w = w $> unit

instance oneShotChangeSquareOsc :: OneShotChange CTOR.TSquareOsc AudioParameter (CTOR.SquareOsc (Maybe APOnOff) (Maybe AudioParameter)) where
  oneShotChange _ freq = CTOR.SquareOsc Nothing (Just freq)

instance oneShotChangeSquareOscOO :: OneShotChange CTOR.TSquareOsc APOnOff (CTOR.SquareOsc (Maybe APOnOff) (Maybe AudioParameter)) where
  oneShotChange _ oo = CTOR.SquareOsc (Just oo) Nothing

instance changeSquareOsc ::
  ( IsSymbol ptr
  , MM mAPOnOff (Maybe onOff)
  , OnOffable onOff
  , MM mArgA (Maybe argA)
  , Paramable argA
  , R.Cons ptr (NodeC CTOR.TSquareOsc edges) ignore graph
  ) =>
  Change' ptr (CTOR.SquareOsc mAPOnOff mArgA) graph where
  change' ptr w = o
    where
    { context: i, value: (CTOR.SquareOsc onOff argA) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    oo_Changes = maybe [] (\onOff' -> [ setOnOff nn (onOffIze onOff') ]) (mm onOff)

    argA_iv' = paramize <$> (mm argA)

    argA_Changes = maybe [] (\argA_iv'' -> [ setFrequency nn argA_iv'' ]) argA_iv'

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> oo_Changes <> argA_Changes
              }
        , value: unit
        }

instance oneShotChangeStereoPanner :: OneShotChange CTOR.TStereoPanner AudioParameter (CTOR.StereoPanner (Maybe AudioParameter)) where
  oneShotChange _ pan = CTOR.StereoPanner (Just pan)

instance changeStereoPanner ::
  ( IsSymbol ptr
  , MM mArgA (Maybe argA)
  , Paramable argA
  , R.Cons ptr (NodeC CTOR.TStereoPanner edges) ignore graph
  ) =>
  Change' ptr (CTOR.StereoPanner mArgA) graph where
  change' ptr w = o
    where
    { context: i, value: (CTOR.StereoPanner argA) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    argA_iv' = paramize <$> (mm argA)

    argA_Changes = maybe [] (\argA_iv'' -> [ setPan nn argA_iv'' ]) argA_iv'

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> argA_Changes
              }
        , value: unit
        }

instance oneShotChangeTriangleOsc :: OneShotChange CTOR.TTriangleOsc AudioParameter (CTOR.TriangleOsc (Maybe APOnOff) (Maybe AudioParameter)) where
  oneShotChange _ freq = CTOR.TriangleOsc Nothing (Just freq)

instance oneShotChangeTriangleOscOO :: OneShotChange CTOR.TTriangleOsc APOnOff (CTOR.TriangleOsc (Maybe APOnOff) (Maybe AudioParameter)) where
  oneShotChange _ oo = CTOR.TriangleOsc (Just oo) Nothing

instance changeTriangleOsc ::
  ( IsSymbol ptr
  , MM mAPOnOff (Maybe onOff)
  , OnOffable onOff
  , MM mArgA (Maybe argA)
  , Paramable argA
  , R.Cons ptr (NodeC CTOR.TTriangleOsc edges) ignore graph
  ) =>
  Change' ptr (CTOR.TriangleOsc mAPOnOff mArgA) graph where
  change' ptr w = o
    where
    { context: i, value: (CTOR.TriangleOsc onOff argA) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    oo_Changes = maybe [] (\onOff' -> [ setOnOff nn (onOffIze onOff') ]) (mm onOff)

    argA_iv' = paramize <$> (mm argA)

    argA_Changes = maybe [] (\argA_iv'' -> [ setFrequency nn argA_iv'' ]) argA_iv'

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> oo_Changes <> argA_Changes
              }
        , value: unit
        }

instance oneShotChangeWaveshaper :: (IsOversample oversample, Monoid oversample) => OneShotChange (CTOR.TWaveShaper oversample) BrowserFloatArray (CTOR.WaveShaper BrowserFloatArray Oversample) where
  oneShotChange _ bfa = CTOR.WaveShaper bfa (reflectOversample (mempty :: oversample))

instance changeWaveShaper ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC (CTOR.TWaveShaper a) edges) ignore graph
  ) =>
  Change' ptr (CTOR.WaveShaper BrowserFloatArray b) graph where
  change' ptr w = o
    where
    { context: i, value: (CTOR.WaveShaper fa _) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> [ setWaveShaperCurve nn fa ]
              }
        , value: unit
        }
