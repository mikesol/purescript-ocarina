module WAGS.Get where

import Prelude
import Control.Monad.State as MS
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\), type (/\))
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Partial.Unsafe (unsafePartial)
import Prim.Row as R
import Record as Record
import Type.Proxy (Proxy(..))
import WAGS.Control.Functions (proof, withProof)
import WAGS.Control.Qualified as WAGS
import WAGS.Control.Types (FrameT, unsafeFrame)
import WAGS.Graph.AudioUnit (OnOff)
import WAGS.Graph.AudioUnit as CTOR
import WAGS.Graph.Graph (Graph)
import WAGS.Graph.Node (NodeC)
import WAGS.Graph.Parameter (AudioParameter)
import WAGS.Interpret (class AudioInterpret)
import WAGS.Rendered (AnAudioUnit(..))

type GetType (ptr :: Symbol) (graph :: Graph) (b :: Type)
  = forall proxy env audio engine proof m res. Monad m => AudioInterpret audio engine => proxy ptr -> FrameT env audio engine proof m res { | graph } { | graph } b

-- | Get an audio unit `node` in `igraph` with index `ptr`.
class Get' (ptr :: Symbol) (graph :: Graph) (b :: Type) where
  get' :: GetType ptr graph b

data GetFoldingWithIndex
  = GetFoldingWithIndex

instance getFoldingWithIndex ::
  ( Monad m
  , AudioInterpret audio engine
  , Get' sym inGraph outNode
  , IsSymbol sym
  , R.Lacks sym inRecord
  , R.Cons sym outNode inRecord outRecord
  ) =>
  FoldingWithIndex
    GetFoldingWithIndex
    (proxy sym)
    ( FrameT
        env
        audio
        engine
        proof
        m
        res
        { | inGraph }
        { | inGraph }
        { | inRecord }
    )
    anything
    ( FrameT
        env
        audio
        engine
        proof
        m
        res
        { | inGraph }
        { | inGraph }
        { | outRecord }
    ) where
  foldingWithIndex GetFoldingWithIndex prop ifr _ = WAGS.do
    r <- ifr
    res <- get' prop
    pr <- proof
    withProof pr (Record.insert prop res r)

-- | Similar to `get'`, but accepts a record with multiple units to get.
get ::
  forall r rr env audio engine proof m res inGraph.
  Monad m =>
  AudioInterpret audio engine =>
  HFoldlWithIndex
    GetFoldingWithIndex
    ( FrameT
        env
        audio
        engine
        proof
        m
        res
        { | inGraph }
        { | inGraph }
        {}
    )
    { | r }
    ( FrameT
        env
        audio
        engine
        proof
        m
        res
        { | inGraph }
        { | inGraph }
        rr
    ) =>
  { | r } ->
  FrameT
    env
    audio
    engine
    proof
    m
    res
    { | inGraph }
    { | inGraph }
    rr
get r =
  hfoldlWithIndex
    GetFoldingWithIndex
    ( (unsafeFrame (pure {})) ::
        FrameT
          env
          audio
          engine
          proof
          m
          res
          { | inGraph }
          { | inGraph }
          {}
    )
    r

instance getAllpass ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.TAllpass edges) ignore graphi
  ) =>
  Get' ptr graphi (CTOR.Allpass AudioParameter AudioParameter) where
  get' ptr =
    unsafeFrame
      $ do
          let
            nn = reflectSymbol ptr
          lookup <- MS.gets (M.lookup nn <<< _.internalNodes)
          let
            partial :: Partial => Maybe AnAudioUnit -> Tuple AudioParameter AudioParameter
            partial (Just (AAllpass a b)) = a /\ b

            v_argA /\ v_argB = unsafePartial $ partial lookup
          pure (CTOR.Allpass v_argA v_argB)

instance getBandpass ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.TBandpass edges) ignore graphi
  ) =>
  Get' ptr graphi (CTOR.Bandpass AudioParameter AudioParameter) where
  get' ptr =
    unsafeFrame
      $ do
          let
            nn = reflectSymbol ptr
          lookup <- MS.gets (M.lookup nn <<< _.internalNodes)
          let
            partial :: Partial => Maybe AnAudioUnit -> Tuple AudioParameter AudioParameter
            partial (Just (ABandpass a b)) = a /\ b

            v_argA /\ v_argB = unsafePartial $ partial lookup
          pure (CTOR.Bandpass v_argA v_argB)

instance getConstant ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.TConstant edges) ignore graphi
  ) =>
  Get' ptr graphi (CTOR.Constant AudioParameter) where
  get' ptr =
    unsafeFrame
      $ do
          let
            nn = reflectSymbol ptr
          lookup <- MS.gets (M.lookup nn <<< _.internalNodes)
          let
            partial :: Partial => Maybe AnAudioUnit -> Tuple OnOff AudioParameter
            partial (Just (AConstant a b)) = a /\ b

            oldOnOff /\ v_argA = unsafePartial $ partial lookup
          pure (CTOR.Constant oldOnOff v_argA)

instance getDelay ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.TDelay edges) ignore graphi
  ) =>
  Get' ptr graphi (CTOR.Delay AudioParameter) where
  get' ptr =
    unsafeFrame
      $ do
          let
            nn = reflectSymbol ptr
          lookup <- MS.gets (M.lookup nn <<< _.internalNodes)
          let
            partial :: Partial => Maybe AnAudioUnit -> AudioParameter
            partial (Just (ADelay a)) = a

            v_argA = unsafePartial $ partial lookup
          pure (CTOR.Delay v_argA)

instance getDynamicsCompressor ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.TDynamicsCompressor edges) ignore graphi
  ) =>
  Get' ptr graphi (CTOR.DynamicsCompressor AudioParameter AudioParameter AudioParameter AudioParameter AudioParameter) where
  get' ptr =
    unsafeFrame
      $ do
          let
            nn = reflectSymbol ptr
          lookup <- MS.gets (M.lookup nn <<< _.internalNodes)
          let
            partial :: Partial => Maybe AnAudioUnit -> AudioParameter /\ AudioParameter /\ AudioParameter /\ AudioParameter /\ AudioParameter
            partial (Just (ADynamicsCompressor a b c d e)) = a /\ b /\ c /\ d /\ e

            v_argA /\ v_argB /\ v_argC /\ v_argD /\ v_argE = unsafePartial $ partial lookup
          pure (CTOR.DynamicsCompressor v_argA v_argB v_argC v_argD v_argE)

instance getGain ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.TGain edges) ignore graphi
  ) =>
  Get' ptr graphi (CTOR.Gain AudioParameter) where
  get' ptr =
    unsafeFrame
      $ do
          let
            nn = reflectSymbol ptr
          lookup <- MS.gets (M.lookup nn <<< _.internalNodes)
          let
            partial :: Partial => Maybe AnAudioUnit -> AudioParameter
            partial (Just (AGain a)) = a

            v_argA = unsafePartial $ partial lookup
          pure (CTOR.Gain v_argA)

instance getHighpass ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.THighpass edges) ignore graphi
  ) =>
  Get' ptr graphi (CTOR.Highpass AudioParameter AudioParameter) where
  get' ptr =
    unsafeFrame
      $ do
          let
            nn = reflectSymbol ptr
          lookup <- MS.gets (M.lookup nn <<< _.internalNodes)
          let
            partial :: Partial => Maybe AnAudioUnit -> Tuple AudioParameter AudioParameter
            partial (Just (AHighpass a b)) = a /\ b

            v_argA /\ v_argB = unsafePartial $ partial lookup
          pure (CTOR.Highpass v_argA v_argB)

instance getHighshelf ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.THighshelf edges) ignore graphi
  ) =>
  Get' ptr graphi (CTOR.Highshelf AudioParameter AudioParameter) where
  get' ptr =
    unsafeFrame
      $ do
          let
            nn = reflectSymbol ptr
          lookup <- MS.gets (M.lookup nn <<< _.internalNodes)
          let
            partial :: Partial => Maybe AnAudioUnit -> Tuple AudioParameter AudioParameter
            partial (Just (AHighshelf a b)) = a /\ b

            v_argA /\ v_argB = unsafePartial $ partial lookup
          pure (CTOR.Highshelf v_argA v_argB)

instance getLoopBuf ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.TLoopBuf edges) ignore graphi
  ) =>
  Get' ptr graphi (CTOR.LoopBuf AudioParameter) where
  get' ptr =
    unsafeFrame
      $ do
          let
            nn = reflectSymbol ptr
          lookup <- MS.gets (M.lookup nn <<< _.internalNodes)
          let
            partial :: Partial => Maybe AnAudioUnit -> String /\ OnOff /\ AudioParameter /\ Number /\ Number
            partial (Just (ALoopBuf a b c d e)) = a /\ b /\ c /\ d /\ e

            oldBuffer /\ oldOnOff /\ v_argA /\ oldLoopStart /\ oldLoopEnd = unsafePartial $ partial lookup
          pure (CTOR.LoopBuf oldBuffer oldOnOff v_argA oldLoopStart oldLoopEnd)

instance getLowpass ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.TLowpass edges) ignore graphi
  ) =>
  Get' ptr graphi (CTOR.Lowpass AudioParameter AudioParameter) where
  get' ptr =
    unsafeFrame
      $ do
          let
            nn = reflectSymbol ptr
          lookup <- MS.gets (M.lookup nn <<< _.internalNodes)
          let
            partial :: Partial => Maybe AnAudioUnit -> Tuple AudioParameter AudioParameter
            partial (Just (ALowpass a b)) = a /\ b

            v_argA /\ v_argB = unsafePartial $ partial lookup
          pure (CTOR.Lowpass v_argA v_argB)

instance getLowshelf ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.TLowshelf edges) ignore graphi
  ) =>
  Get' ptr graphi (CTOR.Lowshelf AudioParameter AudioParameter) where
  get' ptr =
    unsafeFrame
      $ do
          let
            nn = reflectSymbol ptr
          lookup <- MS.gets (M.lookup nn <<< _.internalNodes)
          let
            partial :: Partial => Maybe AnAudioUnit -> Tuple AudioParameter AudioParameter
            partial (Just (ALowshelf a b)) = a /\ b

            v_argA /\ v_argB = unsafePartial $ partial lookup
          pure (CTOR.Lowshelf v_argA v_argB)

instance getMicrophone ::
  R.Cons "microphone" (NodeC CTOR.TMicrophone edges) ignore graphi =>
  Get' "microphone" graphi CTOR.Microphone where
  get' _ = unsafeFrame $ pure CTOR.Microphone

instance getNotch ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.TNotch edges) ignore graphi
  ) =>
  Get' ptr graphi (CTOR.Notch AudioParameter AudioParameter) where
  get' ptr =
    unsafeFrame
      $ do
          let
            nn = reflectSymbol ptr
          lookup <- MS.gets (M.lookup nn <<< _.internalNodes)
          let
            partial :: Partial => Maybe AnAudioUnit -> Tuple AudioParameter AudioParameter
            partial (Just (ANotch a b)) = a /\ b

            v_argA /\ v_argB = unsafePartial $ partial lookup
          pure (CTOR.Notch v_argA v_argB)

instance getPeaking ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.TPeaking edges) ignore graphi
  ) =>
  Get' ptr graphi (CTOR.Peaking AudioParameter AudioParameter AudioParameter) where
  get' ptr =
    unsafeFrame
      $ do
          let
            nn = reflectSymbol ptr
          lookup <- MS.gets (M.lookup nn <<< _.internalNodes)
          let
            partial :: Partial => Maybe AnAudioUnit -> AudioParameter /\ AudioParameter /\ AudioParameter
            partial (Just (APeaking a b c)) = a /\ b /\ c

            v_argA /\ v_argB /\ v_argC = unsafePartial $ partial lookup
          pure (CTOR.Peaking v_argA v_argB v_argC)

instance getPeriodicOsc ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.TPeriodicOsc edges) ignore graphi
  ) =>
  Get' ptr graphi (CTOR.PeriodicOsc AudioParameter) where
  get' ptr =
    unsafeFrame
      $ do
          let
            nn = reflectSymbol ptr
          lookup <- MS.gets (M.lookup nn <<< _.internalNodes)
          let
            partial :: Partial => Maybe AnAudioUnit -> String /\ OnOff /\ AudioParameter
            partial (Just (APeriodicOsc a b c)) = a /\ b /\ c

            oldPeriodicWave /\ oldOnOff /\ v_argA = unsafePartial $ partial lookup
          pure (CTOR.PeriodicOsc oldPeriodicWave oldOnOff v_argA)

instance getPlayBuf ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.TPlayBuf edges) ignore graphi
  ) =>
  Get' ptr graphi (CTOR.PlayBuf AudioParameter) where
  get' ptr =
    unsafeFrame
      $ do
          let
            nn = reflectSymbol ptr
          lookup <- MS.gets (M.lookup nn <<< _.internalNodes)
          let
            partial :: Partial => Maybe AnAudioUnit -> String /\ Number /\ OnOff /\ AudioParameter
            partial (Just (APlayBuf a b c d)) = a /\ b /\ c /\ d

            oldBuffer /\ oldOffset /\ oldOnOff /\ v_argA = unsafePartial $ partial lookup
          pure (CTOR.PlayBuf oldBuffer oldOffset oldOnOff v_argA)

instance getRecorder ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC (CTOR.TRecorder sym) edges) ignore graphi
  ) =>
  Get'
    ptr
    graphi
    (CTOR.Recorder sym) where
  get' _ = unsafeFrame $ pure (CTOR.Recorder (Proxy :: _ sym))

instance getSawtoothOsc ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.TSawtoothOsc edges) ignore graphi
  ) =>
  Get' ptr graphi (CTOR.SawtoothOsc AudioParameter) where
  get' ptr =
    unsafeFrame
      $ do
          let
            nn = reflectSymbol ptr
          lookup <- MS.gets (M.lookup nn <<< _.internalNodes)
          let
            partial :: Partial => Maybe AnAudioUnit -> Tuple OnOff AudioParameter
            partial (Just (ASawtoothOsc a b)) = a /\ b

            oldOnOff /\ v_argA = unsafePartial $ partial lookup
          pure (CTOR.SawtoothOsc oldOnOff v_argA)

instance getSinOsc ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.TSinOsc edges) ignore graphi
  ) =>
  Get' ptr graphi (CTOR.SinOsc AudioParameter) where
  get' ptr =
    unsafeFrame
      $ do
          let
            nn = reflectSymbol ptr
          lookup <- MS.gets (M.lookup nn <<< _.internalNodes)
          let
            partial :: Partial => Maybe AnAudioUnit -> Tuple OnOff AudioParameter
            partial (Just (ASinOsc a b)) = a /\ b

            oldOnOff /\ v_argA = unsafePartial $ partial lookup
          pure (CTOR.SinOsc oldOnOff v_argA)

instance getSpeaker ::
  R.Cons "speaker" (NodeC (CTOR.TSpeaker) edges) ignore graphi =>
  Get' "speaker" graphi CTOR.Speaker where
  get' _ = unsafeFrame $ pure CTOR.Speaker

instance getSquareOsc ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.TSquareOsc edges) ignore graphi
  ) =>
  Get' ptr graphi (CTOR.SquareOsc AudioParameter) where
  get' ptr =
    unsafeFrame
      $ do
          let
            nn = reflectSymbol ptr
          lookup <- MS.gets (M.lookup nn <<< _.internalNodes)
          let
            partial :: Partial => Maybe AnAudioUnit -> Tuple OnOff AudioParameter
            partial (Just (ASquareOsc a b)) = a /\ b

            oldOnOff /\ v_argA = unsafePartial $ partial lookup
          pure (CTOR.SquareOsc oldOnOff v_argA)

instance getStereoPanner ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.TStereoPanner edges) ignore graphi
  ) =>
  Get' ptr graphi (CTOR.StereoPanner AudioParameter) where
  get' ptr =
    unsafeFrame
      $ do
          let
            nn = reflectSymbol ptr
          lookup <- MS.gets (M.lookup nn <<< _.internalNodes)
          let
            partial :: Partial => Maybe AnAudioUnit -> AudioParameter
            partial (Just (AStereoPanner a)) = a

            v_argA = unsafePartial $ partial lookup
          pure (CTOR.StereoPanner v_argA)

instance getTriangleOsc ::
  ( IsSymbol ptr
  , R.Cons ptr (NodeC CTOR.TTriangleOsc edges) ignore graphi
  ) =>
  Get' ptr graphi (CTOR.TriangleOsc AudioParameter) where
  get' ptr =
    unsafeFrame
      $ do
          let
            nn = reflectSymbol ptr
          lookup <- MS.gets (M.lookup nn <<< _.internalNodes)
          let
            partial :: Partial => Maybe AnAudioUnit -> Tuple OnOff AudioParameter
            partial (Just (ATriangleOsc a b)) = a /\ b

            oldOnOff /\ v_argA = unsafePartial $ partial lookup
          pure (CTOR.TriangleOsc oldOnOff v_argA)

instance getWaveShaper ::
  (IsSymbol ptr, Monoid b, R.Cons ptr (NodeC (CTOR.TWaveShaper a b) edges) ignore graphi) =>
  Get'
    ptr
    graphi
    (CTOR.WaveShaper a b) where
  get' _ = unsafeFrame $ pure (CTOR.WaveShaper (Proxy :: _ a) mempty)
