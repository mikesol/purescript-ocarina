-- | Typelevel declarations for the create function
-- | As of PureScript 14.2, this is still necessary in order to
-- | work with `Create` at the type level without incuring term-level computations.
module WAGS.CreateT where

import Prelude
import Data.Tuple (Tuple)
import Data.Vec as V
import Prim.Row as R
import Prim.RowList as RL
import Data.Tuple.Nested(type(/\))
import WAGS.Connect (class ConnectT)
import WAGS.Edgeable (class EdgeableT)
import WAGS.Graph.AudioUnit (OnOff)
import WAGS.Graph.AudioUnit as CTOR
import WAGS.Graph.Graph (Graph)
import WAGS.Graph.Node (NodeC)
import WAGS.Graph.Oversample (class IsOversampleT)

class CreateStepT (r :: Row Type) (inGraph :: Graph) (outGraph :: Graph) | r inGraph -> outGraph

class CreateStepRLT (rl :: RL.RowList Type) (r :: Row Type) (inGraph :: Graph) (outGraph :: Graph) | rl r inGraph -> outGraph

instance createStepTAll :: (RL.RowToList r rl, CreateStepRLT rl r inGraph outGraph) => CreateStepT r inGraph outGraph

instance createStepRLTNil :: CreateStepRLT RL.Nil r inGraph inGraph

instance createStepRLTCons ::
  ( R.Cons key val ignore r
  , EdgeableT val (node /\ { | edges })
  , CreateT' key node graph0 graph1
  , CreateStepT edges graph1 graph2
  , CreateStepRLT rest r graph2 graph3
  ) =>
  CreateStepRLT (RL.Cons key val rest) r graph0 graph3

class ConnectEdgesToNodeT (sources :: RL.RowList Type) (dest :: Symbol) (inGraph :: Graph) (outGraph :: Graph) | sources dest inGraph -> outGraph

instance connectEdgesToNodeTNil :: ConnectEdgesToNodeT RL.Nil dest inGraph inGraph

instance connectEdgesToNodeTCons :: (ConnectT key dest inGraph midGraph, ConnectEdgesToNodeT rest dest midGraph outGraph) => ConnectEdgesToNodeT (RL.Cons key ignore rest) dest inGraph outGraph

class ConnectAfterCreateT (rl :: RL.RowList Type) (inGraph :: Graph) (outGraph :: Graph) | rl inGraph -> outGraph

instance connectAfterCreateTNil :: ConnectAfterCreateT RL.Nil graph0 graph0

instance connectAfterCreateTCons ::
  ( EdgeableT node' (Tuple node { | edges })
  , RL.RowToList edges edgesList
  , ConnectEdgesToNodeT edgesList sym graph0 graph1
  , ConnectAfterCreateT edgesList graph1 graph2
  , ConnectAfterCreateT rest graph2 graph3
  ) =>
  ConnectAfterCreateT (RL.Cons sym node' rest) graph0 graph3

class CreateT (r :: Row Type) (inGraph :: Graph) (outGraph :: Graph) | r inGraph -> outGraph

instance createTAll ::
  ( CreateStepT r inGraph midGraph
  , RL.RowToList r rl
  , ConnectAfterCreateT rl midGraph outGraph
  ) =>
  CreateT r inGraph outGraph

-- | Create an audio unit `node` in `igraph` with index `ptr`, resulting in `ograph`.
class CreateT' (ptr :: Symbol) (node :: Type) (inGraph :: Graph) (outGraph :: Graph) | ptr node inGraph -> outGraph

instance createTUnit ::
  CreateT'
    ptr
    Unit
    graphi
    graphi

instance createTAllpass ::
  ( R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TAllpass {}) graphi grapho
  ) =>
  CreateT'
    ptr
    (CTOR.Allpass argA argB)
    graphi
    grapho
instance createTBandpass ::
  ( R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TBandpass {}) graphi grapho
  ) =>
  CreateT'
    ptr
    (CTOR.Bandpass argA argB)
    graphi
    grapho

instance createTConstant ::
  ( R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TConstant {}) graphi grapho
  ) =>
  CreateT'
    ptr
    (CTOR.Constant OnOff argA)
    graphi
    grapho

instance createTConvolver ::
  ( R.Lacks ptr graphi
  , R.Cons ptr (NodeC (CTOR.TConvolver buffer) {}) graphi grapho
  ) =>
  CreateT'
    ptr
    (CTOR.Convolver buffer)
    graphi
    grapho

instance createTDelay ::
  ( R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TDelay {}) graphi grapho
  ) =>
  CreateT'
    ptr
    (CTOR.Delay argA)
    graphi
    grapho

instance createTDynamicsCompressor ::
  ( R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TDynamicsCompressor {}) graphi grapho
  ) =>
  CreateT'
    ptr
    (CTOR.DynamicsCompressor argA argB argC argD argE)
    graphi
    grapho

instance createTGain ::
  ( R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TGain {}) graphi grapho
  ) =>
  CreateT'
    ptr
    (CTOR.Gain argA)
    graphi
    grapho

instance createTHighpass ::
  ( R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.THighpass {}) graphi grapho
  ) =>
  CreateT'
    ptr
    (CTOR.Highpass argA argB)
    graphi
    grapho

instance createTHighshelf ::
  ( R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.THighshelf {}) graphi grapho
  ) =>
  CreateT'
    ptr
    (CTOR.Highshelf argA argB)
    graphi
    grapho

instance createTLoopBuf ::
  ( R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TLoopBuf {}) graphi grapho
  ) =>
  CreateT'
    ptr
    (CTOR.LoopBuf String OnOff argA Number Number)
    graphi
    grapho

instance createTLowpass ::
  ( R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TLowpass {}) graphi grapho
  ) =>
  CreateT'
    ptr
    (CTOR.Lowpass argA argB)
    graphi
    grapho

instance createTLowshelf ::
  ( R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TLowshelf {}) graphi grapho
  ) =>
  CreateT'
    ptr
    (CTOR.Lowshelf argA argB)
    graphi
    grapho

instance createTMicrophone ::
  ( R.Lacks "microphone" graphi
  , R.Cons "microphone" (NodeC CTOR.TMicrophone {}) graphi grapho
  ) =>
  CreateT'
    "microphone"
    CTOR.Microphone
    graphi
    grapho

instance createTNotch ::
  ( R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TNotch {}) graphi grapho
  ) =>
  CreateT'
    ptr
    (CTOR.Notch argA argB)
    graphi
    grapho

instance createTPeaking ::
  ( R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TPeaking {}) graphi grapho
  ) =>
  CreateT'
    ptr
    (CTOR.Peaking argA argB argC)
    graphi
    grapho

instance createTPeriodicOsc ::
  ( R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TPeriodicOsc {}) graphi grapho
  ) =>
  CreateT'
    ptr
    (CTOR.PeriodicOsc String OnOff argA)
    graphi
    grapho

instance createTPeriodicOsc2 ::
  ( R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TPeriodicOsc {}) graphi grapho
  ) =>
  CreateT'
    ptr
    (CTOR.PeriodicOsc (V.Vec a Number /\ V.Vec a Number) OnOff argA)
    graphi
    grapho

instance createTPlayBuf ::
  ( R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TPlayBuf {}) graphi grapho
  ) =>
  CreateT'
    ptr
    (CTOR.PlayBuf String Number OnOff argA)
    graphi
    grapho
instance createTRecorder ::
  ( R.Lacks ptr graphi
  , R.Cons ptr (NodeC (CTOR.TRecorder recorder) {}) graphi grapho
  ) =>
  CreateT'
    ptr
    (CTOR.Recorder recorder)
    graphi
    grapho

instance createTSawtoothOsc ::
  ( R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TSawtoothOsc {}) graphi grapho
  ) =>
  CreateT'
    ptr
    (CTOR.SawtoothOsc OnOff argA)
    graphi
    grapho

instance createTSinOsc ::
  ( R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TSinOsc {}) graphi grapho
  ) =>
  CreateT'
    ptr
    (CTOR.SinOsc OnOff argA)
    graphi
    grapho

instance createTSpeaker ::
  ( R.Lacks "speaker" graphi
  , R.Cons "speaker" (NodeC CTOR.TSpeaker {}) graphi grapho
  ) =>
  CreateT'
    "speaker"
    CTOR.Speaker
    graphi
    grapho

instance createTSquareOsc ::
  ( R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TSquareOsc {}) graphi grapho
  ) =>
  CreateT'
    ptr
    (CTOR.SquareOsc OnOff argA)
    graphi
    grapho

instance createTStereoPanner ::
  ( R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TStereoPanner {}) graphi grapho
  ) =>
  CreateT'
    ptr
    (CTOR.StereoPanner argA)
    graphi
    grapho

instance createTTriangleOsc ::
  ( R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TTriangleOsc {}) graphi grapho
  ) =>
  CreateT'
    ptr
    (CTOR.TriangleOsc OnOff argA)
    graphi
    grapho

instance createTWaveShaper ::
  ( IsOversampleT oversample
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC (CTOR.TWaveShaper floatArray oversample) {}) graphi grapho
  ) =>
  CreateT'
    ptr
    (CTOR.WaveShaper floatArray oversample)
    graphi
    grapho
