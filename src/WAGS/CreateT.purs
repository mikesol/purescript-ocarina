-- | Typelevel declarations for the create function
-- | As of PureScript 14.2, this is still necessary in order to
-- | work with `Create` at the type level without incuring term-level computations.
module WAGS.CreateT where

import Prelude

import Data.Tuple (Tuple)
import Data.Tuple.Nested (type (/\))
import Data.Vec as V
import Prim.Row as R
import Prim.RowList as RL
import Prim.Symbol as Sym
import WAGS.Connect (class ConnectT)
import WAGS.ConstructEdges (class ConstructEdgesT)
import WAGS.Graph.AudioUnit as CTOR
import WAGS.Graph.Graph (Graph)
import WAGS.Graph.Node (NodeC)
import WAGS.Graph.Oversample (class IsOversampleT)
import WAGS.Graph.Parameter (AudioParameter)
import WAGS.Interpret (AsSubgraph)
import WAGS.Util (class AddPrefixToRowList, class CoercePrefixToString, class MakePrefixIfNeeded)
import WAGS.WebAPI (AnalyserNodeCb, BrowserAudioBuffer, BrowserFloatArray, BrowserMediaElement, BrowserPeriodicWave, MediaRecorderCb)

class CreateStepT (prefix :: Type) (map :: Type) (r :: Row Type) (inGraph :: Graph) (outGraph :: Graph) | r inGraph -> outGraph

class CreateStepRLT (rl :: RL.RowList Type) (prefix :: Type) (map :: Type) (r :: Row Type) (inGraph :: Graph) (outGraph :: Graph) | rl r inGraph -> outGraph

instance createStepTAll :: (RL.RowToList r rl, CreateStepRLT rl prefix map r inGraph outGraph) => CreateStepT prefix map r inGraph outGraph

instance createStepRLTNil :: CreateStepRLT RL.Nil prefix map r inGraph inGraph

instance createStepRLTCons ::
  ( R.Cons key val ignore r
  , MakePrefixIfNeeded key prefix prefix'
  , ConstructEdgesT prefix' map val newPrefix newMap (node /\ { | edges })
  , CoercePrefixToString prefix realPrefix
  , Sym.Append realPrefix key newKey
  , CreateT' newKey node graph0 graph1
  -- push the new prefix and new map down to the edges
  , CreateStepT newPrefix newMap edges graph1 graph2
  -- on this level, we keep the old stuff
  , CreateStepRLT rest prefix map r graph2 graph3
  ) =>
  CreateStepRLT (RL.Cons key val rest) prefix map r graph0 graph3

class ConnectEdgesToNodeT (sources :: RL.RowList Type) (dest :: Symbol) (inGraph :: Graph) (outGraph :: Graph) | sources dest inGraph -> outGraph

instance connectEdgesToNodeTNil :: ConnectEdgesToNodeT RL.Nil dest inGraph inGraph

instance connectEdgesToNodeTCons :: (ConnectT key dest inGraph midGraph, ConnectEdgesToNodeT rest dest midGraph outGraph) => ConnectEdgesToNodeT (RL.Cons key ignore rest) dest inGraph outGraph

class ConnectAfterCreateT (prefix :: Type) (map :: Type) (rl :: RL.RowList Type) (inGraph :: Graph) (outGraph :: Graph) | rl inGraph -> outGraph

instance connectAfterCreateTNil :: ConnectAfterCreateT prefix map RL.Nil graph0 graph0

instance connectAfterCreateTCons ::
  ( MakePrefixIfNeeded sym prefix prefix'
  , ConstructEdgesT prefix' map node' newPrefix newMap (Tuple node { | edges })
  , RL.RowToList edges edgesList
  , CoercePrefixToString prefix realPrefix
  , Sym.Append realPrefix sym newKey
  , AddPrefixToRowList newPrefix edgesList oel
  , ConnectEdgesToNodeT oel newKey graph0 graph1
  , ConnectAfterCreateT newPrefix newMap edgesList graph1 graph2
  , ConnectAfterCreateT prefix map rest graph2 graph3
  ) =>
  ConnectAfterCreateT prefix map (RL.Cons sym node' rest) graph0 graph3

class CreateInternalT (prefix :: Type) (map :: Type) (r :: Row Type) (inGraph :: Graph) (outGraph :: Graph) | prefix map r inGraph -> outGraph

instance createInternalTAll ::
  ( CreateStepT prefix map r inGraph midGraph
  , RL.RowToList r rl
  , ConnectAfterCreateT prefix map rl midGraph outGraph
  ) =>
  CreateInternalT prefix map r inGraph outGraph

class CreateT (r :: Row Type) (inGraph :: Graph) (outGraph :: Graph) | r inGraph -> outGraph

instance createTAll ::
  ( CreateInternalT Unit Unit r inGraph outGraph
  ) =>
  CreateT r inGraph outGraph

-- | Create an audio unit `node` in `igraph` with index `ptr`, resulting in `ograph`.
class CreateT' (ptr :: Symbol) (node :: Type) (inGraph :: Graph) (outGraph :: Graph) | ptr node inGraph -> outGraph

instance createTUnit ::
  CreateT' ptr Unit graphi graphi

instance createTAllpass ::
  ( R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TAllpass {}) graphi grapho
  ) =>
  CreateT' ptr CTOR.Allpass graphi grapho

instance createTAnalyser ::
  ( R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TAnalyser {}) graphi grapho
  ) =>
  CreateT' ptr (CTOR.Analyser AnalyserNodeCb) graphi grapho

class CreateParametersT (parameterRL :: RL.RowList Type) (parameterData :: Row Type)

instance createParametersTNil :: CreateParametersT RL.Nil parameterData

instance createParametersTCons ::
  ( R.Cons key AudioParameter parameters' parameterData
  , CreateParametersT rest parameterData
  ) =>
  CreateParametersT (RL.Cons key AudioParameter rest) parameterData

instance createAudioWorkletNode ::
  ( R.Lacks ptr graphi
  , R.Cons ptr (NodeC (CTOR.TAudioWorkletNode name numberOfInputs numberOfOutputs outputChannelCount parameterData processorOptions) {}) graphi grapho
  , RL.RowToList parameterData parameterDataRL
  , CreateParametersT parameterDataRL parameterData
  ) =>
  CreateT'
    ptr
    (CTOR.AudioWorkletNode sym numberOfInputs numberOfOutputs outputChannelCount parameterData processorOptions)
    graphi
    grapho

instance createTBandpass ::
  ( R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TBandpass {}) graphi grapho
  ) =>
  CreateT' ptr CTOR.Bandpass graphi grapho

instance createTConstant ::
  ( R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TConstant {}) graphi grapho
  ) =>
  CreateT' ptr CTOR.Constant graphi grapho

instance createTConvolver ::
  ( R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TConvolver {}) graphi grapho
  ) =>
  CreateT' ptr (CTOR.Convolver BrowserAudioBuffer) graphi grapho

instance createTDelay ::
  ( R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TDelay {}) graphi grapho
  ) =>
  CreateT' ptr CTOR.Delay graphi grapho

instance createTDynamicsCompressor ::
  ( R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TDynamicsCompressor {}) graphi grapho
  ) =>
  CreateT' ptr (CTOR.DynamicsCompressor argA argB argC argD argE) graphi grapho

instance createTGain ::
  ( R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TGain {}) graphi grapho
  ) =>
  CreateT' ptr (CTOR.Gain argA) graphi grapho

instance createTHighpass ::
  ( R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.THighpass {}) graphi grapho
  ) =>
  CreateT' ptr (CTOR.Highpass argA argB) graphi grapho

instance createTHighshelf ::
  ( R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.THighshelf {}) graphi grapho
  ) =>
  CreateT' ptr (CTOR.Highshelf argA argB) graphi grapho

instance createTInput ::
  ( R.Lacks ptr graphi
  , R.Cons ptr (NodeC (CTOR.TInput proxy) {}) graphi grapho
  ) =>
  CreateT' ptr (CTOR.Input proxy) graphi grapho

instance createTLoopBuf ::
  ( R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TLoopBuf {}) graphi grapho
  ) =>
  CreateT' ptr (CTOR.LoopBuf BrowserAudioBuffer onOff argA Number Number) graphi grapho

instance createTLowpass ::
  ( R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TLowpass {}) graphi grapho
  ) =>
  CreateT' ptr (CTOR.Lowpass argA argB) graphi grapho

instance createTLowshelf ::
  ( R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TLowshelf {}) graphi grapho
  ) =>
  CreateT' ptr (CTOR.Lowshelf argA argB) graphi grapho

instance createTMediaElement ::
  ( R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TMediaElement {}) graphi grapho
  ) =>
  CreateT' ptr (CTOR.MediaElement BrowserMediaElement) graphi grapho

instance createTMicrophone ::
  ( R.Lacks "microphone" graphi
  , R.Cons "microphone" (NodeC CTOR.TMicrophone {}) graphi grapho
  ) =>
  CreateT' "microphone" (CTOR.Microphone microphone) graphi grapho

instance createTNotch ::
  ( R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TNotch {}) graphi grapho
  ) =>
  CreateT' ptr (CTOR.Notch argA argB) graphi grapho

instance createTPeaking ::
  ( R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TPeaking {}) graphi grapho
  ) =>
  CreateT' ptr (CTOR.Peaking argA argB argC) graphi grapho

instance createTPeriodicOsc ::
  ( R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TPeriodicOsc {}) graphi grapho
  ) =>
  CreateT' ptr (CTOR.PeriodicOsc BrowserPeriodicWave onOff argA) graphi grapho

instance createTPeriodicOsc2 ::
  ( R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TPeriodicOsc {}) graphi grapho
  ) =>
  CreateT' ptr (CTOR.PeriodicOsc (V.Vec a Number /\ V.Vec a Number) onOff argA) graphi grapho

instance createTPlayBuf ::
  ( R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TPlayBuf {}) graphi grapho
  ) =>
  CreateT' ptr (CTOR.PlayBuf BrowserAudioBuffer Number onOff argA) graphi grapho

instance createTRecorder ::
  ( R.Lacks ptr graphi
  , R.Cons ptr (NodeC (CTOR.TRecorder) {}) graphi grapho
  ) =>
  CreateT' ptr (CTOR.Recorder MediaRecorderCb) graphi grapho

instance createTSawtoothOsc ::
  ( R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TSawtoothOsc {}) graphi grapho
  ) =>
  CreateT' ptr (CTOR.SawtoothOsc onOff argA) graphi grapho

instance createTSinOsc ::
  ( R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TSinOsc {}) graphi grapho
  ) =>
  CreateT' ptr (CTOR.SinOsc onOff argA) graphi grapho

instance createTSpeaker ::
  ( R.Lacks "speaker" graphi
  , R.Cons "speaker" (NodeC CTOR.TSpeaker {}) graphi grapho
  ) =>
  CreateT' "speaker" CTOR.Speaker graphi grapho

instance createTSquareOsc ::
  ( R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TSquareOsc {}) graphi grapho
  ) =>
  CreateT' ptr (CTOR.SquareOsc onOff argA) graphi grapho

instance createTStereoPanner ::
  ( R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TStereoPanner {}) graphi grapho
  ) =>
  CreateT' ptr (CTOR.StereoPanner argA) graphi grapho

instance createSubgraph ::
  ( R.Lacks ptr graphi
  , R.Cons ptr (NodeC (CTOR.TSubgraph n terminus inputs env) {}) graphi grapho
  ) =>
  CreateT' ptr (CTOR.Subgraph inputs (AsSubgraph terminus inputs env) (V.Vec n env)) graphi grapho

instance createTTriangleOsc ::
  ( R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TTriangleOsc {}) graphi grapho
  ) =>
  CreateT' ptr (CTOR.TriangleOsc onOff argA) graphi grapho

instance createTWaveShaper ::
  ( IsOversampleT oversample
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC (CTOR.TWaveShaper oversample) {}) graphi grapho
  ) =>
  CreateT' ptr (CTOR.WaveShaper BrowserFloatArray oversample) graphi grapho
