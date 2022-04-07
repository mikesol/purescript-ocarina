module WAGS.Tumult.Create where

import Prelude

import Data.Set (insert)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Typelevel.Num (class Lt, class Nat, class Pos, D1, toInt')
import Data.Variant.Maybe (nothing)
import Data.Vec as V
import Foreign.Object (Object)
import Foreign.Object as Object
import Prim.Row as R
import Prim.RowList as RL
import Prim.Symbol as Sym
import Record as Record
import Simple.JSON as JSON
import Type.Proxy (Proxy(..))
import WAGS.Core (AudioWorkletNodeOptions_(..), RealImg(..), _realImg, _wave)
import WAGS.Core as Core
import WAGS.Parameter (AudioParameter)
import WAGS.Tumult.Connect (class Connect, connect)
import WAGS.Tumult.ConstructEdges (class ConstructEdges, class ConstructEdgesT, constructEdges)
import WAGS.Tumult.Control.Types (WAG(..))
import WAGS.Tumult.Graph.AudioUnit (AudioWorkletNodeOptions(..))
import WAGS.Tumult.Graph.AudioUnit as CTOR
import WAGS.Tumult.Graph.Graph (Graph)
import WAGS.Tumult.Graph.Node (NodeC)
import WAGS.Tumult.Graph.Oversample (class IsOversample, reflectOversample)
import WAGS.Tumult.Instructions as I
import WAGS.Tumult.Util (class AddPrefixToRowList, class CoercePrefixToString, class MakePrefixIfNeeded, class ValidateOutputChannelCount, toOutputChannelCount)
import WAGS.WebAPI (AnalyserNodeCb, BrowserPeriodicWave)

type CreateStepRLSig
  (rl :: RL.RowList Type)
  (prefix :: Type)
  (map :: Type)
  (r :: Row Type)
  (inGraph :: Graph)
  (outGraph :: Graph) =
  forall proxyPrefix proxyMap proxyRL
   . proxyRL rl
  -> proxyPrefix prefix
  -> proxyMap map
  -> Object String
  -> { | r }
  -> WAG inGraph
  -> WAG outGraph /\ Object String

class
  CreateStepRL
    (rl :: RL.RowList Type)
    (prefix :: Type)
    (map :: Type)
    (r :: Row Type)
    (inGraph :: Graph)
    (outGraph :: Graph)
  | rl prefix map r inGraph -> outGraph where
  createStepRL :: CreateStepRLSig rl prefix map r inGraph outGraph

instance createStepRLNil :: CreateStepRL RL.Nil prefix map r inGraph inGraph where
  createStepRL _ _ _ o _ r = r /\ o

instance createStepRLCons ::
  ( IsSymbol key
  , R.Cons key val ignore r
  , MakePrefixIfNeeded key prefix prefix'
  , ConstructEdges prefix' map val newPrefix newMap (node /\ { | edges })
  , CoercePrefixToString prefix realPrefix
  , Sym.Append realPrefix key newKey
  , Create' newKey node graph0 graph1
  , RL.RowToList edges edgesRL
  -- push the new prefix and new map down to the edges
  , CreateStepRL edgesRL newPrefix newMap edges graph1 graph2
  -- on this level, we keep the old stuff
  , CreateStepRL rest prefix map r graph2 graph3
  ) =>
  CreateStepRL (RL.Cons key val rest) prefix map r graph0 graph3 where
  createStepRL _ _ _ o rx r = step3 /\ inputCache3
    where
    (_ /\ _ /\ (node /\ edges)) = constructEdges (Proxy :: _ prefix')
      (Proxy :: _ map)
      (Record.get (Proxy :: _ key) rx)
    step1 /\ inputCache1 = create' (Proxy :: _ newKey) node (r)
    step2 /\ inputCache2 =
      ( createStepRL
          :: CreateStepRLSig edgesRL newPrefix newMap edges graph1 graph2
      ) Proxy Proxy Proxy (Object.union inputCache1 o) edges
        (step1)
    step3 /\ inputCache3 = createStepRL (Proxy :: _ rest) (Proxy :: _ prefix)
      (Proxy :: _ map)
      inputCache2
      rx
      (step2)

class
  ConnectEdgesToNode
    (sources :: RL.RowList Type)
    (dest :: Symbol)
    (inGraph :: Graph)
    (outGraph :: Graph)
  | sources dest inGraph -> outGraph where
  connectEdgesToNode
    :: forall proxyRL proxyS
     . proxyRL sources
    -> proxyS dest
    -> Object String
    -> WAG inGraph
    -> WAG outGraph

instance connectEdgesToNodeNil :: ConnectEdgesToNode RL.Nil dest inGraph inGraph where
  connectEdgesToNode _ _ _ w = w

instance connectEdgesToNodeCons ::
  ( IsSymbol key
  , Connect key dest inGraph midGraph
  , ConnectEdgesToNode rest dest midGraph outGraph
  ) =>
  ConnectEdgesToNode (RL.Cons key ignore rest) dest inGraph outGraph where
  connectEdgesToNode _ px o w = step2
    where
    step1 = connect (Object.lookup (reflectSymbol (Proxy :: _ key)) o)
      { source: (Proxy :: _ key), dest: (Proxy :: _ dest) }
      (w)

    step2 = connectEdgesToNode (Proxy :: _ rest) px o (step1)

type ConnectAfterCreateSig
  (prefix :: Type)
  (map :: Type)
  (rl :: RL.RowList Type)
  (inGraph :: Graph)
  (outGraph :: Graph) =
  forall proxyPrefix proxyMap proxyRL
   . proxyPrefix prefix
  -> proxyMap map
  -> proxyRL rl
  -> Object String
  -> WAG inGraph
  -> WAG outGraph

class
  ConnectAfterCreate
    (prefix :: Type)
    (map :: Type)
    (rl :: RL.RowList Type)
    (inGraph :: Graph)
    (outGraph :: Graph)
  | prefix map rl inGraph -> outGraph where
  connectAfterCreate :: ConnectAfterCreateSig prefix map rl inGraph outGraph

instance connectAfterCreateNil ::
  ConnectAfterCreate prefix map RL.Nil graph0 graph0 where
  connectAfterCreate _ _ _ _ w = w

instance connectAfterCreateCons ::
  ( MakePrefixIfNeeded sym prefix prefix'
  , ConstructEdgesT prefix' map node' newPrefix newMap (Tuple node { | edges })
  , RL.RowToList edges edgesList
  , CoercePrefixToString prefix realPrefix
  , Sym.Append realPrefix sym newKey
  , AddPrefixToRowList newPrefix edgesList oel
  , ConnectEdgesToNode oel newKey graph0 graph1
  , ConnectAfterCreate newPrefix newMap edgesList graph1 graph2
  , ConnectAfterCreate prefix map rest graph2 graph3
  ) =>
  ConnectAfterCreate prefix map (RL.Cons sym node' rest) graph0 graph3 where
  connectAfterCreate _ _ _ o w = step3
    where
    step1 = connectEdgesToNode (Proxy :: _ oel) (Proxy :: _ newKey) o (w)

    step2 = connectAfterCreate (Proxy :: _ newPrefix) (Proxy :: _ newMap)
      (Proxy :: _ edgesList)
      o
      (step1)

    step3 = connectAfterCreate (Proxy :: _ prefix) (Proxy :: _ map)
      (Proxy :: _ rest)
      o
      (step2)

type CreateInternalSig
  (prefix :: Type)
  (map :: Type)
  (r :: Row Type)
  (inGraph :: Graph)
  (outGraph :: Graph) =
  forall proxyPrefix proxyMap
   . proxyPrefix prefix
  -> proxyMap map
  -> { | r }
  -> WAG inGraph
  -> WAG outGraph

class
  CreateInternal
    (prefix :: Type)
    (map :: Type)
    (r :: Row Type)
    (inGraph :: Graph)
    (outGraph :: Graph)
  | prefix map r inGraph -> outGraph where
  createInternal :: CreateInternalSig prefix map r inGraph outGraph

instance createInternalAll ::
  ( RL.RowToList r rl
  , CreateStepRL rl prefix map r inGraph midGraph
  , ConnectAfterCreate prefix map rl midGraph outGraph
  ) =>
  CreateInternal prefix map r inGraph outGraph where
  createInternal _ _ z r = step1
    where
    step0 /\ inputCache =
      (createStepRL :: CreateStepRLSig rl prefix map r inGraph midGraph)
        Proxy
        Proxy
        Proxy
        Object.empty
        z
        r

    step1 = connectAfterCreate (Proxy :: _ prefix) (Proxy :: _ map)
      (Proxy :: _ rl)
      inputCache
      (step0)

class
  Create (r :: Row Type) (inGraph :: Graph) (outGraph :: Graph)
  | r inGraph -> outGraph where
  create
    :: { | r }
    -> WAG inGraph
    -> WAG outGraph

instance createAll ::
  CreateInternal Unit Unit r inGraph outGraph =>
  Create r inGraph outGraph where
  create =
    (createInternal :: CreateInternalSig Unit Unit r inGraph outGraph)
      Proxy
      Proxy

-- | Create an audio unit `node` in `igraph` with index `ptr`, resulting in `ograph`.
class
  Create' (ptr :: Symbol) (node :: Type) (inGraph :: Graph) (outGraph :: Graph)
  | ptr node inGraph -> outGraph where
  create'
    :: forall proxy
     . proxy ptr
    -> node
    -> WAG inGraph
    -> WAG outGraph /\ Object String

instance createUnit ::
  Create' ptr Unit graphi graphi where
  create' _ _ w = w /\ Object.empty

instance createInput ::
  ( IsSymbol ptr
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TInput {}) graphi grapho
  ) =>
  Create' ptr (Core.Input) graphi grapho where
  create' ptr (Core.Input input) w = o /\ Object.singleton id input
    where
    WAG { instructions } = w

    id = reflectSymbol ptr
    -- todo: un-hard-code
    o =
      WAG
        { instructions: insert
            (I.iMakeInput { id, input })
            instructions
        }

instance createAnalyser ::
  ( IsSymbol ptr
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TAnalyser {}) graphi grapho
  ) =>
  Create' ptr (CTOR.Analyser AnalyserNodeCb) graphi grapho where
  create' ptr (CTOR.Analyser cb) w = o /\ Object.empty
    where
    WAG { instructions } = w

    id = reflectSymbol ptr
    -- todo: un-hard-code
    o =
      WAG
        { instructions: insert
            ( I.iMakeAnalyser
                { id
                , cb
                , channelCount: 2
                , channelCountMode: "max"
                , channelInterpretation: "speakers"
                , fftSize: 2048
                , maxDecibels: -30.0
                , minDecibels: -100.0
                , smoothingTimeConstant: 0.8
                , parent: nothing
                }
            )
            instructions
        }

instance createAllpass ::
  ( IsSymbol ptr
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TAllpass {}) graphi grapho
  ) =>
  Create' ptr CTOR.Allpass graphi grapho where
  create' ptr (CTOR.Allpass { frequency, q }) w = o /\ Object.empty
    where
    WAG { instructions } = w
    id = reflectSymbol ptr
    o =
      WAG
        { instructions: insert
            (I.iMakeAllpass { id, frequency, q, parent: nothing })
            instructions
        }

class
  CreateParameters (parameterRL :: RL.RowList Type) (parameterData :: Row Type) where
  createParameters
    :: forall proxyRL
     . proxyRL parameterRL
    -> { | parameterData }
    -> Object AudioParameter

instance createParametersNil :: CreateParameters RL.Nil parameterData where
  createParameters _ _ = Object.empty

instance createParametersCons ::
  ( IsSymbol key
  , R.Cons key AudioParameter parameters' parameterData
  , CreateParameters rest parameterData
  ) =>
  CreateParameters (RL.Cons key AudioParameter rest) parameterData where
  createParameters _ p =
    let
      px = Proxy :: _ key
      param = Record.get px p
      rest = createParameters (Proxy :: _ rest) p
    in
      Object.insert (reflectSymbol px) param rest

instance createAudioWorkletNode ::
  ( IsSymbol ptr
  , IsSymbol sym
  , R.Lacks ptr graphi
  , R.Cons ptr
      ( NodeC
          ( CTOR.TAudioWorkletNode sym numberOfInputs numberOfOutputs
              outputChannelCount
              parameterData
              processorOptions
          )
          {}
      )
      graphi
      grapho
  , RL.RowToList parameterData parameterDataRL
  , CreateParameters parameterDataRL parameterData
  , Monoid { | processorOptions }
  , Nat numberOfInputs
  , Pos numberOfOutputs
  , ValidateOutputChannelCount numberOfOutputs outputChannelCount
  , JSON.WriteForeign { | processorOptions }
  ) =>
  Create'
    ptr
    ( CTOR.AudioWorkletNode sym numberOfInputs numberOfOutputs
        outputChannelCount
        parameterData
        processorOptions
    )
    graphi
    grapho where
  create' ptr (CTOR.AudioWorkletNode _ (AudioWorkletNodeOptions options)) w = o
    /\ Object.empty
    where
    WAG { instructions } = w

    id = reflectSymbol ptr

    o =
      WAG
        { instructions: insert
            ( I.iMakeAudioWorkletNode
                { id
                , options: AudioWorkletNodeOptions_
                    { name: reflectSymbol (Proxy :: _ sym)
                    , numberOfInputs: toInt' (Proxy :: _ numberOfInputs)
                    , numberOfOutputs: toInt' (Proxy :: _ numberOfOutputs)
                    , outputChannelCount: toOutputChannelCount
                        (Proxy :: _ numberOfOutputs)
                        (Proxy :: _ outputChannelCount)
                    , parameterData: createParameters
                        (Proxy :: _ parameterDataRL)
                        options.parameterData
                    , processorOptions: JSON.writeImpl
                        (mempty :: { | processorOptions })
                    }
                , parent: nothing
                }
            )
            instructions
        }

instance createBandpass ::
  ( IsSymbol ptr
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TBandpass {}) graphi grapho
  ) =>
  Create' ptr CTOR.Bandpass graphi grapho where
  create' ptr (CTOR.Bandpass { frequency, q }) w = o /\ Object.empty
    where
    WAG { instructions } = w
    id = reflectSymbol ptr
    o =
      WAG
        { instructions: insert
            ( I.iMakeBandpass
                { id
                , frequency
                , q
                , parent: nothing
                }
            )
            instructions

        }

instance createConstant ::
  ( IsSymbol ptr
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TConstant {}) graphi grapho
  ) =>
  Create' ptr CTOR.Constant graphi grapho where
  create' ptr (CTOR.Constant { onOff, offset }) w = o /\ Object.empty
    where
    WAG { instructions } = w

    id = reflectSymbol ptr

    o =
      WAG
        { instructions: insert
            ( I.iMakeConstant
                { id
                , onOff
                , offset
                , parent: nothing
                }
            )
            instructions

        }

instance createConvolver ::
  ( IsSymbol ptr
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TConvolver {}) graphi grapho
  ) =>
  Create' ptr (CTOR.Convolver) graphi grapho where
  create' ptr (CTOR.Convolver { buffer }) w = o /\ Object.empty
    where
    WAG { instructions } = w

    id = reflectSymbol ptr

    o =
      WAG
        { instructions: insert
            (I.iMakeConvolver { id, buffer, parent: nothing })
            instructions
        }

instance createDelay ::
  ( IsSymbol ptr
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TDelay {}) graphi grapho
  ) =>
  Create' ptr CTOR.Delay graphi grapho where
  create' ptr (CTOR.Delay { delayTime }) w = o /\ Object.empty
    where
    WAG { instructions } = w

    id = reflectSymbol ptr

    o =
      WAG
        { instructions: insert
            ( I.iMakeDelay
                { id
                , delayTime
                , parent: nothing
                }
            )
            instructions

        }

instance createDynamicsCompressor ::
  ( IsSymbol ptr
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TDynamicsCompressor {}) graphi grapho
  ) =>
  Create' ptr (CTOR.DynamicsCompressor) graphi grapho where
  create'
    ptr
    (CTOR.DynamicsCompressor { knee, threshold, ratio, attack, release })
    w = o /\ Object.empty
    where
    WAG { instructions } = w

    id = reflectSymbol ptr

    o =
      WAG
        { instructions: insert
            ( I.iMakeDynamicsCompressor
                { id
                , knee
                , threshold
                , ratio
                , attack
                , release
                , parent: nothing
                }
            )
            instructions
        }

instance createGain ::
  ( IsSymbol ptr
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TGain {}) graphi grapho
  ) =>
  Create' ptr CTOR.Gain graphi grapho where
  create' ptr (CTOR.Gain { gain }) w = o /\ Object.empty
    where
    WAG { instructions } = w

    id = reflectSymbol ptr

    o = WAG
      { instructions: insert (I.iMakeGain { id, gain, parent: nothing })
          instructions
      }

instance createHighpass ::
  ( IsSymbol ptr
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.THighpass {}) graphi grapho
  ) =>
  Create' ptr (CTOR.Highpass) graphi grapho where
  create' ptr (CTOR.Highpass { frequency, q }) w = o /\ Object.empty
    where
    WAG { instructions } = w
    id = reflectSymbol ptr

    o = WAG
      { instructions: insert
          ( I.iMakeHighpass
              { id
              , frequency
              , q
              , parent: nothing
              }
          )
          instructions

      }

instance createHighshelf ::
  ( IsSymbol ptr
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.THighshelf {}) graphi grapho
  ) =>
  Create' ptr (CTOR.Highshelf) graphi grapho where
  create' ptr (CTOR.Highshelf { frequency, gain }) w = o /\ Object.empty
    where
    WAG { instructions } = w

    id = reflectSymbol ptr

    o = WAG
      { instructions: insert
          ( I.iMakeHighshelf
              { id
              , frequency
              , gain
              , parent: nothing
              }
          )
          instructions

      }

instance createLoopBuf ::
  ( IsSymbol ptr
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TLoopBuf {}) graphi grapho
  ) =>
  Create' ptr CTOR.LoopBuf graphi grapho where
  create'
    ptr
    (CTOR.LoopBuf { buffer, onOff, playbackRate, loopStart, loopEnd, duration })
    w = o /\ Object.empty
    where
    WAG { instructions } = w

    id = reflectSymbol ptr

    o = WAG
      { instructions: insert
          ( I.iMakeLoopBuf
              { id
              , buffer
              , onOff
              , playbackRate
              , loopStart
              , loopEnd
              , duration
              , parent: nothing
              }
          )
          instructions

      }

instance createLowpass ::
  ( IsSymbol ptr
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TLowpass {}) graphi grapho
  ) =>
  Create' ptr (CTOR.Lowpass) graphi grapho where
  create' ptr (CTOR.Lowpass { frequency, q }) w = o /\ Object.empty
    where
    WAG { instructions } = w

    id = reflectSymbol ptr

    o =
      WAG
        { instructions: insert
            ( I.iMakeLowpass
                { id
                , frequency
                , q
                , parent: nothing
                }
            )
            instructions

        }

instance createLowshelf ::
  ( IsSymbol ptr
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TLowshelf {}) graphi grapho
  ) =>
  Create' ptr (CTOR.Lowshelf) graphi grapho where
  create' ptr (CTOR.Lowshelf { frequency, gain }) w = o /\ Object.empty
    where
    WAG { instructions } = w

    id = reflectSymbol ptr

    o = WAG
      { instructions: insert
          ( I.iMakeLowshelf
              { id
              , frequency
              , gain
              , parent: nothing
              }
          )
          instructions

      }

instance createMediaElement ::
  ( IsSymbol ptr
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TMediaElement {}) graphi grapho
  ) =>
  Create' ptr (CTOR.MediaElement) graphi grapho where
  create' ptr (CTOR.MediaElement { element }) w = o /\ Object.empty
    where
    WAG { instructions } = w

    id = reflectSymbol ptr

    o = WAG
      { instructions: insert
          (I.iMakeMediaElement { id, element, parent: nothing })
          instructions
      }

instance createMicrophone ::
  ( R.Lacks "microphone" graphi
  , R.Cons "microphone" (NodeC CTOR.TMicrophone {}) graphi grapho
  ) =>
  Create' "microphone" (CTOR.Microphone) graphi grapho where
  create' _ (CTOR.Microphone { microphone }) w = o /\ Object.empty
    where
    WAG { instructions } = w

    o = WAG
      { instructions: insert
          (I.iMakeMicrophone { id: "microphone", microphone, parent: nothing })
          instructions

      }

instance createNotch ::
  ( IsSymbol ptr
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TNotch {}) graphi grapho
  ) =>
  Create' ptr (CTOR.Notch) graphi grapho where
  create' ptr (CTOR.Notch { frequency, q }) w = o /\ Object.empty
    where
    WAG { instructions } = w

    id = reflectSymbol ptr

    o = WAG
      { instructions: insert
          ( I.iMakeNotch
              { id
              , frequency
              , q
              , parent: nothing
              }
          )
          instructions
      }

instance createPeaking ::
  ( IsSymbol ptr
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TPeaking {}) graphi grapho
  ) =>
  Create' ptr (CTOR.Peaking) graphi grapho where
  create' ptr (CTOR.Peaking { frequency, gain, q }) w = o /\ Object.empty
    where
    WAG { instructions } = w

    id = reflectSymbol ptr

    o = WAG
      { instructions: insert
          (I.iMakePeaking { id, frequency, gain, q, parent: nothing })
          instructions
      }

instance createPeriodicOsc ::
  ( IsSymbol ptr
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TPeriodicOsc {}) graphi grapho
  ) =>
  Create' ptr (CTOR.PeriodicOsc BrowserPeriodicWave) graphi grapho where
  create' ptr (CTOR.PeriodicOsc { wave, onOff, frequency }) w = o /\
    Object.empty
    where
    WAG { instructions } = w

    id = reflectSymbol ptr

    o = WAG
      { instructions: insert
          ( I.iMakePeriodicOsc
              { id, spec: _wave wave, onOff, frequency, parent: nothing }
          )
          instructions

      }

instance createPeriodicOsc2 ::
  ( IsSymbol ptr
  , Lt D1 a
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TPeriodicOsc {}) graphi grapho
  ) =>
  Create' ptr
    (CTOR.PeriodicOsc (V.Vec a Number /\ V.Vec a Number))
    graphi
    grapho where
  create' ptr (CTOR.PeriodicOsc { wave, onOff, frequency }) w = o /\
    Object.empty
    where
    WAG { instructions } = w

    id = reflectSymbol ptr

    o = WAG
      { instructions: insert
          ( I.iMakePeriodicOsc
              { id
              , spec: _realImg
                  ( wave # \(real /\ img) -> RealImg
                      { real: V.toArray real, img: V.toArray img }
                  )
              , onOff: onOff
              , frequency
              , parent: nothing
              }
          )
          instructions

      }

instance createPlayBuf ::
  ( IsSymbol ptr
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TPlayBuf {}) graphi grapho
  ) =>
  Create' ptr (CTOR.PlayBuf) graphi grapho where
  create'
    ptr
    (CTOR.PlayBuf { buffer, bufferOffset, playbackRate, onOff, duration })
    w = o /\ Object.empty
    where
    WAG { instructions } = w
    id = reflectSymbol ptr
    o = WAG
      { instructions: insert
          ( I.iMakePlayBuf
              { id
              , buffer
              , bufferOffset
              , onOff
              , playbackRate
              , duration
              , parent: nothing
              }
          )
          instructions

      }

instance createRecorder ::
  ( IsSymbol ptr
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TRecorder {}) graphi grapho
  ) =>
  Create' ptr (CTOR.Recorder) graphi grapho where
  create' ptr (CTOR.Recorder { cb }) w = o /\ Object.empty
    where
    WAG { instructions } = w

    id = reflectSymbol ptr

    o = WAG
      { instructions: insert
          ( I.iMakeRecorder
              { id
              , cb
              , parent: nothing
              }
          )
          instructions

      }

instance createSawtoothOsc ::
  ( IsSymbol ptr
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TSawtoothOsc {}) graphi grapho
  ) =>
  Create' ptr (CTOR.SawtoothOsc) graphi grapho where
  create' ptr (CTOR.SawtoothOsc { onOff, frequency }) w = o /\ Object.empty
    where
    WAG { instructions } = w

    id = reflectSymbol ptr

    o = WAG
      { instructions: insert
          (I.iMakeSawtoothOsc { id, onOff, frequency, parent: nothing })
          instructions
      }

instance createSinOsc ::
  ( IsSymbol ptr
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TSinOsc {}) graphi grapho
  ) =>
  Create' ptr (CTOR.SinOsc) graphi grapho where
  create' ptr (CTOR.SinOsc { onOff, frequency }) w = o /\ Object.empty
    where
    WAG { instructions } = w

    id = reflectSymbol ptr

    o = WAG
      { instructions: insert
          (I.iMakeSinOsc { id, onOff, frequency, parent: nothing })
          instructions

      }

instance createSquareOsc ::
  ( IsSymbol ptr
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TSquareOsc {}) graphi grapho
  ) =>
  Create' ptr (CTOR.SquareOsc) graphi grapho where
  create' ptr (CTOR.SquareOsc { frequency, onOff }) w = o /\ Object.empty
    where
    WAG { instructions } = w

    id = reflectSymbol ptr

    o = WAG
      { instructions: insert
          (I.iMakeSquareOsc { id, onOff, frequency, parent: nothing })
          instructions

      }

instance createStereoPanner ::
  ( IsSymbol ptr
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TStereoPanner {}) graphi grapho
  ) =>
  Create' ptr (CTOR.StereoPanner) graphi grapho where
  create' ptr (CTOR.StereoPanner { pan }) w = o /\ Object.empty
    where
    WAG { instructions } = w

    id = reflectSymbol ptr

    o = WAG
      { instructions: insert (I.iMakeStereoPanner { id, pan, parent: nothing })
          instructions
      }

instance createTriangleOsc ::
  ( IsSymbol ptr
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TTriangleOsc {}) graphi grapho
  ) =>
  Create' ptr (CTOR.TriangleOsc) graphi grapho where
  create' ptr (CTOR.TriangleOsc { frequency, onOff }) w = o /\ Object.empty
    where
    WAG { instructions } = w

    id = reflectSymbol ptr

    o = WAG
      { instructions: insert
          ( I.iMakeTriangleOsc
              { id, onOff, frequency, parent: nothing }
          )
          instructions
      }

instance createWaveShaper ::
  ( IsSymbol ptr
  , IsOversample oversample
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC (CTOR.TWaveShaper oversample) {}) graphi grapho
  ) =>
  Create' ptr (CTOR.WaveShaper oversample) graphi grapho where
  create' ptr (CTOR.WaveShaper { floatArray, oversample: oversample' }) w = o /\
    Object.empty
    where
    WAG { instructions } = w

    id = reflectSymbol ptr

    oversample = reflectOversample oversample'

    o =
      WAG
        { instructions: insert
            ( I.iMakeWaveShaper
                { id, curve: floatArray, oversample, parent: nothing }
            )
            instructions

        }