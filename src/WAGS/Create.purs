module WAGS.Create where

import Prelude

import Control.Comonad (extract)
import Data.Functor (voidRight)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Typelevel.Num (class Lt, class Nat, class Pos, D1, toInt')
import Data.Vec as V
import Foreign.Object (Object)
import Foreign.Object as Object
import Prim.Row as R
import Prim.RowList as RL
import Prim.Symbol as Sym
import Record as Record
import Simple.JSON as JSON
import Type.Proxy (Proxy(..))
import WAGS.Connect (class Connect, connect)
import WAGS.ConstructEdges (class ConstructEdges, class ConstructEdgesT, constructEdges)
import WAGS.Control.Indexed (IxWAG(..))
import WAGS.Control.Types (WAG, unsafeUnWAG, unsafeWAG)
import WAGS.Graph.AudioUnit (AudioWorkletNodeOptions(..))
import WAGS.Graph.AudioUnit as CTOR
import WAGS.Graph.Graph (Graph)
import WAGS.Graph.Node (NodeC)
import WAGS.Graph.Oversample (class IsOversample, reflectOversample)
import WAGS.Graph.Parameter (AudioParameter)
import WAGS.Interpret (class AudioInterpret, AsSubgraph, makeAllpass, makeAnalyser, makeAudioWorkletNode, makeBandpass, makeConstant, makeConvolver, makeDelay, makeDynamicsCompressor, makeGain, makeHighpass, makeHighshelf, makeInput, makeLoopBuf, makeLowpass, makeLowshelf, makeMediaElement, makeMicrophone, makeNotch, makePeaking, makePeriodicOsc, makePeriodicOscV, makePlayBuf, makeRecorder, makeSawtoothOsc, makeSinOsc, makeSpeaker, makeSquareOsc, makeStereoPanner, makeSubgraph, makeTriangleOsc, makeTumult, makeWaveShaper, unAsSubGraph)
import WAGS.Rendered (AudioWorkletNodeOptions_(..), RealImg(..))
import WAGS.Tumult (safeUntumult)
import WAGS.Util (class AddPrefixToRowList, class CoercePrefixToString, class MakePrefixIfNeeded, class ValidateOutputChannelCount, toOutputChannelCount)
import WAGS.WebAPI (AnalyserNodeCb, BrowserPeriodicWave)

type CreateStepRLSig
  (rl :: RL.RowList Type)
  (prefix :: Type)
  (map :: Type)
  (r :: Row Type)
  (inGraph :: Graph)
  (outGraph :: Graph) =
  forall proxyPrefix proxyMap proxyRL audio engine proof res
   . AudioInterpret audio engine
  => proxyRL rl
  -> proxyPrefix prefix
  -> proxyMap map
  -> WAG audio engine proof res inGraph { | r }
  -> WAG audio engine proof res outGraph Unit

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
  createStepRL _ _ _ r = r $> unit

instance createStepRLConsB ::
  ( IsSymbol key
  , R.Cons key (Tuple (CTOR.Subgraph inputs subgraphMaker env) ignoreMe) ignore
      r
  , MakePrefixIfNeeded key prefix prefix'
  , ConstructEdges prefix' map
      (Tuple (CTOR.Subgraph inputs subgraphMaker env) ignoreMe)
      newPrefix
      newMap
      (node /\ { | edges })
  , CoercePrefixToString prefix realPrefix
  , Sym.Append realPrefix key newKey
  , RL.RowToList edges edgesRL
  -- push the new prefix and new map down to the edges
  , CreateStepRL edgesRL newPrefix newMap edges graph0 graph1
  -- on this level, we keep the old stuff
  , CreateStepRL rest prefix map r graph1 graph2
  , Create' newKey node graph2 graph3
  ) =>
  CreateStepRL ( RL.Cons key
        (Tuple (CTOR.Subgraph inputs subgraphMaker env) ignoreMe)
        rest
    )
    prefix
    map
    r
    graph0
    graph3 where
  createStepRL _ _ _ r = step3
    where
    rx = extract r
    (_ /\ _ /\ (node /\ edges)) = constructEdges (Proxy :: _ prefix')
      (Proxy :: _ map)
      (Record.get (Proxy :: _ key) rx)
    step1 =
      ( createStepRL
          :: CreateStepRLSig edgesRL newPrefix newMap edges graph0 graph1
      ) Proxy Proxy Proxy
        (r $> edges)
    step2 = createStepRL (Proxy :: _ rest) (Proxy :: _ prefix) (Proxy :: _ map)
      (step1 $> rx)
    step3 = create' (Proxy :: _ newKey) (step2 $> node)
else instance createStepRLConsC ::
  ( IsSymbol key
  , R.Cons key
      (Tuple (CTOR.Tumult tumultN tumultTerminus tumultInputs) ignoreMe)
      ignore
      r
  , MakePrefixIfNeeded key prefix prefix'
  , ConstructEdges prefix' map
      (Tuple (CTOR.Tumult tumultN tumultTerminus tumultInputs) ignoreMe)
      newPrefix
      newMap
      (node /\ { | edges })
  , CoercePrefixToString prefix realPrefix
  , Sym.Append realPrefix key newKey
  , RL.RowToList edges edgesRL
  -- push the new prefix and new map down to the edges
  , CreateStepRL edgesRL newPrefix newMap edges graph0 graph1
  -- on this level, we keep the old stuff
  , CreateStepRL rest prefix map r graph1 graph2
  , Create' newKey node graph2 graph3
  ) =>
  CreateStepRL ( RL.Cons key
        (Tuple (CTOR.Tumult tumultN tumultTerminus tumultInputs) ignoreMe)
        rest
    )
    prefix
    map
    r
    graph0
    graph3 where
  createStepRL _ _ _ r = step3
    where
    rx = extract r
    (_ /\ _ /\ (node /\ edges)) = constructEdges (Proxy :: _ prefix')
      (Proxy :: _ map)
      (Record.get (Proxy :: _ key) rx)
    step1 =
      ( createStepRL
          :: CreateStepRLSig edgesRL newPrefix newMap edges graph0 graph1
      ) Proxy Proxy Proxy
        (r $> edges)
    step2 = createStepRL (Proxy :: _ rest) (Proxy :: _ prefix) (Proxy :: _ map)
      (step1 $> rx)
    step3 = create' (Proxy :: _ newKey) (step2 $> node)
else instance createStepRLCons ::
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
  createStepRL _ _ _ r = step3
    where
    rx = extract r
    (_ /\ _ /\ (node /\ edges)) = constructEdges (Proxy :: _ prefix')
      (Proxy :: _ map)
      (Record.get (Proxy :: _ key) rx)
    step1 = create' (Proxy :: _ newKey) (r $> node)
    step2 =
      ( createStepRL
          :: CreateStepRLSig edgesRL newPrefix newMap edges graph1 graph2
      ) Proxy Proxy Proxy
        (step1 $> edges)
    step3 = createStepRL (Proxy :: _ rest) (Proxy :: _ prefix) (Proxy :: _ map)
      (step2 $> rx)

class
  ConnectEdgesToNode
    (sources :: RL.RowList Type)
    (dest :: Symbol)
    (inGraph :: Graph)
    (outGraph :: Graph)
  | sources dest inGraph -> outGraph where
  connectEdgesToNode
    :: forall proxyRL proxyS audio engine proof res
     . AudioInterpret audio engine
    => proxyRL sources
    -> WAG audio engine proof res inGraph (proxyS dest)
    -> WAG audio engine proof res outGraph (proxyS dest)

instance connectEdgesToNodeNil :: ConnectEdgesToNode RL.Nil dest inGraph inGraph where
  connectEdgesToNode _ w = w

instance connectEdgesToNodeCons ::
  ( Connect key dest inGraph midGraph
  , ConnectEdgesToNode rest dest midGraph outGraph
  ) =>
  ConnectEdgesToNode (RL.Cons key ignore rest) dest inGraph outGraph where
  connectEdgesToNode _ w = step2
    where
    step1 = connect (w $> { source: (Proxy :: _ key), dest: (Proxy :: _ dest) })

    step2 = connectEdgesToNode (Proxy :: _ rest) (step1 $> (extract w))

type ConnectAfterCreateSig
  (prefix :: Type)
  (map :: Type)
  (rl :: RL.RowList Type)
  (inGraph :: Graph)
  (outGraph :: Graph) =
  forall proxyPrefix proxyMap audio engine proof res
   . AudioInterpret audio engine
  => proxyPrefix prefix
  -> proxyMap map
  -> WAG audio engine proof res inGraph (Proxy rl)
  -> WAG audio engine proof res outGraph Unit

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
  connectAfterCreate _ _ w = w $> unit

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
  connectAfterCreate _ _ w = step3
    where
    step1 = connectEdgesToNode (Proxy :: _ oel) (w $> (Proxy :: _ newKey))

    step2 = connectAfterCreate (Proxy :: _ newPrefix) (Proxy :: _ newMap)
      (step1 $> (Proxy :: _ edgesList))

    step3 = connectAfterCreate (Proxy :: _ prefix) (Proxy :: _ map)
      (step2 $> (Proxy :: _ rest))

type CreateInternalSig
  (prefix :: Type)
  (map :: Type)
  (r :: Row Type)
  (inGraph :: Graph)
  (outGraph :: Graph) =
  forall proxyPrefix proxyMap audio engine proof res
   . AudioInterpret audio engine
  => proxyPrefix prefix
  -> proxyMap map
  -> WAG audio engine proof res inGraph { | r }
  -> WAG audio engine proof res outGraph Unit

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
  createInternal _ _ r = step1
    where
    step0 = (createStepRL :: CreateStepRLSig rl prefix map r inGraph midGraph)
      Proxy
      Proxy
      Proxy
      r

    step1 = connectAfterCreate (Proxy :: _ prefix) (Proxy :: _ map)
      (step0 $> (Proxy :: _ rl))

class
  Create (r :: Row Type) (inGraph :: Graph) (outGraph :: Graph)
  | r inGraph -> outGraph where
  create
    :: forall audio engine proof res
     . AudioInterpret audio engine
    => WAG audio engine proof res inGraph { | r }
    -> WAG audio engine proof res outGraph Unit

instance createAll ::
  CreateInternal Unit Unit r inGraph outGraph =>
  Create r inGraph outGraph where
  create =
    (createInternal :: CreateInternalSig Unit Unit r inGraph outGraph)
      Proxy
      Proxy

icreate
  :: forall r audio engine proof res inGraph outGraph
   . AudioInterpret audio engine
  => Create r inGraph outGraph
  => { | r }
  -> IxWAG audio engine proof res inGraph outGraph Unit
icreate r = IxWAG (create <<< voidRight r)

-- | Create an audio unit `node` in `igraph` with index `ptr`, resulting in `ograph`.
class
  Create' (ptr :: Symbol) (node :: Type) (inGraph :: Graph) (outGraph :: Graph)
  | ptr node inGraph -> outGraph where
  create'
    :: forall proxy audio engine proof res
     . AudioInterpret audio engine
    => proxy ptr
    -> WAG audio engine proof res inGraph node
    -> WAG audio engine proof res outGraph Unit

icreate'
  :: forall proxy ptr node audio engine proof res i o
   . AudioInterpret audio engine
  => Create' ptr node i o
  => proxy ptr
  -> node
  -> IxWAG audio engine proof res i o Unit
icreate' ptr node = IxWAG (create' ptr <<< voidRight node)

instance createUnit ::
  Create' ptr Unit graphi graphi where
  create' _ w = w

instance createAnalyser ::
  ( IsSymbol ptr
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TAnalyser {}) graphi grapho
  ) =>
  Create' ptr (CTOR.Analyser AnalyserNodeCb) graphi grapho where
  create' ptr w = o
    where
    { context: i, value: (CTOR.Analyser cb) } = unsafeUnWAG w

    id = reflectSymbol ptr

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> [ makeAnalyser { id, cb } ]
              }
        , value: unit
        }

instance createAllpass ::
  ( IsSymbol ptr
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TAllpass {}) graphi grapho
  ) =>
  Create' ptr CTOR.Allpass graphi grapho where
  create' ptr w = o
    where
    { context: i, value: (CTOR.Allpass { freq, q }) } = unsafeUnWAG w
    id = reflectSymbol ptr
    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <>
                  [ makeAllpass
                      { id
                      , freq
                      , q
                      }
                  ]
              }
        , value: unit
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
  create' ptr w = o
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
                  [ makeAudioWorkletNode
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
                      }
                  ] --(createParameters nn (Proxy :: _ parameterData) (Proxy :: _ parametersRL) parameters)
              }
        , value: unit
        }

instance createBandpass ::
  ( IsSymbol ptr
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TBandpass {}) graphi grapho
  ) =>
  Create' ptr CTOR.Bandpass graphi grapho where
  create' ptr w = o
    where
    { context: i, value: (CTOR.Bandpass { freq, q }) } = unsafeUnWAG w
    id = reflectSymbol ptr
    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <>
                  [ makeBandpass
                      { id
                      , freq
                      , q
                      }
                  ]
              }
        , value: unit
        }

instance createConstant ::
  ( IsSymbol ptr
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TConstant {}) graphi grapho
  ) =>
  Create' ptr CTOR.Constant graphi grapho where
  create' ptr w = o
    where
    { context: i, value: (CTOR.Constant { onOff, offset }) } = unsafeUnWAG w

    id = reflectSymbol ptr

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <>
                  [ makeConstant
                      { id
                      , onOff
                      , offset
                      }
                  ]
              }
        , value: unit
        }

instance createConvolver ::
  ( IsSymbol ptr
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TConvolver {}) graphi grapho
  ) =>
  Create' ptr (CTOR.Convolver) graphi grapho where
  create' ptr w = o
    where
    { context: i, value: (CTOR.Convolver { buffer }) } = unsafeUnWAG w

    id = reflectSymbol ptr

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <>
                  [ makeConvolver { id, buffer } ]
              }
        , value: unit
        }

instance createDelay ::
  ( IsSymbol ptr
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TDelay {}) graphi grapho
  ) =>
  Create' ptr CTOR.Delay graphi grapho where
  create' ptr w = o
    where
    { context: i, value: (CTOR.Delay { delayTime }) } = unsafeUnWAG w

    id = reflectSymbol ptr

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <>
                  [ makeDelay
                      { id
                      , delayTime
                      }
                  ]
              }
        , value: unit
        }

instance createDynamicsCompressor ::
  ( IsSymbol ptr
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TDynamicsCompressor {}) graphi grapho
  ) =>
  Create' ptr (CTOR.DynamicsCompressor) graphi grapho where
  create' ptr w = o
    where
    { context: i
    , value:
        (CTOR.DynamicsCompressor { knee, threshold, ratio, attack, release })
    } = unsafeUnWAG w

    id = reflectSymbol ptr

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <>
                  [ makeDynamicsCompressor
                      { id
                      , knee
                      , threshold
                      , ratio
                      , attack
                      , release
                      }
                  ]
              }
        , value: unit
        }

instance createGain ::
  ( IsSymbol ptr
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TGain {}) graphi grapho
  ) =>
  Create' ptr CTOR.Gain graphi grapho where
  create' ptr w = o
    where
    { context: i, value: (CTOR.Gain { gain }) } = unsafeUnWAG w

    id = reflectSymbol ptr

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> [ makeGain { id, gain } ]
              }
        , value: unit
        }

instance createHighpass ::
  ( IsSymbol ptr
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.THighpass {}) graphi grapho
  ) =>
  Create' ptr (CTOR.Highpass) graphi grapho where
  create' ptr w = o
    where
    { context: i, value: (CTOR.Highpass { freq, q }) } = unsafeUnWAG w

    id = reflectSymbol ptr

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <>
                  [ makeHighpass
                      { id
                      , freq
                      , q
                      }
                  ]
              }
        , value: unit
        }

instance createHighshelf ::
  ( IsSymbol ptr
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.THighshelf {}) graphi grapho
  ) =>
  Create' ptr (CTOR.Highshelf) graphi grapho where
  create' ptr w = o
    where
    { context: i, value: (CTOR.Highshelf { freq, gain }) } = unsafeUnWAG w

    id = reflectSymbol ptr

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <>
                  [ makeHighshelf
                      { id
                      , freq
                      , gain
                      }
                  ]
              }
        , value: unit
        }

instance createInput ::
  ( IsSymbol ptr
  , IsSymbol sym
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC (CTOR.TInput sym) {}) graphi grapho
  ) =>
  Create' ptr (CTOR.Input sym) graphi grapho where
  create' ptr w = o
    where
    { context: i, value: CTOR.Input } = unsafeUnWAG w

    id = reflectSymbol ptr

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <>
                  [ makeInput { id, input: reflectSymbol (Proxy :: _ sym) } ]
              }
        , value: unit
        }

instance createLoopBuf ::
  ( IsSymbol ptr
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TLoopBuf {}) graphi grapho
  ) =>
  Create' ptr CTOR.LoopBuf graphi grapho where
  create' ptr w = o
    where
    { context: i
    , value: CTOR.LoopBuf { buffer, onOff, playbackRate, loopStart, loopEnd }
    } = unsafeUnWAG w

    id = reflectSymbol ptr

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <>
                  [ makeLoopBuf
                      { id
                      , buffer
                      , onOff
                      , playbackRate
                      , loopStart
                      , loopEnd
                      }
                  ]
              }
        , value: unit
        }

instance createLowpass ::
  ( IsSymbol ptr
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TLowpass {}) graphi grapho
  ) =>
  Create' ptr (CTOR.Lowpass) graphi grapho where
  create' ptr w = o
    where
    { context: i, value: CTOR.Lowpass { freq, q } } = unsafeUnWAG w

    id = reflectSymbol ptr

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <>
                  [ makeLowpass
                      { id
                      , freq
                      , q
                      }
                  ]
              }
        , value: unit
        }

instance createLowshelf ::
  ( IsSymbol ptr
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TLowshelf {}) graphi grapho
  ) =>
  Create' ptr (CTOR.Lowshelf) graphi grapho where
  create' ptr w = o
    where
    { context: i, value: (CTOR.Lowshelf { freq, gain }) } = unsafeUnWAG w

    id = reflectSymbol ptr

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <>
                  [ makeLowshelf
                      { id
                      , freq
                      , gain
                      }
                  ]
              }
        , value: unit
        }

instance createMediaElement ::
  ( IsSymbol ptr
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TMediaElement {}) graphi grapho
  ) =>
  Create' ptr (CTOR.MediaElement) graphi grapho where
  create' ptr w = o
    where
    { context: i, value: (CTOR.MediaElement { media }) } = unsafeUnWAG w

    id = reflectSymbol ptr

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <>
                  [ makeMediaElement { id, element: media } ]
              }
        , value: unit
        }

instance createMicrophone ::
  ( R.Lacks "microphone" graphi
  , R.Cons "microphone" (NodeC CTOR.TMicrophone {}) graphi grapho
  ) =>
  Create' "microphone" (CTOR.Microphone) graphi grapho where
  create' _ w = o
    where
    { context: i, value: (CTOR.Microphone { microphone }) } = unsafeUnWAG w

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <>
                  [ makeMicrophone { microphone } ]
              }
        , value: unit
        }

instance createNotch ::
  ( IsSymbol ptr
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TNotch {}) graphi grapho
  ) =>
  Create' ptr (CTOR.Notch) graphi grapho where
  create' ptr w = o
    where
    { context: i, value: (CTOR.Notch { freq, q }) } = unsafeUnWAG w

    id = reflectSymbol ptr

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <>
                  [ makeNotch
                      { id
                      , freq
                      , q
                      }
                  ]
              }
        , value: unit
        }

instance createPeaking ::
  ( IsSymbol ptr
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TPeaking {}) graphi grapho
  ) =>
  Create' ptr (CTOR.Peaking) graphi grapho where
  create' ptr w = o
    where
    { context: i, value: (CTOR.Peaking { freq, gain, q }) } = unsafeUnWAG w

    id = reflectSymbol ptr

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <>
                  [ makePeaking { id, freq, gain, q } ]
              }
        , value: unit
        }

instance createPeriodicOsc ::
  ( IsSymbol ptr
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TPeriodicOsc {}) graphi grapho
  ) =>
  Create' ptr (CTOR.PeriodicOsc BrowserPeriodicWave) graphi grapho where
  create' ptr w = o
    where
    { context: i, value: (CTOR.PeriodicOsc { wave, onOff, freq }) } =
      unsafeUnWAG w

    id = reflectSymbol ptr

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <>
                  [ makePeriodicOsc { id, wave, onOff, freq } ]
              }
        , value: unit
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
  create' ptr w = o
    where
    { context: i, value: (CTOR.PeriodicOsc { wave, onOff, freq }) } =
      unsafeUnWAG w

    id = reflectSymbol ptr

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <>
                  [ makePeriodicOscV
                      { id
                      , realImg: wave # \(real /\ img) -> RealImg
                          { real: V.toArray real, img: V.toArray img }
                      , onOff: onOff
                      , freq
                      }
                  ]
              }
        , value: unit
        }

instance createPlayBuf ::
  ( IsSymbol ptr
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TPlayBuf {}) graphi grapho
  ) =>
  Create' ptr (CTOR.PlayBuf) graphi grapho where
  create' ptr w = o
    where
    { context: i
    , value: (CTOR.PlayBuf { buffer, bufferOffset, playbackRate, onOff })
    } = unsafeUnWAG w
    id = reflectSymbol ptr
    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <>
                  [ makePlayBuf
                      { id, buffer, bufferOffset, onOff, playbackRate }
                  ]
              }
        , value: unit
        }

instance createRecorder ::
  ( IsSymbol ptr
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TRecorder {}) graphi grapho
  ) =>
  Create' ptr (CTOR.Recorder) graphi grapho where
  create' ptr w = o
    where
    { context: i, value: (CTOR.Recorder { cb }) } = unsafeUnWAG w

    id = reflectSymbol ptr

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <>
                  [ makeRecorder
                      { id
                      , cb
                      }
                  ]
              }
        , value: unit
        }

instance createSawtoothOsc ::
  ( IsSymbol ptr
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TSawtoothOsc {}) graphi grapho
  ) =>
  Create' ptr (CTOR.SawtoothOsc) graphi grapho where
  create' ptr w = o
    where
    { context: i, value: (CTOR.SawtoothOsc { onOff, freq }) } = unsafeUnWAG w

    id = reflectSymbol ptr

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <>
                  [ makeSawtoothOsc { id, onOff, freq } ]
              }
        , value: unit
        }

instance createSinOsc ::
  ( IsSymbol ptr
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TSinOsc {}) graphi grapho
  ) =>
  Create' ptr (CTOR.SinOsc) graphi grapho where
  create' ptr w = o
    where
    { context: i, value: (CTOR.SinOsc { onOff, freq }) } = unsafeUnWAG w

    id = reflectSymbol ptr

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <>
                  [ makeSinOsc { id, onOff, freq } ]
              }
        , value: unit
        }

instance createSpeaker ::
  ( R.Lacks "speaker" graphi
  , R.Cons "speaker" (NodeC CTOR.TSpeaker {}) graphi grapho
  ) =>
  Create' "speaker" CTOR.Speaker graphi grapho where
  create' _ w = o
    where
    { context: i } = unsafeUnWAG w
    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> [ makeSpeaker ]
              }
        , value: unit
        }

instance createSquareOsc ::
  ( IsSymbol ptr
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TSquareOsc {}) graphi grapho
  ) =>
  Create' ptr (CTOR.SquareOsc) graphi grapho where
  create' ptr w = o
    where
    { context: i, value: (CTOR.SquareOsc { freq, onOff }) } = unsafeUnWAG w

    id = reflectSymbol ptr

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <>
                  [ makeSquareOsc { id, onOff, freq }
                  ]
              }
        , value: unit
        }

instance createStereoPanner ::
  ( IsSymbol ptr
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TStereoPanner {}) graphi grapho
  ) =>
  Create' ptr (CTOR.StereoPanner) graphi grapho where
  create' ptr w = o
    where
    { context: i, value: (CTOR.StereoPanner { pan }) } = unsafeUnWAG w

    id = reflectSymbol ptr

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <>
                  [ makeStereoPanner { id, pan } ]
              }
        , value: unit
        }

instance createSubgraph ::
  ( IsSymbol ptr
  , IsSymbol terminus
  , Pos n
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC (CTOR.TSubgraph n terminus inputs env) {}) graphi grapho
  ) =>
  Create' ptr
    (CTOR.Subgraph inputs (AsSubgraph terminus inputs env) (V.Vec n env))
    graphi
    grapho where
  create' ptr w = o
    where
    { context: i, value: (CTOR.Subgraph { subgraphMaker, envs }) } = unsafeUnWAG
      w
    id = reflectSymbol ptr
    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <>
                  [ makeSubgraph
                      { id
                      , terminus: Proxy :: _ terminus
                      , envs
                      , scenes: unAsSubGraph subgraphMaker
                      }
                  ]
              }
        , value: unit
        }

instance createTumult ::
  ( IsSymbol ptr
  , IsSymbol terminus
  , Pos n
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC (CTOR.TTumult n terminus inputs) {}) graphi grapho
  ) =>
  Create' ptr (CTOR.Tumult n terminus inputs) graphi grapho where
  create' ptr w = o
    where
    { context: i, value: (CTOR.Tumult { tumult }) } = unsafeUnWAG w
    id = reflectSymbol ptr
    pt = reflectSymbol (Proxy :: _ terminus)
    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <>
                  [ makeTumult
                      { id, terminus: pt, instructions: safeUntumult tumult }
                  ]
              }
        , value: unit
        }

instance createTriangleOsc ::
  ( IsSymbol ptr
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TTriangleOsc {}) graphi grapho
  ) =>
  Create' ptr (CTOR.TriangleOsc) graphi grapho where
  create' ptr w = o
    where
    { context: i, value: (CTOR.TriangleOsc { freq, onOff }) } = unsafeUnWAG w

    id = reflectSymbol ptr

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <>
                  [ makeTriangleOsc
                      { id, onOff, freq }
                  ]
              }
        , value: unit
        }

instance createWaveShaper ::
  ( IsSymbol ptr
  , IsOversample oversample
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC (CTOR.TWaveShaper oversample) {}) graphi grapho
  ) =>
  Create' ptr (CTOR.WaveShaper oversample) graphi grapho where
  create' ptr w = o
    where
    { context: i
    , value: (CTOR.WaveShaper { floatArray, oversample: oversample' })
    } =
      unsafeUnWAG w

    id = reflectSymbol ptr

    oversample = reflectOversample oversample'

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <>
                  [ makeWaveShaper { id, curve: floatArray, oversample } ]
              }
        , value: unit
        }
