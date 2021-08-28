module WAGS.Create where

import Prelude

import Control.Comonad (extract)
import Data.Functor (voidRight)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Vec as V
import Prim.Row as R
import Prim.RowList as RL
import Prim.Symbol as Sym
import Record as Record
import Type.Proxy (Proxy(..))
import WAGS.Assets (class AssetsHave, Buffers, FloatArrays, PeriodicWaves, Recorders, Analysers)
import WAGS.Connect (class Connect, connect)
import WAGS.ConstructEdges (class ConstructEdges, class ConstructEdgesT, constructEdges)
import WAGS.Control.Indexed (IxWAG(..))
import WAGS.Control.Types (WAG, unsafeUnWAG, unsafeWAG)
import WAGS.Graph.AudioUnit as CTOR
import WAGS.Graph.Graph (Graph)
import WAGS.Graph.Node (NodeC)
import WAGS.Graph.Oversample (class IsOversample, reflectOversample)
import WAGS.Graph.Paramable (class Paramable, paramize, class OnOffable, onOffIze)
import WAGS.Interpret (class AudioInterpret, makeAllpass, makeAnalyser, makeBandpass, makeConstant, makeConvolver, makeDelay, makeDynamicsCompressor, makeGain, makeHighpass, makeHighshelf, makeLoopBuf, makeLowpass, makeLowshelf, makeMicrophone, makeNotch, makePeaking, makePeriodicOsc, makePeriodicOscV, makePlayBuf, makeRecorder, makeSawtoothOsc, makeSinOsc, makeSpeaker, makeSquareOsc, makeStereoPanner, makeTriangleOsc, makeWaveShaper)
import WAGS.Util (class AddPrefixToRowList, class CoercePrefixToString, class MakePrefixIfNeeded)

type CreateStepRLSig (rl :: RL.RowList Type) (prefix :: Type) (map :: Type) (assets :: Row Type) (r :: Row Type) (inGraph :: Graph) (outGraph :: Graph)
  = forall proxyPrefix proxyMap proxyRL audio engine proof res.
    AudioInterpret audio engine =>
    proxyRL rl ->
    proxyPrefix prefix ->
    proxyMap map ->
    WAG assets audio engine proof res { | inGraph } { | r } ->
    WAG assets audio engine proof res { | outGraph } Unit

class CreateStepRL (rl :: RL.RowList Type) (prefix :: Type) (map :: Type) (assets :: Row Type) (r :: Row Type) (inGraph :: Graph) (outGraph :: Graph) | rl prefix map r inGraph -> outGraph where
  createStepRL :: CreateStepRLSig rl prefix map assets r inGraph outGraph

instance createStepRLNil :: CreateStepRL RL.Nil prefix map assets r inGraph inGraph where
  createStepRL _ _ _ r = r $> unit

instance createStepRLCons ::
  ( IsSymbol key
  , R.Cons key val ignore r
  , MakePrefixIfNeeded key prefix prefix'
  , ConstructEdges prefix' map val newPrefix newMap (node /\ { | edges })
  , CoercePrefixToString prefix realPrefix
  , Sym.Append realPrefix key newKey
  , Create' assets newKey node graph0 graph1
  , RL.RowToList edges edgesRL
  -- push the new prefix and new map down to the edges
  , CreateStepRL edgesRL newPrefix newMap assets edges graph1 graph2
  -- on this level, we keep the old stuff
  , CreateStepRL rest prefix map assets r graph2 graph3
  ) =>
  CreateStepRL (RL.Cons key val rest) prefix map assets r graph0 graph3 where
  createStepRL _ _ _ r = step3
    where
    rx = extract r

    (_ /\ _ /\ (node /\ edges)) = constructEdges (Proxy :: _ prefix') (Proxy :: _ map) (Record.get (Proxy :: _ key) rx)

    step1 = create' (Proxy :: _ newKey) (r $> node)

    step2 =
      (createStepRL :: CreateStepRLSig edgesRL newPrefix newMap assets edges graph1 graph2) Proxy Proxy Proxy
        (step1 $> edges)

    step3 = createStepRL (Proxy :: _ rest) (Proxy :: _ prefix) (Proxy :: _ map) (step2 $> rx)

class ConnectEdgesToNode (sources :: RL.RowList Type) (dest :: Symbol) (inGraph :: Graph) (outGraph :: Graph) | sources dest inGraph -> outGraph where
  connectEdgesToNode ::
    forall proxyRL proxyS assets audio engine proof res.
    AudioInterpret audio engine =>
    proxyRL sources ->
    WAG assets audio engine proof res { | inGraph } (proxyS dest) ->
    WAG assets audio engine proof res { | outGraph } (proxyS dest)

instance connectEdgesToNodeNil :: ConnectEdgesToNode RL.Nil dest inGraph inGraph where
  connectEdgesToNode _ w = w

instance connectEdgesToNodeCons :: (Connect key dest inGraph midGraph, ConnectEdgesToNode rest dest midGraph outGraph) => ConnectEdgesToNode (RL.Cons key ignore rest) dest inGraph outGraph where
  connectEdgesToNode _ w = step2
    where
    step1 = connect (w $> { source: (Proxy :: _ key), dest: (Proxy :: _ dest) })

    step2 = connectEdgesToNode (Proxy :: _ rest) (step1 $> (extract w))

type ConnectAfterCreateSig (prefix :: Type) (map :: Type) (rl :: RL.RowList Type) (inGraph :: Graph) (outGraph :: Graph)
  = forall proxyPrefix proxyMap assets audio engine proof res.
    AudioInterpret audio engine =>
    proxyPrefix prefix ->
    proxyMap map ->
    WAG assets audio engine proof res { | inGraph } (Proxy rl) ->
    WAG assets audio engine proof res { | outGraph } Unit

class ConnectAfterCreate (prefix :: Type) (map :: Type) (rl :: RL.RowList Type) (inGraph :: Graph) (outGraph :: Graph) | prefix map rl inGraph -> outGraph where
  connectAfterCreate :: ConnectAfterCreateSig prefix map rl inGraph outGraph

instance connectAfterCreateNil :: ConnectAfterCreate prefix map RL.Nil graph0 graph0 where
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

    step2 = connectAfterCreate (Proxy :: _ newPrefix) (Proxy :: _ newMap) (step1 $> (Proxy :: _ edgesList))

    step3 = connectAfterCreate (Proxy :: _ prefix) (Proxy :: _ map) (step2 $> (Proxy :: _ rest))

type CreateInternalSig (prefix :: Type) (map :: Type) (assets :: Row Type) (r :: Row Type) (inGraph :: Graph) (outGraph :: Graph)
  = forall proxyPrefix proxyMap audio engine proof res.
    AudioInterpret audio engine =>
    proxyPrefix prefix ->
    proxyMap map ->
    WAG assets audio engine proof res { | inGraph } { | r } ->
    WAG assets audio engine proof res { | outGraph } Unit

class CreateInternal (prefix :: Type) (map :: Type) (assets :: Row Type) (r :: Row Type) (inGraph :: Graph) (outGraph :: Graph) | prefix map r inGraph -> outGraph where
  createInternal :: CreateInternalSig prefix map assets r inGraph outGraph

instance createInternalAll ::
  ( RL.RowToList r rl
  , CreateStepRL rl prefix map assets r inGraph midGraph
  , ConnectAfterCreate prefix map rl midGraph outGraph
  ) =>
  CreateInternal prefix map assets r inGraph outGraph where
  createInternal _ _ r = step1
    where
    step0 = (createStepRL :: CreateStepRLSig rl prefix map assets r inGraph midGraph) Proxy Proxy Proxy r

    step1 = connectAfterCreate (Proxy :: _ prefix) (Proxy :: _ map) (step0 $> (Proxy :: _ rl))

class Create (assets :: Row Type) (r :: Row Type) (inGraph :: Graph) (outGraph :: Graph) | r inGraph -> outGraph where
  create ::
    forall audio engine proof res.
    AudioInterpret audio engine =>
    WAG assets audio engine proof res { | inGraph } { | r } ->
    WAG assets audio engine proof res { | outGraph } Unit

instance createAll ::
  CreateInternal Unit Unit assets r inGraph outGraph =>
  Create assets r inGraph outGraph where
  create =
    (createInternal :: CreateInternalSig Unit Unit assets r inGraph outGraph)
      Proxy
      Proxy

icreate ::
  forall r assets audio engine proof res inGraph outGraph.
  AudioInterpret audio engine =>
  Create assets r inGraph outGraph =>
  { | r } ->
  IxWAG assets audio engine proof res { | inGraph } { | outGraph } Unit
icreate r = IxWAG (create <<< voidRight r)

-- | Create an audio unit `node` in `igraph` with index `ptr`, resulting in `ograph`.
class Create' (assets :: Row Type) (ptr :: Symbol) (node :: Type) (inGraph :: Graph) (outGraph :: Graph) | ptr node inGraph -> outGraph where
  create' ::
    forall proxy audio engine proof res.
    AudioInterpret audio engine =>
    proxy ptr ->
    WAG assets audio engine proof res { | inGraph } node ->
    WAG assets audio engine proof res { | outGraph } Unit

icreate' ::
  forall proxy ptr node assets audio engine proof res i o.
  AudioInterpret audio engine =>
  Create' assets ptr node i o =>
  proxy ptr ->
  node ->
  IxWAG assets audio engine proof res { | i } { | o } Unit
icreate' ptr node = IxWAG (create' ptr <<< voidRight node)

instance createUnit ::
  Create' assets ptr Unit graphi graphi where
  create' _ w = w

instance createAnalyser ::
  ( IsSymbol ptr
  , IsSymbol recorder
  , R.Lacks ptr graphi
  , AssetsHave Analysers recorder assets
  , R.Cons ptr (NodeC (CTOR.TAnalyser recorder) {}) graphi grapho
  ) =>
  Create' assets ptr (CTOR.Analyser recorder) graphi grapho where
  create' ptr w = o
    where
    { context: i, value: (CTOR.Analyser sym) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    recorder = reflectSymbol sym

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> [ makeAnalyser nn recorder ]
              }
        , value: unit
        }


instance createAllpass ::
  ( IsSymbol ptr
  , Paramable argA
  , Paramable argB
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TAllpass {}) graphi grapho
  ) =>
  Create' assets ptr (CTOR.Allpass argA argB) graphi grapho where
  create' ptr w = o
    where
    { context: i, value: (CTOR.Allpass argA argB) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    argA_iv' = paramize argA

    argB_iv' = paramize argB

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> [ makeAllpass nn argA_iv' argB_iv' ]
              }
        , value: unit
        }

instance createBandpass ::
  ( IsSymbol ptr
  , Paramable argA
  , Paramable argB
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TBandpass {}) graphi grapho
  ) =>
  Create' assets ptr (CTOR.Bandpass argA argB) graphi grapho where
  create' ptr w = o
    where
    { context: i, value: (CTOR.Bandpass argA argB) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    argA_iv' = paramize argA

    argB_iv' = paramize argB

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> [ makeBandpass nn argA_iv' argB_iv' ]
              }
        , value: unit
        }

instance createConstant ::
  ( IsSymbol ptr
  , Paramable argA
  , R.Lacks ptr graphi
  , OnOffable onOff
  , OnOffable onOff
  , R.Cons ptr (NodeC CTOR.TConstant {}) graphi grapho
  ) =>
  Create' assets ptr (CTOR.Constant onOff argA) graphi grapho where
  create' ptr w = o
    where
    { context: i, value: (CTOR.Constant onOff argA) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    argA_iv' = paramize argA

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> [ makeConstant nn (onOffIze onOff) argA_iv' ]
              }
        , value: unit
        }

instance createConvolver ::
  ( IsSymbol ptr
  , IsSymbol buffer
  , R.Lacks ptr graphi
  , AssetsHave Buffers buffer assets
  , R.Cons ptr (NodeC (CTOR.TConvolver buffer) {}) graphi grapho
  ) =>
  Create' assets ptr (CTOR.Convolver buffer) graphi grapho where
  create' ptr w = o
    where
    { context: i, value: (CTOR.Convolver sym) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    buffer = reflectSymbol sym

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> [ makeConvolver nn buffer ]
              }
        , value: unit
        }

instance createDelay ::
  ( IsSymbol ptr
  , Paramable argA
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TDelay {}) graphi grapho
  ) =>
  Create' assets ptr (CTOR.Delay argA) graphi grapho where
  create' ptr w = o
    where
    { context: i, value: (CTOR.Delay argA) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    argA_iv' = paramize argA

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> [ makeDelay nn argA_iv' ]
              }
        , value: unit
        }

instance createDynamicsCompressor ::
  ( IsSymbol ptr
  , Paramable argA
  , Paramable argB
  , Paramable argC
  , Paramable argD
  , Paramable argE
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TDynamicsCompressor {}) graphi grapho
  ) =>
  Create' assets ptr (CTOR.DynamicsCompressor argA argB argC argD argE) graphi grapho where
  create' ptr w = o
    where
    { context: i, value: (CTOR.DynamicsCompressor argA argB argC argD argE) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    argA_iv' = paramize argA

    argB_iv' = paramize argB

    argC_iv' = paramize argC

    argD_iv' = paramize argD

    argE_iv' = paramize argE

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> [ makeDynamicsCompressor nn argA_iv' argB_iv' argC_iv' argD_iv' argE_iv' ]
              }
        , value: unit
        }

instance createGain ::
  ( IsSymbol ptr
  , Paramable argA
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TGain {}) graphi grapho
  ) =>
  Create' assets ptr (CTOR.Gain argA) graphi grapho where
  create' ptr w = o
    where
    { context: i, value: (CTOR.Gain argA) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    argA_iv' = paramize argA

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> [ makeGain nn argA_iv' ]
              }
        , value: unit
        }

instance createHighpass ::
  ( IsSymbol ptr
  , Paramable argA
  , Paramable argB
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.THighpass {}) graphi grapho
  ) =>
  Create' assets ptr (CTOR.Highpass argA argB) graphi grapho where
  create' ptr w = o
    where
    { context: i, value: (CTOR.Highpass argA argB) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    argA_iv' = paramize argA

    argB_iv' = paramize argB

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> [ makeHighpass nn argA_iv' argB_iv' ]
              }
        , value: unit
        }

instance createHighshelf ::
  ( IsSymbol ptr
  , Paramable argA
  , Paramable argB
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.THighshelf {}) graphi grapho
  ) =>
  Create' assets ptr (CTOR.Highshelf argA argB) graphi grapho where
  create' ptr w = o
    where
    { context: i, value: (CTOR.Highshelf argA argB) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    argA_iv' = paramize argA

    argB_iv' = paramize argB

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> [ makeHighshelf nn argA_iv' argB_iv' ]
              }
        , value: unit
        }

instance createLoopBuf ::
  ( IsSymbol ptr
  , IsSymbol sym
  , Paramable argA
  , R.Lacks ptr graphi
  , OnOffable onOff
  , AssetsHave Buffers sym assets
  , R.Cons ptr (NodeC CTOR.TLoopBuf {}) graphi grapho
  ) =>
  Create' assets ptr (CTOR.LoopBuf (Proxy sym) onOff argA Number Number) graphi grapho where
  create' ptr w = o
    where
    { context: i, value: (CTOR.LoopBuf bufname onOff argA loopStart loopEnd) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    argA_iv' = paramize argA

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> [ makeLoopBuf nn (reflectSymbol bufname) (onOffIze onOff) argA_iv' loopStart loopEnd ]
              }
        , value: unit
        }

instance createLowpass ::
  ( IsSymbol ptr
  , Paramable argA
  , Paramable argB
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TLowpass {}) graphi grapho
  ) =>
  Create' assets ptr (CTOR.Lowpass argA argB) graphi grapho where
  create' ptr w = o
    where
    { context: i, value: (CTOR.Lowpass argA argB) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    argA_iv' = paramize argA

    argB_iv' = paramize argB

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> [ makeLowpass nn argA_iv' argB_iv' ]
              }
        , value: unit
        }

instance createLowshelf ::
  ( IsSymbol ptr
  , Paramable argA
  , Paramable argB
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TLowshelf {}) graphi grapho
  ) =>
  Create' assets ptr (CTOR.Lowshelf argA argB) graphi grapho where
  create' ptr w = o
    where
    { context: i, value: (CTOR.Lowshelf argA argB) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    argA_iv' = paramize argA

    argB_iv' = paramize argB

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> [ makeLowshelf nn argA_iv' argB_iv' ]
              }
        , value: unit
        }

instance createMicrophone ::
  ( R.Lacks "microphone" graphi
  , R.Cons "microphone" (NodeC CTOR.TMicrophone {}) graphi grapho
  ) =>
  Create' assets "microphone" CTOR.Microphone graphi grapho where
  create' _ w = o
    where
    { context: i } = unsafeUnWAG w

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> [ makeMicrophone ]
              }
        , value: unit
        }

instance createNotch ::
  ( IsSymbol ptr
  , Paramable argA
  , Paramable argB
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TNotch {}) graphi grapho
  ) =>
  Create' assets ptr (CTOR.Notch argA argB) graphi grapho where
  create' ptr w = o
    where
    { context: i, value: (CTOR.Notch argA argB) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    argA_iv' = paramize argA

    argB_iv' = paramize argB

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> [ makeNotch nn argA_iv' argB_iv' ]
              }
        , value: unit
        }

instance createPeaking ::
  ( IsSymbol ptr
  , Paramable argA
  , Paramable argB
  , Paramable argC
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TPeaking {}) graphi grapho
  ) =>
  Create' assets ptr (CTOR.Peaking argA argB argC) graphi grapho where
  create' ptr w = o
    where
    { context: i, value: (CTOR.Peaking argA argB argC) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    argA_iv' = paramize argA

    argB_iv' = paramize argB

    argC_iv' = paramize argC

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> [ makePeaking nn argA_iv' argB_iv' argC_iv' ]
              }
        , value: unit
        }

instance createPeriodicOsc ::
  ( IsSymbol ptr
  , IsSymbol sym
  , Paramable argA
  , R.Lacks ptr graphi
  , OnOffable onOff
  , AssetsHave PeriodicWaves sym assets
  , R.Cons ptr (NodeC CTOR.TPeriodicOsc {}) graphi grapho
  ) =>
  Create' assets ptr (CTOR.PeriodicOsc (Proxy sym) onOff argA) graphi grapho where
  create' ptr w = o
    where
    { context: i, value: (CTOR.PeriodicOsc oscName onOff argA) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    argA_iv' = paramize argA

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> [ makePeriodicOsc nn (reflectSymbol oscName) (onOffIze onOff) argA_iv' ]
              }
        , value: unit
        }

instance createPeriodicOsc2 ::
  ( IsSymbol ptr
  , Paramable argA
  , R.Lacks ptr graphi
  , OnOffable onOff
  , R.Cons ptr (NodeC CTOR.TPeriodicOsc {}) graphi grapho
  ) =>
  Create' assets ptr (CTOR.PeriodicOsc (V.Vec a Number /\ V.Vec a Number) onOff argA) graphi grapho where
  create' ptr w = o
    where
    { context: i, value: (CTOR.PeriodicOsc oscSpec onOff argA) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    argA_iv' = paramize argA

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> [ makePeriodicOscV nn oscSpec (onOffIze onOff) argA_iv' ]
              }
        , value: unit
        }

instance createPlayBuf ::
  ( IsSymbol ptr
  , IsSymbol sym
  , Paramable argA
  , OnOffable onOff
  , R.Lacks ptr graphi
  , AssetsHave Buffers sym assets
  , R.Cons ptr (NodeC CTOR.TPlayBuf {}) graphi grapho
  ) =>
  Create' assets ptr (CTOR.PlayBuf (Proxy sym) Number onOff argA) graphi grapho where
  create' ptr w = o
    where
    { context: i, value: (CTOR.PlayBuf bufname offset onOff argA) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    argA_iv' = paramize argA

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> [ makePlayBuf nn (reflectSymbol bufname) offset (onOffIze onOff) argA_iv' ]
              }
        , value: unit
        }

instance createRecorder ::
  ( IsSymbol ptr
  , IsSymbol recorder
  , R.Lacks ptr graphi
  , AssetsHave Recorders recorder assets
  , R.Cons ptr (NodeC (CTOR.TRecorder recorder) {}) graphi grapho
  ) =>
  Create' assets ptr (CTOR.Recorder recorder) graphi grapho where
  create' ptr w = o
    where
    { context: i, value: (CTOR.Recorder sym) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    recorder = reflectSymbol sym

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> [ makeRecorder nn recorder ]
              }
        , value: unit
        }

instance createSawtoothOsc ::
  ( IsSymbol ptr
  , Paramable argA
  , R.Lacks ptr graphi
  , OnOffable onOff
  , R.Cons ptr (NodeC CTOR.TSawtoothOsc {}) graphi grapho
  ) =>
  Create' assets ptr (CTOR.SawtoothOsc onOff argA) graphi grapho where
  create' ptr w = o
    where
    { context: i, value: (CTOR.SawtoothOsc onOff argA) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    argA_iv' = paramize argA

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> [ makeSawtoothOsc nn (onOffIze onOff) argA_iv' ]
              }
        , value: unit
        }

instance createSinOsc ::
  ( IsSymbol ptr
  , Paramable argA
  , OnOffable onOff
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TSinOsc {}) graphi grapho
  ) =>
  Create' assets ptr (CTOR.SinOsc onOff argA) graphi grapho where
  create' ptr w = o
    where
    { context: i, value: (CTOR.SinOsc onOff argA) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    argA_iv' = paramize argA

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> [ makeSinOsc nn (onOffIze onOff) argA_iv' ]
              }
        , value: unit
        }

instance createSpeaker ::
  ( R.Lacks "speaker" graphi
  , R.Cons "speaker" (NodeC CTOR.TSpeaker {}) graphi grapho
  ) =>
  Create' assets "speaker" CTOR.Speaker graphi grapho where
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
  , Paramable argA
  , OnOffable onOff
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TSquareOsc {}) graphi grapho
  ) =>
  Create' assets ptr (CTOR.SquareOsc onOff argA) graphi grapho where
  create' ptr w = o
    where
    { context: i, value: (CTOR.SquareOsc onOff argA) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    argA_iv' = paramize argA

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> [ makeSquareOsc nn (onOffIze onOff) argA_iv' ]
              }
        , value: unit
        }

instance createStereoPanner ::
  ( IsSymbol ptr
  , Paramable argA
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TStereoPanner {}) graphi grapho
  ) =>
  Create' assets ptr (CTOR.StereoPanner argA) graphi grapho where
  create' ptr w = o
    where
    { context: i, value: (CTOR.StereoPanner argA) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    argA_iv' = paramize argA

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> [ makeStereoPanner nn argA_iv' ]
              }
        , value: unit
        }

instance createTriangleOsc ::
  ( IsSymbol ptr
  , Paramable argA
  , OnOffable onOff
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TTriangleOsc {}) graphi grapho
  ) =>
  Create' assets ptr (CTOR.TriangleOsc onOff argA) graphi grapho where
  create' ptr w = o
    where
    { context: i, value: (CTOR.TriangleOsc onOff argA) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    argA_iv' = paramize argA

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> [ makeTriangleOsc nn (onOffIze onOff) argA_iv' ]
              }
        , value: unit
        }

instance createWaveShaper ::
  ( IsSymbol ptr
  , IsSymbol floatArray
  , IsOversample oversample
  , R.Lacks ptr graphi
  , AssetsHave FloatArrays floatArray assets
  , R.Cons ptr (NodeC (CTOR.TWaveShaper floatArray oversample) {}) graphi grapho
  ) =>
  Create' assets ptr (CTOR.WaveShaper floatArray oversample) graphi grapho where
  create' ptr w = o
    where
    { context: i, value: (CTOR.WaveShaper floatArray' oversample') } = unsafeUnWAG w

    nn = reflectSymbol ptr

    floatArray = reflectSymbol floatArray'

    oversample = reflectOversample oversample'

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> [ makeWaveShaper nn floatArray oversample ]
              }
        , value: unit
        }
