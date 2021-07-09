module WAGS.Create where

import Prelude
import Control.Comonad (extract)
import Data.Either (Either(..))
import Data.Functor (voidRight)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Vec as V
import Prim.Row as R
import Prim.RowList as RL
import Record as Record
import Type.Proxy (Proxy(..))
import WAGS.Connect (class Connect, connect)
import WAGS.Control.Indexed (IxWAG(..))
import WAGS.Control.Types (WAG, unsafeUnWAG, unsafeWAG)
import WAGS.Edgeable (class Edgeable, withEdge)
import WAGS.Graph.AudioUnit as CTOR
import WAGS.Graph.Graph (Graph)
import WAGS.Graph.Node (NodeC)
import WAGS.Graph.Oversample (class IsOversample, reflectOversample)
import WAGS.Graph.Paramable (class Paramable, paramize, class OnOffable, onOffIze)
import WAGS.Interpret (class AudioInterpret, makeAllpass, makeBandpass, makeConstant, makeConvolver, makeDelay, makeDynamicsCompressor, makeGain, makeHighpass, makeHighshelf, makeLoopBuf, makeLowpass, makeLowshelf, makeMicrophone, makeNotch, makePeaking, makePeriodicOsc, makePeriodicOscV, makePlayBuf, makeRecorder, makeSawtoothOsc, makeSinOsc, makeSpeaker, makeSquareOsc, makeStereoPanner, makeTriangleOsc, makeWaveShaper)
import WAGS.Util (tmap)

type CreateStepSig (suffix :: Symbol) (map :: Type) (r :: Row Type) (inGraph :: Graph) (outGraph :: Graph) = forall proxySuffix proxyMap audio engine proof res.
    AudioInterpret audio engine =>
    proxySuffix suffix ->
    proxyMap map ->
    WAG
      audio
      engine
      proof
      res
      { | inGraph }
      { | r } ->
    WAG
      audio
      engine
      proof
      res
      { | outGraph }
      Unit

class CreateStep (suffix :: Symbol) (map :: Type) (r :: Row Type) (inGraph :: Graph) (outGraph :: Graph) | suffix map r inGraph -> outGraph where
  createStep :: CreateStepSig suffix map r inGraph outGraph

type CreateStepRLSig (rl :: RL.RowList Type) (suffix :: Symbol) (map :: Type) (r :: Row Type) (inGraph :: Graph) (outGraph :: Graph) = forall proxySuffix proxyMap proxyRL audio engine proof res.
    AudioInterpret audio engine =>
    proxyRL rl ->
    proxySuffix suffix ->
    proxyMap map ->
    WAG
      audio
      engine
      proof
      res
      { | inGraph }
      { | r } ->
    WAG
      audio
      engine
      proof
      res
      { | outGraph }
      Unit

class CreateStepRL (rl :: RL.RowList Type) (suffix :: Symbol) (map :: Type) (r :: Row Type) (inGraph :: Graph) (outGraph :: Graph) | rl r inGraph -> outGraph where
  createStepRL :: CreateStepRLSig rl suffix map r inGraph outGraph

instance createStepAll :: (RL.RowToList r rl, CreateStepRL rl suffix map r inGraph outGraph) => CreateStep suffix map r inGraph outGraph where
  createStep = createStepRL (Proxy :: _ rl)

instance createStepRLNil :: CreateStepRL RL.Nil suffix map r inGraph inGraph where
  createStepRL _ _ _ r = r $> unit

instance createStepRLCons ::
  ( IsSymbol key
  , R.Cons key val ignore r
  , Edgeable val (node /\ { | edges })
  , Create' key node graph0 graph1
  , CreateStep suffix map edges graph1 graph2
  , CreateStepRL rest suffix map r graph2 graph3
  ) =>
  CreateStepRL (RL.Cons key val rest) suffix map r graph0 graph3 where
  createStepRL _ _ _ r = step3
    where
    rx = extract r

    node /\ edges = withEdge (Record.get (Proxy :: _ key) rx)

    step1 = create' (Proxy :: _ key) (r $> node)

    step2 =
      ( createStep :: CreateStepSig suffix map edges graph1 graph2) Proxy Proxy
        (step1 $> edges)

    step3 = createStepRL (Proxy :: _ rest) (Proxy :: _ suffix) (Proxy :: _ map) (step2 $> rx)

class ConnectEdgesToNode (sources :: RL.RowList Type) (dest :: Symbol) (inGraph :: Graph) (outGraph :: Graph) | sources dest inGraph -> outGraph where
  connectEdgesToNode ::
    forall proxyRL proxyS audio engine proof res.
    AudioInterpret audio engine =>
    proxyRL sources ->
    WAG
      audio
      engine
      proof
      res
      { | inGraph }
      (proxyS dest) ->
    WAG
      audio
      engine
      proof
      res
      { | outGraph }
      (proxyS dest)

instance connectEdgesToNodeNil :: ConnectEdgesToNode RL.Nil dest inGraph inGraph where
  connectEdgesToNode _ w = w

instance connectEdgesToNodeCons :: (Connect key dest inGraph midGraph, ConnectEdgesToNode rest dest midGraph outGraph) => ConnectEdgesToNode (RL.Cons key ignore rest) dest inGraph outGraph where
  connectEdgesToNode _ w = step2
    where
    step1 = connect (w $> { source: (Proxy :: _ key), dest: (Proxy :: _ dest) })

    step2 = connectEdgesToNode (Proxy :: _ rest) (step1 $> (extract w))

type ConnectAfterCreateSig (suffix :: Symbol) (map :: Type) (rl :: RL.RowList Type) (inGraph :: Graph) (outGraph :: Graph) = forall proxySuffix proxyMap audio engine proof res.
    AudioInterpret audio engine =>
    proxySuffix suffix ->
    proxyMap map ->
    WAG
      audio
      engine
      proof
      res
      { | inGraph }
      (Proxy rl) ->
    WAG
      audio
      engine
      proof
      res
      { | outGraph }
      Unit

class ConnectAfterCreate (suffix :: Symbol) (map :: Type) (rl :: RL.RowList Type) (inGraph :: Graph) (outGraph :: Graph) | suffix map rl inGraph -> outGraph where
  connectAfterCreate ::ConnectAfterCreateSig suffix map rl inGraph outGraph
    

instance connectAfterCreateNil :: ConnectAfterCreate suffix map RL.Nil graph0 graph0 where
  connectAfterCreate _ _ w = w $> unit

instance connectAfterCreateCons ::
  ( Edgeable node' (Tuple node { | edges })
  , RL.RowToList edges edgesList
  , ConnectEdgesToNode edgesList sym graph0 graph1
  , ConnectAfterCreate suffix map edgesList graph1 graph2
  , ConnectAfterCreate suffix map rest graph2 graph3
  ) =>
  ConnectAfterCreate suffix map (RL.Cons sym node' rest) graph0 graph3 where
  connectAfterCreate _ _ w = step3
    where
    step1 = connectEdgesToNode (Proxy :: _ edgesList) (w $> (Proxy :: _ sym))

    step2 = connectAfterCreate (Proxy :: _ suffix) (Proxy :: _ map) (step1 $> (Proxy :: _ edgesList))

    step3 = connectAfterCreate (Proxy :: _ suffix) (Proxy :: _ map) (step2 $> (Proxy :: _ rest))

type CreateInternalSig (suffix :: Symbol) (map :: Type) (r :: Row Type) (inGraph :: Graph) (outGraph :: Graph) = forall proxySuffix proxyMap audio engine proof res.
    AudioInterpret audio engine =>
    proxySuffix suffix ->
    proxyMap map ->
    WAG
      audio
      engine
      proof
      res
      { | inGraph }
      { | r } ->
    WAG
      audio
      engine
      proof
      res
      { | outGraph }
      Unit

class CreateInternal (suffix :: Symbol) (map :: Type) (r :: Row Type) (inGraph :: Graph) (outGraph :: Graph) | suffix map r inGraph -> outGraph where
  createInternal :: CreateInternalSig suffix map r inGraph outGraph

instance createInternalAll ::
  ( CreateStep suffix map r inGraph midGraph
  , RL.RowToList r rl
  , ConnectAfterCreate suffix map rl midGraph outGraph
  ) =>
  CreateInternal suffix map r inGraph outGraph where
  createInternal _ _ r = step1
    where
    step0 = createStep (Proxy :: _ suffix) (Proxy :: _ map) r

    step1 = connectAfterCreate (Proxy :: _ suffix) (Proxy :: _ map) (step0 $> (Proxy :: _ rl))

class Create (r :: Row Type) (inGraph :: Graph) (outGraph :: Graph) | r inGraph -> outGraph where
  create ::
    forall audio engine proof res.
    AudioInterpret audio engine =>
    WAG
      audio
      engine
      proof
      res
      { | inGraph }
      { | r } ->
    WAG
      audio
      engine
      proof
      res
      { | outGraph }
      Unit

instance createAll ::
  CreateInternal "" Unit r inGraph outGraph =>
  Create r inGraph outGraph where
  create =
    ( createInternal :: CreateInternalSig "" Unit r inGraph outGraph)
      Proxy
      Proxy

icreate ::
  forall r audio engine proof res inGraph outGraph.
  AudioInterpret audio engine =>
  Create r inGraph outGraph =>
  { | r } ->
  IxWAG
    audio
    engine
    proof
    res
    { | inGraph }
    { | outGraph }
    Unit
icreate r = IxWAG (create <<< voidRight r)

-- | Create an audio unit `node` in `igraph` with index `ptr`, resulting in `ograph`.
class Create' (ptr :: Symbol) (node :: Type) (inGraph :: Graph) (outGraph :: Graph) | ptr node inGraph -> outGraph where
  create' ::
    forall proxy audio engine proof res.
    AudioInterpret audio engine =>
    proxy ptr ->
    WAG
      audio
      engine
      proof
      res
      { | inGraph }
      node ->
    WAG
      audio
      engine
      proof
      res
      { | outGraph }
      Unit

icreate' ::
  forall proxy ptr node audio engine proof res i o.
  AudioInterpret audio engine =>
  Create' ptr node i o =>
  proxy ptr ->
  node ->
  IxWAG audio engine proof res { | i } { | o } Unit
icreate' ptr node = IxWAG (create' ptr <<< voidRight node)

instance createUnit ::
  Create'
    ptr
    Unit
    graphi
    graphi where
  create' _ w = w

instance createAllpass ::
  ( IsSymbol ptr
  , Paramable argA
  , Paramable argB
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TAllpass {}) graphi grapho
  ) =>
  Create'
    ptr
    (CTOR.Allpass argA argB)
    graphi
    grapho where
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
  Create'
    ptr
    (CTOR.Bandpass argA argB)
    graphi
    grapho where
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
  Create'
    ptr
    (CTOR.Constant onOff argA)
    graphi
    grapho where
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
  , R.Cons ptr (NodeC (CTOR.TConvolver buffer) {}) graphi grapho
  ) =>
  Create'
    ptr
    (CTOR.Convolver buffer)
    graphi
    grapho where
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
  Create'
    ptr
    (CTOR.Delay argA)
    graphi
    grapho where
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
  Create'
    ptr
    (CTOR.DynamicsCompressor argA argB argC argD argE)
    graphi
    grapho where
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
  Create'
    ptr
    (CTOR.Gain argA)
    graphi
    grapho where
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
  Create'
    ptr
    (CTOR.Highpass argA argB)
    graphi
    grapho where
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
  Create'
    ptr
    (CTOR.Highshelf argA argB)
    graphi
    grapho where
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
  , Paramable argA
  , R.Lacks ptr graphi
  , OnOffable onOff
  , R.Cons ptr (NodeC CTOR.TLoopBuf {}) graphi grapho
  ) =>
  Create'
    ptr
    (CTOR.LoopBuf String onOff argA Number Number)
    graphi
    grapho where
  create' ptr w = o
    where
    { context: i, value: (CTOR.LoopBuf bufname onOff argA loopStart loopEnd) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    argA_iv' = paramize argA

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> [ makeLoopBuf nn bufname (onOffIze onOff) argA_iv' loopStart loopEnd ]
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
  Create'
    ptr
    (CTOR.Lowpass argA argB)
    graphi
    grapho where
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
  Create'
    ptr
    (CTOR.Lowshelf argA argB)
    graphi
    grapho where
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
  Create'
    "microphone"
    CTOR.Microphone
    graphi
    grapho where
  create' ptr w = o
    where
    { context: i } = unsafeUnWAG w

    nn = "microphone"

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
  Create'
    ptr
    (CTOR.Notch argA argB)
    graphi
    grapho where
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
  Create'
    ptr
    (CTOR.Peaking argA argB argC)
    graphi
    grapho where
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
  , Paramable argA
  , R.Lacks ptr graphi
  , OnOffable onOff
  , R.Cons ptr (NodeC CTOR.TPeriodicOsc {}) graphi grapho
  ) =>
  Create'
    ptr
    (CTOR.PeriodicOsc String onOff argA)
    graphi
    grapho where
  create' ptr w = o
    where
    { context: i, value: (CTOR.PeriodicOsc oscName onOff argA) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    argA_iv' = paramize argA

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> [ makePeriodicOsc nn oscName (onOffIze onOff) argA_iv' ]
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
  Create'
    ptr
    (CTOR.PeriodicOsc (V.Vec a Number /\ V.Vec a Number) onOff argA)
    graphi
    grapho where
  create' ptr w = o
    where
    { context: i, value: (CTOR.PeriodicOsc oscSpec onOff argA) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    argA_iv' = paramize argA

    rPeriodicWave = Right (tmap V.toArray oscSpec)

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
  , Paramable argA
  , OnOffable onOff
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TPlayBuf {}) graphi grapho
  ) =>
  Create'
    ptr
    (CTOR.PlayBuf String Number onOff argA)
    graphi
    grapho where
  create' ptr w = o
    where
    { context: i, value: (CTOR.PlayBuf bufname offset onOff argA) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    argA_iv' = paramize argA

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> [ makePlayBuf nn bufname offset (onOffIze onOff) argA_iv' ]
              }
        , value: unit
        }

instance createRecorder ::
  ( IsSymbol ptr
  , IsSymbol recorder
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC (CTOR.TRecorder recorder) {}) graphi grapho
  ) =>
  Create'
    ptr
    (CTOR.Recorder recorder)
    graphi
    grapho where
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
  Create'
    ptr
    (CTOR.SawtoothOsc onOff argA)
    graphi
    grapho where
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
  Create'
    ptr
    (CTOR.SinOsc onOff argA)
    graphi
    grapho where
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
  Create'
    "speaker"
    CTOR.Speaker
    graphi
    grapho where
  create' ptr w = o
    where
    { context: i } = unsafeUnWAG w

    nn = "speaker"

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
  Create'
    ptr
    (CTOR.SquareOsc onOff argA)
    graphi
    grapho where
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
  Create'
    ptr
    (CTOR.StereoPanner argA)
    graphi
    grapho where
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
  Create'
    ptr
    (CTOR.TriangleOsc onOff argA)
    graphi
    grapho where
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
  , R.Cons ptr (NodeC (CTOR.TWaveShaper floatArray oversample) {}) graphi grapho
  ) =>
  Create'
    ptr
    (CTOR.WaveShaper floatArray oversample)
    graphi
    grapho where
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
