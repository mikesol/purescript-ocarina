module WAGS.Create where

import Prelude

import Control.Comonad (extract)
import Data.Either (Either(..))
import Data.Functor (voidRight)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Vec as V
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Prim.Row as R
import Type.Proxy (Proxy(..))
import WAGS.Connect (ConnectFoldingWithIndex(..))
import WAGS.Control.Indexed (IxWAG(..))
import WAGS.Control.Types (WAG, unsafeUnWAG, unsafeWAG)
import WAGS.Edgeable (class Edgeable, withEdge)
import WAGS.Graph.AudioUnit (OnOff)
import WAGS.Graph.AudioUnit as CTOR
import WAGS.Graph.Graph (Graph)
import WAGS.Graph.Node (NodeC)
import WAGS.Graph.Oversample (class IsOversample, reflectOversample)
import WAGS.Graph.Parameter (class Paramable, paramize)
import WAGS.Interpret (class AudioInterpret, makeAllpass, makeBandpass, makeConstant, makeConvolver, makeDelay, makeDynamicsCompressor, makeGain, makeHighpass, makeHighshelf, makeLoopBuf, makeLowpass, makeLowshelf, makeMicrophone, makeNotch, makePeaking, makePeriodicOsc, makePeriodicOscV, makePlayBuf, makeRecorder, makeSawtoothOsc, makeSinOsc, makeSpeaker, makeSquareOsc, makeStereoPanner, makeTriangleOsc, makeWaveShaper)
import WAGS.Util (tmap)

data CreateFoldingWithIndex
  = CreateFoldingWithIndex

instance createFoldingWithIndex ::
  ( AudioInterpret audio engine
  , Edgeable node' (Tuple node edges)
  , Create' sym node inGraph midGraph
  , HFoldlWithIndex
      CreateFoldingWithIndex
      ( WAG
          audio
          engine
          proof
          res
          { | midGraph }
          Unit
      )
      edges
      ( WAG
          audio
          engine
          proof
          res
          { | outGraph }
          Unit
      )
  , IsSymbol sym
  ) =>
  FoldingWithIndex
    CreateFoldingWithIndex
    (proxy sym)
    ( WAG
        audio
        engine
        proof
        res
        { | inGraph }
        Unit
    )
    node'
    ( WAG
        audio
        engine
        proof
        res
        { | outGraph }
        Unit
    ) where
  foldingWithIndex CreateFoldingWithIndex prop ifr edgeable =
    let
      node /\ edges = withEdge edgeable

      res = create' prop (ifr $> node)
    in
      hfoldlWithIndex
        CreateFoldingWithIndex
        (res $> unit)
        edges

data ThenConnectFoldingWithIndex
  = ThenConnectFoldingWithIndex

instance thenConnectFoldingWithIndex ::
  ( IsSymbol sym
  , Edgeable node' (Tuple node edges)
  , HFoldlWithIndex
      ConnectFoldingWithIndex
      ( WAG
          audio
          engine
          proof
          res
          { | inGraph }
          (Proxy sym)
      )
      edges
      ( WAG
          audio
          engine
          proof
          res
          { | midGraph }
          (Proxy sym)
      )
  , HFoldlWithIndex
      ThenConnectFoldingWithIndex
      ( WAG
          audio
          engine
          proof
          res
          { | midGraph }
          Unit
      )
      edges
      ( WAG
          audio
          engine
          proof
          res
          { | outGraph }
          Unit
      )
  ) =>
  FoldingWithIndex
    ThenConnectFoldingWithIndex
    (proxy sym)
    ( WAG
        audio
        engine
        proof
        res
        { | inGraph }
        Unit
    )
    node'
    ( WAG
        audio
        engine
        proof
        res
        { | outGraph }
        Unit
    ) where
  foldingWithIndex ThenConnectFoldingWithIndex prop ifr edgeable =
    let
      _ /\ edges = withEdge edgeable
    in
      hfoldlWithIndex
        ThenConnectFoldingWithIndex
        ( hfoldlWithIndex
            ConnectFoldingWithIndex
            (ifr $> (Proxy :: _ sym))
            edges
            $> (extract ifr)
        )
        edges

-- | Similar to `create`, but accepts a record with multiple units to create _and_ connect.
create ::
  forall r audio engine proof res inGraph midGraph outGraph.
  AudioInterpret audio engine =>
  HFoldlWithIndex
    CreateFoldingWithIndex
    ( WAG
        audio
        engine
        proof
        res
        { | inGraph }
        Unit
    )
    { | r }
    ( WAG
        audio
        engine
        proof
        res
        { | midGraph }
        Unit
    ) =>
  HFoldlWithIndex
    ThenConnectFoldingWithIndex
    ( WAG
        audio
        engine
        proof
        res
        { | midGraph }
        Unit
    )
    { | r }
    ( WAG
        audio
        engine
        proof
        res
        { | outGraph }
        Unit
    ) =>
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
create w =
  hfoldlWithIndex
    ThenConnectFoldingWithIndex
    innerStep
    (extract w)
  where
  innerStep =
    hfoldlWithIndex
      CreateFoldingWithIndex
      (w $> unit)
      (extract w)

icreate ::
  forall r audio engine proof res inGraph midGraph outGraph.
  AudioInterpret audio engine =>
  HFoldlWithIndex
    CreateFoldingWithIndex
    ( WAG
        audio
        engine
        proof
        res
        { | inGraph }
        Unit
    )
    { | r }
    ( WAG
        audio
        engine
        proof
        res
        { | midGraph }
        Unit
    ) =>
  HFoldlWithIndex
    ThenConnectFoldingWithIndex
    ( WAG
        audio
        engine
        proof
        res
        { | midGraph }
        Unit
    )
    { | r }
    ( WAG
        audio
        engine
        proof
        res
        { | outGraph }
        Unit
    ) =>
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
  , R.Cons ptr (NodeC CTOR.TConstant {}) graphi grapho
  ) =>
  Create'
    ptr
    (CTOR.Constant OnOff argA)
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
              { instructions = i.instructions <> [ makeConstant nn onOff argA_iv' ]
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
  , R.Cons ptr (NodeC CTOR.TLoopBuf {}) graphi grapho
  ) =>
  Create'
    ptr
    (CTOR.LoopBuf String OnOff argA Number Number)
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
              { instructions = i.instructions <> [ makeLoopBuf nn bufname onOff argA_iv' loopStart loopEnd ]
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
  , R.Cons ptr (NodeC CTOR.TPeriodicOsc {}) graphi grapho
  ) =>
  Create'
    ptr
    (CTOR.PeriodicOsc String OnOff argA)
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
              { instructions = i.instructions <> [ makePeriodicOsc nn oscName onOff argA_iv' ]
              }
        , value: unit
        }

instance createPeriodicOsc2 ::
  ( IsSymbol ptr
  , Paramable argA
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TPeriodicOsc {}) graphi grapho
  ) =>
  Create'
    ptr
    (CTOR.PeriodicOsc (V.Vec a Number /\ V.Vec a Number) OnOff argA)
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
              { instructions = i.instructions <> [ makePeriodicOscV nn oscSpec onOff argA_iv' ]
              }
        , value: unit
        }

instance createPlayBuf ::
  ( IsSymbol ptr
  , Paramable argA
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TPlayBuf {}) graphi grapho
  ) =>
  Create'
    ptr
    (CTOR.PlayBuf String Number OnOff argA)
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
              { instructions = i.instructions <> [ makePlayBuf nn bufname offset onOff argA_iv' ]
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
  , R.Cons ptr (NodeC CTOR.TSawtoothOsc {}) graphi grapho
  ) =>
  Create'
    ptr
    (CTOR.SawtoothOsc OnOff argA)
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
              { instructions = i.instructions <> [ makeSawtoothOsc nn onOff argA_iv' ]
              }
        , value: unit
        }

instance createSinOsc ::
  ( IsSymbol ptr
  , Paramable argA
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TSinOsc {}) graphi grapho
  ) =>
  Create'
    ptr
    (CTOR.SinOsc OnOff argA)
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
              { instructions = i.instructions <> [ makeSinOsc nn onOff argA_iv' ]
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
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TSquareOsc {}) graphi grapho
  ) =>
  Create'
    ptr
    (CTOR.SquareOsc OnOff argA)
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
              { instructions = i.instructions <> [ makeSquareOsc nn onOff argA_iv' ]
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
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TTriangleOsc {}) graphi grapho
  ) =>
  Create'
    ptr
    (CTOR.TriangleOsc OnOff argA)
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
              { instructions = i.instructions <> [ makeTriangleOsc nn onOff argA_iv' ]
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
