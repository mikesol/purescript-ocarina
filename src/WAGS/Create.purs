module WAGS.Create where

import Prelude

import Control.Monad.State (modify_)
import Data.Map as M
import Data.Tuple.Nested((/\))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple, fst)
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Prim.Row as R
import Type.Proxy (Proxy(..))
import WAGS.Connect (ConnectFoldingWithIndex(..))
import WAGS.Control.Qualified as WAGS
import WAGS.Control.Types (FrameT, unsafeFrame)
import WAGS.Edgeable (class Edgeable, withEdge)
import WAGS.Graph.AudioUnit as CTOR
import WAGS.Graph.Graph (Graph)
import WAGS.Graph.Node (NodeC)
import WAGS.Graph.Oversample (class IsOversample, reflectOversample)
import WAGS.Graph.Parameter (AudioParameter(..), defaultParam)
import WAGS.Interpret (class AudioInterpret, makeAllpass, makeBandpass, makeConstant, makeConvolver, makeDelay, makeDynamicsCompressor, makeGain, makeHighpass, makeHighshelf, makeLoopBuf, makeLowpass, makeLowshelf, makeMicrophone, makeNotch, makePeaking, makePeriodicOsc, makePlayBuf, makeRecorder, makeSawtoothOsc, makeSinOsc, makeSpeaker, makeSquareOsc, makeStereoPanner, makeTriangleOsc, makeWaveShaper)
import WAGS.Rendered (AnAudioUnit(..))

data CreateFoldingWithIndex
  = CreateFoldingWithIndex

instance createFoldingWithIndex ::
  ( Monad m
  , AudioInterpret audio engine
  , Edgeable node' (Tuple node edges)
  , Create' sym node midGraph1 midGraph2
  , HFoldlWithIndex
      CreateFoldingWithIndex
      ( FrameT
          env
          audio
          engine
          proof
          m
          res
          { | inGraph }
          { | midGraph2 }
          Unit
      )
      edges
      ( FrameT
          env
          audio
          engine
          proof
          m
          res
          { | inGraph }
          { | outGraph }
          Unit
      )
  , IsSymbol sym
  ) =>
  FoldingWithIndex
    CreateFoldingWithIndex
    (proxy sym)
    ( FrameT
        env
        audio
        engine
        proof
        m
        res
        { | inGraph }
        { | midGraph1 }
        Unit
    )
    node'
    ( FrameT
        env
        audio
        engine
        proof
        m
        res
        { | inGraph }
        { | outGraph }
        Unit
    ) where
  foldingWithIndex CreateFoldingWithIndex prop ifr edgeable =
    let
      node /\ edges = withEdge edgeable
    in
      hfoldlWithIndex
        CreateFoldingWithIndex
        ( WAGS.do
            ifr
            create' prop node
        )
        edges


data ThenConnectFoldingWithIndex
  = ThenConnectFoldingWithIndex

instance thenConnectFoldingWithIndex ::
  ( Monad m
  , IsSymbol sym
  , Edgeable node' (Tuple node edges)
  , HFoldlWithIndex
      ConnectFoldingWithIndex
      ( FrameT
          env
          audio
          engine
          proof
          m
          res
          { | inGraph }
          { | midGraph1 }
          (Proxy sym)
      )
      edges
      ( FrameT
          env
          audio
          engine
          proof
          m
          res
          { | inGraph }
          { | midGraph2 }
          (Proxy sym)
      )
  , HFoldlWithIndex
      ThenConnectFoldingWithIndex
      ( FrameT
          env
          audio
          engine
          proof
          m
          res
          { | inGraph }
          { | midGraph2 }
          Unit
      )
      edges
      ( FrameT
          env
          audio
          engine
          proof
          m
          res
          { | inGraph }
          { | outGraph }
          Unit
      )
  ) =>
  FoldingWithIndex
    ThenConnectFoldingWithIndex
    (proxy sym)
    ( FrameT
        env
        audio
        engine
        proof
        m
        res
        { | inGraph }
        { | midGraph1 }
        Unit
    )
    node'
    ( FrameT
        env
        audio
        engine
        proof
        m
        res
        { | inGraph }
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
            $> unit
        )
        edges

-- | Similar to `create`, but accepts a record with multiple units to create _and_ connect.
create ::
  forall r env audio engine proof m res inGraph midGraph1 midGraph2 outGraph.
  Monad m =>
  AudioInterpret audio engine =>
  HFoldlWithIndex
    CreateFoldingWithIndex
    ( FrameT
        env
        audio
        engine
        proof
        m
        res
        { | inGraph }
        { | midGraph1 }
        Unit
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
        { | midGraph2 }
        Unit
    ) =>
  HFoldlWithIndex
    ThenConnectFoldingWithIndex
    ( FrameT
        env
        audio
        engine
        proof
        m
        res
        { | inGraph }
        { | midGraph2 }
        Unit
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
        { | outGraph }
        Unit
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
    { | outGraph }
    Unit
create r =
  hfoldlWithIndex
    ThenConnectFoldingWithIndex
    innerStep
    r
  where
  innerStep =
    hfoldlWithIndex
      CreateFoldingWithIndex
      ( (unsafeFrame (pure unit)) ::
          FrameT
            env
            audio
            engine
            proof
            m
            res
            { | inGraph }
            { | midGraph1 }
            Unit
      )
      r

-- | Create an audio unit `node` in `igraph` with index `ptr`, resulting in `ograph`.
class Create' (ptr :: Symbol) (node :: Type) (inGraph :: Graph) (outGraph :: Graph) | ptr node inGraph -> outGraph where
  create' ::
    forall proxy env audio engine proof m res.
    Monad m =>
    AudioInterpret audio engine =>
    proxy ptr ->
    node ->
    FrameT
      env
      audio
      engine
      proof
      m
      res
      { | inGraph }
      { | outGraph }
      Unit

-- | A value that can be coerced to an initial control-rate audio parameter.
class InitialVal a where
  initialVal :: a -> AudioParameter

instance initialValNumber :: InitialVal Number where
  initialVal a = AudioParameter $ defaultParam { param = a }

instance initialValAudioParameter :: InitialVal AudioParameter where
  initialVal = identity

-- convention of getter and setter
instance initialValTuple :: InitialVal a => InitialVal (Tuple a b) where
  initialVal = initialVal <<< fst

instance createUnit ::
  Create'
    ptr
    Unit
    graphi
    graphi where
  create' _ _ = unsafeFrame $ pure unit

instance createAllpass ::
  ( IsSymbol ptr
  , InitialVal argA
  , InitialVal argB
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TAllpass {}) graphi grapho
  ) =>
  Create'
    ptr
    (CTOR.Allpass argA argB)
    graphi
    grapho where
  create' ptr (CTOR.Allpass argA argB) =
    unsafeFrame
      $ do
          let
            nn = reflectSymbol ptr

            argA_iv' = initialVal argA

            argB_iv' = initialVal argB
          modify_
            ( \i ->
                i
                  { internalNodes = (M.insert nn (AAllpass argA_iv' argB_iv') i.internalNodes)
                  , instructions = i.instructions <> [ makeAllpass nn argA_iv' argB_iv' ]
                  }
            )

instance createBandpass ::
  ( IsSymbol ptr
  , InitialVal argA
  , InitialVal argB
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TBandpass {}) graphi grapho
  ) =>
  Create'
    ptr
    (CTOR.Bandpass argA argB)
    graphi
    grapho where
  create' ptr (CTOR.Bandpass argA argB) =
    unsafeFrame
      $ do
          let
            nn = reflectSymbol ptr

            argA_iv' = initialVal argA

            argB_iv' = initialVal argB
          modify_
            ( \i ->
                i
                  { internalNodes = (M.insert nn (ABandpass argA_iv' argB_iv') i.internalNodes)
                  , instructions = i.instructions <> [ makeBandpass nn argA_iv' argB_iv' ]
                  }
            )

instance createConstant ::
  ( IsSymbol ptr
  , InitialVal argA
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TConstant {}) graphi grapho
  ) =>
  Create'
    ptr
    (CTOR.Constant argA)
    graphi
    grapho where
  create' ptr (CTOR.Constant onOff argA) =
    unsafeFrame
      $ do
          let
            nn = reflectSymbol ptr

            argA_iv' = initialVal argA
          modify_
            ( \i ->
                i
                  { internalNodes = (M.insert nn (AConstant onOff argA_iv') i.internalNodes)
                  , instructions = i.instructions <> [ makeConstant nn onOff argA_iv' ]
                  }
            )

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
  create' ptr (CTOR.Convolver sym) =
    unsafeFrame
      $ do
          let
            nn = reflectSymbol ptr

            buffer = reflectSymbol sym
          modify_
            ( \i ->
                i
                  { internalNodes = (M.insert nn (AConvolver buffer) i.internalNodes)
                  , instructions = i.instructions <> [ makeConvolver nn buffer ]
                  }
            )

instance createDelay ::
  ( IsSymbol ptr
  , InitialVal argA
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TDelay {}) graphi grapho
  ) =>
  Create'
    ptr
    (CTOR.Delay argA)
    graphi
    grapho where
  create' ptr (CTOR.Delay argA) =
    unsafeFrame
      $ do
          let
            nn = reflectSymbol ptr

            argA_iv' = initialVal argA
          modify_
            ( \i ->
                i
                  { internalNodes = (M.insert nn (ADelay argA_iv') i.internalNodes)
                  , instructions = i.instructions <> [ makeDelay nn argA_iv' ]
                  }
            )

instance createDynamicsCompressor ::
  ( IsSymbol ptr
  , InitialVal argA
  , InitialVal argB
  , InitialVal argC
  , InitialVal argD
  , InitialVal argE
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TDynamicsCompressor {}) graphi grapho
  ) =>
  Create'
    ptr
    (CTOR.DynamicsCompressor argA argB argC argD argE)
    graphi
    grapho where
  create' ptr (CTOR.DynamicsCompressor argA argB argC argD argE) =
    unsafeFrame
      $ do
          let
            nn = reflectSymbol ptr

            argA_iv' = initialVal argA

            argB_iv' = initialVal argB

            argC_iv' = initialVal argC

            argD_iv' = initialVal argD

            argE_iv' = initialVal argE
          modify_
            ( \i ->
                i
                  { internalNodes = (M.insert nn (ADynamicsCompressor argA_iv' argB_iv' argC_iv' argD_iv' argE_iv') i.internalNodes)
                  , instructions = i.instructions <> [ makeDynamicsCompressor nn argA_iv' argB_iv' argC_iv' argD_iv' argE_iv' ]
                  }
            )

instance createGain ::
  ( IsSymbol ptr
  , InitialVal argA
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TGain {}) graphi grapho
  ) =>
  Create'
    ptr
    (CTOR.Gain argA)
    graphi
    grapho where
  create' ptr (CTOR.Gain argA) =
    unsafeFrame
      $ do
          let
            nn = reflectSymbol ptr

            argA_iv' = initialVal argA
          modify_
            ( \i ->
                i
                  { internalNodes = (M.insert nn (AGain argA_iv') i.internalNodes)
                  , instructions = i.instructions <> [ makeGain nn argA_iv' ]
                  }
            )

instance createHighpass ::
  ( IsSymbol ptr
  , InitialVal argA
  , InitialVal argB
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.THighpass {}) graphi grapho
  ) =>
  Create'
    ptr
    (CTOR.Highpass argA argB)
    graphi
    grapho where
  create' ptr (CTOR.Highpass argA argB) =
    unsafeFrame
      $ do
          let
            nn = reflectSymbol ptr

            argA_iv' = initialVal argA

            argB_iv' = initialVal argB
          modify_
            ( \i ->
                i
                  { internalNodes = (M.insert nn (AHighpass argA_iv' argB_iv') i.internalNodes)
                  , instructions = i.instructions <> [ makeHighpass nn argA_iv' argB_iv' ]
                  }
            )

instance createHighshelf ::
  ( IsSymbol ptr
  , InitialVal argA
  , InitialVal argB
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.THighshelf {}) graphi grapho
  ) =>
  Create'
    ptr
    (CTOR.Highshelf argA argB)
    graphi
    grapho where
  create' ptr (CTOR.Highshelf argA argB) =
    unsafeFrame
      $ do
          let
            nn = reflectSymbol ptr

            argA_iv' = initialVal argA

            argB_iv' = initialVal argB
          modify_
            ( \i ->
                i
                  { internalNodes = (M.insert nn (AHighshelf argA_iv' argB_iv') i.internalNodes)
                  , instructions = i.instructions <> [ makeHighshelf nn argA_iv' argB_iv' ]
                  }
            )

instance createLoopBuf ::
  ( IsSymbol ptr
  , InitialVal argA
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TLoopBuf {}) graphi grapho
  ) =>
  Create'
    ptr
    (CTOR.LoopBuf argA)
    graphi
    grapho where
  create' ptr (CTOR.LoopBuf bufname onOff argA loopStart loopEnd) =
    unsafeFrame
      $ do
          let
            nn = reflectSymbol ptr

            argA_iv' = initialVal argA
          modify_
            ( \i ->
                i
                  { internalNodes = (M.insert nn (ALoopBuf bufname onOff argA_iv' loopStart loopEnd) i.internalNodes)
                  , instructions = i.instructions <> [ makeLoopBuf nn bufname onOff argA_iv' loopStart loopEnd ]
                  }
            )

instance createLowpass ::
  ( IsSymbol ptr
  , InitialVal argA
  , InitialVal argB
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TLowpass {}) graphi grapho
  ) =>
  Create'
    ptr
    (CTOR.Lowpass argA argB)
    graphi
    grapho where
  create' ptr (CTOR.Lowpass argA argB) =
    unsafeFrame
      $ do
          let
            nn = reflectSymbol ptr

            argA_iv' = initialVal argA

            argB_iv' = initialVal argB
          modify_
            ( \i ->
                i
                  { internalNodes = (M.insert nn (ALowpass argA_iv' argB_iv') i.internalNodes)
                  , instructions = i.instructions <> [ makeLowpass nn argA_iv' argB_iv' ]
                  }
            )

instance createLowshelf ::
  ( IsSymbol ptr
  , InitialVal argA
  , InitialVal argB
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TLowshelf {}) graphi grapho
  ) =>
  Create'
    ptr
    (CTOR.Lowshelf argA argB)
    graphi
    grapho where
  create' ptr (CTOR.Lowshelf argA argB) =
    unsafeFrame
      $ do
          let
            nn = reflectSymbol ptr

            argA_iv' = initialVal argA

            argB_iv' = initialVal argB
          modify_
            ( \i ->
                i
                  { internalNodes = (M.insert nn (ALowshelf argA_iv' argB_iv') i.internalNodes)
                  , instructions = i.instructions <> [ makeLowshelf nn argA_iv' argB_iv' ]
                  }
            )

instance createMicrophone ::
  ( R.Lacks "microphone" graphi
  , R.Cons "microphone" (NodeC CTOR.TMicrophone {}) graphi grapho
  ) =>
  Create'
    "microphone"
    CTOR.Microphone
    graphi
    grapho where
  create' ptr CTOR.Microphone =
    unsafeFrame
      $ do
          let
            nn = "microphone"
          modify_
            ( \i ->
                i
                  { internalNodes = (M.insert nn AMicrophone i.internalNodes)
                  , instructions = i.instructions <> [ makeMicrophone ]
                  }
            )

instance createNotch ::
  ( IsSymbol ptr
  , InitialVal argA
  , InitialVal argB
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TNotch {}) graphi grapho
  ) =>
  Create'
    ptr
    (CTOR.Notch argA argB)
    graphi
    grapho where
  create' ptr (CTOR.Notch argA argB) =
    unsafeFrame
      $ do
          let
            nn = reflectSymbol ptr

            argA_iv' = initialVal argA

            argB_iv' = initialVal argB
          modify_
            ( \i ->
                i
                  { internalNodes = (M.insert nn (ANotch argA_iv' argB_iv') i.internalNodes)
                  , instructions = i.instructions <> [ makeNotch nn argA_iv' argB_iv' ]
                  }
            )

instance createPeaking ::
  ( IsSymbol ptr
  , InitialVal argA
  , InitialVal argB
  , InitialVal argC
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TPeaking {}) graphi grapho
  ) =>
  Create'
    ptr
    (CTOR.Peaking argA argB argC)
    graphi
    grapho where
  create' ptr (CTOR.Peaking argA argB argC) =
    unsafeFrame
      $ do
          let
            nn = reflectSymbol ptr

            argA_iv' = initialVal argA

            argB_iv' = initialVal argB

            argC_iv' = initialVal argC
          modify_
            ( \i ->
                i
                  { internalNodes = (M.insert nn (APeaking argA_iv' argB_iv' argC_iv') i.internalNodes)
                  , instructions = i.instructions <> [ makePeaking nn argA_iv' argB_iv' argC_iv' ]
                  }
            )

instance createPeriodicOsc ::
  ( IsSymbol ptr
  , InitialVal argA
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TPeriodicOsc {}) graphi grapho
  ) =>
  Create'
    ptr
    (CTOR.PeriodicOsc argA)
    graphi
    grapho where
  create' ptr (CTOR.PeriodicOsc oscName onOff argA) =
    unsafeFrame
      $ do
          let
            nn = reflectSymbol ptr

            argA_iv' = initialVal argA
          modify_
            ( \i ->
                i
                  { internalNodes = (M.insert nn (APeriodicOsc oscName onOff argA_iv') i.internalNodes)
                  , instructions = i.instructions <> [ makePeriodicOsc nn oscName onOff argA_iv' ]
                  }
            )

instance createPlayBuf ::
  ( IsSymbol ptr
  , InitialVal argA
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TPlayBuf {}) graphi grapho
  ) =>
  Create'
    ptr
    (CTOR.PlayBuf argA)
    graphi
    grapho where
  create' ptr (CTOR.PlayBuf bufname offset onOff argA) =
    unsafeFrame
      $ do
          let
            nn = reflectSymbol ptr

            argA_iv' = initialVal argA
          modify_
            ( \i ->
                i
                  { internalNodes = (M.insert nn (APlayBuf bufname offset onOff argA_iv') i.internalNodes)
                  , instructions = i.instructions <> [ makePlayBuf nn bufname offset onOff argA_iv' ]
                  }
            )

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
  create' ptr (CTOR.Recorder sym) =
    unsafeFrame
      $ do
          let
            nn = reflectSymbol ptr

            recorder = reflectSymbol sym
          modify_
            ( \i ->
                i
                  { internalNodes = (M.insert nn (ARecorder recorder) i.internalNodes)
                  , instructions = i.instructions <> [ makeRecorder nn recorder ]
                  }
            )

instance createSawtoothOsc ::
  ( IsSymbol ptr
  , InitialVal argA
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TSawtoothOsc {}) graphi grapho
  ) =>
  Create'
    ptr
    (CTOR.SawtoothOsc argA)
    graphi
    grapho where
  create' ptr (CTOR.SawtoothOsc onOff argA) =
    unsafeFrame
      $ do
          let
            nn = reflectSymbol ptr

            argA_iv' = initialVal argA
          modify_
            ( \i ->
                i
                  { internalNodes = (M.insert nn (ASawtoothOsc onOff argA_iv') i.internalNodes)
                  , instructions = i.instructions <> [ makeSawtoothOsc nn onOff argA_iv' ]
                  }
            )

instance createSinOsc ::
  ( IsSymbol ptr
  , InitialVal argA
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TSinOsc {}) graphi grapho
  ) =>
  Create'
    ptr
    (CTOR.SinOsc argA)
    graphi
    grapho where
  create' ptr (CTOR.SinOsc onOff argA) =
    unsafeFrame
      $ do
          let
            nn = reflectSymbol ptr

            argA_iv' = initialVal argA
          modify_
            ( \i ->
                i
                  { internalNodes = (M.insert nn (ASinOsc onOff argA_iv') i.internalNodes)
                  , instructions = i.instructions <> [ makeSinOsc nn onOff argA_iv' ]
                  }
            )

instance createSpeaker ::
  ( R.Lacks "speaker" graphi
  , R.Cons "speaker" (NodeC CTOR.TSpeaker {}) graphi grapho
  ) =>
  Create'
    "speaker"
    CTOR.Speaker
    graphi
    grapho where
  create' ptr CTOR.Speaker =
    unsafeFrame
      $ do
          let
            nn = "speaker"
          modify_
            ( \i ->
                i
                  { internalNodes = (M.insert nn ASpeaker i.internalNodes)
                  , instructions = i.instructions <> [ makeSpeaker ]
                  }
            )

instance createSquareOsc ::
  ( IsSymbol ptr
  , InitialVal argA
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TSquareOsc {}) graphi grapho
  ) =>
  Create'
    ptr
    (CTOR.SquareOsc argA)
    graphi
    grapho where
  create' ptr (CTOR.SquareOsc onOff argA) =
    unsafeFrame
      $ do
          let
            nn = reflectSymbol ptr

            argA_iv' = initialVal argA
          modify_
            ( \i ->
                i
                  { internalNodes = (M.insert nn (ASquareOsc onOff argA_iv') i.internalNodes)
                  , instructions = i.instructions <> [ makeSquareOsc nn onOff argA_iv' ]
                  }
            )

instance createStereoPanner ::
  ( IsSymbol ptr
  , InitialVal argA
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TStereoPanner {}) graphi grapho
  ) =>
  Create'
    ptr
    (CTOR.StereoPanner argA)
    graphi
    grapho where
  create' ptr (CTOR.StereoPanner argA) =
    unsafeFrame
      $ do
          let
            nn = reflectSymbol ptr

            argA_iv' = initialVal argA
          modify_
            ( \i ->
                i
                  { internalNodes = (M.insert nn (AStereoPanner argA_iv') i.internalNodes)
                  , instructions = i.instructions <> [ makeStereoPanner nn argA_iv' ]
                  }
            )

instance createTriangleOsc ::
  ( IsSymbol ptr
  , InitialVal argA
  , R.Lacks ptr graphi
  , R.Cons ptr (NodeC CTOR.TTriangleOsc {}) graphi grapho
  ) =>
  Create'
    ptr
    (CTOR.TriangleOsc argA)
    graphi
    grapho where
  create' ptr (CTOR.TriangleOsc onOff argA) =
    unsafeFrame
      $ do
          let
            nn = reflectSymbol ptr

            argA_iv' = initialVal argA
          modify_
            ( \i ->
                i
                  { internalNodes = (M.insert nn (ATriangleOsc onOff argA_iv') i.internalNodes)
                  , instructions = i.instructions <> [ makeTriangleOsc nn onOff argA_iv' ]
                  }
            )

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
  create' ptr (CTOR.WaveShaper floatArray' oversample') =
    unsafeFrame
      $ do
          let
            nn = reflectSymbol ptr

            floatArray = reflectSymbol floatArray'

            oversample = reflectOversample oversample'
          modify_
            ( \i ->
                i
                  { internalNodes = (M.insert nn (AWaveShaper floatArray oversample) i.internalNodes)
                  , instructions = i.instructions <> [ makeWaveShaper nn floatArray oversample ]
                  }
            )
