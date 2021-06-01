module WAGS.Change where

import Prelude

import Control.Comonad (extract)
import Data.Functor (voidRight)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple, snd)
import Data.Tuple.Nested ((/\), type (/\))
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Partial.Unsafe (unsafePartial)
import Prim.Row as R
import Record as Record
import WAGS.Control.Indexed (IxWAG(..))
import WAGS.Control.Types (WAG, unsafeUnWAG, unsafeWAG)
import WAGS.Edgeable (class Edgeable, withEdge)
import WAGS.Graph.AudioUnit (OnOff(..))
import WAGS.Graph.AudioUnit as CTOR
import WAGS.Graph.Graph (Graph)
import WAGS.Graph.Node (NodeC)
import WAGS.Graph.Parameter (AudioParameter_(..), AudioParameter, defaultParam, param)
import WAGS.Interpret (class AudioInterpret, setAttack, setBuffer, setBufferOffset, setDelay, setFrequency, setGain, setKnee, setLoopEnd, setLoopStart, setOff, setOffset, setOn, setPan, setPeriodicOsc, setPlaybackRate, setQ, setRatio, setRelease, setThreshold)
import WAGS.Rendered (AnAudioUnit(..))

type ChangeType (ptr :: Symbol) (a :: Type) (graph :: Graph) (b :: Type)
  = forall proxy audio engine proof res.
    AudioInterpret audio engine =>
    proxy ptr ->
    WAG audio engine proof res { | graph } a ->
    WAG audio engine proof res { | graph } b

-- | Change an audio unit `node` in `igraph` with index `ptr`, outputting the changed node.
class Change' (ptr :: Symbol) (a :: Type) (graph :: Graph) (b :: Type) | ptr a graph -> b where
  change' :: ChangeType ptr a graph b

ichange' ::
  forall proxy ptr a audio engine proof res i b.
  AudioInterpret audio engine =>
  Change' ptr a i b =>
  proxy ptr ->
  a ->
  IxWAG audio engine proof res { | i } { | i } b
ichange' ptr a = IxWAG (change' ptr <<< voidRight a)

-- | A term that can be coerced to an setter for a control-rate audio parameter.
class SetterVal a where
  setterVal :: a -> AudioParameter -> AudioParameter

instance setterValNumber :: SetterVal Number where
  setterVal = const <<< AudioParameter <<< defaultParam { param = _ } <<< Just

instance setterValAudioParameter :: SetterVal AudioParameter where
  setterVal = const

instance setterValTuple :: SetterVal (Tuple a (AudioParameter -> AudioParameter)) where
  setterVal = snd

instance setterValTupleN :: SetterVal (Tuple a (AudioParameter -> Number)) where
  setterVal = map param <<< snd

instance setterValFunction :: SetterVal (AudioParameter -> AudioParameter) where
  setterVal = identity

instance setterValFunctionN :: SetterVal (AudioParameter -> Number) where
  setterVal = map param

data ChangeFoldingWithIndex
  = ChangeFoldingWithIndex

instance changeFoldingWithIndexUnit ::
  ( AudioInterpret audio engine
  , Change' sym Unit inGraph Unit
  ) =>
  FoldingWithIndex
    ChangeFoldingWithIndex
    (proxy sym)
    ( WAG
        audio
        engine
        proof
        res
        { | inGraph }
        { | inRecord }
    )
    Unit
    ( WAG
        audio
        engine
        proof
        res
        { | inGraph }
        { | inRecord }
    ) where
  foldingWithIndex ChangeFoldingWithIndex _ ifr node = ifr
else instance changeFoldingWithIndex ::
  ( AudioInterpret audio engine
  , Edgeable node' (Tuple node edges)
  , Change' sym node inGraph outNode
  , IsSymbol sym
  , R.Lacks sym inRecord
  , R.Cons sym outNode inRecord midRecord
  , HFoldlWithIndex
      ChangeFoldingWithIndex
      ( WAG
          audio
          engine
          proof
          res
          { | inGraph }
          { | midRecord }
      )
      edges
      ( WAG
          audio
          engine
          proof
          res
          { | inGraph }
          { | outRecord }
      )
  ) =>
  FoldingWithIndex
    ChangeFoldingWithIndex
    (proxy sym)
    ( WAG
        audio
        engine
        proof
        res
        { | inGraph }
        { | inRecord }
    )
    node'
    ( WAG
        audio
        engine
        proof
        res
        { | inGraph }
        { | outRecord }
    ) where
  foldingWithIndex ChangeFoldingWithIndex prop ifr node' =
    let
      node /\ edges = withEdge node'

      res = change' prop (ifr $> node)
    in
      hfoldlWithIndex
        ChangeFoldingWithIndex
        (res $> (Record.insert prop (extract res) (extract ifr)))
        edges

-- | Similar to `change'`, but accepts a record with multiple units to change.
change ::
  forall r rr audio engine proof res inGraph.
  AudioInterpret audio engine =>
  HFoldlWithIndex
    ChangeFoldingWithIndex
    ( WAG
        audio
        engine
        proof
        res
        { | inGraph }
        {}
    )
    { | r }
    ( WAG
        audio
        engine
        proof
        res
        { | inGraph }
        rr
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
    { | inGraph }
    rr
change r =
  hfoldlWithIndex
    ChangeFoldingWithIndex
    (r $> {})
    (extract r)

ichange ::
  forall r rr audio engine proof res inGraph.
  AudioInterpret audio engine =>
  HFoldlWithIndex
    ChangeFoldingWithIndex
    ( WAG
        audio
        engine
        proof
        res
        { | inGraph }
        {}
    )
    { | r }
    ( WAG
        audio
        engine
        proof
        res
        { | inGraph }
        rr
    ) =>
  { | r } ->
  IxWAG
    audio
    engine
    proof
    res
    { | inGraph }
    { | inGraph }
    rr
ichange r = IxWAG (change <<< voidRight r)

instance changeUnit ::
  Change'
    ptr
    Unit
    graphi
    Unit where
  change' _ w = w $> unit

instance changeAllpass ::
  ( IsSymbol ptr
  , SetterVal argA
  , SetterVal argB
  , R.Cons ptr (NodeC CTOR.TAllpass edges) ignore graphi
  ) =>
  Change' ptr (CTOR.Allpass argA argB) graphi (CTOR.Allpass AudioParameter AudioParameter) where
  change' ptr w = o
    where
    { context: i, value: (CTOR.Allpass argA argB) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    lookup = M.lookup nn i.internalNodes

    partial :: Partial => Maybe AnAudioUnit -> Tuple AudioParameter AudioParameter
    partial (Just (AAllpass a b)) = a /\ b

    v_argA@(AudioParameter v_argA') /\ v_argB@(AudioParameter v_argB') = unsafePartial $ partial lookup

    s_argA = setterVal argA

    argA_iv' = s_argA v_argA

    argA_Changes = let AudioParameter argA_iv = argA_iv' in if argA_iv.param == v_argA'.param && not argA_iv.forceSet then [] else [ setFrequency nn argA_iv' ]

    s_argB = setterVal argB

    argB_iv' = s_argB v_argB

    argB_Changes = let AudioParameter argB_iv = argB_iv' in if argB_iv.param == v_argB'.param && not argB_iv.forceSet then [] else [ setQ nn argB_iv' ]

    o =
      unsafeWAG
        { context:
            i
              { internalNodes = (M.insert nn (AAllpass argA_iv' argB_iv') i.internalNodes)
              , instructions = i.instructions <> (argA_Changes <> argB_Changes)
              }
        , value: CTOR.Allpass argA_iv' argB_iv'
        }

instance changeBandpass ::
  ( IsSymbol ptr
  , SetterVal argA
  , SetterVal argB
  , R.Cons ptr (NodeC CTOR.TBandpass edges) ignore graphi
  ) =>
  Change' ptr (CTOR.Bandpass argA argB) graphi (CTOR.Bandpass AudioParameter AudioParameter) where
  change' ptr w = o
    where
    { context: i, value: (CTOR.Bandpass argA argB) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    lookup = M.lookup nn i.internalNodes

    partial :: Partial => Maybe AnAudioUnit -> Tuple AudioParameter AudioParameter
    partial (Just (ABandpass a b)) = a /\ b

    v_argA@(AudioParameter v_argA') /\ v_argB@(AudioParameter v_argB') = unsafePartial $ partial lookup

    s_argA = setterVal argA

    argA_iv' = s_argA v_argA

    argA_Changes = let AudioParameter argA_iv = argA_iv' in if argA_iv.param == v_argA'.param && not argA_iv.forceSet then [] else [ setFrequency nn argA_iv' ]

    s_argB = setterVal argB

    argB_iv' = s_argB v_argB

    argB_Changes = let AudioParameter argB_iv = argB_iv' in if argB_iv.param == v_argB'.param && not argB_iv.forceSet then [] else [ setQ nn argB_iv' ]

    o =
      unsafeWAG
        { context:
            i
              { internalNodes = (M.insert nn (ABandpass argA_iv' argB_iv') i.internalNodes)
              , instructions = i.instructions <> (argA_Changes <> argB_Changes)
              }
        , value: CTOR.Bandpass argA_iv' argB_iv'
        }

instance changeConstant ::
  ( IsSymbol ptr
  , SetterVal argA
  , R.Cons ptr (NodeC CTOR.TConstant edges) ignore graphi
  ) =>
  Change' ptr (CTOR.Constant argA) graphi (CTOR.Constant AudioParameter) where
  change' ptr w = o
    where
    { context: i, value: (CTOR.Constant onOff argA) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    lookup = M.lookup nn i.internalNodes

    partial :: Partial => Maybe AnAudioUnit -> Tuple OnOff AudioParameter
    partial (Just (AConstant a b)) = a /\ b

    oldOnOff /\ v_argA@(AudioParameter v_argA') = unsafePartial $ partial lookup

    onOffDiff = oldOnOff /= onOff

    s_argA = setterVal argA

    argA_iv' = s_argA v_argA

    argA_Changes = let AudioParameter argA_iv = argA_iv' in if argA_iv.param == v_argA'.param && not argA_iv.forceSet then [] else [ setOffset nn argA_iv' ]

    o =
      unsafeWAG
        { context:
            i
              { internalNodes = (M.insert nn (AConstant onOff argA_iv') i.internalNodes)
              , instructions = i.instructions <> (argA_Changes <> (if onOffDiff then [ (if onOff == On then setOn else setOff) nn ] else []))
              }
        , value: CTOR.Constant onOff argA_iv'
        }

instance changeDelay ::
  ( IsSymbol ptr
  , SetterVal argA
  , R.Cons ptr (NodeC CTOR.TDelay edges) ignore graphi
  ) =>
  Change' ptr (CTOR.Delay argA) graphi (CTOR.Delay AudioParameter) where
  change' ptr w = o
    where
    { context: i, value: (CTOR.Delay argA) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    lookup = M.lookup nn i.internalNodes

    partial :: Partial => Maybe AnAudioUnit -> AudioParameter
    partial (Just (ADelay a)) = a

    v_argA@(AudioParameter v_argA') = unsafePartial $ partial lookup

    s_argA = setterVal argA

    argA_iv' = s_argA v_argA

    argA_Changes = let AudioParameter argA_iv = argA_iv' in if argA_iv.param == v_argA'.param && not argA_iv.forceSet then [] else [ setDelay nn argA_iv' ]

    o =
      unsafeWAG
        { context:
            i
              { internalNodes = (M.insert nn (ADelay argA_iv') i.internalNodes)
              , instructions = i.instructions <> argA_Changes
              }
        , value: CTOR.Delay argA_iv'
        }

instance changeDynamicsCompressor ::
  ( IsSymbol ptr
  , SetterVal argA
  , SetterVal argB
  , SetterVal argC
  , SetterVal argD
  , SetterVal argE
  , R.Cons ptr (NodeC CTOR.TDynamicsCompressor edges) ignore graphi
  ) =>
  Change' ptr (CTOR.DynamicsCompressor argA argB argC argD argE) graphi (CTOR.DynamicsCompressor AudioParameter AudioParameter AudioParameter AudioParameter AudioParameter) where
  change' ptr w = o
    where
    { context: i, value: (CTOR.DynamicsCompressor argA argB argC argD argE) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    lookup = M.lookup nn i.internalNodes

    partial :: Partial => Maybe AnAudioUnit -> AudioParameter /\ AudioParameter /\ AudioParameter /\ AudioParameter /\ AudioParameter
    partial (Just (ADynamicsCompressor a b c d e)) = a /\ b /\ c /\ d /\ e

    v_argA@(AudioParameter v_argA') /\ v_argB@(AudioParameter v_argB') /\ v_argC@(AudioParameter v_argC') /\ v_argD@(AudioParameter v_argD') /\ v_argE@(AudioParameter v_argE') = unsafePartial $ partial lookup

    s_argA = setterVal argA

    argA_iv' = s_argA v_argA

    argA_Changes = let AudioParameter argA_iv = argA_iv' in if argA_iv.param == v_argA'.param && not argA_iv.forceSet then [] else [ setThreshold nn argA_iv' ]

    s_argB = setterVal argB

    argB_iv' = s_argB v_argB

    argB_Changes = let AudioParameter argB_iv = argB_iv' in if argB_iv.param == v_argB'.param && not argB_iv.forceSet then [] else [ setKnee nn argB_iv' ]

    s_argC = setterVal argC

    argC_iv' = s_argC v_argC

    argC_Changes = let AudioParameter argC_iv = argC_iv' in if argC_iv.param == v_argC'.param && not argC_iv.forceSet then [] else [ setRatio nn argC_iv' ]

    s_argD = setterVal argD

    argD_iv' = s_argD v_argD

    argD_Changes = let AudioParameter argD_iv = argD_iv' in if argD_iv.param == v_argD'.param && not argD_iv.forceSet then [] else [ setAttack nn argD_iv' ]

    s_argE = setterVal argE

    argE_iv' = s_argE v_argE

    argE_Changes = let AudioParameter argE_iv = argE_iv' in if argE_iv.param == v_argE'.param && not argE_iv.forceSet then [] else [ setRelease nn argE_iv' ]

    o =
      unsafeWAG
        { context:
            i
              { internalNodes = (M.insert nn (ADynamicsCompressor argA_iv' argB_iv' argC_iv' argD_iv' argE_iv') i.internalNodes)
              , instructions = i.instructions <> (argA_Changes <> argB_Changes <> argC_Changes <> argD_Changes <> argE_Changes)
              }
        , value:
            CTOR.DynamicsCompressor argA_iv' argB_iv' argC_iv' argD_iv' argE_iv'
        }

instance changeGain ::
  ( IsSymbol ptr
  , SetterVal argA
  , R.Cons ptr (NodeC CTOR.TGain edges) ignore graphi
  ) =>
  Change' ptr (CTOR.Gain argA) graphi (CTOR.Gain AudioParameter) where
  change' ptr w = o
    where
    { context: i, value: (CTOR.Gain argA) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    lookup = M.lookup nn i.internalNodes

    partial :: Partial => Maybe AnAudioUnit -> AudioParameter
    partial (Just (AGain a)) = a

    v_argA@(AudioParameter v_argA') = unsafePartial $ partial lookup

    s_argA = setterVal argA

    argA_iv' = s_argA v_argA

    argA_Changes = let AudioParameter argA_iv = argA_iv' in if argA_iv.param == v_argA'.param && not argA_iv.forceSet then [] else [ setGain nn argA_iv' ]

    o =
      unsafeWAG
        { context:
            i
              { internalNodes = (M.insert nn (AGain argA_iv') i.internalNodes)
              , instructions = i.instructions <> argA_Changes
              }
        , value: CTOR.Gain argA_iv'
        }

instance changeHighpass ::
  ( IsSymbol ptr
  , SetterVal argA
  , SetterVal argB
  , R.Cons ptr (NodeC CTOR.THighpass edges) ignore graphi
  ) =>
  Change' ptr (CTOR.Highpass argA argB) graphi (CTOR.Highpass AudioParameter AudioParameter) where
  change' ptr w = o
    where
    { context: i, value: (CTOR.Highpass argA argB) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    lookup = M.lookup nn i.internalNodes

    partial :: Partial => Maybe AnAudioUnit -> Tuple AudioParameter AudioParameter
    partial (Just (AHighpass a b)) = a /\ b

    v_argA@(AudioParameter v_argA') /\ v_argB@(AudioParameter v_argB') = unsafePartial $ partial lookup

    s_argA = setterVal argA

    argA_iv' = s_argA v_argA

    argA_Changes = let AudioParameter argA_iv = argA_iv' in if argA_iv.param == v_argA'.param && not argA_iv.forceSet then [] else [ setFrequency nn argA_iv' ]

    s_argB = setterVal argB

    argB_iv' = s_argB v_argB

    argB_Changes = let AudioParameter argB_iv = argB_iv' in if argB_iv.param == v_argB'.param && not argB_iv.forceSet then [] else [ setQ nn argB_iv' ]

    o =
      unsafeWAG
        { context:
            i
              { internalNodes = (M.insert nn (AHighpass argA_iv' argB_iv') i.internalNodes)
              , instructions = i.instructions <> (argA_Changes <> argB_Changes)
              }
        , value: CTOR.Highpass argA_iv' argB_iv'
        }

instance changeHighshelf ::
  ( IsSymbol ptr
  , SetterVal argA
  , SetterVal argB
  , R.Cons ptr (NodeC CTOR.THighshelf edges) ignore graphi
  ) =>
  Change' ptr (CTOR.Highshelf argA argB) graphi (CTOR.Highshelf AudioParameter AudioParameter) where
  change' ptr w = o
    where
    { context: i, value: (CTOR.Highshelf argA argB) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    lookup = M.lookup nn i.internalNodes

    partial :: Partial => Maybe AnAudioUnit -> Tuple AudioParameter AudioParameter
    partial (Just (AHighshelf a b)) = a /\ b

    v_argA@(AudioParameter v_argA') /\ v_argB@(AudioParameter v_argB') = unsafePartial $ partial lookup

    s_argA = setterVal argA

    argA_iv' = s_argA v_argA

    argA_Changes = let AudioParameter argA_iv = argA_iv' in if argA_iv.param == v_argA'.param && not argA_iv.forceSet then [] else [ setFrequency nn argA_iv' ]

    s_argB = setterVal argB

    argB_iv' = s_argB v_argB

    argB_Changes = let AudioParameter argB_iv = argB_iv' in if argB_iv.param == v_argB'.param && not argB_iv.forceSet then [] else [ setGain nn argB_iv' ]

    o =
      unsafeWAG
        { context:
            i
              { internalNodes = (M.insert nn (AHighshelf argA_iv' argB_iv') i.internalNodes)
              , instructions = i.instructions <> (argA_Changes <> argB_Changes)
              }
        , value: CTOR.Highshelf argA_iv' argB_iv'
        }

instance changeLoopBuf ::
  ( IsSymbol ptr
  , SetterVal argA
  , R.Cons ptr (NodeC CTOR.TLoopBuf edges) ignore graphi
  ) =>
  Change' ptr (CTOR.LoopBuf argA) graphi (CTOR.LoopBuf AudioParameter) where
  change' ptr w = o
    where
    { context: i, value: (CTOR.LoopBuf buffer onOff argA loopStart loopEnd) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    lookup = M.lookup nn i.internalNodes

    partial :: Partial => Maybe AnAudioUnit -> String /\ OnOff /\ AudioParameter /\ Number /\ Number
    partial (Just (ALoopBuf a b c d e)) = a /\ b /\ c /\ d /\ e

    oldBuffer /\ oldOnOff /\ v_argA@(AudioParameter v_argA') /\ oldLoopStart /\ oldLoopEnd = unsafePartial $ partial lookup

    onOffDiff = oldOnOff /= onOff

    s_argA = setterVal argA

    argA_iv' = s_argA v_argA

    argA_Changes = let AudioParameter argA_iv = argA_iv' in if argA_iv.param == v_argA'.param && not argA_iv.forceSet && not (onOffDiff && onOff == On) then [] else [ setPlaybackRate nn argA_iv' ]

    o =
      unsafeWAG
        { context:
            i
              { internalNodes = (M.insert nn (ALoopBuf buffer onOff argA_iv' loopStart loopEnd) i.internalNodes)
              , instructions =
                i.instructions
                  <> ( (if buffer /= oldBuffer then [ setBuffer nn buffer ] else [])
                        <> argA_Changes
                        <> (if (oldLoopStart /= loopStart) || (onOffDiff && onOff == On) then [ setLoopStart nn loopStart ] else [])
                        <> (if (oldLoopEnd /= loopEnd) || (onOffDiff && onOff == On) then [ setLoopEnd nn loopEnd ] else [])
                        <> (if onOffDiff then [ (if onOff == On then setOn else setOff) nn ] else [])
                    )
              }
        , value: CTOR.LoopBuf buffer onOff argA_iv' loopStart loopEnd
        }

instance changeLowpass ::
  ( IsSymbol ptr
  , SetterVal argA
  , SetterVal argB
  , R.Cons ptr (NodeC CTOR.TLowpass edges) ignore graphi
  ) =>
  Change' ptr (CTOR.Lowpass argA argB) graphi (CTOR.Lowpass AudioParameter AudioParameter) where
  change' ptr w = o
    where
    { context: i, value: (CTOR.Lowpass argA argB) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    lookup = M.lookup nn i.internalNodes

    partial :: Partial => Maybe AnAudioUnit -> Tuple AudioParameter AudioParameter
    partial (Just (ALowpass a b)) = a /\ b

    v_argA@(AudioParameter v_argA') /\ v_argB@(AudioParameter v_argB') = unsafePartial $ partial lookup

    s_argA = setterVal argA

    argA_iv' = s_argA v_argA

    argA_Changes = let AudioParameter argA_iv = argA_iv' in if argA_iv.param == v_argA'.param && not argA_iv.forceSet then [] else [ setFrequency nn argA_iv' ]

    s_argB = setterVal argB

    argB_iv' = s_argB v_argB

    argB_Changes = let AudioParameter argB_iv = argB_iv' in if argB_iv.param == v_argB'.param && not argB_iv.forceSet then [] else [ setQ nn argB_iv' ]

    o =
      unsafeWAG
        { context:
            i
              { internalNodes = (M.insert nn (ALowpass argA_iv' argB_iv') i.internalNodes)
              , instructions = i.instructions <> (argA_Changes <> argB_Changes)
              }
        , value: CTOR.Lowpass argA_iv' argB_iv'
        }

instance changeLowshelf ::
  ( IsSymbol ptr
  , SetterVal argA
  , SetterVal argB
  , R.Cons ptr (NodeC CTOR.TLowshelf edges) ignore graphi
  ) =>
  Change' ptr (CTOR.Lowshelf argA argB) graphi (CTOR.Lowshelf AudioParameter AudioParameter) where
  change' ptr w = o
    where
    { context: i, value: (CTOR.Lowshelf argA argB) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    lookup = M.lookup nn i.internalNodes

    partial :: Partial => Maybe AnAudioUnit -> Tuple AudioParameter AudioParameter
    partial (Just (ALowshelf a b)) = a /\ b

    v_argA@(AudioParameter v_argA') /\ v_argB@(AudioParameter v_argB') = unsafePartial $ partial lookup

    s_argA = setterVal argA

    argA_iv' = s_argA v_argA

    argA_Changes = let AudioParameter argA_iv = argA_iv' in if argA_iv.param == v_argA'.param && not argA_iv.forceSet then [] else [ setFrequency nn argA_iv' ]

    s_argB = setterVal argB

    argB_iv' = s_argB v_argB

    argB_Changes = let AudioParameter argB_iv = argB_iv' in if argB_iv.param == v_argB'.param && not argB_iv.forceSet then [] else [ setGain nn argB_iv' ]

    o =
      unsafeWAG
        { context:
            i
              { internalNodes = (M.insert nn (ALowshelf argA_iv' argB_iv') i.internalNodes)
              , instructions = i.instructions <> (argA_Changes <> argB_Changes)
              }
        , value: CTOR.Lowshelf argA_iv' argB_iv'
        }

instance changeMicrophone ::
  ( R.Cons "microphone" (NodeC CTOR.TMicrophone edges) ignore graphi
    ) =>
  Change'
    "microphone"
    CTOR.Microphone
    graphi
    CTOR.Microphone where
  change' _ w = w

instance changeNotch ::
  ( IsSymbol ptr
  , SetterVal argA
  , SetterVal argB
  , R.Cons ptr (NodeC CTOR.TNotch edges) ignore graphi
  ) =>
  Change' ptr (CTOR.Notch argA argB) graphi (CTOR.Notch AudioParameter AudioParameter) where
  change' ptr w = o
    where
    { context: i, value: (CTOR.Notch argA argB) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    lookup = M.lookup nn i.internalNodes

    partial :: Partial => Maybe AnAudioUnit -> Tuple AudioParameter AudioParameter
    partial (Just (ANotch a b)) = a /\ b

    v_argA@(AudioParameter v_argA') /\ v_argB@(AudioParameter v_argB') = unsafePartial $ partial lookup

    s_argA = setterVal argA

    argA_iv' = s_argA v_argA

    argA_Changes = let AudioParameter argA_iv = argA_iv' in if argA_iv.param == v_argA'.param && not argA_iv.forceSet then [] else [ setFrequency nn argA_iv' ]

    s_argB = setterVal argB

    argB_iv' = s_argB v_argB

    argB_Changes = let AudioParameter argB_iv = argB_iv' in if argB_iv.param == v_argB'.param && not argB_iv.forceSet then [] else [ setQ nn argB_iv' ]

    o =
      unsafeWAG
        { context:
            i
              { internalNodes = (M.insert nn (ANotch argA_iv' argB_iv') i.internalNodes)
              , instructions = i.instructions <> (argA_Changes <> argB_Changes)
              }
        , value: CTOR.Notch argA_iv' argB_iv'
        }

instance changePeaking ::
  ( IsSymbol ptr
  , SetterVal argA
  , SetterVal argB
  , SetterVal argC
  , R.Cons ptr (NodeC CTOR.TPeaking edges) ignore graphi
  ) =>
  Change' ptr (CTOR.Peaking argA argB argC) graphi (CTOR.Peaking AudioParameter AudioParameter AudioParameter) where
  change' ptr w = o
    where
    { context: i, value: (CTOR.Peaking argA argB argC) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    lookup = M.lookup nn i.internalNodes

    partial :: Partial => Maybe AnAudioUnit -> AudioParameter /\ AudioParameter /\ AudioParameter
    partial (Just (APeaking a b c)) = a /\ b /\ c

    v_argA@(AudioParameter v_argA') /\ v_argB@(AudioParameter v_argB') /\ v_argC@(AudioParameter v_argC') = unsafePartial $ partial lookup

    s_argA = setterVal argA

    argA_iv' = s_argA v_argA

    argA_Changes = let AudioParameter argA_iv = argA_iv' in if argA_iv.param == v_argA'.param && not argA_iv.forceSet then [] else [ setFrequency nn argA_iv' ]

    s_argB = setterVal argB

    argB_iv' = s_argB v_argB

    argB_Changes = let AudioParameter argB_iv = argB_iv' in if argB_iv.param == v_argB'.param && not argB_iv.forceSet then [] else [ setQ nn argB_iv' ]

    s_argC = setterVal argC

    argC_iv' = s_argC v_argC

    argC_Changes = let AudioParameter argC_iv = argC_iv' in if argC_iv.param == v_argC'.param && not argC_iv.forceSet then [] else [ setGain nn argC_iv' ]

    o =
      unsafeWAG
        { context:
            i
              { internalNodes = (M.insert nn (APeaking argA_iv' argB_iv' argC_iv') i.internalNodes)
              , instructions = i.instructions <> (argA_Changes <> argB_Changes <> argC_Changes)
              }
        , value: CTOR.Peaking argA_iv' argB_iv' argC_iv'
        }

instance changePeriodicOsc ::
  ( IsSymbol ptr
  , SetterVal argA
  , R.Cons ptr (NodeC CTOR.TPeriodicOsc edges) ignore graphi
  ) =>
  Change' ptr (CTOR.PeriodicOsc argA) graphi (CTOR.PeriodicOsc AudioParameter) where
  change' ptr w = o
    where
    { context: i, value: (CTOR.PeriodicOsc periodicWave onOff argA) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    lookup = M.lookup nn i.internalNodes

    partial :: Partial => Maybe AnAudioUnit -> String /\ OnOff /\ AudioParameter
    partial (Just (APeriodicOsc a b c)) = a /\ b /\ c

    oldPeriodicWave /\ oldOnOff /\ v_argA@(AudioParameter v_argA') = unsafePartial $ partial lookup

    onOffDiff = oldOnOff /= onOff

    s_argA = setterVal argA

    argA_iv' = s_argA v_argA

    argA_Changes = let AudioParameter argA_iv = argA_iv' in if argA_iv.param == v_argA'.param && not argA_iv.forceSet then [] else [ setFrequency nn argA_iv' ]

    o =
      unsafeWAG
        { context:
            i
              { internalNodes = (M.insert nn (APeriodicOsc periodicWave onOff argA_iv') i.internalNodes)
              , instructions = i.instructions <> ((if periodicWave /= oldPeriodicWave then [ setPeriodicOsc nn periodicWave ] else []) <> argA_Changes <> (if onOffDiff then [ (if onOff == On then setOn else setOff) nn ] else []))
              }
        , value: CTOR.PeriodicOsc periodicWave onOff argA_iv'
        }

instance changePlayBuf ::
  ( IsSymbol ptr
  , SetterVal argA
  , R.Cons ptr (NodeC CTOR.TPlayBuf edges) ignore graphi
  ) =>
  Change' ptr (CTOR.PlayBuf argA) graphi (CTOR.PlayBuf AudioParameter) where
  change' ptr w = o
    where
    { context: i, value: (CTOR.PlayBuf buffer offset onOff argA) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    lookup = M.lookup nn i.internalNodes

    partial :: Partial => Maybe AnAudioUnit -> String /\ Number /\ OnOff /\ AudioParameter
    partial (Just (APlayBuf a b c d)) = a /\ b /\ c /\ d

    oldBuffer /\ oldOffset /\ oldOnOff /\ v_argA@(AudioParameter v_argA') = unsafePartial $ partial lookup

    onOffDiff = oldOnOff /= onOff

    s_argA = setterVal argA

    argA_iv' = s_argA v_argA

    argA_Changes = let AudioParameter argA_iv = argA_iv' in if argA_iv.param == v_argA'.param && not argA_iv.forceSet && not (onOffDiff && onOff == On) then [] else [ setPlaybackRate nn argA_iv' ]

    o =
      unsafeWAG
        { context:
            i
              { internalNodes = (M.insert nn (APlayBuf buffer offset onOff argA_iv') i.internalNodes)
              , instructions =
                i.instructions
                  <> ( (if buffer /= oldBuffer then [ setBuffer nn buffer ] else [])
                        <> argA_Changes
                        <> (if (oldOffset /= offset) || (onOffDiff && onOff == On) then [ setBufferOffset nn offset ] else [])
                        <> (if onOffDiff then [ (if onOff == On then setOn else setOff) nn ] else [])
                    )
              }
        , value: CTOR.PlayBuf buffer offset onOff argA_iv'
        }

instance changeRecorder ::
  ( IsSymbol ptr, R.Cons ptr (NodeC (CTOR.TRecorder sym) edges) ignore graphi
  ) =>
  Change'
    ptr
    (CTOR.Recorder sym)
    graphi
    (CTOR.Recorder sym) where
  change' _ w = w

instance changeSawtoothOsc ::
  ( IsSymbol ptr
  , SetterVal argA
  , R.Cons ptr (NodeC CTOR.TSawtoothOsc edges) ignore graphi
  ) =>
  Change' ptr (CTOR.SawtoothOsc argA) graphi (CTOR.SawtoothOsc AudioParameter) where
  change' ptr w = o
    where
    { context: i, value: (CTOR.SawtoothOsc onOff argA) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    lookup = M.lookup nn i.internalNodes

    partial :: Partial => Maybe AnAudioUnit -> Tuple OnOff AudioParameter
    partial (Just (ASawtoothOsc a b)) = a /\ b

    oldOnOff /\ v_argA@(AudioParameter v_argA') = unsafePartial $ partial lookup

    onOffDiff = oldOnOff /= onOff

    s_argA = setterVal argA

    argA_iv' = s_argA v_argA

    argA_Changes = let AudioParameter argA_iv = argA_iv' in if argA_iv.param == v_argA'.param && not argA_iv.forceSet then [] else [ setFrequency nn argA_iv' ]

    o =
      unsafeWAG
        { context:
            i
              { internalNodes = (M.insert nn (ASawtoothOsc onOff argA_iv') i.internalNodes)
              , instructions = i.instructions <> (argA_Changes <> (if onOffDiff then [ (if onOff == On then setOn else setOff) nn ] else []))
              }
        , value: CTOR.SawtoothOsc onOff argA_iv'
        }

instance changeSinOsc ::
  ( IsSymbol ptr
  , SetterVal argA
  , R.Cons ptr (NodeC CTOR.TSinOsc edges) ignore graphi
  ) =>
  Change' ptr (CTOR.SinOsc argA) graphi (CTOR.SinOsc AudioParameter) where
  change' ptr w = o
    where
    { context: i, value: (CTOR.SinOsc onOff argA) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    lookup = M.lookup nn i.internalNodes

    partial :: Partial => Maybe AnAudioUnit -> Tuple OnOff AudioParameter
    partial (Just (ASinOsc a b)) = a /\ b

    oldOnOff /\ v_argA@(AudioParameter v_argA') = unsafePartial $ partial lookup

    onOffDiff = oldOnOff /= onOff

    s_argA = setterVal argA

    argA_iv' = s_argA v_argA

    argA_Changes = let AudioParameter argA_iv = argA_iv' in if argA_iv.param == v_argA'.param && not argA_iv.forceSet then [] else [ setFrequency nn argA_iv' ]

    o =
      unsafeWAG
        { context:
            i
              { internalNodes = (M.insert nn (ASinOsc onOff argA_iv') i.internalNodes)
              , instructions = i.instructions <> (argA_Changes <> (if onOffDiff then [ (if onOff == On then setOn else setOff) nn ] else []))
              }
        , value: CTOR.SinOsc onOff argA_iv'
        }

instance changeSpeaker ::
  ( R.Cons "speaker" (NodeC (CTOR.TSpeaker) edges) ignore graphi
    ) =>
  Change'
    "speaker"
    (CTOR.Speaker)
    graphi
    (CTOR.Speaker) where
  change' _ w = w

instance changeSquareOsc ::
  ( IsSymbol ptr
  , SetterVal argA
  , R.Cons ptr (NodeC CTOR.TSquareOsc edges) ignore graphi
  ) =>
  Change' ptr (CTOR.SquareOsc argA) graphi (CTOR.SquareOsc AudioParameter) where
  change' ptr w = o
    where
    { context: i, value: (CTOR.SquareOsc onOff argA) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    lookup = M.lookup nn i.internalNodes

    partial :: Partial => Maybe AnAudioUnit -> Tuple OnOff AudioParameter
    partial (Just (ASquareOsc a b)) = a /\ b

    oldOnOff /\ v_argA@(AudioParameter v_argA') = unsafePartial $ partial lookup

    onOffDiff = oldOnOff /= onOff

    s_argA = setterVal argA

    argA_iv' = s_argA v_argA

    argA_Changes = let AudioParameter argA_iv = argA_iv' in if argA_iv.param == v_argA'.param && not argA_iv.forceSet then [] else [ setFrequency nn argA_iv' ]

    o =
      unsafeWAG
        { context:
            i
              { internalNodes = (M.insert nn (ASquareOsc onOff argA_iv') i.internalNodes)
              , instructions = i.instructions <> (argA_Changes <> (if onOffDiff then [ (if onOff == On then setOn else setOff) nn ] else []))
              }
        , value: CTOR.SquareOsc onOff argA_iv'
        }

instance changeStereoPanner ::
  ( IsSymbol ptr
  , SetterVal argA
  , R.Cons ptr (NodeC CTOR.TStereoPanner edges) ignore graphi
  ) =>
  Change' ptr (CTOR.StereoPanner argA) graphi (CTOR.StereoPanner AudioParameter) where
  change' ptr w = o
    where
    { context: i, value: (CTOR.StereoPanner argA) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    lookup = M.lookup nn i.internalNodes

    partial :: Partial => Maybe AnAudioUnit -> AudioParameter
    partial (Just (AStereoPanner a)) = a

    v_argA@(AudioParameter v_argA') = unsafePartial $ partial lookup

    s_argA = setterVal argA

    argA_iv' = s_argA v_argA

    argA_Changes = let AudioParameter argA_iv = argA_iv' in if argA_iv.param == v_argA'.param && not argA_iv.forceSet then [] else [ setPan nn argA_iv' ]

    o =
      unsafeWAG
        { context:
            i
              { internalNodes = (M.insert nn (AStereoPanner argA_iv') i.internalNodes)
              , instructions = i.instructions <> argA_Changes
              }
        , value: CTOR.StereoPanner argA_iv'
        }

instance changeTriangleOsc ::
  ( IsSymbol ptr
  , SetterVal argA
  , R.Cons ptr (NodeC CTOR.TTriangleOsc edges) ignore graphi
  ) =>
  Change' ptr (CTOR.TriangleOsc argA) graphi (CTOR.TriangleOsc AudioParameter) where
  change' ptr w = o
    where
    { context: i, value: (CTOR.TriangleOsc onOff argA) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    lookup = M.lookup nn i.internalNodes

    partial :: Partial => Maybe AnAudioUnit -> Tuple OnOff AudioParameter
    partial (Just (ATriangleOsc a b)) = a /\ b

    oldOnOff /\ v_argA@(AudioParameter v_argA') = unsafePartial $ partial lookup

    onOffDiff = oldOnOff /= onOff

    s_argA = setterVal argA

    argA_iv' = s_argA v_argA

    argA_Changes = let AudioParameter argA_iv = argA_iv' in if argA_iv.param == v_argA'.param && not argA_iv.forceSet then [] else [ setFrequency nn argA_iv' ]

    o =
      unsafeWAG
        { context:
            i
              { internalNodes = (M.insert nn (ATriangleOsc onOff argA_iv') i.internalNodes)
              , instructions = i.instructions <> (argA_Changes <> (if onOffDiff then [ (if onOff == On then setOn else setOff) nn ] else []))
              }
        , value: CTOR.TriangleOsc onOff argA_iv'
        }

instance changeWaveShaper ::
  ( IsSymbol ptr, R.Cons ptr (NodeC (CTOR.TWaveShaper a b) edges) ignore graphi
  ) =>
  Change'
    ptr
    (CTOR.WaveShaper a b)
    graphi
    (CTOR.WaveShaper a b) where
  change' _ w = w
