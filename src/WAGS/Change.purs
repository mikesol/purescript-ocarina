module WAGS.Change where

import Prelude
import Control.Comonad (extract)
import Data.Functor (voidRight)
import Data.Maybe (Maybe(..), maybe)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Vec as V
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Prim.Row as R
import WAGS.Control.Indexed (IxWAG(..))
import WAGS.Control.Types (WAG, unsafeUnWAG, unsafeWAG)
import WAGS.Edgeable (class Edgeable, withEdge)
import WAGS.Graph.AudioUnit (OnOff(..))
import WAGS.Graph.AudioUnit as CTOR
import WAGS.Graph.Graph (Graph)
import WAGS.Graph.Node (NodeC)
import WAGS.Graph.Parameter (class MM, class Paramable, AudioParameter_(..), AudioParameter, mm, paramize)
import WAGS.Interpret (class AudioInterpret, setAttack, setBuffer, setBufferOffset, setDelay, setFrequency, setGain, setKnee, setLoopEnd, setLoopStart, setOff, setOffset, setOn, setPan, setPeriodicOsc, setPeriodicOscV, setPlaybackRate, setQ, setRatio, setRelease, setThreshold)

type ChangeType (ptr :: Symbol) (a :: Type) (graph :: Graph)
  = forall proxy audio engine proof res.
    AudioInterpret audio engine =>
    proxy ptr ->
    WAG audio engine proof res { | graph } a ->
    WAG audio engine proof res { | graph } Unit

-- | Change an audio unit `node` in `igraph` with index `ptr`, outputting the changed node.
class Change' (ptr :: Symbol) (a :: Type) (graph :: Graph) where
  change' :: ChangeType ptr a graph

ichange' ::
  forall proxy ptr a audio engine proof res i.
  AudioInterpret audio engine =>
  Change' ptr a i =>
  proxy ptr ->
  a ->
  IxWAG audio engine proof res { | i } { | i } Unit
ichange' ptr a = IxWAG (change' ptr <<< voidRight a)

data ChangeFoldingWithIndex
  = ChangeFoldingWithIndex

instance changeFoldingWithIndexUnit ::
  ( AudioInterpret audio engine
  , Change' sym Unit inGraph
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
        Unit
    ) where
  foldingWithIndex ChangeFoldingWithIndex _ ifr node = ifr $> unit
else instance changeFoldingWithIndex ::
  ( AudioInterpret audio engine
  , Edgeable node' (node /\ edges)
  , Change' sym node inGraph
  , HFoldlWithIndex
      ChangeFoldingWithIndex
      ( WAG
          audio
          engine
          proof
          res
          { | inGraph }
          Unit
      )
      edges
      ( WAG
          audio
          engine
          proof
          res
          { | inGraph }
          Unit
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
        Unit
    )
    node'
    ( WAG
        audio
        engine
        proof
        res
        { | inGraph }
        Unit
    ) where
  foldingWithIndex ChangeFoldingWithIndex prop ifr node' =
    let
      node /\ edges = withEdge node'

      res = change' prop (ifr $> node)
    in
      hfoldlWithIndex
        ChangeFoldingWithIndex
        (res $> unit)
        edges

-- | Similar to `change'`, but accepts a record with multiple units to change.
change ::
  forall r audio engine proof res inGraph.
  AudioInterpret audio engine =>
  HFoldlWithIndex
    ChangeFoldingWithIndex
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
        { | inGraph }
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
    { | inGraph }
    Unit
change r =
  hfoldlWithIndex
    ChangeFoldingWithIndex
    (r $> unit)
    (extract r)

ichange ::
  forall r audio engine proof res inGraph.
  AudioInterpret audio engine =>
  HFoldlWithIndex
    ChangeFoldingWithIndex
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
        { | inGraph }
        Unit
    ) =>
  { | r } ->
  IxWAG
    audio
    engine
    proof
    res
    { | inGraph }
    { | inGraph }
    Unit
ichange r = IxWAG (change <<< voidRight r)

class Detup (a :: Type) (b :: Type) | a -> b

instance detupT :: Detup (a /\ b) a
else instance detupOther :: Detup a a

class
  Monoid tau <= OneShotChange tau p au | tau p -> au where
  oneShotChange :: tau -> p -> au

instance changeNumber ::
  (Change' ptr AudioParameter graphi) =>
  Change'
    ptr
    Number
    graphi where
  change' px w = change' px (map (pure :: forall a. a -> AudioParameter_ a) w)

instance changeAudioParameter ::
  ( R.Cons ptr tau' ignore graphi
  , Detup tau' tau
  , Monoid tau
  , OneShotChange tau AudioParameter au
  , Change' ptr au graphi
  ) =>
  Change'
    ptr
    AudioParameter
    graphi where
  change' px w = change' px (oneShotChange (mempty :: tau) <$> w)

instance changeOO ::
  ( R.Cons ptr tau' ignore graphi
  , Detup tau' tau
  , Monoid tau
  , OneShotChange tau OnOff au
  , Change' ptr au graphi
  ) =>
  Change'
    ptr
    OnOff
    graphi where
  change' px w = change' px (oneShotChange (mempty :: tau) <$> w)

instance changeString ::
  ( R.Cons ptr tau' ignore graphi
  , Detup tau' tau
  , Monoid tau
  , OneShotChange tau String au
  , Change' ptr au graphi
  ) =>
  Change'
    ptr
    String
    graphi where
  change' px w = change' px (oneShotChange (mempty :: tau) <$> w)

instance changeVec ::
  ( R.Cons ptr tau' ignore graphi
  , Detup tau' tau
  , Monoid tau
  , OneShotChange tau (V.Vec size Number /\ V.Vec size Number) au
  , Change' ptr au graphi
  ) =>
  Change'
    ptr
    (V.Vec size Number /\ V.Vec size Number)
    graphi where
  change' px w = change' px (oneShotChange (mempty :: tau) <$> w)

instance changeUnit ::
  Change'
    ptr
    Unit
    graphi where
  change' _ w = w $> unit

instance oneShotChangeAllpass :: OneShotChange CTOR.TAllpass AudioParameter (CTOR.Allpass (Maybe AudioParameter) (Maybe AudioParameter)) where
  oneShotChange _ freq = CTOR.Allpass (Just freq) Nothing

instance changeAllpass ::
  ( IsSymbol ptr
  , MM mArgA (Maybe argA)
  , Paramable argA
  , MM mArgB (Maybe argB)
  , Paramable argB
  , R.Cons ptr (NodeC CTOR.TAllpass edges) ignore graphi
  ) =>
  Change' ptr (CTOR.Allpass mArgA mArgB) graphi where
  change' ptr w = o
    where
    { context: i, value: (CTOR.Allpass argA argB) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    argA_iv' = paramize <$> (mm argA)

    argA_Changes = maybe [] (\argA_iv'' -> let AudioParameter argA_iv = argA_iv'' in [ setFrequency nn argA_iv'' ]) argA_iv'

    argB_iv' = paramize <$> (mm argB)

    argB_Changes = maybe [] (\argB_iv'' -> let AudioParameter argB_iv = argB_iv'' in [ setQ nn argB_iv'' ]) argB_iv'

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> argA_Changes <> argB_Changes
              }
        , value: unit
        }

instance oneShotChangeBandpass :: OneShotChange CTOR.TBandpass AudioParameter (CTOR.Bandpass (Maybe AudioParameter) (Maybe AudioParameter)) where
  oneShotChange _ freq = CTOR.Bandpass (Just freq) Nothing

instance changeBandpass ::
  ( IsSymbol ptr
  , MM mArgA (Maybe argA)
  , Paramable argA
  , MM mArgB (Maybe argB)
  , Paramable argB
  , R.Cons ptr (NodeC CTOR.TBandpass edges) ignore graphi
  ) =>
  Change' ptr (CTOR.Bandpass mArgA mArgB) graphi where
  change' ptr w = o
    where
    { context: i, value: (CTOR.Bandpass argA argB) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    argA_iv' = paramize <$> (mm argA)

    argA_Changes = maybe [] (\argA_iv'' -> let AudioParameter argA_iv = argA_iv'' in [ setFrequency nn argA_iv'' ]) argA_iv'

    argB_iv' = paramize <$> (mm argB)

    argB_Changes = maybe [] (\argB_iv'' -> let AudioParameter argB_iv = argB_iv'' in [ setQ nn argB_iv'' ]) argB_iv'

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> argA_Changes <> argB_Changes
              }
        , value: unit
        }

instance oneShotChangeConstant :: OneShotChange CTOR.TConstant AudioParameter (CTOR.Constant (Maybe OnOff) (Maybe AudioParameter)) where
  oneShotChange _ offset = CTOR.Constant Nothing (Just offset)

instance oneShotChangeConstantOO :: OneShotChange CTOR.TConstant OnOff (CTOR.Constant (Maybe OnOff) (Maybe AudioParameter)) where
  oneShotChange _ oo = CTOR.Constant (Just oo) Nothing

instance changeConstant ::
  ( IsSymbol ptr
  , MM mOnOff (Maybe OnOff)
  , MM mArgA (Maybe argA)
  , Paramable argA
  , R.Cons ptr (NodeC CTOR.TConstant edges) ignore graphi
  ) =>
  Change' ptr (CTOR.Constant mOnOff mArgA) graphi where
  change' ptr w = o
    where
    { context: i, value: (CTOR.Constant onOff argA) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    oo_Changes = maybe [] (\onOff' -> [ (if onOff' == On then setOn else setOff) nn ]) (mm onOff)

    argA_iv' = paramize <$> (mm argA)

    argA_Changes = maybe [] (\argA_iv'' -> let AudioParameter argA_iv = argA_iv'' in [ setOffset nn argA_iv'' ]) argA_iv'

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> (argA_Changes <> oo_Changes)
              }
        , value: unit
        }

instance oneShotChangeDelay :: OneShotChange CTOR.TDelay AudioParameter (CTOR.Delay (Maybe AudioParameter)) where
  oneShotChange _ delay = CTOR.Delay (Just delay)

instance changeDelay ::
  ( IsSymbol ptr
  , MM mArgA (Maybe argA)
  , Paramable argA
  , R.Cons ptr (NodeC CTOR.TDelay edges) ignore graphi
  ) =>
  Change' ptr (CTOR.Delay mArgA) graphi where
  change' ptr w = o
    where
    { context: i, value: (CTOR.Delay argA) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    argA_iv' = paramize <$> (mm argA)

    argA_Changes = maybe [] (\argA_iv'' -> let AudioParameter argA_iv = argA_iv'' in [ setDelay nn argA_iv'' ]) argA_iv'

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> argA_Changes
              }
        , value: unit
        }

instance changeDynamicsCompressor ::
  ( IsSymbol ptr
  , MM mArgA (Maybe argA)
  , Paramable argA
  , MM mArgB (Maybe argB)
  , Paramable argB
  , MM mArgC (Maybe argC)
  , Paramable argC
  , MM mArgD (Maybe argD)
  , Paramable argD
  , MM mArgE (Maybe argE)
  , Paramable argE
  , R.Cons ptr (NodeC CTOR.TDynamicsCompressor edges) ignore graphi
  ) =>
  Change' ptr (CTOR.DynamicsCompressor mArgA mArgB mArgC mArgD mArgE) graphi where
  change' ptr w = o
    where
    { context: i, value: (CTOR.DynamicsCompressor argA argB argC argD argE) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    argA_iv' = paramize <$> (mm argA)

    argA_Changes = maybe [] (\argA_iv'' -> let AudioParameter argA_iv = argA_iv'' in [ setThreshold nn argA_iv'' ]) argA_iv'

    argB_iv' = paramize <$> (mm argB)

    argB_Changes = maybe [] (\argB_iv'' -> let AudioParameter argB_iv = argB_iv'' in [ setKnee nn argB_iv'' ]) argB_iv'

    argC_iv' = paramize <$> (mm argC)

    argC_Changes = maybe [] (\argC_iv'' -> let AudioParameter argC_iv = argC_iv'' in [ setRatio nn argC_iv'' ]) argC_iv'

    argD_iv' = paramize <$> (mm argD)

    argD_Changes = maybe [] (\argD_iv'' -> let AudioParameter argD_iv = argD_iv'' in [ setAttack nn argD_iv'' ]) argD_iv'

    argE_iv' = paramize <$> (mm argE)

    argE_Changes = maybe [] (\argE_iv'' -> let AudioParameter argE_iv = argE_iv'' in [ setRelease nn argE_iv'' ]) argE_iv'

    o =
      unsafeWAG
        { context:
            i
              { instructions =
                i.instructions
                  <> argA_Changes
                  <> argB_Changes
                  <> argC_Changes
                  <> argD_Changes
                  <> argE_Changes
              }
        , value: unit
        }

instance oneShotChangeGain :: OneShotChange CTOR.TGain AudioParameter (CTOR.Gain (Maybe AudioParameter)) where
  oneShotChange _ gain = CTOR.Gain (Just gain)

instance changeGain ::
  ( IsSymbol ptr
  , MM mArgA (Maybe argA)
  , Paramable argA
  , R.Cons ptr (NodeC CTOR.TGain edges) ignore graphi
  ) =>
  Change' ptr (CTOR.Gain mArgA) graphi where
  change' ptr w = o
    where
    { context: i, value: (CTOR.Gain argA) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    argA_iv' = paramize <$> (mm argA)

    argA_Changes = maybe [] (\argA_iv'' -> let AudioParameter argA_iv = argA_iv'' in [ setGain nn argA_iv'' ]) argA_iv'

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> argA_Changes
              }
        , value: unit
        }

instance oneShotChangeHighpass :: OneShotChange CTOR.THighpass AudioParameter (CTOR.Highpass (Maybe AudioParameter) (Maybe AudioParameter)) where
  oneShotChange _ freq = CTOR.Highpass (Just freq) Nothing

instance changeHighpass ::
  ( IsSymbol ptr
  , MM mArgA (Maybe argA)
  , Paramable argA
  , MM mArgB (Maybe argB)
  , Paramable argB
  , R.Cons ptr (NodeC CTOR.THighpass edges) ignore graphi
  ) =>
  Change' ptr (CTOR.Highpass mArgA mArgB) graphi where
  change' ptr w = o
    where
    { context: i, value: (CTOR.Highpass argA argB) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    argA_iv' = paramize <$> (mm argA)

    argA_Changes = maybe [] (\argA_iv'' -> let AudioParameter argA_iv = argA_iv'' in [ setFrequency nn argA_iv'' ]) argA_iv'

    argB_iv' = paramize <$> (mm argB)

    argB_Changes = maybe [] (\argB_iv'' -> let AudioParameter argB_iv = argB_iv'' in [ setQ nn argB_iv'' ]) argB_iv'

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> argA_Changes <> argB_Changes
              }
        , value: unit
        }

instance oneShotChangeHighshelf :: OneShotChange CTOR.THighshelf AudioParameter (CTOR.Highshelf (Maybe AudioParameter) (Maybe AudioParameter)) where
  oneShotChange _ freq = CTOR.Highshelf (Just freq) Nothing

instance changeHighshelf ::
  ( IsSymbol ptr
  , MM mArgA (Maybe argA)
  , Paramable argA
  , MM mArgB (Maybe argB)
  , Paramable argB
  , R.Cons ptr (NodeC CTOR.THighshelf edges) ignore graphi
  ) =>
  Change' ptr (CTOR.Highshelf mArgA mArgB) graphi where
  change' ptr w = o
    where
    { context: i, value: (CTOR.Highshelf argA argB) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    argA_iv' = paramize <$> (mm argA)

    argA_Changes = maybe [] (\argA_iv'' -> let AudioParameter argA_iv = argA_iv'' in [ setFrequency nn argA_iv'' ]) argA_iv'

    argB_iv' = paramize <$> (mm argB)

    argB_Changes = maybe [] (\argB_iv'' -> let AudioParameter argB_iv = argB_iv'' in [ setGain nn argB_iv'' ]) argB_iv'

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> argA_Changes <> argB_Changes
              }
        , value: unit
        }

instance oneShotChangeLoopBuf :: OneShotChange CTOR.TLoopBuf AudioParameter (CTOR.LoopBuf (Maybe String) (Maybe OnOff) (Maybe AudioParameter) (Maybe Number) (Maybe Number)) where
  oneShotChange _ rate = CTOR.LoopBuf Nothing Nothing (Just rate) Nothing Nothing

instance oneShotChangeLoopBufOO :: OneShotChange CTOR.TLoopBuf OnOff (CTOR.LoopBuf (Maybe String) (Maybe OnOff) (Maybe AudioParameter) (Maybe Number) (Maybe Number)) where
  oneShotChange _ onOff = CTOR.LoopBuf Nothing (Just onOff) Nothing Nothing Nothing

instance oneShotChangeLoopBufStr :: OneShotChange CTOR.TLoopBuf String (CTOR.LoopBuf (Maybe String) (Maybe OnOff) (Maybe AudioParameter) (Maybe Number) (Maybe Number)) where
  oneShotChange _ buffer = CTOR.LoopBuf (Just buffer) Nothing Nothing Nothing Nothing

instance changeLoopBuf ::
  ( IsSymbol ptr
  , MM mBuffer (Maybe String)
  , MM mOnOff (Maybe OnOff)
  , MM mArgA (Maybe argA)
  , MM mLoopStart (Maybe Number)
  , MM mLoopEnd (Maybe Number)
  , Paramable argA
  , R.Cons ptr (NodeC CTOR.TLoopBuf edges) ignore graphi
  ) =>
  Change' ptr (CTOR.LoopBuf mBuffer mOnOff mArgA mLoopStart mLoopEnd) graphi where
  change' ptr w = o
    where
    { context: i, value: (CTOR.LoopBuf buffer onOff argA loopStart loopEnd) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    buffer_Changes = maybe [] (\buffer' -> [ setBuffer nn buffer' ]) (mm buffer)

    oo_Changes = maybe [] (\onOff' -> [ (if onOff' == On then setOn else setOff) nn ]) (mm onOff)

    argA_iv' = paramize <$> (mm argA)

    argA_Changes = maybe [] (\argA_iv'' -> let AudioParameter argA_iv = argA_iv'' in [ setPlaybackRate nn argA_iv'' ]) argA_iv'

    loopStart_Changes = maybe [] (\loopStart' -> [ setLoopStart nn loopStart' ]) (mm loopStart)

    loopEnd_Changes = maybe [] (\loopEnd' -> [ setLoopEnd nn loopEnd' ]) (mm loopEnd)

    o =
      unsafeWAG
        { context:
            i
              { instructions =
                i.instructions
                  <> buffer_Changes
                  <> oo_Changes
                  <> argA_Changes
                  <> loopStart_Changes
                  <> loopEnd_Changes
              }
        , value: unit
        }

instance oneShotChangeLowpass :: OneShotChange CTOR.TLowpass AudioParameter (CTOR.Lowpass (Maybe AudioParameter) (Maybe AudioParameter)) where
  oneShotChange _ freq = CTOR.Lowpass (Just freq) Nothing

instance changeLowpass ::
  ( IsSymbol ptr
  , MM mArgA (Maybe argA)
  , Paramable argA
  , MM mArgB (Maybe argB)
  , Paramable argB
  , R.Cons ptr (NodeC CTOR.TLowpass edges) ignore graphi
  ) =>
  Change' ptr (CTOR.Lowpass mArgA mArgB) graphi where
  change' ptr w = o
    where
    { context: i, value: (CTOR.Lowpass argA argB) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    argA_iv' = paramize <$> (mm argA)

    argA_Changes = maybe [] (\argA_iv'' -> let AudioParameter argA_iv = argA_iv'' in [ setFrequency nn argA_iv'' ]) argA_iv'

    argB_iv' = paramize <$> (mm argB)

    argB_Changes = maybe [] (\argB_iv'' -> let AudioParameter argB_iv = argB_iv'' in [ setQ nn argB_iv'' ]) argB_iv'

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> argA_Changes <> argB_Changes
              }
        , value: unit
        }

instance oneShotChangeLowshelf :: OneShotChange CTOR.TLowshelf AudioParameter (CTOR.Lowshelf (Maybe AudioParameter) (Maybe AudioParameter)) where
  oneShotChange _ freq = CTOR.Lowshelf (Just freq) Nothing

instance changeLowshelf ::
  ( IsSymbol ptr
  , MM mArgA (Maybe argA)
  , Paramable argA
  , MM mArgB (Maybe argB)
  , Paramable argB
  , R.Cons ptr (NodeC CTOR.TLowshelf edges) ignore graphi
  ) =>
  Change' ptr (CTOR.Lowshelf mArgA mArgB) graphi where
  change' ptr w = o
    where
    { context: i, value: (CTOR.Lowshelf argA argB) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    argA_iv' = paramize <$> (mm argA)

    argA_Changes = maybe [] (\argA_iv'' -> let AudioParameter argA_iv = argA_iv'' in [ setFrequency nn argA_iv'' ]) argA_iv'

    argB_iv' = paramize <$> (mm argB)

    argB_Changes = maybe [] (\argB_iv'' -> let AudioParameter argB_iv = argB_iv'' in [ setGain nn argB_iv'' ]) argB_iv'

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> argA_Changes <> argB_Changes
              }
        , value: unit
        }

instance changeMicrophone ::
  ( R.Cons "microphone" (NodeC CTOR.TMicrophone edges) ignore graphi
    ) =>
  Change'
    "microphone"
    CTOR.Microphone
    graphi where
  change' _ w = w $> unit

instance oneShotChangeNotch :: OneShotChange CTOR.TNotch AudioParameter (CTOR.Notch (Maybe AudioParameter) (Maybe AudioParameter)) where
  oneShotChange _ freq = CTOR.Notch (Just freq) Nothing

instance changeNotch ::
  ( IsSymbol ptr
  , MM mArgA (Maybe argA)
  , Paramable argA
  , MM mArgB (Maybe argB)
  , Paramable argB
  , R.Cons ptr (NodeC CTOR.TNotch edges) ignore graphi
  ) =>
  Change' ptr (CTOR.Notch mArgA mArgB) graphi where
  change' ptr w = o
    where
    { context: i, value: (CTOR.Notch argA argB) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    argA_iv' = paramize <$> (mm argA)

    argA_Changes = maybe [] (\argA_iv'' -> let AudioParameter argA_iv = argA_iv'' in [ setFrequency nn argA_iv'' ]) argA_iv'

    argB_iv' = paramize <$> (mm argB)

    argB_Changes = maybe [] (\argB_iv'' -> let AudioParameter argB_iv = argB_iv'' in [ setQ nn argB_iv'' ]) argB_iv'

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> argA_Changes <> argB_Changes
              }
        , value: unit
        }

instance oneShotChangePeaking :: OneShotChange CTOR.TPeaking AudioParameter (CTOR.Peaking (Maybe AudioParameter) (Maybe AudioParameter) (Maybe AudioParameter)) where
  oneShotChange _ freq = CTOR.Peaking (Just freq) Nothing Nothing

instance changePeaking ::
  ( IsSymbol ptr
  , MM mArgA (Maybe argA)
  , Paramable argA
  , MM mArgB (Maybe argB)
  , Paramable argB
  , MM mArgC (Maybe argC)
  , Paramable argC
  , R.Cons ptr (NodeC CTOR.TPeaking edges) ignore graphi
  ) =>
  Change' ptr (CTOR.Peaking mArgA mArgB mArgC) graphi where
  change' ptr w = o
    where
    { context: i, value: (CTOR.Peaking argA argB argC) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    argA_iv' = paramize <$> (mm argA)

    argA_Changes = maybe [] (\argA_iv'' -> let AudioParameter argA_iv = argA_iv'' in [ setFrequency nn argA_iv'' ]) argA_iv'

    argB_iv' = paramize <$> (mm argB)

    argB_Changes = maybe [] (\argB_iv'' -> let AudioParameter argB_iv = argB_iv'' in [ setQ nn argB_iv'' ]) argB_iv'

    argC_iv' = paramize <$> (mm argC)

    argC_Changes = maybe [] (\argC_iv'' -> let AudioParameter argC_iv = argC_iv'' in [ setGain nn argC_iv'' ]) argC_iv'

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> argA_Changes <> argB_Changes <> argC_Changes
              }
        , value: unit
        }

instance oneShotChangePeriodicOsc :: OneShotChange CTOR.TPeriodicOsc AudioParameter (CTOR.PeriodicOsc (Maybe String) (Maybe OnOff) (Maybe AudioParameter)) where
  oneShotChange _ freq = CTOR.PeriodicOsc Nothing Nothing (Just freq)

instance oneShotChangePeriodicOscOO :: OneShotChange CTOR.TPeriodicOsc OnOff (CTOR.PeriodicOsc (Maybe String) (Maybe OnOff) (Maybe AudioParameter)) where
  oneShotChange _ oo = CTOR.PeriodicOsc Nothing (Just oo) Nothing

instance oneShotChangePeriodicOscStr :: OneShotChange CTOR.TPeriodicOsc String (CTOR.PeriodicOsc (Maybe String) (Maybe OnOff) (Maybe AudioParameter)) where
  oneShotChange _ osc = CTOR.PeriodicOsc (Just osc) Nothing Nothing

instance oneShotChangePeriodicOscVec :: OneShotChange CTOR.TPeriodicOsc (V.Vec size Number /\ V.Vec size Number) (CTOR.PeriodicOsc (Maybe (V.Vec size Number /\ V.Vec size Number)) (Maybe OnOff) (Maybe AudioParameter)) where
  oneShotChange _ osc = CTOR.PeriodicOsc (Just osc) Nothing Nothing

class ChangePeriodicOsc a where
  setPosc :: forall audio engine. AudioInterpret audio engine => String -> a -> audio -> engine

instance changePeriodicOscV :: ChangePeriodicOsc (V.Vec size Number /\ V.Vec size Number) where
  setPosc = setPeriodicOscV

instance changePeriodicOscS :: ChangePeriodicOsc String where
  setPosc = setPeriodicOsc

instance changePeriodicOsc ::
  ( IsSymbol ptr
  , MM mOsc (Maybe osc)
  , MM mOnOff (Maybe OnOff)
  , ChangePeriodicOsc osc
  , MM mArgA (Maybe argA)
  , Paramable argA
  , R.Cons ptr (NodeC CTOR.TPeriodicOsc edges) ignore graphi
  ) =>
  Change' ptr (CTOR.PeriodicOsc mOsc mOnOff mArgA) graphi where
  change' ptr w = o
    where
    { context: i, value: (CTOR.PeriodicOsc periodicWave onOff argA) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    pw_Changes = maybe [] (\periodicWave' -> [ setPosc nn periodicWave' ]) (mm periodicWave)

    oo_Changes = maybe [] (\onOff' -> [ (if onOff' == On then setOn else setOff) nn ]) (mm onOff)

    argA_iv' = paramize <$> (mm argA)

    argA_Changes = maybe [] (\argA_iv'' -> let AudioParameter argA_iv = argA_iv'' in [ setFrequency nn argA_iv'' ]) argA_iv'

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> pw_Changes <> oo_Changes <> argA_Changes
              }
        , value: unit
        }

instance oneShotChangePlayBuf :: OneShotChange CTOR.TPlayBuf AudioParameter (CTOR.PlayBuf (Maybe String) (Maybe Number) (Maybe OnOff) (Maybe AudioParameter)) where
  oneShotChange _ rate = CTOR.PlayBuf Nothing Nothing Nothing (Just rate)

instance oneShotChangePlayBufOO :: OneShotChange CTOR.TPlayBuf OnOff (CTOR.PlayBuf (Maybe String) (Maybe Number) (Maybe OnOff) (Maybe AudioParameter)) where
  oneShotChange _ oo = CTOR.PlayBuf Nothing Nothing (Just oo) Nothing

instance oneShotChangePlayBufStr :: OneShotChange CTOR.TPlayBuf String (CTOR.PlayBuf (Maybe String) (Maybe Number) (Maybe OnOff) (Maybe AudioParameter)) where
  oneShotChange _ buffer = CTOR.PlayBuf (Just buffer) Nothing Nothing Nothing

instance changePlayBuf ::
  ( IsSymbol ptr
  , MM mBuffer (Maybe String)
  , MM mOffset (Maybe Number)
  , MM mOnOff (Maybe OnOff)
  , MM mArgA (Maybe argA)
  , Paramable argA
  , R.Cons ptr (NodeC CTOR.TPlayBuf edges) ignore graphi
  ) =>
  Change' ptr (CTOR.PlayBuf mBuffer mOffset mOnOff mArgA) graphi where
  change' ptr w = o
    where
    { context: i, value: (CTOR.PlayBuf buffer offset onOff argA) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    buffer_Changes = maybe [] (\buffer' -> [ setBuffer nn buffer' ]) (mm buffer)

    offset_Changes = maybe [] (\offset' -> [ setBufferOffset nn offset' ]) (mm offset)

    oo_Changes = maybe [] (\onOff' -> [ (if onOff' == On then setOn else setOff) nn ]) (mm onOff)

    argA_iv' = paramize <$> (mm argA)

    argA_Changes = maybe [] (\argA_iv'' -> let AudioParameter argA_iv = argA_iv'' in [ setPlaybackRate nn argA_iv'' ]) argA_iv'

    o =
      unsafeWAG
        { context:
            i
              { instructions =
                i.instructions
                  <> buffer_Changes
                  <> offset_Changes
                  <> oo_Changes
                  <> argA_Changes
              }
        , value: unit
        }

instance changeRecorder ::
  ( IsSymbol ptr, R.Cons ptr (NodeC (CTOR.TRecorder sym) edges) ignore graphi
  ) =>
  Change'
    ptr
    (CTOR.Recorder sym)
    graphi where
  change' _ w = w $> unit

instance oneShotChangeSawtoothOsc :: OneShotChange CTOR.TSawtoothOsc AudioParameter (CTOR.SawtoothOsc (Maybe OnOff) (Maybe AudioParameter)) where
  oneShotChange _ freq = CTOR.SawtoothOsc Nothing (Just freq)

instance oneShotChangeSawtoothOscOO :: OneShotChange CTOR.TSawtoothOsc OnOff (CTOR.SawtoothOsc (Maybe OnOff) (Maybe AudioParameter)) where
  oneShotChange _ oo = CTOR.SawtoothOsc (Just oo) Nothing

instance changeSawtoothOsc ::
  ( IsSymbol ptr
  , MM mOnOff (Maybe OnOff)
  , MM mArgA (Maybe argA)
  , Paramable argA
  , R.Cons ptr (NodeC CTOR.TSawtoothOsc edges) ignore graphi
  ) =>
  Change' ptr (CTOR.SawtoothOsc mOnOff mArgA) graphi where
  change' ptr w = o
    where
    { context: i, value: (CTOR.SawtoothOsc onOff argA) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    oo_Changes = maybe [] (\onOff' -> [ (if onOff' == On then setOn else setOff) nn ]) (mm onOff)

    argA_iv' = paramize <$> (mm argA)

    argA_Changes = maybe [] (\argA_iv'' -> let AudioParameter argA_iv = argA_iv'' in [ setFrequency nn argA_iv'' ]) argA_iv'

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> oo_Changes <> argA_Changes
              }
        , value: unit
        }

instance oneShotChangeSinOsc :: OneShotChange CTOR.TSinOsc AudioParameter (CTOR.SinOsc (Maybe OnOff) (Maybe AudioParameter)) where
  oneShotChange _ freq = CTOR.SinOsc Nothing (Just freq)

instance oneShotChangeSinOscOO :: OneShotChange CTOR.TSinOsc OnOff (CTOR.SinOsc (Maybe OnOff) (Maybe AudioParameter)) where
  oneShotChange _ oo = CTOR.SinOsc (Just oo) Nothing

instance changeSinOsc ::
  ( IsSymbol ptr
  , MM mOnOff (Maybe OnOff)
  , MM mArgA (Maybe argA)
  , Paramable argA
  , R.Cons ptr (NodeC CTOR.TSinOsc edges) ignore graphi
  ) =>
  Change' ptr (CTOR.SinOsc mOnOff mArgA) graphi where
  change' ptr w = o
    where
    { context: i, value: (CTOR.SinOsc onOff argA) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    oo_Changes = maybe [] (\onOff' -> [ (if onOff' == On then setOn else setOff) nn ]) (mm onOff)

    argA_iv' = paramize <$> (mm argA)

    argA_Changes = maybe [] (\argA_iv'' -> let AudioParameter argA_iv = argA_iv'' in [ setFrequency nn argA_iv'' ]) argA_iv'

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> oo_Changes <> argA_Changes
              }
        , value: unit
        }

instance changeSpeaker ::
  ( R.Cons "speaker" (NodeC (CTOR.TSpeaker) edges) ignore graphi
    ) =>
  Change'
    "speaker"
    (CTOR.Speaker)
    graphi where
  change' _ w = w $> unit

instance oneShotChangeSquareOsc :: OneShotChange CTOR.TSquareOsc AudioParameter (CTOR.SquareOsc (Maybe OnOff) (Maybe AudioParameter)) where
  oneShotChange _ freq = CTOR.SquareOsc Nothing (Just freq)

instance oneShotChangeSquareOscOO :: OneShotChange CTOR.TSquareOsc OnOff (CTOR.SquareOsc (Maybe OnOff) (Maybe AudioParameter)) where
  oneShotChange _ oo = CTOR.SquareOsc (Just oo) Nothing

instance changeSquareOsc ::
  ( IsSymbol ptr
  , MM mOnOff (Maybe OnOff)
  , MM mArgA (Maybe argA)
  , Paramable argA
  , R.Cons ptr (NodeC CTOR.TSquareOsc edges) ignore graphi
  ) =>
  Change' ptr (CTOR.SquareOsc mOnOff mArgA) graphi where
  change' ptr w = o
    where
    { context: i, value: (CTOR.SquareOsc onOff argA) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    oo_Changes = maybe [] (\onOff' -> [ (if onOff' == On then setOn else setOff) nn ]) (mm onOff)

    argA_iv' = paramize <$> (mm argA)

    argA_Changes = maybe [] (\argA_iv'' -> let AudioParameter argA_iv = argA_iv'' in [ setFrequency nn argA_iv'' ]) argA_iv'

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> oo_Changes <> argA_Changes
              }
        , value: unit
        }

instance oneShotChangeStereoPanner :: OneShotChange CTOR.TStereoPanner AudioParameter (CTOR.StereoPanner (Maybe AudioParameter)) where
  oneShotChange _ pan = CTOR.StereoPanner (Just pan)

instance changeStereoPanner ::
  ( IsSymbol ptr
  , MM mArgA (Maybe argA)
  , Paramable argA
  , R.Cons ptr (NodeC CTOR.TStereoPanner edges) ignore graphi
  ) =>
  Change' ptr (CTOR.StereoPanner mArgA) graphi where
  change' ptr w = o
    where
    { context: i, value: (CTOR.StereoPanner argA) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    argA_iv' = paramize <$> (mm argA)

    argA_Changes = maybe [] (\argA_iv'' -> let AudioParameter argA_iv = argA_iv'' in [ setPan nn argA_iv'' ]) argA_iv'

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> argA_Changes
              }
        , value: unit
        }

instance oneShotChangeTriangleOsc :: OneShotChange CTOR.TTriangleOsc AudioParameter (CTOR.TriangleOsc (Maybe OnOff) (Maybe AudioParameter)) where
  oneShotChange _ freq = CTOR.TriangleOsc Nothing (Just freq)

instance oneShotChangeTriangleOscOO :: OneShotChange CTOR.TTriangleOsc OnOff (CTOR.TriangleOsc (Maybe OnOff) (Maybe AudioParameter)) where
  oneShotChange _ oo = CTOR.TriangleOsc (Just oo) Nothing

instance changeTriangleOsc ::
  ( IsSymbol ptr
  , MM mOnOff (Maybe OnOff)
  , MM mArgA (Maybe argA)
  , Paramable argA
  , R.Cons ptr (NodeC CTOR.TTriangleOsc edges) ignore graphi
  ) =>
  Change' ptr (CTOR.TriangleOsc mOnOff mArgA) graphi where
  change' ptr w = o
    where
    { context: i, value: (CTOR.TriangleOsc onOff argA) } = unsafeUnWAG w

    nn = reflectSymbol ptr

    oo_Changes = maybe [] (\onOff' -> [ (if onOff' == On then setOn else setOff) nn ]) (mm onOff)

    argA_iv' = paramize <$> (mm argA)

    argA_Changes = maybe [] (\argA_iv'' -> let AudioParameter argA_iv = argA_iv'' in [ setFrequency nn argA_iv'' ]) argA_iv'

    o =
      unsafeWAG
        { context:
            i
              { instructions = i.instructions <> oo_Changes <> argA_Changes
              }
        , value: unit
        }

instance changeWaveShaper ::
  ( IsSymbol ptr, R.Cons ptr (NodeC (CTOR.TWaveShaper a b) edges) ignore graphi
  ) =>
  Change'
    ptr
    (CTOR.WaveShaper a b)
    graphi where
  change' _ w = w $> unit
