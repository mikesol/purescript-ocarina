module Test.Ops where

import Prelude

import Data.Functor.Indexed (ivoid)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\), type (/\))
import Type.Data.Peano (Z)
import Type.Proxy (Proxy)
import WAGS.Control.Qualified as WAGS
import WAGS.Control.Types (Frame)
import WAGS.Create (create)
import WAGS.Cursor (cursor)
import WAGS.Destroy (destroy)
import WAGS.Disconnect (disconnect)
import WAGS.Graph.Constructors (Dup(..), Gain(..), Highpass(..), OnOff(..), SinOsc(..), Speaker(..))
import WAGS.Graph.Decorators (Decorating(..), decorate, dk)
import WAGS.Interpret (class AudioInterpret)
import WAGS.Universe.AudioUnit (AudioUnitRef(..), TGain, THighpass, TSinOsc, TSpeaker)
import WAGS.Universe.Bin (D0, D1, D2, D3, D4, PtrListCons, PtrListNil)
import WAGS.Universe.EdgeProfile (ManyEdges, NoEdge, SingleEdge)
import WAGS.Universe.Graph (GraphC, InitialGraph)
import WAGS.Universe.Node (NodeC, NodeListCons, NodeListNil)
import WAGS.Universe.Skolems (SkolemListNil)
import WAGS.Universe.Universe (UniverseC)

data MyGain

data MySinOsc

opsTest0 ::
  forall audio engine.
  AudioInterpret audio engine =>
  Frame Unit audio engine Void (UniverseC D0 InitialGraph Z SkolemListNil)
    ( UniverseC D3
        ( GraphC
            ( NodeC (TGain D1)
                (ManyEdges D1 (PtrListCons D2 (PtrListCons D0 PtrListNil)))
            )
            ( NodeListCons
                (NodeC (THighpass D2) (SingleEdge D0))
                (NodeListCons (NodeC (TSinOsc D0) NoEdge) NodeListNil)
            )
        )
        Z
        SkolemListNil
    )
    (AudioUnitRef D1)
opsTest0 =
  create
    $ Dup (SinOsc On 440.0) \(mySinOsc :: Proxy MySinOsc) ->
        Gain 1.0 \(gain :: Proxy MyGain) ->
          gain /\ Highpass 330.0 1.0 mySinOsc /\ mySinOsc /\ unit

type OT1Type f g h
  = { hpf :: Decorating f
    , osc :: Decorating g
    , gain :: Decorating h
    } ->
    Speaker
      ( Dup (g (SinOsc Number))
          ( Proxy MySinOsc ->
            h
              ( Gain Number
                  ( Proxy MyGain ->
                    Proxy MyGain /\ (f (Highpass Number Number (Proxy MySinOsc)))
                      /\ Proxy MySinOsc
                      /\ Unit
                  )
              )
          )
      )

ot1Cursors = decorate ot1Type

ot1Type :: forall f g h. OT1Type f g h
ot1Type { hpf, gain, osc } =
  Speaker
    $ Dup (dk osc (SinOsc On 440.0)) \(mySinOsc :: Proxy MySinOsc) ->
        dk gain
          ( Gain 1.0 \(myGain :: Proxy MyGain) ->
              myGain /\ dk hpf (Highpass 330.0 1.0 mySinOsc) /\ mySinOsc /\ unit
          )

opsTest1 ::
  forall audio engine.
  AudioInterpret audio engine =>
  Frame Unit audio engine Void (UniverseC D0 InitialGraph Z SkolemListNil)
    ( UniverseC D4
        ( GraphC
            (NodeC (TSpeaker D0) (SingleEdge D2))
            ( NodeListCons
                ( NodeC (TGain D2)
                    (ManyEdges D2 (PtrListCons D3 (PtrListCons D1 PtrListNil)))
                )
                ( NodeListCons (NodeC (THighpass D3) (SingleEdge D1))
                    (NodeListCons (NodeC (TSinOsc D1) NoEdge) NodeListNil)
                )
            )
        )
        Z
        SkolemListNil
    )
    (AudioUnitRef D0)
opsTest1 = create $ ot1Type (fst ot1Cursors)

opsTest2 ::
  forall audio engine.
  AudioInterpret audio engine =>
  Frame Unit audio engine Void (UniverseC D0 InitialGraph Z SkolemListNil)
    ( UniverseC D4
        ( GraphC
            (NodeC (TSpeaker D0) (SingleEdge D2))
            ( NodeListCons
                ( NodeC (TGain D2)
                    (ManyEdges D2 (PtrListCons D3 (PtrListCons D1 PtrListNil)))
                )
                ( NodeListCons (NodeC (THighpass D3) (SingleEdge D1))
                    (NodeListCons (NodeC (TSinOsc D1) NoEdge) NodeListNil)
                )
            )
        )
        Z
        SkolemListNil
    )
    (AudioUnitRef D0)
opsTest2 = create $ ot1Type (snd ot1Cursors).hpf

opsTest3 ::
  forall audio engine.
  AudioInterpret audio engine =>
  Frame Unit audio engine Void (UniverseC D0 InitialGraph Z SkolemListNil)
    ( UniverseC D4
        ( GraphC
            (NodeC (TSpeaker D0) (SingleEdge D2))
            ( NodeListCons
                ( NodeC (TGain D2)
                    (ManyEdges D2 (PtrListCons D3 (PtrListCons D1 PtrListNil)))
                )
                ( NodeListCons (NodeC (THighpass D3) (SingleEdge D1))
                    (NodeListCons (NodeC (TSinOsc D1) NoEdge) NodeListNil)
                )
            )
        )
        Z
        SkolemListNil
    )
    (AudioUnitRef D3)
opsTest3 = WAGS.do
  ivoid $ create $ ot1Type (fst ot1Cursors)
  cursor (ot1Type (snd ot1Cursors).hpf)

opsTest4 ::
  forall audio engine.
  AudioInterpret audio engine =>
  Frame Unit audio engine Void (UniverseC D0 InitialGraph Z SkolemListNil)
    ( UniverseC D4
        ( GraphC
            (NodeC (TSpeaker D0) (SingleEdge D2))
            ( NodeListCons
                ( NodeC (TGain D2)
                    (ManyEdges D2 (PtrListCons D3 (PtrListCons D1 PtrListNil)))
                )
                ( NodeListCons (NodeC (THighpass D3) (SingleEdge D1))
                    (NodeListCons (NodeC (TSinOsc D1) NoEdge) NodeListNil)
                )
            )
        )
        Z
        SkolemListNil
    )
    (AudioUnitRef D1)
opsTest4 = WAGS.do
  ivoid $ create $ ot1Type (fst ot1Cursors)
  chpf <- cursor $ ot1Type (snd ot1Cursors).hpf
  cursor $ ot1Type (snd ot1Cursors).osc

opsTest5 ::
  forall audio engine.
  AudioInterpret audio engine =>
  Frame Unit audio engine Void (UniverseC D0 InitialGraph Z SkolemListNil)
    ( UniverseC D4
        ( GraphC
            (NodeC (TSpeaker D0) (SingleEdge D2))
            ( NodeListCons
                ( NodeC (TGain D2)
                    (ManyEdges D2 (PtrListCons D3 (PtrListCons D1 PtrListNil)))
                )
                ( NodeListCons (NodeC (THighpass D3) (SingleEdge D1))
                    (NodeListCons (NodeC (TSinOsc D1) NoEdge) NodeListNil)
                )
            )
        )
        Z
        SkolemListNil
    )
    (AudioUnitRef D2)
opsTest5 = WAGS.do
  ivoid $ create $ ot1Type (fst ot1Cursors)
  chpf <- cursor $ ot1Type (snd ot1Cursors).hpf
  csin <- cursor $ ot1Type (snd ot1Cursors).osc
  cursor $ ot1Type (snd ot1Cursors).gain

opsTest6 ::
  forall audio engine.
  AudioInterpret audio engine =>
  Frame Unit audio engine Void (UniverseC D0 InitialGraph Z SkolemListNil)
    ( UniverseC D4
        ( GraphC
            (NodeC (TSpeaker D0) (SingleEdge D2))
            ( NodeListCons
                ( NodeC (TGain D2)
                    (ManyEdges D2 (PtrListCons D3 (PtrListCons D1 PtrListNil)))
                )
                ( NodeListCons (NodeC (THighpass D3) NoEdge)
                    (NodeListCons (NodeC (TSinOsc D1) NoEdge) NodeListNil)
                )
            )
        )
        Z
        SkolemListNil
    )
    Unit
opsTest6 = WAGS.do
  ivoid $ create $ ot1Type (fst ot1Cursors)
  chpf <- cursor $ ot1Type (snd ot1Cursors).hpf
  csin <- cursor $ ot1Type (snd ot1Cursors).osc
  cgain <- cursor $ ot1Type (snd ot1Cursors).gain
  disconnect csin chpf

opsTest7 ::
  forall audio engine.
  AudioInterpret audio engine =>
  Frame Unit audio engine Void (UniverseC D0 InitialGraph Z SkolemListNil)
    ( UniverseC D4
        ( GraphC
            (NodeC (TSpeaker D0) (SingleEdge D2))
            ( NodeListCons
                ( NodeC (TGain D2)
                    (ManyEdges D2 (PtrListCons D1 PtrListNil))
                )
                ( NodeListCons (NodeC (THighpass D3) NoEdge)
                    (NodeListCons (NodeC (TSinOsc D1) NoEdge) NodeListNil)
                )
            )
        )
        Z
        SkolemListNil
    )
    Unit
opsTest7 = WAGS.do
  ivoid $ create $ ot1Type (fst ot1Cursors)
  chpf <- cursor $ ot1Type (snd ot1Cursors).hpf
  csin <- cursor $ ot1Type (snd ot1Cursors).osc
  cgain <- cursor $ ot1Type (snd ot1Cursors).gain
  disconnect csin chpf
  disconnect chpf cgain

opsTest8 ::
  forall audio engine.
  AudioInterpret audio engine =>
  Frame Unit audio engine Void (UniverseC D0 InitialGraph Z SkolemListNil)
    ( UniverseC D4
        ( GraphC
            (NodeC (TSpeaker D0) (SingleEdge D2))
            ( NodeListCons
                ( NodeC (TGain D2)
                    (ManyEdges D2 (PtrListCons D1 PtrListNil))
                )
                (NodeListCons (NodeC (TSinOsc D1) NoEdge) NodeListNil)
            )
        )
        Z
        SkolemListNil
    )
    Unit
opsTest8 = WAGS.do
  ivoid $ create $ ot1Type (fst ot1Cursors)
  chpf <- cursor $ ot1Type (snd ot1Cursors).hpf
  csin <- cursor $ ot1Type (snd ot1Cursors).osc
  cgain <- cursor $ ot1Type (snd ot1Cursors).gain
  disconnect csin chpf
  disconnect chpf cgain
  destroy chpf
