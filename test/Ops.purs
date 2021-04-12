module Test.Ops where

import Prelude
import Data.Functor.Indexed (ivoid)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\), type (/\))
import Type.Data.Peano (Z)
import Type.Proxy (Proxy)
import WAGS as W
import WAGS.Interpret (class AudioInterpret)
import WAGS.Control.Qualified as Ix

data MyGain

data MySinOsc

opsTest0 ::
  forall audio engine.
  AudioInterpret audio engine =>
  W.Frame Unit audio engine Void (W.UniverseC W.D0 W.InitialGraph Z W.SkolemListNil)
    ( W.UniverseC W.D3
        ( W.GraphC
            ( W.NodeC (W.TGain W.D1)
                (W.ManyEdges W.D1 (W.PtrListCons W.D2 (W.PtrListCons W.D0 W.PtrListNil)))
            )
            ( W.NodeListCons
                (W.NodeC (W.THighpass W.D2) (W.SingleEdge W.D0))
                (W.NodeListCons (W.NodeC (W.TSinOsc W.D0) W.NoEdge) W.NodeListNil)
            )
        )
        Z
        W.SkolemListNil
    )
    (W.AudioUnitRef W.D1)
opsTest0 =
  W.create
    $ W.Dup (W.SinOsc W.On 440.0) \(mySinOsc :: Proxy MySinOsc) ->
        W.Gain 1.0 \(gain :: Proxy MyGain) ->
          gain /\ W.Highpass 330.0 1.0 mySinOsc /\ mySinOsc /\ unit

type OT1Type f g h
  = { hpf :: W.Decorating f
    , osc :: W.Decorating g
    , gain :: W.Decorating h
    } ->
    W.Speaker
      ( W.Dup (g (W.SinOsc Number))
          ( Proxy MySinOsc ->
            h
              ( W.Gain Number
                  ( Proxy MyGain ->
                    Proxy MyGain /\ (f (W.Highpass Number Number (Proxy MySinOsc)))
                      /\ Proxy MySinOsc
                      /\ Unit
                  )
              )
          )
      )

ot1Cursors = W.decorate ot1Type

ot1Type :: forall f g h. OT1Type f g h
ot1Type { hpf, gain, osc } =
  W.Speaker
    $ W.Dup (W.dk osc (W.SinOsc W.On 440.0)) \(mySinOsc :: Proxy MySinOsc) ->
        W.dk gain
          ( W.Gain 1.0 \(myGain :: Proxy MyGain) ->
              myGain /\ W.dk hpf (W.Highpass 330.0 1.0 mySinOsc) /\ mySinOsc /\ unit
          )

opsTest1 ::
  forall audio engine.
  AudioInterpret audio engine =>
  W.Frame Unit audio engine Void (W.UniverseC W.D0 W.InitialGraph Z W.SkolemListNil)
    ( W.UniverseC W.D4
        ( W.GraphC
            (W.NodeC (W.TSpeaker W.D0) (W.SingleEdge W.D2))
            ( W.NodeListCons
                ( W.NodeC (W.TGain W.D2)
                    (W.ManyEdges W.D2 (W.PtrListCons W.D3 (W.PtrListCons W.D1 W.PtrListNil)))
                )
                ( W.NodeListCons (W.NodeC (W.THighpass W.D3) (W.SingleEdge W.D1))
                    (W.NodeListCons (W.NodeC (W.TSinOsc W.D1) W.NoEdge) W.NodeListNil)
                )
            )
        )
        Z
        W.SkolemListNil
    )
    (W.AudioUnitRef W.D0)
opsTest1 = W.create $ ot1Type (fst ot1Cursors)

opsTest2 ::
  forall audio engine.
  AudioInterpret audio engine =>
  W.Frame Unit audio engine Void (W.UniverseC W.D0 W.InitialGraph Z W.SkolemListNil)
    ( W.UniverseC W.D4
        ( W.GraphC
            (W.NodeC (W.TSpeaker W.D0) (W.SingleEdge W.D2))
            ( W.NodeListCons
                ( W.NodeC (W.TGain W.D2)
                    (W.ManyEdges W.D2 (W.PtrListCons W.D3 (W.PtrListCons W.D1 W.PtrListNil)))
                )
                ( W.NodeListCons (W.NodeC (W.THighpass W.D3) (W.SingleEdge W.D1))
                    (W.NodeListCons (W.NodeC (W.TSinOsc W.D1) W.NoEdge) W.NodeListNil)
                )
            )
        )
        Z
        W.SkolemListNil
    )
    (W.AudioUnitRef W.D0)
opsTest2 = W.create $ ot1Type (snd ot1Cursors).hpf

opsTest3 ::
  forall audio engine.
  AudioInterpret audio engine =>
  W.Frame Unit audio engine Void (W.UniverseC W.D0 W.InitialGraph Z W.SkolemListNil)
    ( W.UniverseC W.D4
        ( W.GraphC
            (W.NodeC (W.TSpeaker W.D0) (W.SingleEdge W.D2))
            ( W.NodeListCons
                ( W.NodeC (W.TGain W.D2)
                    (W.ManyEdges W.D2 (W.PtrListCons W.D3 (W.PtrListCons W.D1 W.PtrListNil)))
                )
                ( W.NodeListCons (W.NodeC (W.THighpass W.D3) (W.SingleEdge W.D1))
                    (W.NodeListCons (W.NodeC (W.TSinOsc W.D1) W.NoEdge) W.NodeListNil)
                )
            )
        )
        Z
        W.SkolemListNil
    )
    (W.AudioUnitRef W.D3)
opsTest3 = Ix.do
  ivoid $ W.create $ ot1Type (fst ot1Cursors)
  W.cursor (ot1Type (snd ot1Cursors).hpf)

opsTest4 ::
  forall audio engine.
  AudioInterpret audio engine =>
  W.Frame Unit audio engine Void (W.UniverseC W.D0 W.InitialGraph Z W.SkolemListNil)
    ( W.UniverseC W.D4
        ( W.GraphC
            (W.NodeC (W.TSpeaker W.D0) (W.SingleEdge W.D2))
            ( W.NodeListCons
                ( W.NodeC (W.TGain W.D2)
                    (W.ManyEdges W.D2 (W.PtrListCons W.D3 (W.PtrListCons W.D1 W.PtrListNil)))
                )
                ( W.NodeListCons (W.NodeC (W.THighpass W.D3) (W.SingleEdge W.D1))
                    (W.NodeListCons (W.NodeC (W.TSinOsc W.D1) W.NoEdge) W.NodeListNil)
                )
            )
        )
        Z
        W.SkolemListNil
    )
    (W.AudioUnitRef W.D1)
opsTest4 = Ix.do
  ivoid $ W.create $ ot1Type (fst ot1Cursors)
  chpf <- W.cursor $ ot1Type (snd ot1Cursors).hpf
  W.cursor $ ot1Type (snd ot1Cursors).osc

opsTest5 ::
  forall audio engine.
  AudioInterpret audio engine =>
  W.Frame Unit audio engine Void (W.UniverseC W.D0 W.InitialGraph Z W.SkolemListNil)
    ( W.UniverseC W.D4
        ( W.GraphC
            (W.NodeC (W.TSpeaker W.D0) (W.SingleEdge W.D2))
            ( W.NodeListCons
                ( W.NodeC (W.TGain W.D2)
                    (W.ManyEdges W.D2 (W.PtrListCons W.D3 (W.PtrListCons W.D1 W.PtrListNil)))
                )
                ( W.NodeListCons (W.NodeC (W.THighpass W.D3) (W.SingleEdge W.D1))
                    (W.NodeListCons (W.NodeC (W.TSinOsc W.D1) W.NoEdge) W.NodeListNil)
                )
            )
        )
        Z
        W.SkolemListNil
    )
    (W.AudioUnitRef W.D2)
opsTest5 = Ix.do
  ivoid $ W.create $ ot1Type (fst ot1Cursors)
  chpf <- W.cursor $ ot1Type (snd ot1Cursors).hpf
  csin <- W.cursor $ ot1Type (snd ot1Cursors).osc
  W.cursor $ ot1Type (snd ot1Cursors).gain

opsTest6 ::
  forall audio engine.
  AudioInterpret audio engine =>
  W.Frame Unit audio engine Void (W.UniverseC W.D0 W.InitialGraph Z W.SkolemListNil)
    ( W.UniverseC W.D4
        ( W.GraphC
            (W.NodeC (W.TSpeaker W.D0) (W.SingleEdge W.D2))
            ( W.NodeListCons
                ( W.NodeC (W.TGain W.D2)
                    (W.ManyEdges W.D2 (W.PtrListCons W.D3 (W.PtrListCons W.D1 W.PtrListNil)))
                )
                ( W.NodeListCons (W.NodeC (W.THighpass W.D3) W.NoEdge)
                    (W.NodeListCons (W.NodeC (W.TSinOsc W.D1) W.NoEdge) W.NodeListNil)
                )
            )
        )
        Z
        W.SkolemListNil
    )
    Unit
opsTest6 = Ix.do
  ivoid $ W.create $ ot1Type (fst ot1Cursors)
  chpf <- W.cursor $ ot1Type (snd ot1Cursors).hpf
  csin <- W.cursor $ ot1Type (snd ot1Cursors).osc
  cgain <- W.cursor $ ot1Type (snd ot1Cursors).gain
  W.disconnect csin chpf

opsTest7 ::
  forall audio engine.
  AudioInterpret audio engine =>
  W.Frame Unit audio engine Void (W.UniverseC W.D0 W.InitialGraph Z W.SkolemListNil)
    ( W.UniverseC W.D4
        ( W.GraphC
            (W.NodeC (W.TSpeaker W.D0) (W.SingleEdge W.D2))
            ( W.NodeListCons
                ( W.NodeC (W.TGain W.D2)
                    (W.ManyEdges W.D2 (W.PtrListCons W.D1 W.PtrListNil))
                )
                ( W.NodeListCons (W.NodeC (W.THighpass W.D3) W.NoEdge)
                    (W.NodeListCons (W.NodeC (W.TSinOsc W.D1) W.NoEdge) W.NodeListNil)
                )
            )
        )
        Z
        W.SkolemListNil
    )
    Unit
opsTest7 = Ix.do
  ivoid $ W.create $ ot1Type (fst ot1Cursors)
  chpf <- W.cursor $ ot1Type (snd ot1Cursors).hpf
  csin <- W.cursor $ ot1Type (snd ot1Cursors).osc
  cgain <- W.cursor $ ot1Type (snd ot1Cursors).gain
  W.disconnect csin chpf
  W.disconnect chpf cgain

opsTest8 ::
  forall audio engine.
  AudioInterpret audio engine =>
  W.Frame Unit audio engine Void (W.UniverseC W.D0 W.InitialGraph Z W.SkolemListNil)
    ( W.UniverseC W.D4
        ( W.GraphC
            (W.NodeC (W.TSpeaker W.D0) (W.SingleEdge W.D2))
            ( W.NodeListCons
                ( W.NodeC (W.TGain W.D2)
                    (W.ManyEdges W.D2 (W.PtrListCons W.D1 W.PtrListNil))
                )
                (W.NodeListCons (W.NodeC (W.TSinOsc W.D1) W.NoEdge) W.NodeListNil)
            )
        )
        Z
        W.SkolemListNil
    )
    Unit
opsTest8 = Ix.do
  ivoid $ W.create $ ot1Type (fst ot1Cursors)
  chpf <- W.cursor $ ot1Type (snd ot1Cursors).hpf
  csin <- W.cursor $ ot1Type (snd ot1Cursors).osc
  cgain <- W.cursor $ ot1Type (snd ot1Cursors).gain
  W.disconnect csin chpf
  W.disconnect chpf cgain
  W.destroy chpf
