module Test.Ops where

import Prelude

import Control.Monad.Indexed.Qualified as Ix
import Data.Functor.Indexed (ivoid)
import Data.Identity (Identity(..))
import Data.Tuple.Nested ((/\), type (/\))
import Type.Proxy (Proxy)
import WAGS as W


data MyGain

data MySinOsc

opsTest0 ::
  W.Frame Unit Void (W.UniverseC W.D0 W.InitialGraph W.SkolemListNil)
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
        W.SkolemListNil
    )
    (W.AudioUnitRef W.D1)
opsTest0 =
  W.create
    $ W.Dup (W.SinOsc 440.0) \(mySinOsc :: Proxy MySinOsc) ->
        W.Gain 1.0 \(gain :: Proxy MyGain) ->
          gain /\ W.Highpass 330.0 1.0 mySinOsc /\ mySinOsc /\ unit

ot1Type ::
  forall f g h.
  (forall a. a -> f a) ->
  (forall a. a -> g a) ->
  (forall a. a -> h a) ->
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
ot1Type f g h =
  W.Speaker
    $ W.Dup (g (W.SinOsc 440.0)) \(mySinOsc :: Proxy MySinOsc) ->
        h
          ( W.Gain 1.0 \(gain :: Proxy MyGain) ->
              gain /\ f (W.Highpass 330.0 1.0 mySinOsc) /\ mySinOsc /\ unit
          )

opsTest1 ::
  W.Frame Unit Void (W.UniverseC W.D0 W.InitialGraph W.SkolemListNil)
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
        W.SkolemListNil
    )
    (W.AudioUnitRef W.D0)
opsTest1 = W.create $ ot1Type Identity Identity Identity

opsTest2 ::
  W.Frame Unit Void (W.UniverseC W.D0 W.InitialGraph W.SkolemListNil)
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
        W.SkolemListNil
    )
    (W.AudioUnitRef W.D0)
opsTest2 = W.create $ ot1Type W.Focus Identity Identity

opsTest3 ::
  W.Frame Unit Void (W.UniverseC W.D0 W.InitialGraph W.SkolemListNil)
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
        W.SkolemListNil
    )
    (W.AudioUnitRef W.D3)
opsTest3 = Ix.do
  ivoid $ W.create $ ot1Type Identity Identity Identity
  W.cursor (ot1Type W.Focus Identity Identity)

opsTest4 ::
  W.Frame Unit Void (W.UniverseC W.D0 W.InitialGraph W.SkolemListNil)
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
        W.SkolemListNil
    )
    (W.AudioUnitRef W.D1)
opsTest4 = Ix.do
  ivoid $ W.create $ ot1Type Identity Identity Identity
  chpf <- W.cursor (ot1Type W.Focus Identity Identity)
  W.cursor (ot1Type Identity W.Focus Identity)

opsTest5 ::
  W.Frame Unit Void (W.UniverseC W.D0 W.InitialGraph W.SkolemListNil)
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
        W.SkolemListNil
    )
    (W.AudioUnitRef W.D2)
opsTest5 = Ix.do
  ivoid $ W.create $ ot1Type Identity Identity Identity
  chpf <- W.cursor (ot1Type W.Focus Identity Identity)
  csin <- W.cursor (ot1Type Identity W.Focus Identity)
  W.cursor (ot1Type Identity Identity W.Focus)

opsTest6 ::
  W.Frame Unit Void (W.UniverseC W.D0 W.InitialGraph W.SkolemListNil)
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
        W.SkolemListNil
    )
    Unit
opsTest6 = Ix.do
  ivoid $ W.create $ ot1Type Identity Identity Identity
  chpf <- W.cursor (ot1Type W.Focus Identity Identity)
  csin <- W.cursor (ot1Type Identity W.Focus Identity)
  cgain <- W.cursor (ot1Type Identity Identity W.Focus)
  W.disconnect csin chpf

opsTest7 ::
  W.Frame Unit Void (W.UniverseC W.D0 W.InitialGraph W.SkolemListNil)
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
        W.SkolemListNil
    )
    Unit
opsTest7 = Ix.do
  ivoid $ W.create $ ot1Type Identity Identity Identity
  chpf <- W.cursor (ot1Type W.Focus Identity Identity)
  csin <- W.cursor (ot1Type Identity W.Focus Identity)
  cgain <- W.cursor (ot1Type Identity Identity W.Focus)
  W.disconnect csin chpf
  W.disconnect chpf cgain

opsTest8 ::
  W.Frame Unit Void (W.UniverseC W.D0 W.InitialGraph W.SkolemListNil)
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
        W.SkolemListNil
    )
    Unit
opsTest8 = Ix.do
  ivoid $ W.create $ ot1Type Identity Identity Identity
  chpf <- W.cursor (ot1Type W.Focus Identity Identity)
  csin <- W.cursor (ot1Type Identity W.Focus Identity)
  cgain <- W.cursor (ot1Type Identity Identity W.Focus)
  W.disconnect csin chpf
  W.disconnect chpf cgain
  W.destroy chpf
