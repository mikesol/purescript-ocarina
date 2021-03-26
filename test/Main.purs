module Test.Main where

import Prelude

import Data.Tuple (Tuple(..))
import Data.Typelevel.Bool (True, False)
import Effect (Effect)
import Effect.Class.Console (log)
import Stream8 (class AudioUnitEq, class Gate, class Lookup, class PtrEq, Changing, GraphC, NoEdge, NodeC, NodeListCons, NodeListNil, PtrSucc, PtrZ, Static, THighpass, TSinOsc)
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy(..))

---------------------------
----- tests

testPtrEq :: Proxy True
testPtrEq =
  Proxy ::
    forall tf.
    PtrEq PtrZ PtrZ tf =>
    Proxy tf

testPtrEq2 :: Proxy False
testPtrEq2 =
  Proxy ::
    forall tf.
    PtrEq PtrZ (PtrSucc PtrZ) tf =>
    Proxy tf

testGate :: Proxy "a"
testGate =
  Proxy ::
    forall s.
    Gate True "a" "b" s =>
    Proxy s

testGate2 :: Proxy "b"
testGate2 =
  Proxy ::
    forall s.
    Gate False "a" "b" s =>
    Proxy s

testAudioUnitEq :: Proxy False
testAudioUnitEq =
  Proxy ::
    forall tf.
    AudioUnitEq (TSinOsc PtrZ Changing) (TSinOsc PtrZ Static) tf =>
    Proxy tf

testAudioUnitEq2 :: Proxy True
testAudioUnitEq2 =
  Proxy ::
    forall tf.
    AudioUnitEq (TSinOsc PtrZ Changing) (TSinOsc PtrZ Changing) tf =>
    Proxy tf


testLookup :: Proxy (NodeC (TSinOsc PtrZ Changing) NoEdge)
testLookup =
  Proxy ::
    forall node.
    Lookup PtrZ (GraphC (NodeC (TSinOsc PtrZ Changing) NoEdge) NodeListNil) node =>
    Proxy node

testLookup2 :: Proxy (NodeC (TSinOsc (PtrSucc PtrZ) Changing) NoEdge)
testLookup2 =
  Proxy ::
    forall node.
    Lookup (PtrSucc PtrZ) (GraphC (NodeC (TSinOsc (PtrSucc PtrZ) Changing) NoEdge) NodeListNil) node =>
    Proxy node

testLookup3 :: Proxy (NodeC (TSinOsc (PtrSucc PtrZ) Changing) NoEdge)
testLookup3 =
  Proxy ::
    forall node.
    Lookup (PtrSucc PtrZ) (GraphC (NodeC (TSinOsc (PtrSucc PtrZ) Changing) NoEdge) (NodeListCons (NodeC (TSinOsc PtrZ Static) NoEdge) NodeListNil)) node =>
    Proxy node

testLookup4 :: Proxy (NodeC (TSinOsc PtrZ Static) NoEdge)
testLookup4 =
  Proxy ::
    forall node.
    Lookup PtrZ (GraphC (NodeC (TSinOsc (PtrSucc PtrZ) Changing) NoEdge) (NodeListCons (NodeC (TSinOsc PtrZ Static) NoEdge) NodeListNil)) node =>
    Proxy node

testLookup5 :: Proxy (NodeC (TSinOsc PtrZ Static) NoEdge)
testLookup5 =
  Proxy ::
    forall node.
    Lookup PtrZ (GraphC (NodeC (TSinOsc (PtrSucc PtrZ) Changing) NoEdge) (NodeListCons (NodeC (TSinOsc PtrZ Static) NoEdge) (NodeListCons (NodeC (THighpass (PtrSucc (PtrSucc PtrZ)) Static Static) NoEdge) NodeListNil))) node =>
    Proxy node

main :: Effect Unit
main = do
  log "🍝"
  log "You should add some tests."
