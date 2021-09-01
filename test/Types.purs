module Test.WAGS.Types where

import Prelude
import Data.Typelevel.Bool (False, True)
import Type.Proxy (Proxy(..))
import WAGS.Graph.AudioUnit (TGain, THighpass, TSinOsc)
import WAGS.Util (class Gate)
import Data.Tuple.Nested (type (/\), (/\))
import WAGS.Validation (class AllEdgesPointToNodes, class HasSourceNodes, class NoNodesAreDuplicated, class NoParallelEdges, class UniqueTerminus)

testGate :: Proxy "a"
testGate =
  Proxy
    :: forall s
     . Gate True "a" "b" s
    => Proxy s

testGate2 :: Proxy "b"
testGate2 =
  Proxy
    :: forall s
     . Gate False "a" "b" s
    => Proxy s

testNoNodesAreDuplicated
  :: Proxy
    ( sinOsc :: TSinOsc /\ {}
    , highpass :: THighpass /\ { sinOsc :: Unit }
    )
testNoNodesAreDuplicated =
  Proxy
    :: forall node
     . NoNodesAreDuplicated node
    => Proxy node

-- AllEdgesPointToNodes
testAllEdgesPointToNodes
  :: Proxy
    ( sinOsc :: TSinOsc /\ {}
    , highpass :: THighpass /\ { sinOsc :: Unit }
    )
testAllEdgesPointToNodes =
  Proxy
    :: forall node
     . AllEdgesPointToNodes node
    => Proxy node

testAllEdgesPointToNodes2
  :: Proxy
    ( sinOsc :: TSinOsc /\ {}
    , highpass :: THighpass /\ { sinOsc :: Unit }
    , gain :: TGain /\ { highpass :: Unit, sinOsc :: Unit }
    )
testAllEdgesPointToNodes2 =
  Proxy
    :: forall node
     . AllEdgesPointToNodes node
    => Proxy node

-- NoParallelEdges
testNoParallelEdges
  :: Proxy
    ( sinOsc :: TSinOsc /\ {}
    , highpass :: THighpass /\ { sinOsc :: Unit }
    , gain :: TGain /\ { highpass :: Unit, sinOsc :: Unit }
    )
testNoParallelEdges =
  Proxy
    :: forall node
     . NoParallelEdges node
    => Proxy node

testNoParallelEdges2
  :: Proxy
    ( sinOsc :: TSinOsc /\ {}
    , sinOsc2 :: TSinOsc /\ {}
    , highpass :: THighpass /\ { sinOsc :: Unit }
    , gain :: TGain /\ { highpass :: Unit, sinOsc :: Unit, sinOsc2 :: Unit }
    )
testNoParallelEdges2 =
  Proxy
    :: forall node
     . NoParallelEdges node
    => Proxy node

-- HasSourceNodes
testHasSourceNodes
  :: Proxy
    ( sinOsc :: TSinOsc /\ {}
    , sinOsc2 :: TSinOsc /\ {}
    , highpass :: THighpass /\ { sinOsc :: Unit }
    , gain :: TGain /\ { highpass :: Unit, sinOsc :: Unit, sinOsc2 :: Unit }
    )
testHasSourceNodes =
  Proxy
    :: forall node
     . HasSourceNodes node
    => Proxy node

-- UniqueTerminus
testUniqueTerminus :: Proxy "gain" /\ Proxy (TGain /\ { highpass :: Unit, sinOsc :: Unit, sinOsc2 :: Unit })
testUniqueTerminus =
  (Proxy /\ Proxy)
    :: forall sym node
     . UniqueTerminus
         ( sinOsc :: TSinOsc /\ {}
         , sinOsc2 :: TSinOsc /\ {}
         , highpass :: THighpass /\ { sinOsc :: Unit }
         , gain :: TGain /\ { highpass :: Unit, sinOsc :: Unit, sinOsc2 :: Unit }
         )
         sym
         node
    => (Proxy sym /\ Proxy node)
