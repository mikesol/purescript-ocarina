module Test.Computed where

import Prelude

import Data.Tuple.Nested (type (/\))
import Type.Proxy (Proxy(..))
import WAGS.Create.Optionals (gain, loopBuf, speaker)
import WAGS.Graph.AudioUnit (TGain, TLoopBuf, TSpeaker, reifyAUs)

type SceneType
  =
  { speaker :: TSpeaker /\ { gain0 :: Unit, gain1 :: Unit, gain2 :: Unit }
  , gain0 :: TGain /\ { loop0 :: Unit }
  , loop0 :: TLoopBuf /\ {}
  , gain1 :: TGain /\ { loop1 :: Unit }
  , loop1 :: TLoopBuf /\ {}
  , gain2 :: TGain /\ { loop2 :: Unit }
  , loop2 :: TLoopBuf /\ {}
  }

testReifyAUs :: SceneType
testReifyAUs =
  reifyAUs
    $ speaker
      { gain0:
          gain (0.3)
            { loop0: loopBuf { playbackRate: 1.0 + 0.1 } (Proxy :: _ "atar") }
      , gain1:
          gain (0.15)
            { loop1:
                loopBuf
                  { playbackRate: 1.5 + 0.1
                  , loopStart: 0.1 + 0.1
                  , loopEnd: 0.5 + 0.25
                  }
                  (Proxy :: _ "atar")
            }
      , gain2:
          gain (0.3)
            { loop2: loopBuf { playbackRate: 0.25 } (Proxy :: _ "atar") }
      }
