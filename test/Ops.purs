module Test.Ops where

import Prelude
import Data.Tuple.Nested (type (/\))
import Type.Proxy (Proxy(..))
import WAGS.Control.Qualified as WAGS
import WAGS.Control.Types (Frame)
import WAGS.Create (create)
import WAGS.Destroy (destroy)
import WAGS.Disconnect (disconnect)
import WAGS.Graph.AudioUnit (TGain, THighpass, TSinOsc, TSpeaker)
import WAGS.Graph.Optionals (gain, highpass, ref, sinOsc, speaker, speaker')
import WAGS.Interpret (class AudioInterpret)

opsTest0 ::
  forall audio engine.
  AudioInterpret audio engine =>
  Frame Unit audio engine Void {}
    { sinOsc :: TSinOsc /\ {}
    , gain :: TGain /\ { highpass :: Unit, sinOsc :: Unit }
    , highpass :: THighpass /\ { sinOsc :: Unit }
    }
    Unit
opsTest0 =
  create
    { sinOsc: sinOsc 440.0
    , gain: gain 1.0 { highpass: ref, sinOsc: ref }
    , highpass: highpass 330.0 { sinOsc: ref }
    }

opsTest1 ::
  forall audio engine.
  AudioInterpret audio engine =>
  Frame Unit audio engine Void {}
    { sinOsc :: TSinOsc /\ {}
    , gain :: TGain /\ { highpass :: Unit, sinOsc :: Unit }
    , highpass :: THighpass /\ { sinOsc :: Unit }
    , speaker :: TSpeaker /\ { gain :: Unit }
    }
    Unit
opsTest1 =
  create
    { sinOsc: sinOsc 440.0
    , gain: gain 1.0 { highpass: ref, sinOsc: ref }
    , highpass: highpass 330.0 { sinOsc: ref }
    , speaker: speaker' { gain: ref }
    }

opsTest2 ::
  forall audio engine.
  AudioInterpret audio engine =>
  Frame Unit audio engine Void {}
    { mySine :: TSinOsc /\ {}
    , gain :: TGain /\ { highpass :: Unit, mySine :: Unit }
    , highpass :: THighpass /\ { mySine :: Unit }
    , speaker :: TSpeaker /\ { gain :: Unit }
    }
    Unit
opsTest2 =
  create
    $ speaker
        { gain:
            gain 1.0
              { highpass:
                  highpass 330.0 { mySine: ref }
              , mySine: sinOsc 440.0
              }
        }

opsTest6 ::
  forall audio engine.
  AudioInterpret audio engine =>
  Frame Unit audio engine Void {}
    { sinOsc :: TSinOsc /\ {}
    , gain :: TGain /\ { highpass :: Unit, sinOsc :: Unit }
    , highpass :: THighpass /\ {}
    }
    Unit
opsTest6 = WAGS.do
  create
    { gain:
        gain 1.0
          { highpass:
              highpass 330.0
                { sinOsc: sinOsc 440.0 }
          , sinOsc: ref
          }
    }
  disconnect (Proxy :: _ "sinOsc") (Proxy :: _ "highpass")

opsTest7 ::
  forall audio engine.
  AudioInterpret audio engine =>
  Frame Unit audio engine Void {}
    { sinOsc :: TSinOsc /\ {}
    , gain :: TGain /\ { sinOsc :: Unit }
    , highpass :: THighpass /\ {}
    }
    Unit
opsTest7 = WAGS.do
  create
    { sinOsc: sinOsc 440.0
    , gain: gain 1.0 { highpass: ref, sinOsc: ref }
    , highpass: highpass 330.0 { sinOsc: ref }
    }
  disconnect (Proxy :: _ "sinOsc") (Proxy :: _ "highpass")
  disconnect (Proxy :: _ "highpass") (Proxy :: _ "gain")

opsTest8 ::
  forall audio engine.
  AudioInterpret audio engine =>
  Frame Unit audio engine Void {}
    { sinOsc :: TSinOsc /\ {}
    , gain :: TGain /\ { sinOsc :: Unit }
    }
    Unit
opsTest8 = WAGS.do
  create
    { sinOsc: sinOsc 440.0
    , gain: gain 1.0 { highpass: ref, sinOsc: ref }
    , highpass: highpass 330.0 { sinOsc: ref }
    }
  disconnect (Proxy :: _ "sinOsc") (Proxy :: _ "highpass")
  disconnect (Proxy :: _ "highpass") (Proxy :: _ "gain")
  destroy (Proxy :: _ "highpass")
