module Test.Ops where

import Prelude

import Data.Tuple.Nested (type (/\))
import Type.Proxy (Proxy(..))
import WAGS.Change (change)
import WAGS.Control.Functions (start)
import WAGS.Control.Types (Frame0, WAG)
import WAGS.Create (create)
import WAGS.Create.Optionals (gain, highpass, ref, sinOsc, speaker, speaker')
import WAGS.Destroy (destroy)
import WAGS.Disconnect (disconnect)
import WAGS.Graph.AudioUnit (TGain, THighpass, TSinOsc, TSpeaker)
import WAGS.Graph.Parameter (_on)
import WAGS.Interpret (class AudioInterpret)

opsTest0
  :: forall audio engine
   . AudioInterpret audio engine
  => WAG audio engine Frame0 Unit
       ( sinOsc :: TSinOsc /\ {}
       , gain :: TGain /\ { highpass :: Unit, sinOsc :: Unit }
       , highpass :: THighpass /\ { sinOsc :: Unit }
       )
       Unit
opsTest0 =
  start
    $>
      { sinOsc: sinOsc 440.0
      , gain: gain 1.0 { highpass: ref, sinOsc: ref }
      , highpass: highpass 330.0 { sinOsc: ref }
      }
    # create

opsTest1
  :: forall audio engine
   . AudioInterpret audio engine
  => WAG audio engine Frame0 Unit
       ( sinOsc :: TSinOsc /\ {}
       , gain :: TGain /\ { highpass :: Unit, sinOsc :: Unit }
       , highpass :: THighpass /\ { sinOsc :: Unit }
       , speaker :: TSpeaker /\ { gain :: Unit }
       )
       Unit
opsTest1 =
  start
    $>
      { sinOsc: sinOsc 440.0
      , gain: gain 1.0 { highpass: ref, sinOsc: ref }
      , highpass: highpass 330.0 { sinOsc: ref }
      , speaker: speaker' { gain: ref }
      }
    # create

opsTest2
  :: forall audio engine
   . AudioInterpret audio engine
  => WAG audio engine Frame0 Unit
       ( mySine :: TSinOsc /\ {}
       , gain :: TGain /\ { highpass :: Unit, mySine :: Unit }
       , highpass :: THighpass /\ { mySine :: Unit }
       , speaker :: TSpeaker /\ { gain :: Unit }
       )
       Unit
opsTest2 =
  start
    $> speaker
      { gain:
          gain 1.0
            { highpass:
                highpass 330.0 { mySine: ref }
            , mySine: sinOsc 440.0
            }
      }
    # create

opsTest3
  :: forall audio engine
   . AudioInterpret audio engine
  => WAG audio engine Frame0 Unit
       ( mySine :: TSinOsc /\ {}
       , gain :: TGain /\ { highpass :: Unit, mySine :: Unit }
       , highpass :: THighpass /\ { mySine :: Unit }
       , speaker :: TSpeaker /\ { gain :: Unit }
       )
       Unit
opsTest3 =
  start
    $> speaker
      { gain:
          gain 1.0
            { highpass:
                highpass 330.0 { mySine: ref }
            , mySine: sinOsc 440.0
            }
      }
    # create
    -- test various changes to make sure they compile

    # (<$) { highpass: 400.0 }
    # change
    # (<$) { highpass: { freq: 400.0 } }
    # change
    # (<$) { highpass: { q: 50.0 } }
    # change
    # (<$) { mySine: 330.0 }
    # change
    # (<$) { mySine: _on }
    # change
    # (<$) { mySine: { freq: 550.0 } }
    # change
    # (<$) { mySine: { freq: 550.0, onOff: _on } }
    # change

opsTest6
  :: forall audio engine
   . AudioInterpret audio engine
  => WAG audio engine Frame0 Unit
       ( sinOsc :: TSinOsc /\ {}
       , gain :: TGain /\ { highpass :: Unit, sinOsc :: Unit }
       , highpass :: THighpass /\ {}
       )
       Unit
opsTest6 =
  start
    $>
      { gain:
          gain 1.0
            { highpass:
                highpass 330.0
                  { sinOsc: sinOsc 440.0 }
            , sinOsc: ref
            }
      }
    # create
    # (<$) { source: Proxy :: _ "sinOsc", dest: Proxy :: _ "highpass" }
    # disconnect

opsTest7
  :: forall audio engine
   . AudioInterpret audio engine
  => WAG audio engine Frame0 Unit
       ( sinOsc :: TSinOsc /\ {}
       , gain :: TGain /\ { sinOsc :: Unit }
       , highpass :: THighpass /\ {}
       )
       Unit
opsTest7 =
  start
    $>
      { sinOsc: sinOsc 440.0
      , gain: gain 1.0 { highpass: ref, sinOsc: ref }
      , highpass: highpass 330.0 { sinOsc: ref }
      }
    # create
    # (<$) { source: Proxy :: _ "sinOsc", dest: Proxy :: _ "highpass" }
    # disconnect
    # (<$) { source: Proxy :: _ "highpass", dest: Proxy :: _ "gain" }
    # disconnect

opsTest8
  :: forall audio engine
   . AudioInterpret audio engine
  => WAG audio engine Frame0 Unit
       ( sinOsc :: TSinOsc /\ {}
       , gain :: TGain /\ { sinOsc :: Unit }
       )
       Unit
opsTest8 =
  start
    $>
      { sinOsc: sinOsc 440.0
      , gain: gain 1.0 { highpass: ref, sinOsc: ref }
      , highpass: highpass 330.0 { sinOsc: ref }
      }
    # create
    # (<$) { source: Proxy :: _ "sinOsc", dest: Proxy :: _ "highpass" }
    # disconnect
    # (<$) { source: Proxy :: _ "highpass", dest: Proxy :: _ "gain" }
    # disconnect
    # (<$) (Proxy :: _ "highpass")
    # destroy
