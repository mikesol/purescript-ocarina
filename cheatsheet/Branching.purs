module WAGS.CheatSheet.Branching where

import Prelude

import Control.Apply.Indexed ((:*>))
import Control.Plus (empty)
import Data.Either (Either(..))
import Data.Tuple.Nested (type (/\))
import Math ((%))
import Type.Proxy (Proxy(..))
import WAGS.Change (ichange)
import WAGS.Control.Functions (icont)
import WAGS.Control.Functions.Validated (ibranch, (@!>))
import WAGS.Control.Indexed (IxWAG)
import WAGS.Control.Types (Frame0, Scene, WAG)
import WAGS.Graph.AudioUnit as AU
import WAGS.Patch (ipatch)
import WAGS.Run (RunAudio, RunEngine, SceneI(..))
import WAGS.WebAPI (BrowserAudioBuffer)

type World = { myBuffer :: BrowserAudioBuffer }

type MyGraph1
  =
  ( speaker :: AU.TSpeaker /\ { gain :: Unit }
  , gain :: AU.TGain /\ { osc :: Unit }
  , osc :: AU.TSinOsc /\ {}
  )

type MyGraph2
  =
  ( speaker :: AU.TSpeaker /\ { gain :: Unit }
  , gain :: AU.TGain /\ { buf :: Unit }
  , buf :: AU.TLoopBuf /\ {}
  )

initialFrame :: IxWAG RunAudio RunEngine Frame0 Unit () MyGraph1 Number
initialFrame = ipatch { microphone: empty } $> 42.0

branch1
  :: forall proof
   . WAG RunAudio RunEngine proof Unit MyGraph1 Number
  -> Scene (SceneI Unit World ()) RunAudio RunEngine proof Unit
branch1 =
  ibranch \(SceneI e) a ->
    if e.time % 2.0 < 1.0 then
      Right $ ichange { osc: 330.0 } $> a
    else
      Left $ icont branch2 (ipatch { microphone: empty } :*> ichange { buf: e.world.myBuffer } $> "hello")

branch2
  :: forall proof
   . WAG RunAudio RunEngine proof Unit MyGraph2 String
  -> Scene (SceneI Unit World ()) RunAudio RunEngine proof Unit
branch2 =
  ibranch \(SceneI e) a ->
    if e.time % 2.0 > 1.0 then
      Right
        $
          ichange
            { buf:
                { playbackRate: 2.1
                , buffer: e.world.myBuffer
                }
            }
            $> a
    else
      Left $ icont branch1 (ipatch { microphone: empty } $> 42.0)

piece :: Scene (SceneI Unit World ()) RunAudio RunEngine Frame0 Unit
piece = const initialFrame @!> branch1
