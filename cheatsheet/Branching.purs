module WAGS.CheatSheet.Branching where

import Prelude
import Control.Apply.Indexed ((:*>))
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
import WAGS.Interpret (BrowserAudioBuffer)
import WAGS.Patch (ipatch)
import WAGS.Run (RunAudio, RunEngine, SceneI(..))

type Assets
  = ( buffers :: { "my-buffer" :: BrowserAudioBuffer } )

type MyGraph1
  = ( speaker :: AU.TSpeaker /\ { gain :: Unit }
    , gain :: AU.TGain /\ { osc :: Unit }
    , osc :: AU.TSinOsc /\ {}
    )

type MyGraph2
  = ( speaker :: AU.TSpeaker /\ { gain :: Unit }
    , gain :: AU.TGain /\ { buf :: Unit }
    , buf :: AU.TLoopBuf /\ {}
    )

initialFrame :: IxWAG Assets RunAudio RunEngine Frame0 Unit {} { | MyGraph1 } Number
initialFrame = ipatch $> 42.0

branch1 ::
  forall proof.
  WAG Assets RunAudio RunEngine proof Unit { | MyGraph1 } Number ->
  Scene (SceneI Unit Unit) Assets RunAudio RunEngine proof Unit
branch1 =
  ibranch \(SceneI e) a ->
    if e.time % 2.0 < 1.0 then
      Right $ ichange { osc: 330.0 } $> a
    else
      Left $ icont branch2 (ipatch :*> ichange { buf: Proxy :: _ "my-buffer" } $> "hello")

branch2 ::
  forall proof.
  WAG Assets RunAudio RunEngine proof Unit { | MyGraph2 } String ->
  Scene (SceneI Unit Unit) Assets RunAudio RunEngine proof Unit
branch2 =
  ibranch \(SceneI e) a ->
    if e.time % 2.0 > 1.0 then
      Right
        $ ichange
            { buf:
                { playbackRate: 2.1
                , buffer: Proxy :: _ "my-buffer"
                }
            }
        $> a
    else
      Left $ icont branch1 (ipatch $> 42.0)

piece :: Scene (SceneI Unit Unit) Assets RunAudio RunEngine Frame0 Unit
piece = const initialFrame @!> branch1
