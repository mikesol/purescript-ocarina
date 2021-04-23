module WAGS.Example.KitchenSink.Types.Microphone where

import Prelude

import Data.Identity (Identity(..))
import Math (cos, pi, (%))
import WAGS.Control.Types (Universe')
import WAGS.Example.KitchenSink.Timing (pieceTime, timing)
import WAGS.Example.KitchenSink.Types.Empty (BaseGraph, EI0, EI1, TopLevel)
import WAGS.Graph.Constructors (Gain, Microphone, Speaker)
import WAGS.Graph.Decorators (Focus(..), Decorating')
import WAGS.Graph.Optionals (GetSetAP, gain, microphone, speaker)
import WAGS.Universe.AudioUnit (TMicrophone)
import WAGS.Universe.EdgeProfile (NoEdge)
import WAGS.Universe.Graph (GraphC)
import WAGS.Universe.Node (NodeC)


type MicrophoneGraph
  = GraphC
      (NodeC (TMicrophone EI0) NoEdge)
      (BaseGraph EI0)

type MicrophoneUniverse cb
  = Universe' EI1 MicrophoneGraph cb

type KsMicrophone g s
  = TopLevel g (s Microphone)

ksMicrophone' ::
  forall g s.
  Decorating' g ->
  Decorating' s ->
  KsMicrophone g s
ksMicrophone' fg fs = speaker (fg $ gain 0.0 (fs $ microphone))

ksMicrophone :: KsMicrophone Identity Identity
ksMicrophone = ksMicrophone' Identity Identity

ksMicrophoneMicrophone :: KsMicrophone Identity Focus
ksMicrophoneMicrophone = ksMicrophone' Identity Focus

ksMicrophoneGain :: KsMicrophone Focus Identity
ksMicrophoneGain = ksMicrophone' Focus Identity

deltaKsMicrophone :: Number -> Speaker (Gain GetSetAP Microphone)
deltaKsMicrophone =
  (_ % pieceTime)
    >>> (_ - timing.ksMicrophone.begin)
    >>> (max 0.0)
    >>> \time ->
        let
          rad = pi * time
        in
          speaker
            $ gain (0.1 - 0.1 * (cos time)) microphone
