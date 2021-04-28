module WAGS.Example.KitchenSink.Types.WaveShaper where

import Prelude
import Data.Identity (Identity(..))
import Math ((%))
import Type.Proxy (Proxy(..))
import WAGS.Control.Types (Universe')
import WAGS.Example.KitchenSink.Timing (pieceTime, timing)
import WAGS.Example.KitchenSink.Types.Empty (BaseGraph, EI0, EI1, EI2, TopLevel)
import WAGS.Graph.Constructors (OversampleTwoX(..), PlayBuf, WaveShaper)
import WAGS.Graph.Decorators (Focus(..), Decorating')
import WAGS.Graph.Optionals (GetSetAP, gain, playBuf, speaker, waveShaper)
import WAGS.Universe.AudioUnit (TWaveShaper, TPlayBuf)
import WAGS.Universe.EdgeProfile (NoEdge, SingleEdge)
import WAGS.Universe.Graph (GraphC)
import WAGS.Universe.Node (NodeC, NodeListCons)

type WaveShaperGraph
  = GraphC
      (NodeC (TWaveShaper EI0 "my-waveshaper") (SingleEdge EI1))
      ( NodeListCons
          (NodeC (TPlayBuf EI1) NoEdge)
          (BaseGraph EI0)
      )

type WaveShaperUniverse cb
  = Universe' EI2 WaveShaperGraph cb

type KsWaveShaperCreate (t :: Type -> Type) b
  = t (WaveShaper "my-waveshaper" OversampleTwoX (b (PlayBuf GetSetAP)))

type KsWaveShaper g t b
  = TopLevel g (KsWaveShaperCreate t b)

ksWaveShaperCreate ::
  forall t b.
  Decorating' t ->
  Decorating' b ->
  KsWaveShaperCreate t b
ksWaveShaperCreate ft fb =
  ft
    $ waveShaper (Proxy :: _ "my-waveshaper")
        OversampleTwoX
        (fb $ playBuf "my-buffer")

ksWaveShaper' ::
  forall g t b.
  Decorating' g ->
  Decorating' t ->
  Decorating' b ->
  KsWaveShaper g t b
ksWaveShaper' fg ft fb =
  speaker
    (fg $ gain 0.25 (ksWaveShaperCreate ft fb))

ksWaveShaper :: KsWaveShaper Identity Identity Identity
ksWaveShaper = ksWaveShaper' Identity Identity Identity

ksWaveShaperPlaybuf :: KsWaveShaper Identity Identity Focus
ksWaveShaperPlaybuf = ksWaveShaper' Identity Identity Focus

ksWaveShaperWaveShaper :: KsWaveShaper Identity Focus Identity
ksWaveShaperWaveShaper = ksWaveShaper' Identity Focus Identity

ksWaveShaperGain :: KsWaveShaper Focus Identity Identity
ksWaveShaperGain = ksWaveShaper' Focus Identity Identity

deltaKsWaveShaper :: Number -> KsWaveShaper Identity Identity Identity
deltaKsWaveShaper =
  (_ % pieceTime)
    >>> (_ - timing.ksWaveShaper.begin)
    >>> (max 0.0)
    >>> \time ->
        speaker
          ( Identity
              $ gain (if time > (timing.ksWaveShaper.dur - 1.0) then 0.0 else 0.25)
                  ( Identity
                      $ waveShaper (Proxy :: _ "my-waveshaper")
                          OversampleTwoX
                          (Identity $ playBuf "my-buffer")
                  )
          )
