module WAGS.Example.KitchenSink.Types.DynamicsCompressor where

import Prelude
import Data.Identity (Identity(..))
import Math ((%))
import Type.Proxy (Proxy(..))
import WAGS.Control.Types (Universe')
import WAGS.Example.KitchenSink.Timing (pieceTime, timing)
import WAGS.Example.KitchenSink.Types.Empty (BaseGraph, EI0, EI1, EI2)
import WAGS.Graph.Constructors (DynamicsCompressor, Gain, PlayBuf, Speaker)
import WAGS.Graph.Decorators (Focus(..), Decorating')
import WAGS.Graph.Optionals (GetSetAP, compressor, gain, playBuf, speaker)
import WAGS.Universe.AudioUnit (TDynamicsCompressor, TPlayBuf)
import WAGS.Universe.EdgeProfile (NoEdge, SingleEdge)
import WAGS.Universe.Graph (GraphC)
import WAGS.Universe.Node (NodeC, NodeListCons)

type DynamicsCompressorGraph
  = GraphC
      (NodeC (TDynamicsCompressor EI0) (SingleEdge EI1))
      ( NodeListCons
          (NodeC (TPlayBuf EI1 "my-buffer") NoEdge)
          (BaseGraph EI0)
      )

type DynamicsCompressorUniverse cb
  = Universe' EI2 DynamicsCompressorGraph cb

type KsDynamicsCompressorreate (t :: Type -> Type) b
  = t (DynamicsCompressor GetSetAP GetSetAP GetSetAP GetSetAP GetSetAP (b (PlayBuf "my-buffer" GetSetAP)))

type KsDynamicsCompressor g t b
  = Speaker (g (Gain GetSetAP (KsDynamicsCompressorreate t b)))

ksDynamicsCompressorCreate ::
  forall t b.
  Decorating' t ->
  Decorating' b ->
  KsDynamicsCompressorreate t b
ksDynamicsCompressorCreate ft fb = ft $ compressor (fb $ playBuf (Proxy :: _ "my-buffer"))

ksDynamicsCompressor' ::
  forall g t b.
  Decorating' g ->
  Decorating' t ->
  Decorating' b ->
  KsDynamicsCompressor g t b
ksDynamicsCompressor' fg ft fb = speaker (fg $ gain 1.0 (ksDynamicsCompressorCreate ft fb))

ksDynamicsCompressor :: KsDynamicsCompressor Identity Identity Identity
ksDynamicsCompressor = ksDynamicsCompressor' Identity Identity Identity

ksDynamicsCompressorPlaybuf :: KsDynamicsCompressor Identity Identity Focus
ksDynamicsCompressorPlaybuf = ksDynamicsCompressor' Identity Identity Focus

ksDynamicsCompressorDynamicsCompressor :: KsDynamicsCompressor Identity Focus Identity
ksDynamicsCompressorDynamicsCompressor = ksDynamicsCompressor' Identity Focus Identity

ksDynamicsCompressorGain :: KsDynamicsCompressor Focus Identity Identity
ksDynamicsCompressorGain = ksDynamicsCompressor' Focus Identity Identity

deltaKsDynamicsCompressor :: Number -> KsDynamicsCompressor Identity Identity Identity
deltaKsDynamicsCompressor =
  (_ % pieceTime)
    >>> (_ - timing.ksDynamicsCompressor.begin)
    >>> (max 0.0)
    >>> \time ->
        speaker
          ( Identity
              $ gain (if time > (timing.ksDynamicsCompressor.dur - 1.0) then 0.0 else 1.0)
                  (Identity $ compressor (Identity $ playBuf (Proxy :: _ "my-buffer")))
          )
