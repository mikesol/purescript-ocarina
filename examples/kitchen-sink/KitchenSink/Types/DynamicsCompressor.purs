module WAGS.Example.KitchenSink.Types.DynamicsCompressor where

import Prelude

import Data.Identity (Identity(..))
import Math ((%))
import Type.Proxy (Proxy(..))
import WAGS.Control.Types (Universe')
import WAGS.Example.KitchenSink.Timing (pieceTime, timing)
import WAGS.Example.KitchenSink.Types.Empty (BaseGraph, EI0, EI1, EI2, TopLevel)
import WAGS.Graph.Constructors (DynamicsCompressor, PlayBuf)
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
  = TopLevel g (KsDynamicsCompressorreate t b)

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
              $ gain (if time > (dur - 1.0) then 0.0 else 1.0)
                  ( Identity
                      $ compressor
                          { threshold: if time > (dur / 2.0) then -50.0 else -40.0
                          , knee: if time > (dur / 3.0) then 20.0 else 40.0
                          , ratio: if time > (dur / 4.0) then 2.0 else 5.0
                          , attack: if time > (dur / 5.0) then 0.003 else 0.005
                          , release: if time > (dur / 6.0) then 0.25 else 0.5
                          }
                          (Identity $ playBuf (Proxy :: _ "my-buffer"))
                  )
          )
  where
  dur = timing.ksDynamicsCompressor.dur