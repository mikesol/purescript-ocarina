module WAGS.Example.KitchenSink.Types.Notch where

import Prelude

import Data.Identity (Identity(..))
import Math ((%))
import Type.Proxy (Proxy(..))
import WAGS.Control.Types (Universe')
import WAGS.Example.KitchenSink.Timing (calcSlope, pieceTime, timing)
import WAGS.Example.KitchenSink.Types.Empty (BaseGraph, EI0, EI1, EI2, TopLevel)
import WAGS.Graph.Constructors (Notch, PlayBuf)
import WAGS.Graph.Decorators (Focus(..), Decorating')
import WAGS.Graph.Optionals (GetSetAP, gain, notch, playBuf, speaker)
import WAGS.Universe.AudioUnit (TNotch, TPlayBuf)
import WAGS.Universe.EdgeProfile (NoEdge, SingleEdge)
import WAGS.Universe.Graph (GraphC)
import WAGS.Universe.Node (NodeC, NodeListCons)


type NotchGraph
  = GraphC
      (NodeC (TNotch EI0) (SingleEdge EI1))
      ( NodeListCons
          (NodeC (TPlayBuf EI1 "my-buffer") NoEdge)
          (BaseGraph EI0)
      )

type NotchUniverse cb
  = Universe' EI2 NotchGraph cb

type KsNotchreate (t :: Type -> Type) b
  = t (Notch GetSetAP GetSetAP (b (PlayBuf "my-buffer" GetSetAP)))

type KsNotch g t b
  = TopLevel g (KsNotchreate t b)

ksNotchCreate ::
  forall t b.
  Decorating' t ->
  Decorating' b ->
  KsNotchreate t b
ksNotchCreate ft fb = ft $ notch { freq: 300.0 } (fb $ playBuf (Proxy :: _ "my-buffer"))

ksNotch' ::
  forall g t b.
  Decorating' g ->
  Decorating' t ->
  Decorating' b ->
  KsNotch g t b
ksNotch' fg ft fb =
  speaker
    (fg $ gain 1.0 (ksNotchCreate ft fb))

ksNotch :: KsNotch Identity Identity Identity
ksNotch = ksNotch' Identity Identity Identity

ksNotchPlaybuf :: KsNotch Identity Identity Focus
ksNotchPlaybuf = ksNotch' Identity Identity Focus

ksNotchNotch :: KsNotch Identity Focus Identity
ksNotchNotch = ksNotch' Identity Focus Identity

ksNotchGain :: KsNotch Focus Identity Identity
ksNotchGain = ksNotch' Focus Identity Identity

deltaKsNotch :: Number -> KsNotch Identity Identity Identity
deltaKsNotch =
  (_ % pieceTime)
    >>> (_ - timing.ksNotch.begin)
    >>> (max 0.0)
    >>> \time ->
        speaker
          ( Identity
              $ gain (if time > (timing.ksNotch.dur - 1.0) then 0.0 else 1.0)
                  ( Identity
                      $ notch { freq: calcSlope 0.0 300.0 timing.ksNotch.dur 2000.0 time }
                          (Identity $ playBuf (Proxy :: _ "my-buffer"))
                  )
          )
