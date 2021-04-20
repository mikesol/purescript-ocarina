module WAGS.Example.KitchenSink.Types.Highpass where

import Prelude

import Data.Identity (Identity(..))
import Math ((%))
import Type.Proxy (Proxy(..))
import WAGS.Control.Types (Universe')
import WAGS.Example.KitchenSink.Timing (calcSlope, ksHighpassIntegral, ksHighpassTime, pieceTime)
import WAGS.Example.KitchenSink.Types.Empty (EI0, EI1, EI2, BaseGraph)
import WAGS.Graph.Constructors (Highpass, Gain, PlayBuf, Speaker)
import WAGS.Graph.Decorators (Focus(..), Decorating')
import WAGS.Graph.Optionals (GetSetAP, gain, highpass, playBuf, speaker)
import WAGS.Universe.AudioUnit (THighpass, TPlayBuf)
import WAGS.Universe.EdgeProfile (NoEdge, SingleEdge)
import WAGS.Universe.Graph (GraphC)
import WAGS.Universe.Node (NodeC, NodeListCons)

ksHighpassBegins = ksHighpassIntegral - ksHighpassTime :: Number

type HighpassGraph
  = GraphC
      (NodeC (THighpass EI0) (SingleEdge EI1))
      ( NodeListCons
          (NodeC (TPlayBuf EI1 "my-buffer") NoEdge)
          (BaseGraph EI0)
      )

type HighpassUniverse cb
  = Universe' EI2 HighpassGraph cb

type KsHighpassreate (t :: Type -> Type) b
  = t (Highpass GetSetAP GetSetAP (b (PlayBuf "my-buffer" GetSetAP)))

type KsHighpass g t b
  = Speaker (g (Gain GetSetAP (KsHighpassreate t b)))

ksHighpassCreate ::
  forall t b.
  Decorating' t ->
  Decorating' b ->
  KsHighpassreate t b
ksHighpassCreate ft fb = ft $ highpass { freq: 300.0 } (fb $ playBuf (Proxy :: _ "my-buffer"))

ksHighpass' ::
  forall g t b.
  Decorating' g ->
  Decorating' t ->
  Decorating' b ->
  KsHighpass g t b
ksHighpass' fg ft fb =
  speaker
    (fg $ gain 1.0 (ksHighpassCreate ft fb))

ksHighpass :: KsHighpass Identity Identity Identity
ksHighpass = ksHighpass' Identity Identity Identity

ksHighpassPlaybuf :: KsHighpass Identity Identity Focus
ksHighpassPlaybuf = ksHighpass' Identity Identity Focus

ksHighpassHighpass :: KsHighpass Identity Focus Identity
ksHighpassHighpass = ksHighpass' Identity Focus Identity

ksHighpassGain :: KsHighpass Focus Identity Identity
ksHighpassGain = ksHighpass' Focus Identity Identity

deltaKsHighpass :: Number -> KsHighpass Identity Identity Identity
deltaKsHighpass =
  (_ % pieceTime)
    >>> (_ - ksHighpassBegins)
    >>> (max 0.0)
    >>> \time ->
        speaker
          ( Identity
              $ gain (if time > 9.0 then 0.0 else 1.0)
                  (Identity $ highpass { freq: calcSlope 0.0 300.0 ksHighpassTime 200.0 time } (Identity $ playBuf (Proxy :: _ "my-buffer")))
          )
