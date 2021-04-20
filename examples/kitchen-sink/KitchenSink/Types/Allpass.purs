module WAGS.Example.KitchenSink.Types.Allpass where

import Prelude
import Data.Identity (Identity(..))
import Type.Proxy (Proxy(..))
import Math ((%))
import WAGS.Control.Types (Universe')
import WAGS.Example.KitchenSink.Timing (calcSlope, phase5Integral, phase6Time, pieceTime)
import WAGS.Graph.Constructors (Allpass(..), Gain(..), OnOff(..), PlayBuf(..), Speaker(..))
import WAGS.Graph.Decorators (Focus(..), Decorating')
import WAGS.Universe.AudioUnit (TAllpass, TGain, TPlayBuf, TSpeaker)
import WAGS.Universe.Bin (D0, D1, D7, D8, D9)
import WAGS.Universe.EdgeProfile (NoEdge, SingleEdge)
import WAGS.Universe.Graph (GraphC)
import WAGS.Universe.Node (NodeC, NodeListCons, NodeListNil)

type AllpassGraph
  = GraphC
      (NodeC (TAllpass D7) (SingleEdge D8))
      ( NodeListCons
          (NodeC (TPlayBuf D8 "my-buffer") NoEdge)
          ( NodeListCons
              (NodeC (TSpeaker D0) (SingleEdge D1))
              (NodeListCons (NodeC (TGain D1) (SingleEdge D7)) NodeListNil)
          )
      )

type AllpassUniverse cb
  = Universe' D9 AllpassGraph cb

type Phase6reate (t :: Type -> Type) b
  = t (Allpass Number Number (b (PlayBuf "my-buffer" Number)))

type Phase6 g t b
  = Speaker (g (Gain Number (Phase6reate t b)))

phase6Create ::
  forall t b.
  Decorating' t ->
  Decorating' b ->
  Phase6reate t b
phase6Create ft fb = ft $ Allpass 300.0 1.0 (fb $ PlayBuf (Proxy :: _ "my-buffer") 0.0 On 1.0)

phase6' ::
  forall g t b.
  Decorating' g ->
  Decorating' t ->
  Decorating' b ->
  Phase6 g t b
phase6' fg ft fb =
  Speaker
    (fg $ Gain 1.0 (phase6Create ft fb))

phase6 :: Phase6 Identity Identity Identity
phase6 = phase6' Identity Identity Identity

phase6Playbuf :: Phase6 Identity Identity Focus
phase6Playbuf = phase6' Identity Identity Focus

phase6Allpass :: Phase6 Identity Focus Identity
phase6Allpass = phase6' Identity Focus Identity

phase6Gain :: Phase6 Focus Identity Identity
phase6Gain = phase6' Focus Identity Identity

deltaPhase6 :: Number -> Phase6 Identity Identity Identity
deltaPhase6 =
  (_ % pieceTime)
    >>> (_ - phase5Integral)
    >>> (max 0.0)
    >>> \time ->
        Speaker
          ( Identity
              $ Gain (if time > 9.0 then 0.0 else 1.0)
                  (Identity $ Allpass (calcSlope 0.0 300.0 phase6Time 200.0 time) 1.0 (Identity $ PlayBuf (Proxy :: _ "my-buffer") 0.0 On 1.0))
          )
