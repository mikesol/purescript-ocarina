module WAGS.Example.KitchenSink.Types.Highpass where

import Prelude
import Data.Identity (Identity(..))
import Type.Proxy (Proxy(..))
import Math ((%))
import WAGS.Control.Types (Universe')
import WAGS.Example.KitchenSink.Timing (calcSlope, phase5Integral, phase7Time, pieceTime)
import WAGS.Graph.Constructors (Highpass(..), Gain(..), OnOff(..), PlayBuf(..), Speaker(..))
import WAGS.Graph.Decorators (Focus(..), Decorating')
import WAGS.Universe.AudioUnit (THighpass, TGain, TPlayBuf, TSpeaker)
import WAGS.Universe.Bin (D0, D1, D9, D10, D11)
import WAGS.Universe.EdgeProfile (NoEdge, SingleEdge)
import WAGS.Universe.Graph (GraphC)
import WAGS.Universe.Node (NodeC, NodeListCons, NodeListNil)

type HighpassGraph
  = GraphC
      (NodeC (THighpass D9) (SingleEdge D10))
      ( NodeListCons
          (NodeC (TPlayBuf D10 "my-buffer") NoEdge)
          ( NodeListCons
              (NodeC (TSpeaker D0) (SingleEdge D1))
              (NodeListCons (NodeC (TGain D1) (SingleEdge D9)) NodeListNil)
          )
      )

type HighpassUniverse cb
  = Universe' D11 HighpassGraph cb

type Phase7reate (t :: Type -> Type) b
  = t (Highpass Number Number (b (PlayBuf "my-buffer" Number)))

type Phase7 g t b
  = Speaker (g (Gain Number (Phase7reate t b)))

phase7Create ::
  forall t b.
  Decorating' t ->
  Decorating' b ->
  Phase7reate t b
phase7Create ft fb = ft $ Highpass 300.0 1.0 (fb $ PlayBuf (Proxy :: _ "my-buffer") 0.0 On 1.0)

phase7' ::
  forall g t b.
  Decorating' g ->
  Decorating' t ->
  Decorating' b ->
  Phase7 g t b
phase7' fg ft fb =
  Speaker
    (fg $ Gain 1.0 (phase7Create ft fb))

phase7 :: Phase7 Identity Identity Identity
phase7 = phase7' Identity Identity Identity

phase7Playbuf :: Phase7 Identity Identity Focus
phase7Playbuf = phase7' Identity Identity Focus

phase7Highpass :: Phase7 Identity Focus Identity
phase7Highpass = phase7' Identity Focus Identity

phase7Gain :: Phase7 Focus Identity Identity
phase7Gain = phase7' Focus Identity Identity

deltaPhase7 :: Number -> Phase7 Identity Identity Identity
deltaPhase7 =
  (_ % pieceTime)
    >>> (_ - phase5Integral)
    >>> (max 0.0)
    >>> \time ->
        Speaker
          ( Identity
              $ Gain (if time > 9.0 then 0.0 else 1.0)
                  (Identity $ Highpass (calcSlope 0.0 300.0 phase7Time 200.0 time) 1.0 (Identity $ PlayBuf (Proxy :: _ "my-buffer") 0.0 On 1.0))
          )
