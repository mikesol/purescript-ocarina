module WAGS.Example.KitchenSink.Types.Feedback where

import Prelude
import Data.Identity (Identity(..))
import Data.Tuple.Nested (type (/\), (/\))
import Math ((%))
import Type.Proxy (Proxy(..))
import WAGS.Control.Types (Universe')
import WAGS.Example.KitchenSink.Timing (pieceTime, timing)
import WAGS.Example.KitchenSink.Types.Empty (BaseGraph, EI0, EI1, EI2, EI3, EI4)
import WAGS.Graph.Constructors (Delay, Gain, PlayBuf, Speaker)
import WAGS.Graph.Decorators (Focus(..), Decorating')
import WAGS.Graph.Optionals (GetSetAP, Mix, delay, gain, mix, playBuf, speaker)
import WAGS.Universe.AudioUnit (TDelay, TGain, TPlayBuf)
import WAGS.Universe.Bin (PtrListCons, PtrListNil)
import WAGS.Universe.EdgeProfile (ManyEdges, NoEdge, SingleEdge)
import WAGS.Universe.Graph (GraphC)
import WAGS.Universe.Node (NodeC, NodeListCons)

type FeedbackGraph
  = GraphC
      (NodeC (TGain EI0) (ManyEdges EI1 (PtrListCons EI3 PtrListNil)))
      ( NodeListCons
          (NodeC (TPlayBuf EI3 "my-buffer") NoEdge)
          ( NodeListCons
              (NodeC (TDelay EI1) (SingleEdge EI2))
              ( NodeListCons
                  (NodeC (TGain EI2) (SingleEdge EI0))
                  (BaseGraph EI0)
              )
          )
      )

type FeedbackUniverse cb
  = Universe' EI4 FeedbackGraph cb

data MyMix

type KsFeedbackCreate t b (mx :: Type -> Type) atten
  = mx
      ( Mix
          ( Proxy MyMix ->
            ( (t (Delay GetSetAP (atten (Gain GetSetAP (Proxy MyMix)))))
                /\ (b (PlayBuf "my-buffer" GetSetAP))
                /\ Unit
            )
          )
      )

type KsFeedback g t b mx atten
  = Speaker (g (Gain GetSetAP (KsFeedbackCreate t b mx atten)))

ksFeedbackCreate ::
  forall t b mx atten.
  Decorating' t ->
  Decorating' b ->
  Decorating' mx ->
  Decorating' atten ->
  KsFeedbackCreate t b mx atten
ksFeedbackCreate ft fb fmx fatten =
  fmx
    $ mix
        ( \(myMix :: Proxy MyMix) ->
            (ft (delay 0.3 (fatten (gain 0.2 myMix))))
              /\ (fb $ playBuf (Proxy :: _ "my-buffer"))
              /\ unit
        )

ksFeedback' ::
  forall g t b mx atten.
  Decorating' g ->
  Decorating' t ->
  Decorating' b ->
  Decorating' mx ->
  Decorating' atten ->
  KsFeedback g t b mx atten
ksFeedback' fg ft fb fmx fatten = speaker (fg $ gain 1.0 (ksFeedbackCreate ft fb fmx fatten))

ksFeedback :: KsFeedback Identity Identity Identity Identity Identity
ksFeedback = ksFeedback' Identity Identity Identity Identity Identity

ksFeedbackPlaybuf :: KsFeedback Identity Identity Focus Identity Identity
ksFeedbackPlaybuf = ksFeedback' Identity Identity Focus Identity Identity

ksFeedbackDelay :: KsFeedback Identity Focus Identity Identity Identity
ksFeedbackDelay = ksFeedback' Identity Focus Identity Identity Identity

ksFeedbackGain :: KsFeedback Focus Identity Identity Identity Identity
ksFeedbackGain = ksFeedback' Focus Identity Identity Identity Identity

ksFeedbackMix :: KsFeedback Identity Identity Identity Focus Identity
ksFeedbackMix = ksFeedback' Identity Identity Identity Focus Identity

ksFeedbackAttenuation :: KsFeedback Identity Identity Identity Identity Focus
ksFeedbackAttenuation = ksFeedback' Identity Identity Identity Identity Focus

deltaKsFeedback :: Number -> KsFeedback Identity Identity Identity Identity Identity
deltaKsFeedback =
  (_ % pieceTime)
    >>> (_ - timing.ksFeedback.begin)
    >>> (max 0.0)
    >>> \time ->
        speaker
          ( Identity
              $ gain (if time > (timing.ksFeedback.dur - 1.0) then 0.0 else 1.0)
                  (ksFeedbackCreate Identity Identity Identity Identity)
          )
