module WAGS.Example.KitchenSink.Types.Constant where

import Prelude

import Data.Identity (Identity(..))
import WAGS.Control.Types (Universe')
import WAGS.Example.KitchenSink.Types.Empty (BaseGraph, EI0, EI1, TopLevel)
import WAGS.Graph.Constructors (Gain, Speaker, Constant)
import WAGS.Graph.Decorators (Focus(..), Decorating')
import WAGS.Graph.Optionals (GetSetAP, constant, gain, speaker)
import WAGS.Universe.AudioUnit (TConstant)
import WAGS.Universe.EdgeProfile (NoEdge)
import WAGS.Universe.Graph (GraphC)
import WAGS.Universe.Node (NodeC)

type ConstantGraph
  = GraphC
      (NodeC (TConstant EI0) NoEdge)
      (BaseGraph EI0)

type ConstantUniverse cb
  = Universe' EI1 ConstantGraph cb

type KsConstant g t
  = TopLevel g (t (Constant GetSetAP))

ksConstant' ::
  forall g t.
  Decorating' g ->
  Decorating' t ->
  KsConstant g t
ksConstant' fg ft = speaker (fg $ gain 0.0 (ft $ constant 0.0))

ksConstant :: KsConstant Identity Identity
ksConstant = ksConstant' Identity Identity

ksConstantConstant :: KsConstant Identity Focus
ksConstantConstant = ksConstant' Identity Focus

ksConstantGain :: KsConstant Focus Identity
ksConstantGain = ksConstant' Focus Identity

deltaKsConstant :: Number -> Speaker (Gain GetSetAP (Constant GetSetAP))
deltaKsConstant = const $ speaker $ gain 1.0 (constant 0.0)
