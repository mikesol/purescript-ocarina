module WAGS.Imperative.Create.Convolver where

import Prelude

import Data.Newtype (unwrap)
import Data.Variant.Maybe (nothing)
import FRP.Event (bang)
import Prim.Boolean (False)
import Type.Prelude (class IsSymbol, Proxy(..), reflectSymbol)
import WAGS.Common.Parameters.Convolver as Parameters
import WAGS.Core as Core
import WAGS.Imperative.Monad (class CreateNode, GraphBuilder(..))
import WAGS.Imperative.Types as T

convolver
  :: forall p i o id initialConvolver
   . IsSymbol id
  => Parameters.InitialConvolver initialConvolver
  => CreateNode i id False o
  => Proxy id
  -> initialConvolver
  -> GraphBuilder p i o (T.GraphUnit id T.Convolver)
convolver _ initialConvolver = GraphBuilder go
  where
  initializeConvolver = unwrap $ Parameters.toInitializeConvolver initialConvolver
  go (Core.AudioInterpret { makeConvolver }) =
    { event:
        let
          id = reflectSymbol (Proxy :: _ id)
          event0 = bang $ makeConvolver
            { id
            , parent: nothing
            , scope: "imperative"
            , buffer: initializeConvolver.buffer
            }
        in
          event0
    , result: T.GraphUnit
    }
