module WAGS.Imperative.Create.Constant where

import Prelude

import Control.Alternative ((<|>))
import Data.Newtype (unwrap)
import Data.Variant (match)
import Data.Variant.Maybe (nothing)
import FRP.Event (Event, bang, keepLatest)
import Prim.Boolean (False)
import Type.Prelude (class IsSymbol, Proxy(..), reflectSymbol)
import WAGS.Common.Parameters.Constant as Parameters
import WAGS.Core as Core
import WAGS.Declarative.Create.Gain (tmpResolveAU)
import WAGS.Imperative.Monad (class CreateNode, GraphBuilder(..))
import WAGS.Imperative.Types as T

constant
  :: forall l p i o id initialConstant
   . IsSymbol id
  => Parameters.InitialConstant initialConstant
  => CreateNode i id False o
  => Proxy id
  -> initialConstant
  -> Event (Core.Constant l p)
  -> GraphBuilder p i o (T.GraphUnit id T.Constant)
constant _ initialConstant attributes = GraphBuilder go
  where
  { offset } = unwrap $ Parameters.toInitializeConstant initialConstant
  go i@(Core.AudioInterpret { makeConstant, setOffset, setOnOff }) =
    { event:
        let
          id = reflectSymbol (Proxy :: _ id)
          event0 = bang $ makeConstant
            { id
            , parent: nothing
            , scope: "imperative"
            , offset
            }
          eventN = keepLatest $ attributes <#> unwrap >>> match
            { offset: tmpResolveAU "imperative" i $ setOffset <<< { id, offset: _ }
            , onOff: bang <<< setOnOff <<< { id, onOff: _ }
            }
        in
          event0 <|> eventN
    , result: T.GraphUnit
    }
