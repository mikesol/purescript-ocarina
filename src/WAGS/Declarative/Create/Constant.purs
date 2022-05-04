module WAGS.Declarative.Create.Constant where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Variant (match)
import FRP.Event (Event, bang, keepLatest, makeEvent, subscribe)
import WAGS.Common.Parameters.Constant as Parameters
import WAGS.Core as Core
import WAGS.Declarative.Create.Gain (tmpResolveAU)

__constant
  :: forall i outputChannels lock payload
   . Parameters.InitialConstant i
  => i
  -> Event (Core.Constant lock payload)
  -> Core.Node outputChannels lock payload
__constant i' atts = Core.Node go
  where
  Core.InitializeConstant i = Parameters.toInitializeConstant i'
  go parent di@(Core.AudioInterpret { ids, deleteFromCache, makeConstant, setOffset, setOnOff }) =
    makeEvent \k -> do
      me <- ids
      parent.raiseId me
      map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
        bang
          ( makeConstant
              { id: me
              , parent: parent.parent
              , scope: parent.scope
              , offset: i.offset
              }
          )
          <|>
            ( keepLatest $ map
                ( \(Core.Constant e) -> match
                    { offset: tmpResolveAU parent.scope di (setOffset <<< { id: me, offset: _ })
                    , onOff: \onOff -> bang $ setOnOff { id: me, onOff }
                    }
                    e
                )
                atts
            )

constant
  :: forall i outputChannels lock payload
   . Parameters.InitialConstant i
  => i
  -> Event (Core.Constant lock payload)
  -> Core.Node outputChannels lock payload
constant = __constant

constant_
  :: forall i outputChannels lock payload
   . Parameters.InitialConstant i
  => i
  -> Core.Node outputChannels lock payload
constant_ i = constant i empty
