module WAGS.Declarative.Create.Convolver where

import Prelude

import Control.Alt ((<|>))
import Data.Variant.Maybe (just)
import FRP.Event (bang, makeEvent, subscribe)
import WAGS.Common.Parameters.Convolver as Parameters
import WAGS.Core as Core

convolver
  :: forall i aud (outputChannels :: Type) lock payload
   . Parameters.InitialConvolver i
  => Core.Mix aud (Core.Audible outputChannels lock payload)
  => i
  -> aud
  -> Core.Node outputChannels lock payload
convolver i' elts = Core.Node go
  where
  Core.InitializeConvolver i = Parameters.toInitializeConvolver i'
  go parent di@(Core.AudioInterpret { ids, deleteFromCache, makeConvolver }) =
    makeEvent \k -> do
      me <- ids
      parent.raiseId me
      map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
        bang
          ( makeConvolver
              { id: me
              , parent: parent.parent
              , scope: parent.scope
              , buffer: i.buffer
              }
          )
          <|> Core.__internalWagsFlatten (just me) parent.scope di (Core.mix elts)
