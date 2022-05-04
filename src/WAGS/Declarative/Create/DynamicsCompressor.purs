module WAGS.Declarative.Create.DynamicsCompressor where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Variant (match)
import Data.Variant.Maybe (just)
import FRP.Event (Event, bang, keepLatest, makeEvent, subscribe)
import WAGS.Common.Parameters.DynamicsCompressor as Parameters
import WAGS.Core as Core
import WAGS.Declarative.Create.Gain (tmpResolveAU)

dynamicsCompressor
  :: forall i aud (outputChannels :: Type) lock payload
   . Parameters.InitialDynamicsCompressor i
  => Core.Mix aud (Core.Audible outputChannels lock payload)
  => i
  -> Event (Core.DynamicsCompressor lock payload)
  -> aud
  -> Core.Node outputChannels lock payload
dynamicsCompressor i' atts elts = Core.Node go
  where
  Core.InitializeDynamicsCompressor i = Parameters.toInitializeDynamicsCompressor i'
  go
    parent
    di@
      ( Core.AudioInterpret
          { ids
          , deleteFromCache
          , makeDynamicsCompressor
          , setThreshold
          , setRatio
          , setKnee
          , setAttack
          , setRelease
          }
      ) =
    makeEvent \k -> do
      me <- ids
      parent.raiseId me
      map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
        bang
          ( makeDynamicsCompressor
              { id: me
              , parent: parent.parent
              , scope: parent.scope
              , threshold: i.threshold
              , ratio: i.ratio
              , knee: i.knee
              , attack: i.attack
              , release: i.release
              }
          )
          <|>
            ( keepLatest $ map
                ( \(Core.DynamicsCompressor e) -> match
                    { threshold: tmpResolveAU parent.scope di
                        ( setThreshold <<<
                            { id: me, threshold: _ }
                        )
                    , ratio: tmpResolveAU parent.scope di
                        ( setRatio <<<
                            { id: me, ratio: _ }
                        )
                    , knee: tmpResolveAU parent.scope di
                        ( setKnee <<<
                            { id: me, knee: _ }
                        )
                    , attack: tmpResolveAU parent.scope di
                        ( setAttack <<<
                            { id: me, attack: _ }
                        )
                    , release: tmpResolveAU parent.scope di
                        ( setRelease <<<
                            { id: me, release: _ }
                        )
                    }
                    e
                )
                atts
            )
          <|> Core.__internalWagsFlatten (just me) parent.scope di (Core.mix elts)

dynamicsCompressor_
  :: forall i aud (outputChannels :: Type) lock payload
   . Parameters.InitialDynamicsCompressor i
  => Core.Mix aud (Core.Audible outputChannels lock payload)
  => i
  -> aud
  -> Core.Node outputChannels lock payload
dynamicsCompressor_ i = dynamicsCompressor i empty
