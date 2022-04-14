module WAGS.Example.Docs.Util where

import Prelude

import Control.Alt ((<|>))
import Data.Exists (mkExists)
import Data.Tuple.Nested ((/\))
import Deku.Attribute (cb, (:=))
import Deku.Control (text)
import Deku.Core (Element, SubgraphF(..))
import Deku.DOM as D
import Deku.Subgraph ((@@))
import Deku.Subgraph as Sg
import Effect (Effect)
import Effect.Aff (Aff, error, joinFiber, killFiber, launchAff, launchAff_)
import Effect.Class (liftEffect)
import FRP.Event.Class (class IsEvent, bang)
import WAGS.Example.Docs.Types (CancelCurrentAudio)

foreign import scrollToTop_ :: Effect Unit

scrollToTop :: Effect Unit
scrollToTop = scrollToTop_

data WrapperStates = Loading | Playing (Effect Unit) | Stopped

audioWrapper
  :: forall a event payload
   . IsEvent event
  => CancelCurrentAudio
  -> Aff a
  -> (a -> Effect (Effect Unit))
  -> Element event payload
audioWrapper cca init i = bang (unit /\ Sg.Insert)
  @@ \_ -> mkExists $ SubgraphF \push event' ->
    let
      event = bang Stopped <|> event'
    in
      D.button
        ( map
            ( \e -> D.OnClick :=
                ( cb $
                    ( const $ case e of
                        Loading -> pure unit
                        Playing x -> x *> cca (pure unit) *> push Stopped
                        Stopped -> do
                          push Loading
                          fib <- launchAff do
                            x <- init
                            liftEffect do
                              res <- i x
                              push (Playing res)
                              pure res
                          cca do
                            toKill <- launchAff do
                              joinFiber fib >>= liftEffect
                            launchAff_ $ killFiber
                              (error "We navigated away from the page")
                              toKill
                          pure unit
                    )
                )
            )
            event
        )
        [ text
            ( map
                ( case _ of
                    Stopped -> "Turn on"
                    Loading -> "Loading..."
                    Playing _ -> "Turn off"
                )
                event
            )
        ]
