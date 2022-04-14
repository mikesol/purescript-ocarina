module WAGS.Example.Docs.Util where

import Prelude

import Control.Alt ((<|>))
import Data.Exists (mkExists)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Deku.Attribute (cb, (:=))
import Deku.Control (text, text_)
import Deku.Core (Element, SubgraphF(..))
import Deku.DOM as D
import Deku.Subgraph ((@@))
import Deku.Subgraph as Sg
import Effect (Effect)
import Effect.Aff (Aff, error, joinFiber, killFiber, launchAff, launchAff_)
import Effect.Class (liftEffect)
import FRP.Event.Class (class IsEvent, bang, sampleOn)
import WAGS.Example.Docs.Types (CancelCurrentAudio, SingleSubgraphEvent(..), SingleSubgraphPusher)
import WAGS.Interpret (context)

foreign import scrollToTop_ :: Effect Unit

scrollToTop :: Effect Unit
scrollToTop = scrollToTop_

data WrapperStates = Loading | Playing (Effect Unit) | Stopped

ccassp :: CancelCurrentAudio -> SingleSubgraphPusher -> CancelCurrentAudio
ccassp cca ssp e = do
  cca e
  ssp (SetCancel e)

audioWrapper
  :: forall a event payload
   . IsEvent event
  => event SingleSubgraphEvent
  -> CancelCurrentAudio
  -> Aff a
  -> (a -> Effect (Effect Unit))
  -> Element event payload
audioWrapper ev cca init i = bang (unit /\ Sg.Insert)
  @@ \_ -> mkExists $ SubgraphF \push event' ->
    let
      event = bang Stopped <|> event'
    in
      D.button
        ( map
            ( \(e /\ cncl) -> D.OnClick :=
                ( cb $
                    ( const $ case e of
                        Loading -> pure unit
                        Playing x -> x *> cca (pure unit) *> push Stopped
                        Stopped -> do
                          cncl
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
            (sampleOn (bang (pure unit) <|> (map (\(SetCancel x) -> x) ev)) (map Tuple event))
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

-- todo: merge with above?
audioWrapperSpan
  :: forall a event payload
   . IsEvent event
  => String
  -> event SingleSubgraphEvent
  -> CancelCurrentAudio
  -> Aff a
  -> (a -> Effect (Effect Unit))
  -> Element event payload
audioWrapperSpan txt ev cca init i = bang (unit /\ Sg.Insert)
  @@ \_ -> mkExists $ SubgraphF \push event' ->
    let
      event = bang Stopped <|> event'
    in
      D.span
        ((bang (D.Style := "cursor: pointer;")) <|>  map
            ( \(e /\ cncl) -> D.OnClick :=
                ( cb $
                    ( const $ case e of
                        Loading -> pure unit
                        Playing x -> x *> cca (pure unit) *> push Stopped
                        Stopped -> do
                          cncl
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
            (sampleOn (bang (pure unit) <|> (map (\(SetCancel x) -> x) ev)) (map Tuple event))
        )
        [  text
            ( map
                ( case _ of
                    Stopped -> txt
                    Loading -> "⏳"
                    Playing _ -> "🛑"
                )
                event
            )
        ]

mkNext ev cpage = bang (D.OnClick := cb (const cpage))
              <|> map (\cncl -> D.OnClick := cb (const (cncl *> cpage))) (map (\(SetCancel c) -> c) ev)

ctxAff = liftEffect context