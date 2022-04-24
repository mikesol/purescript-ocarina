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

import Effect (Effect)
import Effect.Aff (Aff, Fiber, bracket, error, joinFiber, killFiber, launchAff, launchAff_, parallel, sequential)
import Effect.Class (liftEffect)
import FRP.Event (Event)
import FRP.Event.Class (class IsEvent, bang, biSampleOn)
import WAGS.Example.Docs.Types (CancelCurrentAudio, SingleSubgraphEvent(..), SingleSubgraphPusher)
import WAGS.Interpret (close, constant0Hack, context)
import WAGS.WebAPI (AudioContext)

foreign import scrollToTop_ :: Effect Unit

scrollToTop :: Effect Unit
scrollToTop = scrollToTop_

data WrapperStates = Loading | Playing (Effect Unit) | Stopped

ccassp :: CancelCurrentAudio -> SingleSubgraphPusher -> CancelCurrentAudio
ccassp cca ssp e = do
  cca e
  ssp (SetCancel e)

raceSelf :: Fiber (Effect Unit) -> Aff Unit
raceSelf fib = sequential
  ( parallel (joinFiber fib >>= liftEffect)
      <|> parallel
        ( killFiber
            (error "We navigated away from the page")
            fib
        )
  )

clickCb cca push init i ev event = map
  ( \(e /\ cncl) -> D.OnClick :=
      ( cb $
          ( const $ case e of
              Loading -> pure unit
              Playing x -> x *> cca (pure unit) *> push Stopped
              Stopped -> do
                cncl
                push Loading
                fib <- launchAff do
                  ctx <- context
                  c0h <- constant0Hack ctx
                  x <- init ctx
                  liftEffect do
                    res' <- i ctx x
                    let res = res' *> c0h *> close ctx
                    push (Playing res)
                    pure res
                cca do
                  push Stopped
                  launchAff_ $ raceSelf fib
                pure unit
          )
      )
  )
  (biSampleOn (bang (pure unit) <|> (map (\(SetCancel x) -> x) ev)) (map Tuple event))

mkWrapperEvent ev event' = bang Stopped <|> event'

audioWrapper
  :: forall a payload
   . Event SingleSubgraphEvent
  -> CancelCurrentAudio
  -> (AudioContext -> Aff a)
  -> (AudioContext -> a -> Effect (Effect Unit))
  -> Element Event payload
audioWrapper ev cca init i = bang (unit /\ Sg.Insert)
  @@ \_ -> mkExists $ SubgraphF \push event' ->
    let
      event = mkWrapperEvent ev event'
    in
      D.button
        (clickCb cca push init i ev event)
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

audioWrapperSpan
  :: forall a payload
   . String
  -> Event SingleSubgraphEvent
  -> CancelCurrentAudio
  -> (AudioContext -> Aff a)
  -> (AudioContext -> a -> Effect (Effect Unit))
  -> Element Event payload
audioWrapperSpan txt ev cca init i = bang (unit /\ Sg.Insert)
  @@ \_ -> mkExists $ SubgraphF \push event' ->
    let
      event = mkWrapperEvent ev event'
    in
      D.span
        ( (bang (D.Style := "cursor: pointer;")) <|> (clickCb cca push init i ev event)
        )
        [ text
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
