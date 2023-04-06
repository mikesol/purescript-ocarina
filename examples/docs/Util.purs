module Ocarina.Example.Docs.Util where

import Prelude

import Control.Alt (class Alt, (<|>))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Deku.Attribute (Attribute, cb, (:=))
import Deku.Control (text)
import Deku.Core (bussed)
import Deku.Core as DC
import Deku.DOM as D
import Effect (Effect)
import Effect.Aff (Aff, Fiber, error, joinFiber, killFiber, launchAff, launchAff_, parallel, sequential)
import Effect.Class (liftEffect)
import FRP.Event (Event)
import Ocarina.Example.Docs.Types (CancelCurrentAudio, SingleSubgraphEvent(..), SingleSubgraphPusher)
import Ocarina.Interpret (close, constant0Hack, context)
import Ocarina.WebAPI (AudioContext)

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

foreign import hackishRandInt :: Effect Int

clickCb
  :: forall i element
   . (Effect Unit -> Effect Unit)
  -> (WrapperStates -> Effect Unit)
  -> (AudioContext -> Aff i)
  -> (AudioContext -> i -> Effect (Effect Unit))
  -> Event SingleSubgraphEvent
  -> Event WrapperStates
  -> Event (Attribute element)
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
  (Tuple <$> event <*> (pure (pure unit) <|> (map (\(SetCancel x) -> x) ev)) )

-- todo, why does this have another argument? what's even going on...?
mkWrapperEvent :: forall t20 f22. Alt f22 => Applicative f22 => t20 -> f22 WrapperStates -> f22 WrapperStates
mkWrapperEvent _ event' = pure Stopped <|> event'

audioWrapper
  :: forall a payload
   . Event SingleSubgraphEvent
  -> CancelCurrentAudio
  -> (AudioContext -> Aff a)
  -> (AudioContext -> a -> Effect (Effect Unit))
  -> DC.Nut
audioWrapper ev cca init i = bussed \push event' ->
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
  -> DC.Nut
audioWrapperSpan txt ev cca init i = bussed \push event' ->
  let
    event = mkWrapperEvent ev event'
  in
    D.span
      ( (pure (D.Style := "cursor: pointer;")) <|> (clickCb cca push init i ev event)
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

mkNext :: forall f29 e35. Alt f29 => Applicative f29 => f29 SingleSubgraphEvent -> Effect Unit -> f29 (Attribute e35)
mkNext ev cpage = pure (D.OnClick := cb (const cpage))
  <|> map (\cncl -> D.OnClick := cb (const (cncl *> cpage))) (map (\(SetCancel c) -> c) ev)
