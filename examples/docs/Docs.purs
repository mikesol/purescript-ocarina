module Ocarina.Example.Docs where

import Prelude

import Control.Alt ((<|>))
import Data.Filterable (filter)
import Data.Foldable (for_, oneOfMap)
import Data.Function (on)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple.Nested ((/\))
import Deku.Attribute (cb, (:=))
import Deku.Control (dekuA, switcher, text_)
import Deku.Core (Domable, envy)
import Deku.DOM as D
import Deku.Interpret (fullDOMInterpret, makeFFIDOMSnapshot)
import Effect (Effect)
import Effect.Ref as Ref
import FRP.Event (Event, bus, create, fold, subscribe)
import FRP.Event.Class (bang)
import Ocarina.Example.Docs.Component as Component
import Ocarina.Example.Docs.Effects as Effects
import Ocarina.Example.Docs.Events as Events
import Ocarina.Example.Docs.FixFan as FixFan
import Ocarina.Example.Docs.HelloWorld as HelloWorld
import Ocarina.Example.Docs.Intro as Intro
import Ocarina.Example.Docs.MultiChannel as Multichannel
import Ocarina.Example.Docs.Params as Params
import Ocarina.Example.Docs.Portals as Portals
import Ocarina.Example.Docs.Pursx1 as Pursx1
import Ocarina.Example.Docs.Pursx2 as Pursx2
import Ocarina.Example.Docs.Subgraphs as Subgraph
import Ocarina.Example.Docs.Types (Page(..), ToplevelEvent(..))
import Web.HTML (window)
import Web.HTML.HTMLDocument (body)
import Web.HTML.HTMLElement (toElement)
import Web.HTML.Window (document)

newtype TopLevelSg = TopLevelSg
  { page :: Page
  , setPage :: Page -> Effect Unit
  , setCancellation :: Effect Unit -> Effect Unit
  }

derive instance Newtype TopLevelSg _

p2tl :: Page -> TopLevelSg
p2tl page = TopLevelSg { page, setPage: mempty, setCancellation: mempty }

scene
  :: forall lock payload
   . (ToplevelEvent -> Effect Unit)
  -> Event ToplevelEvent
  -> Array (Domable Effect lock payload)
scene push event' =
  [ D.div_
      $ map
        ( \(x /\ y /\ z) -> D.span_
            [ D.a
                ( oneOfMap bang
                    [ D.OnClick := cb
                        ( const do
                            push (ChangePage x)
                        )
                    , D.Style := "cursor:pointer;"
                    ] <|> map
                    ( \{ cancel } -> D.OnClick := cb
                        ( const do
                            cancel
                            push (ChangePage x)
                        )
                    )
                    (event # filter (_.pageChange >>> not))
                )
                [ text_ y ]
            , D.span
                ( bang $ D.Style :=
                    if z then ""
                    else "display:none;"
                )
                [ text_ " | " ]
            ]
        )
      $
        [ Intro
            /\ "Home"
            /\ true
        , HelloWorld
            /\ "Hello world"
            /\ true
        , FixFan
            /\ "Array, fan, and fix"
            /\ true
        , AudioUnits
            /\ "Audio units"
            /\ true
        , Events
            /\ "Events"
            /\ true
          , Params
              /\ "Parameters"
              /\ true
        --   , AudioWorklets
        --       /\ "Audio worklets"
        --       /\ true
        , State
            /\ "State"
            /\ true
        , Subgraph
            /\ "Subgraphs"
            /\ false
        --   , Tumult
        --       /\ "Tumult"
        --       /\ true
        --   , Imperative
        --       /\ "Imperative API"
        --       /\ false
        ]
  , D.div_ $ pure $ switcher
      ( \{ curPage } ->
          page
            ( TopLevelSg
                { page: curPage
                , setPage: ChangePage >>> push
                , setCancellation: SetCancelation >>> push
                }
            )
      )
      (event # filter _.pageChange)

  ]
  where
  event = fold
    ( case _ of
        ChangePage p -> \{ curPage, cancel } -> { prevPage: Just curPage, curPage: p, cancel, pageChange: true }
        SetCancelation cancel -> _ { cancel = cancel, pageChange = false }
    )
    event'
    { prevPage: Nothing, curPage: Intro, cancel: pure unit, pageChange: true }

  page :: TopLevelSg -> Domable Effect lock payload
  page (TopLevelSg { page: pg, setCancellation, setPage }) = go pg
    where
    go Intro = D.div_ $ pure $ envy $ bus (Intro.intro setCancellation setPage)
    go HelloWorld = D.div_ $ pure $ envy $ bus (HelloWorld.helloWorld setCancellation setPage)
    go FixFan = D.div_ $ pure $ envy $ bus (FixFan.fixFan setCancellation setPage)
    go AudioUnits = D.div_ $ pure $ envy $ bus (Component.components setCancellation setPage)
    go AudioWorklets = D.div_ $ pure $ envy $ bus (Pursx1.pursx1 setCancellation setPage)
    go Events = D.div_ $ pure $ envy $ bus (Events.events setCancellation setPage)
    go Params = D.div_ $ pure $ envy $ bus (Params.params setCancellation setPage)
    go State = D.div_ $ pure $ envy $ bus (Effects.effects setCancellation setPage)
    go Imperative = D.div_ $ pure $ envy $ bus (Pursx2.pursx2 setCancellation setPage)
    go MultiChannel = D.div_ $ pure $ envy $ bus (Multichannel.multiChannel setCancellation setPage)
    go Subgraph = D.div_ $ pure $ envy $ bus (Subgraph.subgraphs setCancellation setPage)
    go Tumult = D.div_ $ pure $ envy $ bus (Portals.portals setCancellation setPage)

main :: Effect Unit
main = do
  b' <- window >>= document >>= body
  for_ (toElement <$> b') \b -> do
    ffi <- makeFFIDOMSnapshot
    r <- Ref.new 0
    { push, event } <- create
    let evt = dekuA b (scene push event) (fullDOMInterpret r)
    void $ subscribe evt \i -> i ffi
    push (ChangePage Intro)