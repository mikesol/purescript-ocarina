module WAGS.Example.Docs where

import Prelude

import Control.Alt ((<|>))
import Data.Filterable (filter)
import Data.Foldable (for_, oneOfMap)
import Data.Function (on)
import Data.Hashable (class Hashable, hash)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple.Nested ((/\))
import Deku.Attribute (cb, (:=))
import Deku.Control (dekuA, switcher, text_)
import Deku.Core (Element)
import Deku.DOM as D
import Deku.Interpret (effectfulDOMInterpret, makeFFIDOMSnapshot)
import Effect (Effect)
import FRP.Event (Event, bus, create, fold, subscribe)
import FRP.Event.Class (bang)
import WAGS.Example.Docs.Component as Component
import WAGS.Example.Docs.Effects as Effects
import WAGS.Example.Docs.Events as Events
import WAGS.Example.Docs.FixFan as FixFan
import WAGS.Example.Docs.HelloWorld as HelloWorld
import WAGS.Example.Docs.Intro as Intro
import WAGS.Example.Docs.MultiChannel as Multichannel
import WAGS.Example.Docs.Portals as Portals
import WAGS.Example.Docs.Pursx1 as Pursx1
import WAGS.Example.Docs.Pursx2 as Pursx2
import WAGS.Example.Docs.Subgraphs as Subgraph
import WAGS.Example.Docs.Types (Page(..), ToplevelEvent(..))
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
instance Eq TopLevelSg where
  eq = eq `on` (unwrap >>> _.page)

instance Hashable TopLevelSg where
  hash = unwrap >>> _.page >>> hash

p2tl :: Page -> TopLevelSg
p2tl page = TopLevelSg { page, setPage: mempty, setCancellation: mempty }

scene
  :: forall lock payload
   . (ToplevelEvent -> Effect Unit)
  -> Event ToplevelEvent
  -> Array (Element lock payload)
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
        --   , MultiChannel
        --       /\ "Merging and splitting"
        --       /\ true
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
  , D.div_ $ switcher
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

  page :: TopLevelSg -> Element lock payload
  page (TopLevelSg { page: pg, setCancellation, setPage }) = go pg
    where
    go Intro = D.div_ $ bus (Intro.intro setCancellation setPage)
    go HelloWorld = D.div_ $ bus (HelloWorld.helloWorld setCancellation setPage)
    go FixFan = D.div_ $ bus (FixFan.fixFan setCancellation setPage)
    go AudioUnits = D.div_ $ bus (Component.components setCancellation setPage)
    go AudioWorklets = D.div_ $ bus (Pursx1.pursx1 setCancellation setPage)
    go Events = D.div_ $ bus (Events.events setCancellation setPage)
    go State = D.div_ $ bus (Effects.effects setCancellation setPage)
    go Imperative = D.div_ $ bus (Pursx2.pursx2 setCancellation setPage)
    go MultiChannel = D.div_ $ bus (Multichannel.multiChannel setCancellation setPage)
    go Subgraph = D.div_ $ bus (Subgraph.subgraphs setCancellation setPage)
    go Tumult = D.div_ $ bus (Portals.portals setCancellation setPage)

main :: Effect Unit
main = do
  b' <- window >>= document >>= body
  for_ (toElement <$> b') \b -> do
    ffi <- makeFFIDOMSnapshot
    { push, event } <- create
    let evt = dekuA b (scene push event) effectfulDOMInterpret
    void $ subscribe evt \i -> i ffi
    push (ChangePage Intro)