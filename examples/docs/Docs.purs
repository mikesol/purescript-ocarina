module Ocarina.Example.Docs where

import Prelude

import Bolson.Core (envy)
import Control.Alt ((<|>))
import Data.Filterable (filter)
import Data.Foldable (for_, oneOfMap)
import Data.Function (on)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple.Nested ((/\))
import Deku.Attribute (cb, (:=))
import Deku.Control (dekuA, switcher, switcher_, text_)
import Deku.Core (Nut, Domable, bussed)
import Deku.DOM as D
import Deku.Interpret (fullDOMInterpret, makeFFIDOMSnapshot)
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Ref as Ref
import FRP.Event (Event, bus, create, fold, subscribe)
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

scene :: Nut
scene = bussed \push event' -> do
  let event = fold
            ( case _ of
                ChangePage p -> \{ curPage, cancel } -> { prevPage: Just curPage, curPage: p, cancel, pageChange: true }
                SetCancelation cancel -> _ { cancel = cancel, pageChange = false }
            )
            event'
            { prevPage: Nothing, curPage: Intro, cancel: pure unit, pageChange: true }

  D.div_
    [ D.div_
        $ map
            ( \(x /\ y /\ z) -> D.span_
                [ D.a
                    ( oneOfMap pure
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
                    ( pure $ D.Style :=
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
    , switcher_ D.div
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
  page :: TopLevelSg -> Nut
  page (TopLevelSg { page: pg, setCancellation, setPage }) = go pg
    where
    go Intro = D.div_ $ pure $ bussed (Intro.intro setCancellation setPage)
    go HelloWorld = D.div_ $ pure $ bussed (HelloWorld.helloWorld setCancellation setPage)
    go FixFan = D.div_ $ pure $ bussed (FixFan.fixFan setCancellation setPage)
    go AudioUnits = D.div_ $ pure $ bussed (Component.components setCancellation setPage)
    go AudioWorklets = D.div_ $ pure $ bussed (Pursx1.pursx1 setCancellation setPage)
    go Events = D.div_ $ pure $ bussed (Events.events setCancellation setPage)
    go Params = D.div_ $ pure $ bussed (Params.params setCancellation setPage)
    go State = D.div_ $ pure $ bussed (Effects.effects setCancellation setPage)
    go Imperative = D.div_ $ pure $ bussed (Pursx2.pursx2 setCancellation setPage)
    go MultiChannel = D.div_ $ pure $ bussed (Multichannel.multiChannel setCancellation setPage)
    go Subgraph = D.div_ $ pure $ bussed (Subgraph.subgraphs setCancellation setPage)
    go Tumult = D.div_ $ pure $ bussed (Portals.portals setCancellation setPage)

main :: Effect Unit
main = runInBody scene