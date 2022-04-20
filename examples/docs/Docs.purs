module WAGS.Example.Docs where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (class Plus, empty)
import Data.Exists (mkExists)
import Data.Filterable (filter)
import Data.Foldable (for_, oneOfMap)
import Data.Function (on)
import Data.Hashable (class Hashable, hash)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple.Nested ((/\))
import Deku.Attribute (cb, (:=))
import Deku.Control (deku, flatten, text_)
import Deku.Core (Element, Subgraph, SubgraphF(..))
import Deku.DOM as D
import Deku.Interpret (effectfulDOMInterpret, makeFFIDOMSnapshot)
import Deku.Subgraph (SubgraphAction(..), subgraph)
import Effect (Effect)
import FRP.Event (Event, class IsEvent, create, fold, keepLatest, subscribe)
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
  :: forall payload
   . (ToplevelEvent -> Effect Unit)
  -> Event ToplevelEvent
  -> Element Event payload
scene push event' =
  flatten
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
    , subgraph
        ( (event # filter _.pageChange)
            # map
                ( \{ prevPage, curPage } ->
                    ( case prevPage of
                        Nothing -> empty
                        Just x -> bang (p2tl x /\ Remove)
                    ) <|> bang
                      ( TopLevelSg
                          { page: curPage
                          , setPage: ChangePage >>> push
                          , setCancellation: SetCancelation >>> push
                          } /\ Insert
                      )
                )
            # keepLatest
        )
        page
    ]
  where
  event = fold
    ( case _ of
        ChangePage p -> \{ curPage, cancel } -> { prevPage: Just curPage, curPage: p, cancel, pageChange: true }
        SetCancelation cancel -> _ { cancel = cancel, pageChange = false }
    )
    event'
    { prevPage: Nothing, curPage: Intro, cancel: pure unit, pageChange: true }

  page :: Subgraph TopLevelSg Event payload
  page (TopLevelSg { page: pg, setCancellation, setPage }) = go pg
    where
    go Intro = mkExists $ SubgraphF (Intro.intro setCancellation setPage)
    go HelloWorld = mkExists $ SubgraphF (HelloWorld.helloWorld setCancellation setPage)
    go FixFan = mkExists $ SubgraphF (FixFan.fixFan setCancellation setPage)
    go AudioUnits = mkExists $ SubgraphF (Component.components setCancellation setPage)
    go AudioWorklets = mkExists $ SubgraphF (Pursx1.pursx1 setCancellation setPage)
    go Events = mkExists $ SubgraphF (Events.events setCancellation setPage)
    go State = mkExists $ SubgraphF (Effects.effects setCancellation setPage)
    go Imperative = mkExists $ SubgraphF (Pursx2.pursx2 setCancellation setPage)
    go MultiChannel = mkExists $ SubgraphF (Multichannel.multiChannel setCancellation setPage)
    go Subgraph = mkExists $ SubgraphF (Subgraph.subgraphs setCancellation setPage)
    go Tumult = mkExists $ SubgraphF (Portals.portals setCancellation setPage)

main :: Effect Unit
main = do
  b' <- window >>= document >>= body
  for_ (toElement <$> b') \b -> do
    ffi <- makeFFIDOMSnapshot
    { push, event } <- create
    let evt = deku b (scene push event) effectfulDOMInterpret
    void $ subscribe evt \i -> i ffi
    push (ChangePage Intro)