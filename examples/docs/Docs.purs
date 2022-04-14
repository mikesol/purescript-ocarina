module WAGS.Example.Docs where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (class Plus, empty)
import Data.Exists (mkExists)
import Data.Filterable (partitionMap)
import Data.Foldable (for_, oneOfMap)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Deku.Attribute (cb, (:=))
import Deku.Control (deku, flatten, text_)
import Deku.Core (Element, Subgraph, SubgraphF(..))
import Deku.DOM as D
import Deku.Interpret (effectfulDOMInterpret, makeFFIDOMSnapshot)
import Deku.Subgraph (SubgraphAction(..), subgraph)
import Effect (Effect)
import FRP.Event (class IsEvent, create, keepLatest, mapAccum, subscribe)
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
import WAGS.Example.Docs.Types (Navigation(..), Page(..))
import Web.HTML (window)
import Web.HTML.HTMLDocument (body)
import Web.HTML.HTMLElement (toElement)
import Web.HTML.Window (document)

scene
  :: forall event payload
   . IsEvent event
  => Plus event
  => (Page -> Effect Unit)
  -> event Page
  -> Element event payload
scene push event =
  flatten
    [ D.div_
        $ map
            ( \(x /\ y /\ z) -> D.span_
                [ D.a
                    ( oneOfMap bang
                        [ D.OnClick := cb (const $ push x)
                        , D.Style := "cursor:pointer;"
                        ]
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
          , AudioUnits
              /\ "Audio units"
              /\ true
          , Events
              /\ "Events"
              /\ true
          , FixFan
              /\ "Fan and fix"
              /\ true
          , MultiChannel
              /\ "Merging and splitting"
              /\ true
          , AudioWorklets
              /\ "Audio worklets"
              /\ true
          , State
              /\ "State"
              /\ true
          , Subgraph
              /\ "Subgraphs"
              /\ true
          , Tumult
              /\ "Tumult"
              /\ true
          , Imperative
              /\ "Imperative API"
              /\ false
          ]
    , subgraph
        ( mapAccum (\a b -> Just a /\ (b /\ a)) event Nothing
            # map
                ( \(prev /\ cur) ->
                    ( case prev of
                        Nothing -> empty
                        Just x -> bang (x /\ Remove)
                    ) <|> bang (cur /\ Insert)
                )
            # keepLatest
        )
        (page (\_ -> pure unit) push)

    ]
  where
  page :: (Effect Unit -> Effect Unit) -> (Page -> Effect Unit) -> Subgraph Page event payload
  page cancelCb dpage = go
    where
    go Intro = mkExists $ SubgraphF \_ e -> Intro.intro cancelCb dpage
    go HelloWorld = mkExists $ SubgraphF \_ e -> HelloWorld.helloWorld cancelCb dpage
    go AudioUnits = mkExists $ SubgraphF \_ e -> Component.components cancelCb dpage
    go AudioWorklets = mkExists $ SubgraphF \_ e -> Pursx1.pursx1 cancelCb dpage
    go Events = mkExists $ SubgraphF \_ e -> Events.events cancelCb dpage
    go FixFan = mkExists $ SubgraphF \_ e -> FixFan.fixFan cancelCb dpage
    go State = mkExists $ SubgraphF \_ e -> Effects.effects cancelCb dpage
    go Imperative = mkExists $ SubgraphF \_ e -> Pursx2.pursx2 cancelCb dpage
    go MultiChannel = mkExists $ SubgraphF \_ e -> Multichannel.multiChannel cancelCb dpage
    go Subgraph = mkExists $ SubgraphF \_ e -> Subgraph.subgraphs cancelCb dpage
    go Tumult = mkExists $ SubgraphF \_ e -> Portals.portals cancelCb dpage

main :: Effect Unit
main = do
  b' <- window >>= document >>= body
  for_ (toElement <$> b') \b -> do
    ffi <- makeFFIDOMSnapshot
    { push, event } <- create
    let evt = deku b (scene push event) effectfulDOMInterpret
    void $ subscribe evt \i -> i ffi
    push Intro