module WAGS.Example.Docs where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (class Plus, empty)
import Data.Exists (mkExists)
import Data.Foldable (for_, oneOfMap)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Deku.Attribute (cb, (:=))
import Deku.Control (deku, flatten, text_)
import Deku.Core (Element, Subgraph, SubgraphF(..))
import Deku.DOM as D
import WAGS.Example.Docs.Component as Component
import WAGS.Example.Docs.Effects as Effects
import WAGS.Example.Docs.Events as Events
import WAGS.Example.Docs.HelloWorld as HelloWorld
import WAGS.Example.Docs.Intro as Intro
import WAGS.Example.Docs.Portals as Portals
import WAGS.Example.Docs.Pursx1 as Pursx1
import WAGS.Example.Docs.Pursx2 as Pursx2
import WAGS.Example.Docs.Subgraphs as Subgraph
import WAGS.Example.Docs.Types (Page(..))
import Deku.Interpret (effectfulDOMInterpret, makeFFIDOMSnapshot)
import Deku.Subgraph (SubgraphAction(..), subgraph)
import Effect (Effect)
import FRP.Event (class IsEvent, create, keepLatest, mapAccum, subscribe)
import FRP.Event.Class (bang)
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
          , AudioWorklets
              /\ "Audio worklets"
              /\ true
          , State
              /\ "State"
              /\ true
          , Imperative
              /\ "Imperative"
              /\ true
          , Subgraph
              /\ "Subgraphs"
              /\ true
          , Tumult
              /\ "Tumult"
              /\ false
          ]
    , subgraph
        ( mapAccum (\a b -> Just a /\ (b /\ a)) event Nothing
            # map
              ( \(prev /\ cur) ->
                  ( case prev of
                      Nothing -> empty
                      Just x -> bang (x /\ Remove)
                  ) <|> bang (cur /\ InsertOrUpdate unit)
              )
            # keepLatest
        )
        (page push)

    ]
  where
  page :: (Page -> Effect Unit) -> Subgraph Page Unit event payload
  page dpage Intro = mkExists $ SubgraphF \_ _ -> Intro.intro dpage
  page dpage HelloWorld = mkExists $ SubgraphF \_ _ -> HelloWorld.helloWorld
    dpage
  page dpage AudioUnits = mkExists $ SubgraphF \_ _ -> Component.components
    dpage
  page dpage AudioWorklets = mkExists $ SubgraphF \_ _ -> Pursx1.pursx1 dpage
  page dpage Events = mkExists $ SubgraphF \_ _ -> Events.events dpage
  page dpage State = mkExists $ SubgraphF \_ _ -> Effects.effects dpage
  page dpage Imperative = mkExists $ SubgraphF \_ _ -> Pursx2.pursx2 dpage
  page dpage Subgraph = mkExists $ SubgraphF \_ _ -> Subgraph.subgraphs dpage
  page dpage Tumult = mkExists $ SubgraphF \_ _ -> Portals.portals dpage

main :: Effect Unit
main = do
  b' <- window >>= document >>= body
  for_ (toElement <$> b') \b -> do
    ffi <- makeFFIDOMSnapshot
    { push, event } <- create
    let evt = deku b (scene push event) effectfulDOMInterpret
    void $ subscribe evt \i -> i ffi
    push Intro