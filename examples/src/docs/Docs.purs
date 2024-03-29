module Ocarina.Example.Docs where

import Prelude

import Data.Filterable (filter)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple.Nested ((/\))
import Deku.Control (text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners as DL
import Deku.Do as Deku
import Deku.Hooks (useState, useState', (<#~>))
import Deku.Toplevel (runInBody)
import Effect (Effect)
import FRP.Event (fold)
import FRP.Poll (Poll)
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

newtype TopLevelSg = TopLevelSg
  { page :: Page
  , setPage :: Page -> Effect Unit
  , setCancellation :: Effect Unit -> Effect Unit
  }

derive instance Newtype TopLevelSg _

p2tl :: Page -> TopLevelSg
p2tl page = TopLevelSg { page, setPage: mempty, setCancellation: mempty }

scene :: Nut
scene = Deku.do
  push /\ event' <- useState (ChangePage Intro)
  let
    event = fold
      ( flip
          ( case _ of
              ChangePage p -> \{ curPage, cancel } -> { prevPage: Just curPage, curPage: p, cancel, pageChange: true }
              SetCancelation cancel -> _ { cancel = cancel, pageChange = false }
          )
      )
      { prevPage: Nothing, curPage: Intro, cancel: pure unit, pageChange: true }
      event'

  D.div_
    [ D.div_
        $ map
            ( \(x /\ y /\ z) -> D.span_
                [ D.a
                    [ DL.click_ \_ -> push (ChangePage x)
                    , DA.style_ "cursor:pointer;"
                    , DL.runOn DL.click $ map
                        ( \{ cancel } -> do
                            cancel
                            push (ChangePage x)
                        )
                        (event # filter (_.pageChange >>> not))
                    ]
                    [ text_ y ]
                , D.span
                    [ DA.style_ $
                        if z then ""
                        else "display:none;"
                    ]
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
    , (event # filter _.pageChange) <#~>
        ( \{ curPage } ->
            page
              ( TopLevelSg
                  { page: curPage
                  , setPage: ChangePage >>> push
                  , setCancellation: SetCancelation >>> push
                  }
              )
        )

    ]
  where
  page :: TopLevelSg -> Nut
  page (TopLevelSg { page: pg, setCancellation, setPage }) = go pg
    where
    go Intro = D.div_ [ bussed $ Intro.intro setCancellation setPage ]
    go HelloWorld = D.div_ [ bussed $ HelloWorld.helloWorld setCancellation setPage ]
    go FixFan = D.div_ [ bussed $ FixFan.fixFan setCancellation setPage ]
    go AudioUnits = D.div_ [ bussed $ Component.components setCancellation setPage ]
    go AudioWorklets = D.div_ [ bussed $ Pursx1.pursx1 setCancellation setPage ]
    go Events = D.div_ [ bussed $ Events.events setCancellation setPage ]
    go Params = D.div_ [ bussed $ Params.params setCancellation setPage ]
    go State = D.div_ [ bussed $ Effects.effects setCancellation setPage ]
    go Imperative = D.div_ [ bussed $ Pursx2.pursx2 setCancellation setPage ]
    go MultiChannel = D.div_ [ bussed $ Multichannel.multiChannel setCancellation setPage ]
    go Subgraph = D.div_ [ bussed $ Subgraph.subgraphs setCancellation setPage ]
    go Tumult = D.div_ [ bussed $ Portals.portals setCancellation setPage ]

bussed :: forall a. ((a -> Effect Unit) -> Poll a -> Nut) -> Nut
bussed f = Deku.do
  p /\ e <- useState'
  f p e

main :: Effect Unit
main = runInBody scene