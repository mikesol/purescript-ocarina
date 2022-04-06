module WAGS.Example.Storybook where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (class Plus, empty)
import Data.Exists (mkExists)
import Data.Foldable (for_, oneOfMap)
import Data.Generic.Rep (class Generic)
import Data.Hashable (class Hashable, hash)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import Deku.Attribute (cb, (:=))
import Deku.Control (deku, flatten, text_)
import Deku.Core (Element, Subgraph, SubgraphF(..))
import Deku.DOM as D
import Deku.Interpret (effectfulDOMInterpret, makeFFIDOMSnapshot)
import Deku.Subgraph (SubgraphAction(..), subgraph)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import FRP.Event (class IsEvent, create, filterMap, keepLatest, mapAccum, subscribe)
import WAGS.Example.AtariSpeaks as AtariSpeaks
import WAGS.Example.HelloWorld as HelloWorld
import WAGS.Example.MultiBuf as MultiBuf
import WAGS.Example.Tumult as Tummult
import WAGS.Example.Tumult as Tumult
import WAGS.Example.Utils (ToCancel)
import Web.HTML (window)
import Web.HTML.HTMLDocument (body)
import Web.HTML.HTMLElement (toElement)
import Web.HTML.Window (document)

data Page
  = HelloWorld HelloWorld.Init
  | AtariSpeaks AtariSpeaks.Init
  | MultiBuf MultiBuf.Init
  | Tumult Tummult.Init
  | Error String

derive instance eqPage :: Eq Page
derive instance genericPage :: Generic Page _

instance Show Page where
  show p = genericShow p

instance Hashable Page where
  hash = hash <<< show

data UIAction = PageLoading | Page Page | StashCancel ToCancel | Start

scene
  :: forall event payload
   . IsEvent event
  => Plus event
  => (UIAction -> Effect Unit)
  -> event UIAction
  -> Element event payload
scene push event =
  flatten
    [ D.div_
        $ map
            ( \(x' /\ y /\ z) -> D.span_
                [ D.a
                    ( oneOfMap pure
                        [ D.OnClick := cb
                            ( const $ launchAff_ do
                                liftEffect $ push PageLoading
                                x <- x'
                                liftEffect $ push (Page x)
                            )
                        , D.Style := "cursor:pointer;"
                        ]
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
          [ (HelloWorld <$> HelloWorld.initializeHelloWorld)
              /\ "Hello World"
              /\ true
          , (AtariSpeaks <$> AtariSpeaks.initializeAtariSpeaks)
              /\ "Atari speaks"
              /\ true
          , (MultiBuf <$> MultiBuf.initializeMultiBuf)
              /\ "Multi-buf"
              /\ true
          , (Tumult <$> Tumult.initializeTumult)
              /\ "Tumult"
              /\ true
          ]
    , subgraph
        ( mapAccum (\a b -> Just a /\ (b /\ a))
            ( filterMap
                ( case _ of
                    Page p -> Just p
                    _ -> Nothing
                )
                event
            )
            Nothing
            # map
                ( \(prev /\ cur) ->
                    ( case prev of
                        Nothing -> empty
                        Just x -> pure (x /\ Remove)
                    ) <|> pure (cur /\ InsertOrUpdate unit)
                )
            # keepLatest
        )
        (page (StashCancel >>> push))

    ]
  where
  page :: (ToCancel -> Effect Unit) -> Subgraph Page Unit event payload
  page cancelCb p@(HelloWorld hwi) = HelloWorld.helloWorld hwi cancelCb p
  page cancelCb p@(AtariSpeaks ati) = AtariSpeaks.atariSpeaks ati cancelCb p
  page cancelCb p@(MultiBuf mbi) = MultiBuf.multiBuf mbi cancelCb p
  page cancelCb p@(Tumult ti) = Tumult.tumultExample ti cancelCb p
  page _ (Error s) = mkExists $ SubgraphF \_ _ -> D.p_
    [ text_ ("Well, this is embarassing. The following error occurred: " <> s) ]

main :: Effect Unit
main = do
  b' <- window >>= document >>= body
  for_ (toElement <$> b') \b -> do
    ffi <- makeFFIDOMSnapshot
    { push, event } <- create
    let evt = deku b (scene push event) effectfulDOMInterpret
    void $ subscribe evt \i -> i ffi
    push (Page (HelloWorld unit))