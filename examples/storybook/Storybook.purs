module WAGS.Example.Storybook where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (class Plus, empty)
import Data.Either (Either(..))
import Data.Exists (mkExists)
import Data.Foldable (for_)
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
import Effect.Aff (launchAff_, try)
import Effect.Class (liftEffect)
import FRP.Event (class IsEvent, create, filterMap, fold, keepLatest, mapAccum, subscribe)
import WAGS.Example.AtariSpeaks as AtariSpeaks
import WAGS.Example.HelloWorld as HelloWorld
import WAGS.Example.MultiBuf as MultiBuf
import WAGS.Example.Subgraph as Subg
import WAGS.Example.Tumult as Tummult
import WAGS.Example.Tumult as Tumult
import WAGS.Example.Utils (ToCancel)
import WAGS.Interpret (close)
import Web.HTML (window)
import Web.HTML.HTMLDocument (body)
import Web.HTML.HTMLElement (toElement)
import Web.HTML.Window (document)

data Page
  = HelloWorld HelloWorld.Init
  | AtariSpeaks AtariSpeaks.Init
  | MultiBuf MultiBuf.Init
  | Tumult Tummult.Init
  | Subg Subg.Init
  | ErrorPage String
  | LoadingPage

derive instance eqPage :: Eq Page
derive instance genericPage :: Generic Page _

instance Show Page where
  show p = genericShow p

instance Hashable Page where
  hash = hash <<< show

data UIAction = PageLoading | Page Page | StashCancel (Maybe ToCancel) | Start

data OnClickAction = Loaded | Loading

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
                    ( pure (D.Style := "cursor:pointer;") <|>
                        ( ( fold
                              ( \a (i /\ stash) -> case a of
                                  Start -> Loaded /\ stash
                                  PageLoading -> Loading /\ stash
                                  StashCancel newStash -> i /\ newStash
                                  Page _ -> Loaded /\ stash
                              )
                              event
                              (Loaded /\ Nothing)
                          ) <#>
                            \(loadingState /\ stash) ->
                              case loadingState of
                                Loading -> D.OnClick := cb (const $ pure unit)
                                Loaded -> D.OnClick := cb
                                  ( const $ launchAff_ $
                                      ( try do
                                          liftEffect $ push PageLoading
                                          for_ stash \{ unsub, ctx } ->
                                            liftEffect
                                              do
                                                close ctx
                                                unsub
                                          x <- x'
                                          liftEffect $ push (Page x)
                                      ) >>= case _ of
                                        Left e -> liftEffect
                                          (push (Page $ ErrorPage $ show e))
                                        Right _ -> pure unit
                                  )
                        )
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
          , (Subg <$> Subg.initializeSubgraph)
              /\ "Subgraph"
              /\ true
          ]
    , subgraph
        ( mapAccum (\a b -> Just a /\ (b /\ a))
            ( filterMap
                ( case _ of
                    Page p -> Just p
                    PageLoading -> Just LoadingPage
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
  page :: (Maybe ToCancel -> Effect Unit) -> Subgraph Page Unit event payload
  page cancelCb (HelloWorld hwi) = HelloWorld.helloWorld hwi cancelCb
  page cancelCb (AtariSpeaks ati) = AtariSpeaks.atariSpeaks ati cancelCb
  page cancelCb (MultiBuf mbi) = MultiBuf.multiBuf mbi cancelCb
  page cancelCb (Tumult ti) = Tumult.tumultExample ti cancelCb
  page cancelCb (Subg ti) = Subg.subgraphExample ti cancelCb
  page _ (ErrorPage s) = mkExists $ SubgraphF \_ _ -> D.p_
    [ text_ ("Well, this is embarassing. The following error occurred: " <> s) ]
  page _ LoadingPage = mkExists $ SubgraphF \_ _ -> D.p_
    [ text_ "Loading..." ]

main :: Effect Unit
main = do
  b' <- window >>= document >>= body
  for_ (toElement <$> b') \b -> do
    ffi <- makeFFIDOMSnapshot
    { push, event } <- create
    let evt = deku b (scene push event) effectfulDOMInterpret
    void $ subscribe evt \i -> i ffi
    push (Page (HelloWorld unit))