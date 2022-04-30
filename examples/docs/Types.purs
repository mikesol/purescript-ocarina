module WAGS.Example.Docs.Types where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)

data Page
  = Intro
  | HelloWorld
  | AudioUnits
  | Events
  | FixFan
  | MultiChannel
  | AudioWorklets
  | Imperative
  | State
  | Subgraph
  | Tumult
derive instance Eq Page
derive instance Ord Page
derive instance Generic Page _
instance showPage :: Show Page where
  show s = genericShow s

type CancelCurrentAudio = Effect Unit -> Effect Unit

data ToplevelEvent = ChangePage Page | SetCancelation (Effect Unit)

data SingleSubgraphEvent = SetCancel (Effect Unit)

type SingleSubgraphPusher = SingleSubgraphEvent -> Effect Unit