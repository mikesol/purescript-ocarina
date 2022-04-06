module WAGS.Tumult where

import Prelude

import Control.Plus (empty)
import FRP.Behavior (sample_)
import FRP.Event (class IsEvent, keepLatest)
import WAGS.Core as C
import WAGS.Tumult.Tumult (Tumultuous)

-- tumult

tumult
  :: forall outputChannels terminus inputs event payload
   . IsEvent event
  => event (Tumultuous terminus inputs)
  -> C.Node outputChannels () inputs event payload
tumult atts' = C.Node go
  where
  go parent (C.AudioInterpret ai@{ ids }) =
    keepLatest
      ( (sample_ ids (pure unit)) <#> \mePrefix ->
          empty
      )
