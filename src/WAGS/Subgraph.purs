module WAGS.Subgraph where

import Prelude

import WAGS.Control.Types (Frame0, SubScene)
import WAGS.Interpret (class AudioInterpret)

type SubSceneSig terminus inputs env =
  forall audio engine
   . AudioInterpret audio engine
  => SubScene terminus
       inputs
       env
       audio
       engine
       Frame0
       Unit