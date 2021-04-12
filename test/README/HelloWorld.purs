module Test.Readme.HelloWorld where

import Prelude

import Control.Apply.Indexed ((:*>))
import WAGS (create, freeze, oneFrame, sinOsc, speaker, start, (@|>))

scene = (start :*> create (speaker (sinOsc 440.0))) @|> freeze

step0 = oneFrame scene unit

step1 = oneFrame step0.next unit

step2 = oneFrame step1.next unit