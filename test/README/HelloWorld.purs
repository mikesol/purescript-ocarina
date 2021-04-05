module Test.Readme.HelloWorld where

import Prelude

import Control.Apply.Indexed ((:*>))
import WAGS (SinOsc(..), Speaker(..), create, freeze, oneFrame, start, (@|>))

scene = (start :*> create (Speaker (SinOsc 440.0))) @|> freeze

step0 = oneFrame scene unit

step1 = oneFrame step0.next unit

step2 = oneFrame step1.next unit