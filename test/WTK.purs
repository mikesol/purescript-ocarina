module Test.WTK where

import Prelude
import Control.Applicative.Indexed (imap)
import Data.Either (Either(..))
import Data.Functor.Indexed (ivoid)
import Data.Identity (Identity(..))
import Data.List (List(..), (:))
import Data.Map as M
import Data.Set as S
import Data.Tuple (Tuple, fst, snd)
import Data.Tuple.Nested ((/\), type (/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Prim.Row (class Cons)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Type.Proxy (Proxy)
import WAGS (class Connect, class Create, class Cursor, class GraphIsRenderable, AnAudioUnit(..), AudioParameter, AudioUnitRef, Focus(..), FrameT, Instruction(..), SceneT, SinOsc(..), SingleEdge, UniverseC, branch, change, changeAt, create, cursor, env, freeze, gain, highpass, isHere, loop, mix, oneFrame', param, proof, runThunkableWithCount, sinOsc, speaker, start, thunkThunkable, wait, withProof, (@>), (@|>))
import WAGS.Control.Qualified as Ix
import WAGS.Interpret (class AudioInterpret)

{-
addKeys ::
  forall audio engine proofA sosc i m currentIdx mixer graph graphM0 graphM1 changeBit skolems currentIdx' graph' onsetCache acc' acc.
  Monad m =>
  AudioInterpret audio engine =>
  GraphIsRenderable graph =>
  Create (SinOsc AudioParameter) graph graphM (AudioRef sosc)
  Connect (AudioRef sosc) (AudioRef mixer) graphM0 graphM1
  --------------- how do we get from graphM1 to graph'?
  List Int ->
  FrameT Env audio engine proofA m (UniverseC currentIdx graph changeBit skolems) (UniverseC currentIdx' graph' changeBit skolems) { | acc } ->
addKeys (a : b) = ?hole
  -}
{-
type Env
  = { event :: { keys :: List Int, time :: Number }
    , time :: Number
    }

class IsOnsetCache oc <= OnsetCacheGoingToMixer oc mixer graph

class IsOnsetCache a

instance isOnsetCacheUnit :: IsOnsetCache Unit

instance isOnsetCache :: IsOnsetCache b => IsOnsetCache ((AudioUnitRef ptr /\ Number /\ Int) /\ b)

addKeys ::
  forall audio engine proofA i m currentIdx mixer graph changeBit skolems currentIdx' graph' onsetCache acc' acc.
  Monad m =>
  AudioInterpret audio engine =>
  GraphIsRenderable graph =>
  IsOnsetCache onsetCache =>
  Cons "onsetCache" onsetCache acc' acc =>
  Cons "mixer" mixer acc' acc =>
  OnsetCacheGoingToMixer onsetCache mixer graph =>
  List Int ->
  FrameT Env audio engine proofA m (UniverseC currentIdx graph changeBit skolems) (UniverseC currentIdx' graph' changeBit skolems) { | acc } ->
  SceneT Env audio engine proofA m
addKeys fr Nil = freeze fr -- needs to change

addKeys fr (note : rest) = ?hole

player ::
  forall audio engine proofA i m currentIdx mixer graph changeBit skolems onsetCache acc' acc.
  Monad m =>
  AudioInterpret audio engine =>
  GraphIsRenderable graph =>
  IsOnsetCache onsetCache =>
  Cons "onsetCache" onsetCache acc' acc =>
  Cons "mixer" mixer acc' acc =>
  OnsetCacheGoingToMixer onsetCache mixer graph =>
  FrameT Env audio engine proofA m i (UniverseC currentIdx graph changeBit skolems) { | acc } ->
  SceneT Env audio engine proofA m
player = ?hole
-}
{-
          let
            simpleScene =
              ( Ix.do
                  start
                  e <- env
                  create (scene0 e) $> Right unit
              )
                @> ( branch Ix.do
                      { time } <- env
                      pr <- proof
                      withProof pr
                        $ if time < 0.3 then
                            Right
                              ( const
                                  $ Ix.do
                                      e <- env
                                      ivoid $ change (scene0 e)
                              )
                          else
                            Left
                              ( loop
                                  ( const
                                      $ Ix.do
                                          e <- env
                                          ivoid $ change (scene1 e)
                                  )
                              )
                  )
-}
