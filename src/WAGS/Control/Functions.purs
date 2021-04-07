module WAGS.Control.Functions
  ( start
  , makeScene
  , makeScene'
  , loop
  , branch
  , env
  , freeze
  , (@>)
  , (@|>)
  ) where

import Prelude

import Control.Applicative.Indexed (ipure)
import Control.Bind.Indexed (ibind)
import Control.Monad.Indexed.Qualified as Ix
import Control.Monad.Reader (runReaderT, ask)
import Control.Monad.State (put, runState)
import Data.Either (Either(..))
import Data.Functor.Indexed (imap)
import Data.Map as M
import Data.Tuple.Nested ((/\))
import Unsafe.Coerce (unsafeCoerce)
import WAGS.Control.Types (AudioState', Frame(..), InitialFrame, Scene, Scene', oneFrame)
import WAGS.Validation (class TerminalIdentityEdge, class UniverseIsCoherent)

start :: forall env. InitialFrame env Unit
start = Frame (pure unit)

initialAudioState :: AudioState'
initialAudioState =
  { currentIdx: 0
  , instructions: []
  , internalNodes: M.empty
  , internalEdges: M.empty
  }

asScene :: forall env. (env -> Scene' env) -> Scene env
asScene = unsafeCoerce

makeScene ::
  forall env proofA i u a.
  UniverseIsCoherent u =>
  Frame env proofA i u (Either (Scene env) a) ->
  (forall proofB. Frame env proofB i u a -> Scene env) ->
  Scene env
makeScene (Frame m) trans = asScene go
  where
  go ev =
    let
      step1 = runReaderT m ev

      outcome /\ newState = runState step1 initialAudioState
    in
      case outcome of
        Left s -> oneFrame s ev
        Right r ->
          { nodes: newState.internalNodes
          , edges: newState.internalEdges
          , instructions: newState.instructions
          , next:
              trans
                $ Frame do
                    put $ newState { instructions = [] }
                    pure r
          }

infixr 6 makeScene as @>

branch ::
  forall env proofA i u a.
  UniverseIsCoherent u =>
  (forall proofB. Frame env proofB u u (Either (Frame env proofB i u a -> Scene env) (a -> Frame env proofB u u a))) ->
  Frame env proofA i u a ->
  Scene env
branch mch m =
  makeScene
    ( Ix.do
        r <- m
        mbe <- mch
        case mbe of
          Left l -> ipure $ Left (l m)
          Right fa -> imap Right (fa r)
    )
    (branch mch)

loop ::
  forall env proofA i u edge a.
  TerminalIdentityEdge u edge =>
  UniverseIsCoherent u =>
  (forall proofB. a -> Frame env proofB u u a) ->
  Frame env proofA i u a ->
  Scene env
--loop = branch <<< ipure <<< Right
loop fa ma = makeScene (imap Right $ ibind ma fa) (loop fa)

freeze ::
  forall env proof i u x.
  UniverseIsCoherent u =>
  Frame env proof i u x ->
  Scene env
freeze s = makeScene (imap Right s) freeze

makeScene' ::
  forall env proofA i u a.
  UniverseIsCoherent u =>
  Frame env proofA i u a ->
  (forall proofB. Frame env proofB i u a -> Scene env) ->
  Scene env
makeScene' a b = makeScene (imap Right a) b

infixr 6 makeScene' as @|>

env ::
  forall env proof i.
  Frame env proof i i env
env = Frame ask
