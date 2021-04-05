module WAGS.Control.Functions
  ( start
  , makeScene
  , loop
  , branch
  , env
  , freeze
  , (@>)
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
import Data.Set (Set)
import Data.Tuple.Nested ((/\), type (/\))
import Unsafe.Coerce (unsafeCoerce)
import WAGS.Control.Types (AudioState', Frame(..), InitialFrame, Scene, oneFrame)
import WAGS.Rendered (AnAudioUnit, Instruction)
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

asScene :: forall env. (env -> M.Map Int AnAudioUnit /\ M.Map Int (Set Int) /\ Array Instruction /\ (Scene env)) -> Scene env
asScene = unsafeCoerce

makeScene ::
  forall env proofA proofB g0 g1 a.
  UniverseIsCoherent g1 =>
  Frame env proofA g0 g1 (Either (Scene env) a) ->
  (Frame env proofB g0 g1 a -> Scene env) ->
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
          newState.internalNodes /\ newState.internalEdges /\ newState.instructions
            /\ ( trans
                  $ Frame do
                      put $ newState { instructions = [] }
                      pure r
              )

loop ::
  forall env proofA i u edge a.
  TerminalIdentityEdge u edge =>
  UniverseIsCoherent u =>
  (a -> Frame env proofA u u a) ->
  Frame env proofA i u a ->
  Scene env
loop fa ma = makeScene (imap Right $ ibind ma fa) (loop fa)

branch ::
  forall env proofA i u edge a.
  TerminalIdentityEdge u edge =>
  UniverseIsCoherent u =>
  Frame env proofA u u (Either (Frame env proofA i u a -> Scene env) (a -> Frame env proofA u u a)) ->
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

infixr 6 makeScene as @>

freeze ::
  forall env proof g0 g1.
  UniverseIsCoherent g1 =>
  Frame env proof g0 g1 Unit ->
  Scene env
freeze s = makeScene (imap Right s) freeze

env ::
  forall env proof i.
  Frame env proof i i env
env = Frame ask
