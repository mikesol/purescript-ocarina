module WAGS.Control.Env where

import Control.Comonad.Env (Env, env, runEnv)
import Control.Monad.Reader (Reader, runReader)
import Data.Tuple (Tuple(..))
import WAGS.Control.Types (FrameT)

type EFrame env proof iu ou m e a = FrameT env proof m iu ou (Env e (m a))

withReader :: forall e a. Env e (Reader e a) -> Env e a
withReader i = env e (runReader a e)
  where
  Tuple e a = runEnv i