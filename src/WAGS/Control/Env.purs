module WAGS.Control.Env where

import Control.Comonad.Env (Env, env, runEnv)
import Control.Monad.Reader (Reader, runReader)
import Data.Tuple (Tuple(..))
import WAGS.Control.Types (Frame)

type EFrame env proof iu ou e a = Frame env proof iu ou (Env e a)

withReader :: forall e a. Env e (Reader e a) -> Env e a
withReader i = env e (runReader a e)
  where
  Tuple e a = runEnv i