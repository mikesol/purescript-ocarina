-- | Import this module to use `do` notation with `FrameT`.
-- | This qualified file is needed because `FrameT` does not implement `IxMonad`.
module WAGS.Control.Qualified where

import Prelude as P
import WAGS.Control.Types (FrameT, unsafeFrame, unsafeUnframe)

bind :: forall env audio engine proof m iu mu ou a b. P.Monad m => FrameT env audio engine proof m iu mu a -> (a -> FrameT env audio engine proof m mu ou b) -> FrameT env audio engine proof m iu ou b
bind fr function = unsafeFrame ((P.bind monad (P.compose unsafeUnframe function)))
  where
  monad = unsafeUnframe fr

discard :: forall env audio engine proof m iu mu ou b. P.Monad m => FrameT env audio engine proof m iu mu P.Unit -> (P.Unit -> FrameT env audio engine proof m mu ou b) -> FrameT env audio engine proof m iu ou b
discard = bind
