module Stream2 where

import Prelude

import Control.Comonad.Cofree (Cofree, mkCofree)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Unsafe.Coerce (unsafeCoerce)

-- In order to remain polymorphic, `a` will need to store
-- an audio unit (ie SinOsc) _and_ its interpreter.
-- The output of the interpreter is a list of commands (on, off, set, etc).

data Stream :: Type -> Type -> Type
data Stream i a = Stream a Void

mkStream :: forall i a b. Tuple a (i -> a -> Stream i b) -> Stream i Void
mkStream (a /\ b) = Stream (unsafeCoerce a) (unsafeCoerce b)

head :: forall i a. Stream i a -> a
head (Stream a _) = a

tail :: forall i a. Stream i a -> (i -> a -> forall b. Stream i b)
tail (Stream _ b) = unsafeCoerce b
data SinOsc = SinOsc {freq :: Number}

data HNil = HNil

data Gain a = Gain { vol ::  Number } a

-- next step is to make incoming g val NOT accessible to closure
-- currently, g can be passed all the way down, and we don't want that
gainy :: Gain (Tuple SinOsc HNil) -> Stream Int Void
gainy g = mkStream (g /\ (\_ (Gain {vol} (SinOsc { freq } /\ _) ) -> piece))

piece :: Stream Int Void
piece = mkStream (SinOsc { freq: 340.0} /\ (\_ (SinOsc { freq }) ->
   gainy (Gain {vol:1.0} (SinOsc { freq:3.0 } /\ HNil))))