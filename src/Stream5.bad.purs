module Stream5Bad where

import Prelude

import Control.Comonad.Cofree (Cofree, mkCofree)
import Data.Identity (Identity(..))
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\), type (/\))
import Unsafe.Coerce (unsafeCoerce)

-- In order to remain polymorphic, `a` will need to store
-- an audio unit (ie SinOsc) _and_ its interpreter.
-- The output of the interpreter is a list of commands (on, off, set, etc).

data Stream :: (Type -> Type) -> Type -> Type -> Type
data Stream m i a = Stream Void

class Explicate potential actual | potential -> actual, actual -> potential where
   explicate :: potential -> actual
   induce :: actual -> potential

-- next step is to make incoming g val NOT accessible to closure
-- currently, g can be passed all the way down, and we don't want that

-- by using explicate strategy, we can move a to b in a pre-processing step
-- then, we are working with a b

-- a is still at the toplevel though. would be the same problem if it were a function
-- and same issue with b

-- slightly better, but problem is that b has remnants of x and a may have remnants of "the last guy"
-- we still want to seal off a from appearing in any term to the right

-- in general, the problem is that potential is a mix of potential and acutal (naming is bad)
-- so we want to force two things:
-- - the consuming function - in this case (i -> b -> Stream m i (forall c. c /\ c -> m c)) - can _only_ do one thing with b. namely, stash it. can do with monad?

-- this sort of works: we have hidden the a now in the monad n
-- but the issue is the same. let's say we can marshal the x to an a
-- then we'll call some "make monad" type using a
-- we can do this as much as we want, it is exactly like the tuple
-- the only advantage of this for now is that a does not appear as a term we can work with
-- but we will need to work with it
-- one way to do it is to pass a single consumption "witness" term to the function that is used
-- to unlock the monadic construction. the construction then requires a new witness, which renders
-- the previous one obsolete
mkStream :: forall n m i a x. Explicate a x => n a ((x -> m x) /\ (i -> x -> Stream m i (forall c. c /\ c -> m c))) -> Stream m i (forall c. c /\ c -> m c)
mkStream a = Stream (unsafeCoerce a)

-- m m for canceler
run :: forall m i. Applicative m => (forall a. a -> m Unit) -> Stream m i (forall b. b /\ b -> m b) -> m (m Unit)
run a b = pure (a unit)

data SinOsc = SinOsc {freq :: Number}

data HNil = HNil

data Gain a = Gain { vol ::  Number } a


type OutputStream = forall c. c /\ c -> Identity c

{-
gainy :: Gain (Tuple SinOsc HNil) -> Stream Identity Int OutputStream
gainy g = mkStream (g /\ (pure) /\ (\_ (Gain {vol} (SinOsc { freq } /\ _) ) -> piece))

piece :: Stream Identity Int OutputStream
piece = mkStream 
   (SinOsc { freq: 340.0}
      /\ (pure)
      /\ (\_ (SinOsc { freq }) ->
           gainy (Gain {vol:1.0} (SinOsc { freq: freq + 4.0 } /\ HNil))))
-}