module Stream3 where

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

mkStream :: forall m i a b. a /\ (a -> m a) /\ (i -> a -> Stream m i b) -> Stream m i (forall c. c /\ c -> m c)
mkStream a = Stream (unsafeCoerce a)

interpret :: forall m i. (forall a. a -> m Unit) -> Stream m i (forall b. b /\ b -> m b) -> m Unit
interpret a b = (a unit)

data SinOsc = SinOsc {freq :: Number}

data HNil = HNil

data Gain a = Gain { vol ::  Number } a

-- next step is to make incoming g val NOT accessible to closure
-- currently, g can be passed all the way down, and we don't want that

type OutputStream = forall c. c /\ c -> Identity c

gainy :: Gain (Tuple SinOsc HNil) -> Stream Identity Int OutputStream
gainy g = mkStream (g /\ (pure) /\ (\_ (Gain {vol} (SinOsc { freq } /\ _) ) -> piece))

piece :: Stream Identity Int OutputStream
piece = mkStream 
   (SinOsc { freq: 340.0}
      /\ (pure)
      /\ (\_ (SinOsc { freq }) ->
           gainy (Gain {vol:1.0} (SinOsc { freq:3.0 } /\ HNil))))