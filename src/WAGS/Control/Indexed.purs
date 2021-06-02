module WAGS.Control.Indexed where

import Prelude

import Control.Applicative.Indexed (class IxApplicative, class IxApply, iapply, ipure)
import Control.Bind.Indexed (class IxBind)
import Control.Comonad (extract)
import Control.Monad.Indexed (class IxMonad)
import Data.Functor (voidRight)
import Data.Functor.Indexed (class IxFunctor)
import WAGS.Control.Types (WAG)

newtype IxWAG (audio :: Type) (engine :: Type) (proof :: Type) (res :: Type) (graphi :: Type) (grapho :: Type) (a :: Type)
  = IxWAG (forall q. WAG audio engine proof res graphi q -> WAG audio engine proof res grapho a)

instance functorIxWAG :: Functor (IxWAG audio engine proof res graphi grapho) where
  map f (IxWAG a) = IxWAG ((map <<< map) f a)

instance ixFunctorIxWAG :: IxFunctor (IxWAG audio engine proof res) where
  imap f (IxWAG a) = IxWAG ((map <<< map) f a)

instance ixApplyIxWAG :: IxApply (IxWAG audio engine proof res) where
  iapply (IxWAG fab') (IxWAG a') =
    IxWAG \i ->
      let
        fab = fab' i

        a = a' fab
      in
        a $> ((extract fab) (extract a))

instance applyIxWAG :: Apply (IxWAG audio engine proof res graph graph) where
  apply = iapply

instance ixApplicativeIxWAG :: IxApplicative (IxWAG audio engine proof res) where
  ipure a = IxWAG (voidRight a)

instance applicativeIxWAG :: Applicative (IxWAG audio engine proof res graph graph) where
  pure = ipure

instance ixBindIxWAG :: IxBind (IxWAG audio engine proof res) where
  ibind (IxWAG ma') aToB =
    IxWAG \i ->
      let
        ma = ma' i

        IxWAG b = aToB (extract ma)
      in
        b ma

instance ixMonadIxWAG :: IxMonad (IxWAG audio engine proof res)

type IxFrame (env :: Type) (audio :: Type) (engine :: Type) (proof :: Type) (res :: Type) (graphi :: Type) (grapho :: Type) (a :: Type)
  = env -> IxWAG audio engine proof res graphi grapho a

wag_ ::
  forall audio engine proof res graph.
  IxWAG audio engine proof res graph graph (WAG audio engine proof res graph Unit)
wag_ = wag unit

wag ::
  forall audio engine proof res graph a.
  a -> IxWAG audio engine proof res graph graph (WAG audio engine proof res graph a)
wag a = IxWAG (\i -> i $> (i $> a))