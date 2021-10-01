module WAGS.Control.Indexed where

import Prelude

import Control.Applicative.Indexed (class IxApplicative, class IxApply, iapply, ipure)
import Control.Bind.Indexed (class IxBind, ibind)
import Control.Comonad (extract)
import Control.Monad.Indexed (class IxMonad)
import Data.Functor (voidRight)
import Data.Functor.Indexed (class IxFunctor)
import WAGS.Control.Types (WAG)

newtype IxWAG (audio :: Type) (engine :: Type) (proof :: Type) (res :: Type) (graphi :: Row Type) (grapho :: Row Type) (a :: Type)
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

instance bindIxWag :: Bind (IxWAG audio engine proof res graph graph) where
  bind = ibind

instance ixMonadIxWAG :: IxMonad (IxWAG audio engine proof res)

instance monadIxWag :: Monad (IxWAG audio engine proof res graph graph)
