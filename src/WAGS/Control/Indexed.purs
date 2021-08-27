module WAGS.Control.Indexed where

import Prelude

import Control.Applicative.Indexed (class IxApplicative, class IxApply, iapply, ipure)
import Control.Bind.Indexed (class IxBind)
import Control.Comonad (extract)
import Control.Monad.Indexed (class IxMonad)
import Data.Functor (voidRight)
import Data.Functor.Indexed (class IxFunctor)
import WAGS.Control.Types (WAG)

newtype IxWAG (assets :: Row Type) (audio :: Type) (engine :: Type) (proof :: Type) (res :: Type) (graphi :: Type) (grapho :: Type) (a :: Type)
  = IxWAG (forall q. WAG assets audio engine proof res graphi q -> WAG assets audio engine proof res grapho a)

instance functorIxWAG :: Functor (IxWAG assets audio engine proof res graphi grapho) where
  map f (IxWAG a) = IxWAG ((map <<< map) f a)

instance ixFunctorIxWAG :: IxFunctor (IxWAG assets audio engine proof res) where
  imap f (IxWAG a) = IxWAG ((map <<< map) f a)

instance ixApplyIxWAG :: IxApply (IxWAG assets audio engine proof res) where
  iapply (IxWAG fab') (IxWAG a') =
    IxWAG \i ->
      let
        fab = fab' i

        a = a' fab
      in
        a $> ((extract fab) (extract a))

instance applyIxWAG :: Apply (IxWAG assets audio engine proof res graph graph) where
  apply = iapply

instance ixApplicativeIxWAG :: IxApplicative (IxWAG assets audio engine proof res) where
  ipure a = IxWAG (voidRight a)

instance applicativeIxWAG :: Applicative (IxWAG assets audio engine proof res graph graph) where
  pure = ipure

instance ixBindIxWAG :: IxBind (IxWAG assets audio engine proof res) where
  ibind (IxWAG ma') aToB =
    IxWAG \i ->
      let
        ma = ma' i

        IxWAG b = aToB (extract ma)
      in
        b ma

instance ixMonadIxWAG :: IxMonad (IxWAG assets audio engine proof res)

type IxFrame (env :: Type) (assets :: Row Type) (audio :: Type) (engine :: Type) (proof :: Type) (res :: Type) (graphi :: Type) (grapho :: Type) (a :: Type)
  = env -> IxWAG assets audio engine proof res graphi grapho a
