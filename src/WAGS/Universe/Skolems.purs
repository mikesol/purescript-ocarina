module WAGS.Universe.Skolems where

import Prelude

import Data.Typelevel.Bool (False)
import Type.Proxy (Proxy)
import WAGS.Graph.Constructors (Gain(..), Highpass(..), Speaker(..))
import WAGS.Universe.Bin (class PtrListKeepSingleton, Ptr, PtrList, PtrListCons, PtrListNil)
import WAGS.Util (class Gate, class TypeEqualTF)

data SkolemPair

foreign import data SkolemPairC :: Type -> Ptr -> SkolemPair

data SkolemList

foreign import data SkolemListCons :: SkolemPair -> SkolemList -> SkolemList

foreign import data SkolemListNil :: SkolemList

data DiscardableSkolem

class LookupSkolem' (accumulator :: PtrList) (skolem :: Type) (skolemList :: SkolemList) (ptr :: PtrList) | accumulator skolem skolemList -> ptr

instance lookupSkolemNil :: LookupSkolem' accumulator ptr SkolemListNil accumulator

instance lookupSkolemCons ::
  ( TypeEqualTF skolem candidate tf
  , Gate tf (PtrListCons ptr PtrListNil) PtrListNil toComp
  , PtrListKeepSingleton toComp accumulator acc
  , LookupSkolem' acc skolem tail o
  ) =>
  LookupSkolem' accumulator skolem (SkolemListCons (SkolemPairC candidate ptr) tail) o

class LookupSkolem (skolem :: Type) (skolemList :: SkolemList) (ptr :: Ptr) | skolem skolemList -> ptr

instance lookupSkolem :: (LookupSkolem' PtrListNil skolem skolemList (PtrListCons ptr PtrListNil)) => LookupSkolem skolem skolemList ptr

instance skolemNotYetPresentNil :: SkolemNotYetPresent skolem SkolemListNil

instance skolemNotYetPresentCons ::
  ( TypeEqualTF skolem candidate False
  , SkolemNotYetPresentOrDiscardable skolem tail
  ) =>
  SkolemNotYetPresent skolem (SkolemListCons (SkolemPairC candidate ptr) tail)

class SkolemNotYetPresent (skolem :: Type) (skolemList :: SkolemList)

class SkolemNotYetPresentOrDiscardable (skolem :: Type) (skolemList :: SkolemList)

instance skolemNotYetPresentOrDiscardableD :: SkolemNotYetPresentOrDiscardable DiscardableSkolem skolemList
else instance skolemNotYetPresentOrDiscardableO :: SkolemNotYetPresent o skolemList => SkolemNotYetPresentOrDiscardable o skolemList

class MakeInternalSkolemStack (skolem :: Type) (ptr :: Ptr) (skolems :: SkolemList) (skolemsInternal :: SkolemList) | skolem ptr skolems -> skolemsInternal

instance makeInternalSkolemStackDiscardable :: MakeInternalSkolemStack DiscardableSkolem ptr skolems skolems
else instance makeInternalSkolemStack :: MakeInternalSkolemStack skolem ptr skolems (SkolemListCons (SkolemPairC skolem ptr) skolems)

class GetSkolemFromRecursiveArgument (a :: Type) (skolem :: Type) | a -> skolem

instance getSkolemFromRecursiveArgumentF :: GetSkolemFromRecursiveArgument ((Proxy skolem) -> b) skolem
else instance getSkolemFromRecursiveArgumentC :: GetSkolemFromRecursiveArgument b DiscardableSkolem

class ToSkolemizedFunction (a :: Type) (skolem :: Type) (b :: Type) | a skolem -> b where
  toSkolemizedFunction :: a -> (Proxy skolem -> b)

instance toSkolemizedFunctionFunction :: ToSkolemizedFunction (Proxy skolem -> b) skolem b where
  toSkolemizedFunction = identity
else instance toSkolemizedFunctionConst :: ToSkolemizedFunction b skolem b where
  toSkolemizedFunction = const

class GetSkolemizedFunctionFromAU (a :: Type) (skolem :: Type) (b :: Type) | a skolem -> b where
  getSkolemizedFunctionFromAU :: a -> (Proxy skolem -> b)

instance getSkolemizedFunctionFromAUHighpass :: ToSkolemizedFunction i skolem o => GetSkolemizedFunctionFromAU (Highpass a b i) skolem o where
  getSkolemizedFunctionFromAU (Highpass a b c) = toSkolemizedFunction c

instance getSkolemizedFunctionFromAUGain :: ToSkolemizedFunction i skolem o => GetSkolemizedFunctionFromAU (Gain a i) skolem o where
  getSkolemizedFunctionFromAU (Gain a b) = toSkolemizedFunction b

instance getSkolemizedFunctionFromAUSpeaker :: ToSkolemizedFunction i skolem o => GetSkolemizedFunctionFromAU (Speaker i) skolem o where
  getSkolemizedFunctionFromAU (Speaker a) = toSkolemizedFunction a

