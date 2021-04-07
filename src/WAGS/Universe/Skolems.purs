module WAGS.Universe.Skolems where

import Prelude
import Data.Typelevel.Bool (False)
import Type.Proxy (Proxy)
import WAGS.Graph.Constructors as CTOR
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

-- instance getSkolemizedFunctionFromAUHighpass :: ToSkolemizedFunction i skolem o => GetSkolemizedFunctionFromAU (Highpass a b i) skolem o where
--   getSkolemizedFunctionFromAU (Highpass a b c) = toSkolemizedFunction c
-- instance getSkolemizedFunctionFromAUGain :: ToSkolemizedFunction i skolem o => GetSkolemizedFunctionFromAU (Gain a i) skolem o where
--   getSkolemizedFunctionFromAU (Gain a b) = toSkolemizedFunction b
-- instance getSkolemizedFunctionFromAUSpeaker :: ToSkolemizedFunction i skolem o => GetSkolemizedFunctionFromAU (Speaker i) skolem o where
--   getSkolemizedFunctionFromAU (Speaker a) = toSkolemizedFunction a
----------
instance getSkolemizedFunctionFromAUAllpass :: ToSkolemizedFunction argC skolem o => GetSkolemizedFunctionFromAU (CTOR.Allpass argA argB argC) skolem o where
  getSkolemizedFunctionFromAU (CTOR.Allpass argA argB argC) = toSkolemizedFunction argC

instance getSkolemizedFunctionFromAUBandpass :: ToSkolemizedFunction argC skolem o => GetSkolemizedFunctionFromAU (CTOR.Bandpass argA argB argC) skolem o where
  getSkolemizedFunctionFromAU (CTOR.Bandpass argA argB argC) = toSkolemizedFunction argC

instance getSkolemizedFunctionFromAUConvolver :: ToSkolemizedFunction argB skolem o => GetSkolemizedFunctionFromAU (CTOR.Convolver argA argB) skolem o where
  getSkolemizedFunctionFromAU (CTOR.Convolver argA argB) = toSkolemizedFunction argB

instance getSkolemizedFunctionFromAUDelay :: ToSkolemizedFunction argB skolem o => GetSkolemizedFunctionFromAU (CTOR.Delay argA argB) skolem o where
  getSkolemizedFunctionFromAU (CTOR.Delay argA argB) = toSkolemizedFunction argB

instance getSkolemizedFunctionFromAUDynamicsCompressor :: ToSkolemizedFunction argF skolem o => GetSkolemizedFunctionFromAU (CTOR.DynamicsCompressor argA argB argC argD argE argF) skolem o where
  getSkolemizedFunctionFromAU (CTOR.DynamicsCompressor argA argB argC argD argE argF) = toSkolemizedFunction argF

instance getSkolemizedFunctionFromAUGain :: ToSkolemizedFunction argB skolem o => GetSkolemizedFunctionFromAU (CTOR.Gain argA argB) skolem o where
  getSkolemizedFunctionFromAU (CTOR.Gain argA argB) = toSkolemizedFunction argB

instance getSkolemizedFunctionFromAUHighpass :: ToSkolemizedFunction argC skolem o => GetSkolemizedFunctionFromAU (CTOR.Highpass argA argB argC) skolem o where
  getSkolemizedFunctionFromAU (CTOR.Highpass argA argB argC) = toSkolemizedFunction argC

instance getSkolemizedFunctionFromAUHighshelf :: ToSkolemizedFunction argC skolem o => GetSkolemizedFunctionFromAU (CTOR.Highshelf argA argB argC) skolem o where
  getSkolemizedFunctionFromAU (CTOR.Highshelf argA argB argC) = toSkolemizedFunction argC

instance getSkolemizedFunctionFromAULowpass :: ToSkolemizedFunction argC skolem o => GetSkolemizedFunctionFromAU (CTOR.Lowpass argA argB argC) skolem o where
  getSkolemizedFunctionFromAU (CTOR.Lowpass argA argB argC) = toSkolemizedFunction argC

instance getSkolemizedFunctionFromAULowshelf :: ToSkolemizedFunction argC skolem o => GetSkolemizedFunctionFromAU (CTOR.Lowshelf argA argB argC) skolem o where
  getSkolemizedFunctionFromAU (CTOR.Lowshelf argA argB argC) = toSkolemizedFunction argC

instance getSkolemizedFunctionFromAUNotch :: ToSkolemizedFunction argC skolem o => GetSkolemizedFunctionFromAU (CTOR.Notch argA argB argC) skolem o where
  getSkolemizedFunctionFromAU (CTOR.Notch argA argB argC) = toSkolemizedFunction argC

instance getSkolemizedFunctionFromAUPeaking :: ToSkolemizedFunction argD skolem o => GetSkolemizedFunctionFromAU (CTOR.Peaking argA argB argC argD) skolem o where
  getSkolemizedFunctionFromAU (CTOR.Peaking argA argB argC argD) = toSkolemizedFunction argD

instance getSkolemizedFunctionFromAURecorder :: ToSkolemizedFunction argB skolem o => GetSkolemizedFunctionFromAU (CTOR.Recorder argA argB) skolem o where
  getSkolemizedFunctionFromAU (CTOR.Recorder argA argB) = toSkolemizedFunction argB

instance getSkolemizedFunctionFromAUSpeaker :: ToSkolemizedFunction argA skolem o => GetSkolemizedFunctionFromAU (CTOR.Speaker argA) skolem o where
  getSkolemizedFunctionFromAU (CTOR.Speaker argA) = toSkolemizedFunction argA

instance getSkolemizedFunctionFromAUStereoPanner :: ToSkolemizedFunction argB skolem o => GetSkolemizedFunctionFromAU (CTOR.StereoPanner argA argB) skolem o where
  getSkolemizedFunctionFromAU (CTOR.StereoPanner argA argB) = toSkolemizedFunction argB

instance getSkolemizedFunctionFromAUWaveShaper :: ToSkolemizedFunction argC skolem o => GetSkolemizedFunctionFromAU (CTOR.WaveShaper argA argB argC) skolem o where
  getSkolemizedFunctionFromAU (CTOR.WaveShaper argA argB argC) = toSkolemizedFunction argC
