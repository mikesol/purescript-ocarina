-- | This module is used internally to create and use skolem variables in an audio graph.
-- | To learn more about skolemnization, check out the [Skolem normal form](https://en.wikipedia.org/wiki/Skolem_normal_form) wiki.
module WAGS.Universe.Skolems where

import Prelude
import Data.Typelevel.Bool (False)
import Type.Proxy (Proxy)
import WAGS.Graph.Constructors as CTOR
import WAGS.Universe.Bin (class PtrListKeepSingleton, Ptr, PtrList, PtrListCons, PtrListNil)
import WAGS.Util (class Gate, class TypeEqualTF)

-- | A skolem variable that is unified with a pointer.
data SkolemPair

-- | The unique constructor for a `SkolemPair`, accepting a skolem variable and a pointer in the audio graph.
foreign import data SkolemPairC :: Type -> Ptr -> SkolemPair

-- | A list of known skolem variables in the audio graph.
data SkolemList

-- | Cons for a `SkolemList`
foreign import data SkolemListCons :: SkolemPair -> SkolemList -> SkolemList

-- | Nil for a `SkolemList`
foreign import data SkolemListNil :: SkolemList

-- | A skolem variable that is trivial and can be discarded, meaning it will never be used.
data DiscardableSkolem

-- | Tail-recursive algorithm to look up a skolem variable in the substitution map.
class LookupSkolem' (accumulator :: PtrList) (skolem :: Type) (skolemList :: SkolemList) (ptr :: PtrList) | accumulator skolem skolemList -> ptr

instance lookupSkolemNil :: LookupSkolem' accumulator ptr SkolemListNil accumulator

instance lookupSkolemCons ::
  ( TypeEqualTF skolem candidate tf
  , Gate tf (PtrListCons ptr PtrListNil) PtrListNil toComp
  , PtrListKeepSingleton toComp accumulator acc
  , LookupSkolem' acc skolem tail o
  ) =>
  LookupSkolem' accumulator skolem (SkolemListCons (SkolemPairC candidate ptr) tail) o

-- | Class to look up a skolem variable in the substitution map.
class LookupSkolem (skolem :: Type) (skolemList :: SkolemList) (ptr :: Ptr) | skolem skolemList -> ptr

instance lookupSkolem :: (LookupSkolem' PtrListNil skolem skolemList (PtrListCons ptr PtrListNil)) => LookupSkolem skolem skolemList ptr

instance skolemNotYetPresentNil :: SkolemNotYetPresent skolem SkolemListNil

instance skolemNotYetPresentCons ::
  ( TypeEqualTF skolem candidate False
  , SkolemNotYetPresentOrDiscardable skolem tail
  ) =>
  SkolemNotYetPresent skolem (SkolemListCons (SkolemPairC candidate ptr) tail)

-- | Assertion that a skolem is not yet present in a skolem lists. Makes sure each skolem variable is fresh/unique.
class SkolemNotYetPresent (skolem :: Type) (skolemList :: SkolemList)

-- | Assertion that a skolem is not yet present in a skolem lists _or_ that it is discardable.
class SkolemNotYetPresentOrDiscardable (skolem :: Type) (skolemList :: SkolemList)

instance skolemNotYetPresentOrDiscardableD :: SkolemNotYetPresentOrDiscardable DiscardableSkolem skolemList
else instance skolemNotYetPresentOrDiscardableO :: SkolemNotYetPresent o skolemList => SkolemNotYetPresentOrDiscardable o skolemList

-- | Appends `skolem` and `ptr` to `skolems`, creating `skolemsInternal`. The `DiscardableSkolem` is discarded in this append operation.
class MakeInternalSkolemStack (skolem :: Type) (ptr :: Ptr) (skolems :: SkolemList) (skolemsInternal :: SkolemList) | skolem ptr skolems -> skolemsInternal

instance makeInternalSkolemStackDiscardable :: MakeInternalSkolemStack DiscardableSkolem ptr skolems skolems
else instance makeInternalSkolemStack :: MakeInternalSkolemStack skolem ptr skolems (SkolemListCons (SkolemPairC skolem ptr) skolems)

-- | Gets a skolem variable from a function with a single proxy argument, treating the proxied type as the skolem variable.
class GetSkolemFromRecursiveArgument (a :: Type) (skolem :: Type) | a -> skolem

instance getSkolemFromRecursiveArgumentF :: GetSkolemFromRecursiveArgument ((Proxy skolem) -> b) skolem
else instance getSkolemFromRecursiveArgumentC :: GetSkolemFromRecursiveArgument b DiscardableSkolem

-- | Coerces a term to a skolemized function. It is either already a skolemized function or a constant, in which case it is coerced to a skolemized function that uses the `DiscardableSkolem`.
class ToSkolemizedFunction (a :: Type) (skolem :: Type) (b :: Type) | a skolem -> b where
  toSkolemizedFunction :: a -> (Proxy skolem -> b)

instance toSkolemizedFunctionFunction :: ToSkolemizedFunction (Proxy skolem -> b) skolem b where
  toSkolemizedFunction = identity
else instance toSkolemizedFunctionConst :: ToSkolemizedFunction b skolem b where
  toSkolemizedFunction = const

-- | Gets a skolem variable from a specific audio unit `a`.
class GetSkolemizedFunctionFromAU (a :: Type) (skolem :: Type) (b :: Type) | a skolem -> b where
  getSkolemizedFunctionFromAU :: a -> (Proxy skolem -> b)

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
