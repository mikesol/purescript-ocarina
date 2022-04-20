module WAGS.Tumult.Graph.Oversample where

import WAGS.Tumult.Graph.AudioUnit as CTOR
import WAGS.Core (Oversample, _fourX, _none, _twoX)

-- | Class to determine if a type is an oversample directive.
class IsOversample oversample where
  reflectOversample :: oversample -> Oversample

instance isOversampleNone :: IsOversample CTOR.OversampleNone where
  reflectOversample _ = _none

instance isOversampleTwoX :: IsOversample CTOR.OversampleTwoX where
  reflectOversample _ = _twoX

instance isOversampleFourX :: IsOversample CTOR.OversampleFourX where
  reflectOversample _ = _fourX

-- | Typelevel variant of IsOversample
class IsOversampleT (oversample :: Type)

instance isOversampleTNone :: IsOversampleT CTOR.OversampleNone

instance isOversampleTTwoX :: IsOversampleT CTOR.OversampleTwoX

instance isOversampleTFourX :: IsOversampleT CTOR.OversampleFourX
