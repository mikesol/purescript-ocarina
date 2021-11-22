module WAGS.Graph.Oversample where

import WAGS.Graph.AudioUnit as CTOR
import WAGS.Rendered (Oversample, oFourX, oNone, oTwoX)

-- | Class to determine if a type is an oversample directive.
class IsOversample oversample where
  reflectOversample :: oversample -> Oversample

instance isOversampleNone :: IsOversample CTOR.OversampleNone where
  reflectOversample _ = oNone

instance isOversampleTwoX :: IsOversample CTOR.OversampleTwoX where
  reflectOversample _ = oTwoX

instance isOversampleFourX :: IsOversample CTOR.OversampleFourX where
  reflectOversample _ = oFourX

-- | Typelevel variant of IsOversample
class IsOversampleT (oversample :: Type)

instance isOversampleTNone :: IsOversampleT CTOR.OversampleNone

instance isOversampleTTwoX :: IsOversampleT CTOR.OversampleTwoX

instance isOversampleTFourX :: IsOversampleT CTOR.OversampleFourX
