module WAGS.Assets where

import Prim.Row (class Cons)

type Buffers
  = "buffers"

type PeriodicWaves
  = "periodicWaves"

type Recorders
  = "recorders"

type FloatArrays
  = "floatArrays"

class AssetsHave (key :: Symbol) (sym :: Symbol) (assets :: Row Type)

instance assetsHaveBuffer ::
  ( Cons Buffers { | buffers } assets' assets
  , Cons sym v buffers' buffers
  ) =>
  AssetsHave Buffers sym assets

instance assetsHavePeriodicWave ::
  ( Cons PeriodicWaves { | periodicWaves } assets' assets
  , Cons sym v periodicWaves' periodicWaves
  ) =>
  AssetsHave PeriodicWaves sym assets

instance assetsHaveRecorder ::
  ( Cons Recorders { | recorders } assets' assets
  , Cons sym v recorders' recorders
  ) =>
  AssetsHave Recorders sym assets

instance assetsHaveFloatArray ::
  ( Cons FloatArrays { | floatArrays } assets' assets
  , Cons sym v floatArrays' floatArrays
  ) =>
  AssetsHave FloatArrays sym assets
