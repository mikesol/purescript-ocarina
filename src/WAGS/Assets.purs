module WAGS.Assets where

import Prim.Row (class Cons)

type Analysers
  = "analysers"

type Buffers
  = "buffers"

type PeriodicWaves
  = "periodicWaves"

type Recorders
  = "recorders"

type FloatArrays
  = "floatArrays"

type Worklets
  = "worklets"

class AssetsHaveCons (key :: Symbol) (sym :: Symbol) (assets :: Row Type) (v :: Type) | key sym assets -> v

class AssetsHave (key :: Symbol) (sym :: Symbol) (assets :: Row Type)

instance assetsHaveAll :: AssetsHaveCons key sym assets v => AssetsHave key sym assets

instance assetsHaveAnalyser ::
  ( Cons Analysers { | analysers } assets' assets
  , Cons sym v analysers' analysers
  ) =>
  AssetsHaveCons Analysers sym assets v

instance assetsHaveBuffer ::
  ( Cons Buffers { | buffers } assets' assets
  , Cons sym v buffers' buffers
  ) =>
  AssetsHaveCons Buffers sym assets v

instance assetsHavePeriodicWave ::
  ( Cons PeriodicWaves { | periodicWaves } assets' assets
  , Cons sym v periodicWaves' periodicWaves
  ) =>
  AssetsHaveCons PeriodicWaves sym assets v

instance assetsHaveRecorder ::
  ( Cons Recorders { | recorders } assets' assets
  , Cons sym v recorders' recorders
  ) =>
  AssetsHaveCons Recorders sym assets v

instance assetsHaveFloatArray ::
  ( Cons FloatArrays { | floatArrays } assets' assets
  , Cons sym v floatArrays' floatArrays
  ) =>
  AssetsHaveCons FloatArrays sym assets v

instance assetsHaveWorklets ::
  ( Cons Worklets { | worklets } assets' assets
  , Cons sym v worklets' worklets
  ) =>
  AssetsHaveCons Worklets sym assets v