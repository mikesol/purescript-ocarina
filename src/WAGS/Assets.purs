module WAGS.Assets where

import Prim.Row (class Cons)
import Record as Record
import Type.Proxy (Proxy(..))

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

analysersP :: Proxy Analysers
analysersP = Proxy

buffersP :: Proxy Buffers
buffersP = Proxy

periodicWavesP :: Proxy PeriodicWaves
periodicWavesP = Proxy

recordersP :: Proxy Recorders
recordersP = Proxy

floatArraysP :: Proxy FloatArrays
floatArraysP = Proxy

workletsP :: Proxy Worklets
workletsP = Proxy

withAnalysers :: forall r old new. (old -> new) -> { analysers :: old | r } -> { analysers :: new | r }
withAnalysers = Record.modify analysersP
withBuffers :: forall r old new. (old -> new) -> { buffers :: old | r } -> { buffers :: new | r }
withBuffers = Record.modify buffersP
withPeriodicWaves :: forall r old new. (old -> new) -> { periodicWaves :: old | r } -> { periodicWaves :: new | r }
withPeriodicWaves = Record.modify periodicWavesP
withRecorders :: forall r old new. (old -> new) -> { recorders :: old | r } -> { recorders :: new | r }
withRecorders = Record.modify recordersP
withFloatArrays :: forall r old new. (old -> new) -> { floatArrays :: old | r } -> { floatArrays :: new | r }
withFloatArrays = Record.modify floatArraysP
withWorklets :: forall r old new. (old -> new) -> { worklets :: old | r } -> { worklets :: new | r }
withWorklets = Record.modify workletsP

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