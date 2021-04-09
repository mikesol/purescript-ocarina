module WAGS.Graph.Optionals where

import Prelude

import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Data.Tuple (Tuple(..))
import Type.Proxy (Proxy)
import WAGS.Change (class SetterVal, setterVal)
import WAGS.Create (class InitialVal, initialVal)
import WAGS.Graph.Constructors (class IsAudioOrF, toF)
import WAGS.Graph.Constructors as CTOR
import WAGS.Graph.Parameter (AudioParameter, param)

type GetSetAP
  = Tuple AudioParameter (AudioParameter -> AudioParameter)

defaultGetSetAP :: Number -> GetSetAP
defaultGetSetAP n = Tuple p (const p)
  where
  p = param n

-----------
data Allpass'
  = Allpass'

instance convertAllpassFrequency :: (InitialVal a, SetterVal a) => ConvertOption Allpass' "frequency" a GetSetAP where
  convertOption _ _ gvsv = Tuple (initialVal gvsv) (setterVal gvsv)

instance convertAllpassQ :: (InitialVal a, SetterVal a) => ConvertOption Allpass' "q" a GetSetAP where
  convertOption _ _ gvsv = Tuple (initialVal gvsv) (setterVal gvsv)

type AllpassOptional
  = ( q :: GetSetAP )

type AllpassAll
  = ( frequency :: GetSetAP
    | AllpassOptional
    )

defaultAllpass :: { | AllpassOptional }
defaultAllpass = { q: defaultGetSetAP 1.0 }

allpass ::
  forall provided fc c s.
  IsAudioOrF fc s c =>
  ConvertOptionsWithDefaults Allpass' { | AllpassOptional } { | provided } { | AllpassAll } =>
  { | provided } ->
  fc -> CTOR.Allpass GetSetAP GetSetAP (Proxy s -> c)
allpass provided cont = CTOR.Allpass all.frequency all.q (toF cont)
  where
  all :: { | AllpassAll }
  all = convertOptionsWithDefaults Allpass' defaultAllpass provided

------
data Highpass'
  = Highpass'

instance convertHighpassFrequency :: (InitialVal a, SetterVal a) => ConvertOption Highpass' "frequency" a GetSetAP where
  convertOption _ _ gvsv = Tuple (initialVal gvsv) (setterVal gvsv)

instance convertHighpassQ :: (InitialVal a, SetterVal a) => ConvertOption Highpass' "q" a GetSetAP where
  convertOption _ _ gvsv = Tuple (initialVal gvsv) (setterVal gvsv)

type HighpassOptional
  = ( q :: GetSetAP )

type HighpassAll
  = ( frequency :: GetSetAP
    | HighpassOptional
    )

defaultHighpass :: { | HighpassOptional }
defaultHighpass = { q: defaultGetSetAP 1.0 }

highpass ::
  forall provided fc c s.
  IsAudioOrF fc s c =>
  ConvertOptionsWithDefaults Highpass' { | HighpassOptional } { | provided } { | HighpassAll } =>
  { | provided } ->
  fc -> CTOR.Highpass GetSetAP GetSetAP (Proxy s -> c)
highpass provided cont = CTOR.Highpass all.frequency all.q (toF cont)
  where
  all :: { | HighpassAll }
  all = convertOptionsWithDefaults Highpass' defaultHighpass provided

------
data Gain'
  = Gain'

instance convertGainGain :: (InitialVal a, SetterVal a) => ConvertOption Gain' "gain" a GetSetAP where
  convertOption _ _ gvsv = Tuple (initialVal gvsv) (setterVal gvsv)

type GainAll
  = ( gain :: GetSetAP )

gain ::
  forall provided fb b s.
  IsAudioOrF fb s b =>
  ConvertOptionsWithDefaults Gain' { } { | provided } { | GainAll } =>
  { | provided } ->
  fb -> CTOR.Gain GetSetAP (Proxy s -> b)
gain provided cont = CTOR.Gain all.gain (toF cont)
  where
  all :: { | GainAll }
  all = convertOptionsWithDefaults Gain' {} provided