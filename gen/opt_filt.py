def ofilt(s,opt):
  l = s.lower()
  return f'''data {s}'
  = {s}'

instance convert{s}Frequency :: (InitialVal a, SetterVal a) => ConvertOption {s}' "freq" a GetSetAP where
  convertOption _ _ gvsv = Tuple (initialVal gvsv) (setterVal gvsv)

instance convert{s}Q :: (InitialVal a, SetterVal a) => ConvertOption {s}' "{opt}" a GetSetAP where
  convertOption _ _ gvsv = Tuple (initialVal gvsv) (setterVal gvsv)

type {s}Optional
  = ( {opt} :: GetSetAP )

type {s}All
  = ( freq :: GetSetAP
    | {s}Optional
    )

default{s} :: {{ | {s}Optional }}
default{s} = {{ {opt}: defaultGetSetAP 1.0 }}

class {s}Ctor i {l} | i -> {l} where
  {l} :: i -> {l}

instance {l}Ctor1 ::
  ( IsAudioOrF c
  , ConvertOptionsWithDefaults {s}' {{ | {s}Optional }} {{ | provided }} {{ | {s}All }}
  ) =>
  {s}Ctor {{ | provided }} (c -> CTOR.{s} GetSetAP GetSetAP c) where
  {l} provided cont = CTOR.{s} all.freq all.{opt} cont
    where
    all :: {{ | {s}All }}
    all = convertOptionsWithDefaults {s}' default{s} provided
else instance {l}Ctor2 :: (InitialVal a, SetterVal a, IsAudioOrF b) => {s}Ctor a (b -> (CTOR.{s} GetSetAP GetSetAP b)) where
  {l} a cont = CTOR.{s} (Tuple (initialVal a) (setterVal a)) default{s}.{opt} cont
'''

print(ofilt('Peaking', 'q'))