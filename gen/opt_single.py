def ofilt(s,opt):
  l = s.lower()
  return f'''data {s}'
  = {s}'

instance convert{s}Param :: (InitialVal a, SetterVal a) => ConvertOption {s}' "{opt}" a GetSetAP where
  convertOption _ _ gvsv = Tuple (initialVal gvsv) (setterVal gvsv)

type {s}Optional :: Row Type
type {s}Optional
  = ( )

type {s}All
  = ( {opt} :: GetSetAP
    | {s}Optional
    )

default{s} :: {{ | {s}Optional }}
default{s} = {{ }}

{l} ::
  forall provided.
  ConvertOptionsWithDefaults {s}' {{ | {s}Optional }} {{ | provided }} {{ | {s}All }} =>
  {{ | provided }} ->
  CTOR.{s} GetSetAP
{l} provided = CTOR.{s} all.{opt}
  where
  all :: {{ | {s}All }}
  all = convertOptionsWithDefaults {s}' default{s} provided
'''

print(ofilt('Constant', 'val'))