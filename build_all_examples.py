import os
import subprocess
examples = os.listdir('examples')

def as_ps(name):
  return ''.join(x.title() for x in name.split('-'))

for example in examples:
  subprocess.run(
    f'EXAMPLE={example} spago -x examples.dhall bundle-app --main Ocarina.Example.{as_ps(example)} --to examples/{example}/index.js', shell=True, check=True
  )
