A = '''  connectXToY :: Int -> Int -> audio -> engine audio
  disconnectXFromY :: Int -> Int -> audio -> engine audio
  destroyUnit :: Int -> audio -> engine audio
  rebaseAllUnits :: Array { from :: Int, to :: Int } -> audio -> engine audio
  makeAllpass :: Int -> AudioParameter -> AudioParameter -> audio -> engine audio
  makeBandpass :: Int -> AudioParameter -> AudioParameter -> audio -> engine audio
  makeConstant :: Int -> AudioParameter -> audio -> engine audio
  makeConvolver :: Int -> String -> audio -> engine audio
  makeDelay :: Int -> AudioParameter -> audio -> engine audio
  makeDynamicsCompressor :: Int -> AudioParameter -> AudioParameter -> AudioParameter -> AudioParameter -> AudioParameter -> audio -> engine audio
  makeGain :: Int -> AudioParameter -> audio -> engine audio
  makeHighpass :: Int -> AudioParameter -> AudioParameter -> audio -> engine audio
  makeHighshelf :: Int -> AudioParameter -> AudioParameter -> audio -> engine audio
  makeLoopBuf :: Int -> String -> AudioParameter -> Number -> Number -> audio -> engine audio
  makeLowpass :: Int -> AudioParameter -> AudioParameter -> audio -> engine audio
  makeLowshelf :: Int -> AudioParameter -> AudioParameter -> audio -> engine audio
  makeMicrophone :: Int -> audio -> engine audio
  makeNotch :: Int -> AudioParameter -> AudioParameter -> audio -> engine audio
  makePeaking :: Int -> AudioParameter -> AudioParameter -> AudioParameter -> audio -> engine audio
  makePeriodicOsc :: Int -> String -> AudioParameter -> audio -> engine audio
  makePlayBuf :: Int -> String -> Number -> AudioParameter -> audio -> engine audio
  makeRecorder :: Int -> String -> audio -> engine audio
  makeSawtoothOsc :: Int -> AudioParameter -> audio -> engine audio
  makeSinOsc :: Int -> AudioParameter -> audio -> engine audio
  makeSquareOsc :: Int -> AudioParameter -> audio -> engine audio
  makeStereoPanner :: Int -> AudioParameter -> audio -> engine audio
  makeTriangleOsc :: Int -> AudioParameter -> audio -> engine audio
  makeWaveShaper :: Int -> String -> Oversample -> audio -> engine audio
  setRatio :: Int -> AudioParameter -> audio -> engine audio
  setOffset :: Int -> AudioParameter -> audio -> engine audio
  setAttack :: Int -> AudioParameter -> audio -> engine audio
  setGain :: Int -> AudioParameter -> audio -> engine audio
  setQ :: Int -> AudioParameter -> audio -> engine audio
  setPan :: Int -> AudioParameter -> audio -> engine audio
  setThreshold :: Int -> AudioParameter -> audio -> engine audio
  setRelease :: Int -> AudioParameter -> audio -> engine audio
  setKnee :: Int -> AudioParameter -> audio -> engine audio
  setDelay :: Int -> AudioParameter -> audio -> engine audio
  setPlaybackRate :: Int -> AudioParameter -> audio -> engine audio
  setFrequency :: Int -> AudioParameter -> audio -> engine audio'''

for x in A.split('\n'):
  a = [x for x in x.split(' ') if x != '']
  print(' '.join(['  | '+a[0][0].upper()+a[0][1:]]+a[1:]).replace('-> audio -> engine audio','').replace('::','').replace('->','') )