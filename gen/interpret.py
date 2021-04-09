A={'Allpass':1, 'Bandpass':1, 'Constant':0, 'Convolver':1, 'Delay':1, 'Dup':3, 'DynamicsCompressor':1, 'Gain':2, 'Highpass':1, 'Highshelf':1, 'LoopBuf':0, 'Lowpass':1, 'Lowshelf':1, 'Microphone':0, 'Notch':1, 'Peaking':1, 'PeriodicOsc':0, 'PlayBuf':0, 'Recorder':1, 'SawtoothOsc':0, 'SinOsc':0, 'Speaker':2, 'SquareOsc':0, 'StereoPanner':1, 'TriangleOsc':0, 'WaveShaper':1}
STR={'Allpass':dict(_0='SetQ',_1='SetGain'), 'Bandpass':dict(_0='SetFrequency',_1='SetQ'), 'Constant':dict(_0='SetOffset'), 'Convolver':dict(), 'Delay':dict(_0='SetDelay'), 'Dup':dict(), 'DynamicsCompressor':dict(_0='SetThreshold',_1='SetKnee',_2='SetRatio',_3='SetAttack',_4='SetRelease'), 'Gain':dict(_0='SetGain'), 'Highpass':dict(_0='SetFrequency',_1='SetQ'), 'Highshelf':dict(_0='SetFrequency',_1='SetGain'), 'LoopBuf':dict(_1='SetPlaybackRate'), 'Lowpass':dict(_0='SetFrequency',_1='SetQ'), 'Lowshelf':dict(_0='SetFrequency',_1='SetGain'), 'Microphone':dict(), 'Notch':dict(_0='SetFrequency',_1='SetQ'), 'Peaking':dict(_0='SetFrequency',_1='SetQ',_2='SetGain'), 'PeriodicOsc':dict(_1='SetFrequency'), 'PlayBuf':dict(_1='SetPlaybackRate'), 'Recorder':dict(), 'SawtoothOsc':dict(_0='SetFrequency'), 'SinOsc':dict(_0='SetFrequency'), 'Speaker':dict(), 'SquareOsc':dict(_0='SetFrequency'), 'StereoPanner':dict(_0='SetPan'), 'TriangleOsc':dict(_0='SetFrequency'), 'WaveShaper':dict()}

def camelCase(s):
  return s[0].lower() + s[1:]
for key,value in A.items():
  print('  make%s :: Int -> AudioParameter -> audio -> engine audio' % key)
  a = []
for z in STR.values():
  for zz in z.values():
    a.append(zz)
a = list(set(a))
for x in a:
  print('  %s :: Int -> AudioParameter -> audio -> engine audio' % camelCase(x))