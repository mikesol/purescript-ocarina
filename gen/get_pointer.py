A={'Allpass':1, 'Bandpass':1, 'Constant':0, 
'Convolver':1, 
'Delay':1, 'DynamicsCompressor':1,

'Gain':2,
'Highpass':1, 'Highshelf':1, 'LoopBuf':0, 'Lowpass':1, 'Lowshelf':1,

'Microphone':0,
'Notch':1, 'Peaking':1, 'PeriodicOsc':0, 'PlayBuf':0,
'Recorder':1,
'SawtoothOsc':0, 'SinOsc':0, 'Speaker':2, 'SquareOsc':0, 'StereoPanner':1, 'TriangleOsc':0,  'WaveShaper':1
}

for key in A.keys():
  print('instance getPointer%s :: GetPointer (T%s ptr) ptr' % (key, key))