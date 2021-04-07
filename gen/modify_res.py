A={'Allpass':1, 'Bandpass':1, 'Constant':0, 'Convolver':1, 'Delay':1, 'DynamicsCompressor':1, 'Gain':2, 'Highpass':1, 'Highshelf':1, 'LoopBuf':0, 'Lowpass':1, 'Lowshelf':1, 'Microphone':0, 'Notch':1, 'Peaking':1, 'PeriodicOsc':0, 'PlayBuf':0, 'Recorder':1, 'SawtoothOsc':0, 'SinOsc':0, 'Speaker':2, 'SquareOsc':0, 'StereoPanner':1, 'TriangleOsc':0, 'WaveShaper':1}

def mr(s):
  return f'''else instance modifyRes{s} :: ModifyRes (CTOR.{s} a) ptr (NodeC (AU.T{s} ptr) edge) (NodeListCons (NodeC (AU.T{s} ptr) edge) NodeListNil) edge'''

for x in A:
  print(mr(x))