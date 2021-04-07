A={'Allpass':1, 'Bandpass':1, 'Constant':0, 'Convolver':1, 'Delay':1, 'Dup':3, 'DynamicsCompressor':1, 'Gain':2, 'Highpass':1, 'Highshelf':1, 'LoopBuf':0, 'Lowpass':1, 'Lowshelf':1, 'Microphone':0, 'Notch':1, 'Peaking':1, 'PeriodicOsc':0, 'PlayBuf':0, 'Recorder':1, 'SawtoothOsc':0, 'SinOsc':0, 'Speaker':2, 'SquareOsc':0, 'StereoPanner':1, 'TriangleOsc':0, 'WaveShaper':1}

def _0(s):
  return f'''instance pointerNotConnected{s} :: PointerNotConnected ptr (NodeC (T{s} x) NoEdge)
'''

def _1(s):
  return f'''instance pointerNotConnected_NE_{s} :: PointerNotConnected ptr (NodeC (T{s} x) NoEdge)

instance pointerNotConnected_SE_{s} :: BinEq ptr y False => PointerNotConnected ptr (NodeC (T{s} x) (SingleEdge y))'''

def _2(s):
  return f'''instance pointerNotConnected_NE_{s} :: PointerNotConnected ptr (NodeC (T{s} x) NoEdge)

instance pointerNotConnected_SE_{s} :: BinEq ptr y False => PointerNotConnected ptr (NodeC (T{s} x) (SingleEdge y))

instance pointerNotConnected_ME_{s} :: PtrNotInPtrList ptr (PtrListCons y z) => PointerNotConnected ptr (NodeC (T{s} x) (ManyEdges y z))'''

d=dict(_0=_0,_1=_1,_2=_2,_3=lambda x:'')

for key,value in A.items():
  print(d['_'+str(value)](key))