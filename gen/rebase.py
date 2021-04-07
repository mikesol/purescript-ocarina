A={'Allpass':1, 'Bandpass':1, 'Constant':0, 'Convolver':1, 'Delay':1, 'Dup':3, 'DynamicsCompressor':1, 'Gain':2, 'Highpass':1, 'Highshelf':1, 'LoopBuf':0, 'Lowpass':1, 'Lowshelf':1, 'Microphone':0, 'Notch':1, 'Peaking':1, 'PeriodicOsc':0, 'PlayBuf':0, 'Recorder':1, 'SawtoothOsc':0, 'SinOsc':0, 'Speaker':2, 'SquareOsc':0, 'StereoPanner':1, 'TriangleOsc':0, 'WaveShaper':1}

def _0(s):
  return f'''instance rebase{s} ::
  (BinToInt ptrA, BinToInt ptrB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.T{s} ptrA) NoEdge) iA (NodeC (AU.T{s} ptrB) NoEdge) iB where
  rebase' _ _ _ _ _ _ _ = Frame (rebaseAudioUnit (Proxy :: _ ptrA) (Proxy :: _ ptrB))'''

def _1(s):
  return f'''instance rebase{s} ::
  (BinToInt ptrA, BinToInt ptrB, Rebase' rblA rblB RebaseProof eA iA eB iB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.T{s} ptrA) eA) iA (NodeC (AU.T{s} ptrB) eB) iB where
  rebase' _ _ _ _ _ _ _ =
    Frame
      $ append <$> rebaseAudioUnit (Proxy :: _ ptrA) (Proxy :: _ ptrB) <*> rest
    where
    Frame rest = rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ eA) (Proxy :: _ iA) (Proxy :: _ eB) (Proxy :: _ iB)
'''

def _2(s):
  return f'''instance rebase{s} ::
  (BinToInt ptrA, BinToInt ptrB, Rebase' rblA rblB RebaseProof eA iA eB iB) =>
  Rebase' rblA rblB RebaseProof (NodeC (AU.T{s} ptrA) eA) iA (NodeC (AU.T{s} ptrB) eB) iB where
  rebase' _ _ _ _ _ _ _ =
    Frame
      $ append <$> rebaseAudioUnit (Proxy :: _ ptrA) (Proxy :: _ ptrB) <*> rest
    where
    Frame rest = rebase' (Proxy :: _ rblA) (Proxy :: _ rblB) RebaseProof (Proxy :: _ eA) (Proxy :: _ iA) (Proxy :: _ eB) (Proxy :: _ iB)'''

d=dict(_0=_0,_1=_1,_2=_2,_3=lambda x:'')

for key,value in A.items():
  print(d['_'+str(value)](key))