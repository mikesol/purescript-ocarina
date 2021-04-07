A={'Allpass':1, 'Bandpass':1, 'Constant':0, 'Convolver':1, 'Delay':1, 'Dup':3, 'DynamicsCompressor':1, 'Gain':2, 'Highpass':1, 'Highshelf':1, 'LoopBuf':0, 'Lowpass':1, 'Lowshelf':1, 'Microphone':0, 'Notch':1, 'Peaking':1, 'PeriodicOsc':0, 'PlayBuf':0, 'Recorder':1, 'SawtoothOsc':0, 'SinOsc':0, 'Speaker':2, 'SquareOsc':0, 'StereoPanner':1, 'TriangleOsc':0, 'WaveShaper':1}
B ={'Allpass':3, 'Bandpass':3, 'Constant':1, 'Convolver':2, 'Delay':2, 'Dup':-1, 'DynamicsCompressor':6, 'Gain':-1, 'Highpass':3, 'Highshelf':3, 'LoopBuf':4, 'Lowpass':3, 'Lowshelf':3, 'Microphone':1, 'Notch':3, 'Peaking':4, 'PeriodicOsc':1, 'PlayBuf':3, 'Recorder':2, 'SawtoothOsc':1, 'SinOsc':1, 'Speaker':-1, 'SquareOsc':1, 'StereoPanner':2, 'TriangleOsc':1, 'WaveShaper':3}

args = 'argA argB argC argD argE argF argG argH'.split(' ')

def _0(s):
  return f'''instance cursor{s} ::
  BinToInt p =>
  CursorI (SingleEdge p) (CTOR.{s} {' '.join(args[:B[s]])}) inuniv PtrListNil'''

def toF(s): return 'fOf'+s
def lastToF(s): return s[:-1]+['fOf'+s[-1]]

def _1(s):
  return f'''instance cursor{s} ::
  ( BinToInt p
  , GetSkolemFromRecursiveArgument {toF(args[:B[s]][-1])} skolem
  , ToSkolemizedFunction {toF(args[:B[s]][-1])} skolem {args[:B[s]][-1]}
  , CursorX (CTOR.{s} {' '.join(args[:B[s]])}) p inuniv nextP
  , CursorI nextP {args[:B[s]][-1]} inuniv o
  ) =>
  CursorI (SingleEdge p) (CTOR.{s} {' '.join(lastToF(args[:B[s]]))}) inuniv o'''

def _2(s):
  return ''

d=dict(_0=_0,_1=_1,_2=_2,_3=lambda x:'')

for key,value in A.items():
  print(d['_'+str(value)](key))