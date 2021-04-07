A={'Allpass':1, 'Bandpass':1, 'Constant':0, 'Convolver':1, 'Delay':1, 'DynamicsCompressor':1, 'Gain':2, 'Highpass':1, 'Highshelf':1, 'LoopBuf':0, 'Lowpass':1, 'Lowshelf':1, 'Microphone':0, 'Notch':1, 'Peaking':1, 'PeriodicOsc':0, 'PlayBuf':0, 'Recorder':1, 'SawtoothOsc':0, 'SinOsc':0, 'Speaker':2, 'SquareOsc':0, 'StereoPanner':1, 'TriangleOsc':0, 'WaveShaper':1}
ARITY ={'Allpass':3, 'Bandpass':3, 'Constant':1, 'Convolver':2, 'Delay':2, 'Dup':-1, 'DynamicsCompressor':6, 'Gain':2, 'Highpass':3, 'Highshelf':3, 'LoopBuf':2, 'Lowpass':3, 'Lowshelf':3, 'Microphone':1, 'Notch':3, 'Peaking':4, 'PeriodicOsc':2, 'PlayBuf':2, 'Recorder':2, 'SawtoothOsc':1, 'SinOsc':1, 'Speaker':1, 'SquareOsc':1, 'StereoPanner':2, 'TriangleOsc':1, 'WaveShaper':3}
SV={'Allpass':(0,1), 'Bandpass':(0,1), 'Constant':(0,), 'Convolver':(), 'Delay':(0,), 'Dup':(), 'DynamicsCompressor':(0,1,2,3,4), 'Gain':(0,), 'Highpass':(0,1), 'Highshelf':(0,1), 'LoopBuf':(1,), 'Lowpass':(0,1), 'Lowshelf':(0,1), 'Microphone':(), 'Notch':(0,1), 'Peaking':(0,1,2), 'PeriodicOsc':(1,), 'PlayBuf':(1,), 'Recorder':(), 'SawtoothOsc':(0,), 'SinOsc':(0,), 'Speaker':(), 'SquareOsc':(0,), 'StereoPanner':(0,), 'TriangleOsc':(0,), 'WaveShaper':()}
STR={'Allpass':dict(_0='SetQ',_1='SetGain'), 'Bandpass':dict(_0='SetFrequency',_1='SetQ'), 'Constant':dict(_0='SetOffset'), 'Convolver':dict(), 'Delay':dict(_0='SetDelay'), 'Dup':dict(), 'DynamicsCompressor':dict(_0='SetThreshold',_1='SetKnee',_2='SetRatio',_3='SetAttack',_4='SetRelease'), 'Gain':dict(_0='SetGain'), 'Highpass':dict(_0='SetFrequency',_1='SetQ'), 'Highshelf':dict(_0='SetFrequency',_1='SetGain'), 'LoopBuf':dict(_1='SetPlaybackRate'), 'Lowpass':dict(_0='SetFrequency',_1='SetQ'), 'Lowshelf':dict(_0='SetFrequency',_1='SetGain'), 'Microphone':dict(), 'Notch':dict(_0='SetFrequency',_1='SetQ'), 'Peaking':dict(_0='SetFrequency',_1='SetQ',_2='SetGain'), 'PeriodicOsc':dict(_1='SetFrequency'), 'PlayBuf':dict(_1='SetPlaybackRate'), 'Recorder':(), 'SawtoothOsc':dict(_0='SetFrequency'), 'SinOsc':dict(_0='SetFrequency'), 'Speaker':dict(), 'SquareOsc':dict(_0='SetFrequency'), 'StereoPanner':dict(_0='SetPan'), 'TriangleOsc':dict(_0='SetFrequency'), 'WaveShaper':dict()}
args = 'argA argB argC argD argE argF argG argH'.split(' ')

def change_instructions(s):
  if len(SV[s]) == 0:
    return f'''instance creationInstructions{s} :: CreationInstructions (CTOR.{s} {' '.join(args[:ARITY[s]])}) where
  creationInstructions _ _ = [] /\ ASpeaker'''
  return f'''instance creationInstructions{s} :: ({', '.join(['InitialVal '+y for x,y in enumerate(args[:ARITY[s]]) if x in SV[s] ])}) => CreationInstructions (CTOR.{s} {' '.join(args[:ARITY[s]])}) where
  creationInstructions idx (CTOR.{s} {' '.join([y if x in SV[s] else '_' for x,y in enumerate(args[:ARITY[s]])])}) = 
      let
{chr(10).join([f"""        

        {y}_iv' = initialVal {y}

        {y}_StartsWith = let AudioParameter {y}_iv = {y}_iv' in [ {STR[s]['_'+str(x)]} idx {y}_iv.param {y}_iv.timeOffset {y}_iv.transition ]""" for x,y in enumerate(args[:ARITY[s]]) if x in SV[s]] ) }
      in
        ([NewUnit idx "{s.lower()}"] <> {' <> '.join([y+'_StartsWith' for x,y in enumerate(args[:ARITY[s]]) if x in SV[s] ])})
          /\ A{s} {' '.join([y+"_iv'" for x,y in enumerate(args[:ARITY[s]]) if x in SV[s] ])}
'''

for key,value in A.items():
  print(change_instructions(key))