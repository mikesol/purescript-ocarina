module WAGS.Universe.AudioUnit where

import Data.Typelevel.Bool (False, True)
import WAGS.Universe.Bin (Ptr)

-- | A single audio unit at the type level.
data AudioUnit

-- | A list of audio units at the type level.
data AudioUnitList

-- | Cons for an audio unit list.
foreign import data AudioUnitCons :: AudioUnit -> AudioUnitList -> AudioUnitList

-- | Nil for an audio unit list.
foreign import data AudioUnitNil :: AudioUnitList

-- | A reference to an audio unit. This is the return value of `cursor`.
data AudioUnitRef (ptr :: Ptr)
  = AudioUnitRef Int

-- | Class to get a pointer from an audio unit.
class GetPointer (audioUnit :: AudioUnit) (ptr :: Ptr) | audioUnit -> ptr

instance getPointerAllpass :: GetPointer (TAllpass ptr) ptr

instance getPointerBandpass :: GetPointer (TBandpass ptr) ptr

instance getPointerConstant :: GetPointer (TConstant ptr) ptr

instance getPointerConvolver :: GetPointer (TConvolver ptr) ptr

instance getPointerDelay :: GetPointer (TDelay ptr) ptr

instance getPointerDynamicsCompressor :: GetPointer (TDynamicsCompressor ptr) ptr

instance getPointerGain :: GetPointer (TGain ptr) ptr

instance getPointerHighpass :: GetPointer (THighpass ptr) ptr

instance getPointerHighshelf :: GetPointer (THighshelf ptr) ptr

instance getPointerLoopBuf :: GetPointer (TLoopBuf ptr) ptr

instance getPointerLowpass :: GetPointer (TLowpass ptr) ptr

instance getPointerLowshelf :: GetPointer (TLowshelf ptr) ptr

instance getPointerMicrophone :: GetPointer (TMicrophone ptr) ptr

instance getPointerNotch :: GetPointer (TNotch ptr) ptr

instance getPointerPeaking :: GetPointer (TPeaking ptr) ptr

instance getPointerPeriodicOsc :: GetPointer (TPeriodicOsc ptr) ptr

instance getPointerPlayBuf :: GetPointer (TPlayBuf ptr) ptr

instance getPointerRecorder :: GetPointer (TRecorder ptr) ptr

instance getPointerSawtoothOsc :: GetPointer (TSawtoothOsc ptr) ptr

instance getPointerSinOsc :: GetPointer (TSinOsc ptr) ptr

instance getPointerSpeaker :: GetPointer (TSpeaker ptr) ptr

instance getPointerSquareOsc :: GetPointer (TSquareOsc ptr) ptr

instance getPointerStereoPanner :: GetPointer (TStereoPanner ptr) ptr

instance getPointerTriangleOsc :: GetPointer (TTriangleOsc ptr) ptr

instance getPointerWaveShaper :: GetPointer (TWaveShaper ptr) ptr

-- | Type-level constructor for an allpass filter.
foreign import data TAllpass :: Ptr -> AudioUnit

-- | Type-level constructor for a bandpass filter.
foreign import data TBandpass :: Ptr -> AudioUnit

-- | Type-level constructor for a constant value.
foreign import data TConstant :: Ptr -> AudioUnit

-- | Type-level constructor for a convolver, aka reverb.
foreign import data TConvolver :: Ptr -> AudioUnit

-- | Type-level constructor for a delay unit.
foreign import data TDelay :: Ptr -> AudioUnit

-- | Type-level constructor for a compressor.
foreign import data TDynamicsCompressor :: Ptr -> AudioUnit

-- | Type-level constructor for a gain unit.
foreign import data TGain :: Ptr -> AudioUnit

-- | Type-level constructor for a highpass filter.
foreign import data THighpass :: Ptr -> AudioUnit

-- | Type-level constructor for a highshelf filter.
foreign import data THighshelf :: Ptr -> AudioUnit

-- | Type-level constructor for a looping buffer.
foreign import data TLoopBuf :: Ptr -> AudioUnit

-- | Type-level constructor for a lowpass filter.
foreign import data TLowpass :: Ptr -> AudioUnit

-- | Type-level constructor for a lowshelf filter.
foreign import data TLowshelf :: Ptr -> AudioUnit

-- | Type-level constructor for a microphone.
foreign import data TMicrophone :: Ptr -> AudioUnit

-- | Type-level constructor for a notch filter.
foreign import data TNotch :: Ptr -> AudioUnit

-- | Type-level constructor for a peaking filter.
foreign import data TPeaking :: Ptr -> AudioUnit

-- | Type-level constructor for a periodic oscillator.
foreign import data TPeriodicOsc :: Ptr -> AudioUnit

-- | Type-level constructor for playback from a buffer.
foreign import data TPlayBuf :: Ptr -> AudioUnit

-- | Type-level constructor for a recorder.
foreign import data TRecorder :: Ptr -> AudioUnit

-- | Type-level constructor for a sawtooth oscillator.
foreign import data TSawtoothOsc :: Ptr -> AudioUnit

-- | Type-level constructor for a sine-wave oscillator.
foreign import data TSinOsc :: Ptr -> AudioUnit

-- | Type-level constructor for a loudspeaker.
foreign import data TSpeaker :: Ptr -> AudioUnit

-- | Type-level constructor for a square-wave oscillator.
foreign import data TSquareOsc :: Ptr -> AudioUnit

-- | Type-level constructor for a stereo panner.
foreign import data TStereoPanner :: Ptr -> AudioUnit

-- | Type-level constructor for a triangle oscillator.
foreign import data TTriangleOsc :: Ptr -> AudioUnit

-- | Type-level constructor for a wave shaper.
foreign import data TWaveShaper :: Ptr -> AudioUnit

-- | Class that takes `AudioUnit`-s `a` and `b` and outputs `True` as `tf` if they are equal else `False`.
class AudioUnitEq (a :: AudioUnit) (b :: AudioUnit) (tf :: Type) | a b -> tf

instance audioUnitEqTAllpass :: AudioUnitEq (TAllpass idx) (TAllpass idx) True
else instance audioUnitEqTBandpass :: AudioUnitEq (TBandpass idx) (TBandpass idx) True
else instance audioUnitEqTConstant :: AudioUnitEq (TConstant idx) (TConstant idx) True
else instance audioUnitEqTConvolver :: AudioUnitEq (TConvolver idx) (TConvolver idx) True
else instance audioUnitEqTDelay :: AudioUnitEq (TDelay idx) (TDelay idx) True
else instance audioUnitEqTDynamicsCompressor :: AudioUnitEq (TDynamicsCompressor idx) (TDynamicsCompressor idx) True
else instance audioUnitEqTGain :: AudioUnitEq (TGain idx) (TGain idx) True
else instance audioUnitEqTHighpass :: AudioUnitEq (THighpass idx) (THighpass idx) True
else instance audioUnitEqTHighshelf :: AudioUnitEq (THighshelf idx) (THighshelf idx) True
else instance audioUnitEqTLoopBuf :: AudioUnitEq (TLoopBuf idx) (TLoopBuf idx) True
else instance audioUnitEqTLowpass :: AudioUnitEq (TLowpass idx) (TLowpass idx) True
else instance audioUnitEqTLowshelf :: AudioUnitEq (TLowshelf idx) (TLowshelf idx) True
else instance audioUnitEqTMicrophone :: AudioUnitEq (TMicrophone idx) (TMicrophone idx) True
else instance audioUnitEqTNotch :: AudioUnitEq (TNotch idx) (TNotch idx) True
else instance audioUnitEqTPeaking :: AudioUnitEq (TPeaking idx) (TPeaking idx) True
else instance audioUnitEqTPeriodicOsc :: AudioUnitEq (TPeriodicOsc idx) (TPeriodicOsc idx) True
else instance audioUnitEqTPlayBuf :: AudioUnitEq (TPlayBuf idx) (TPlayBuf idx) True
else instance audioUnitEqTRecorder :: AudioUnitEq (TRecorder idx) (TRecorder idx) True
else instance audioUnitEqTSawtoothOsc :: AudioUnitEq (TSawtoothOsc idx) (TSawtoothOsc idx) True
else instance audioUnitEqTSinOsc :: AudioUnitEq (TSinOsc idx) (TSinOsc idx) True
else instance audioUnitEqTSpeaker :: AudioUnitEq (TSpeaker idx) (TSpeaker idx) True
else instance audioUnitEqTSquareOsc :: AudioUnitEq (TSquareOsc idx) (TSquareOsc idx) True
else instance audioUnitEqTStereoPanner :: AudioUnitEq (TStereoPanner idx) (TStereoPanner idx) True
else instance audioUnitEqTTriangleOsc :: AudioUnitEq (TTriangleOsc idx) (TTriangleOsc idx) True
else instance audioUnitEqTWaveShaper :: AudioUnitEq (TWaveShaper idx) (TWaveShaper idx) True
else instance audioUnitNotEq :: AudioUnitEq a b False
