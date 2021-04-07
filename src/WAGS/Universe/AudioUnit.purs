module WAGS.Universe.AudioUnit where

import Data.Typelevel.Bool (False, True)
import WAGS.Universe.Bin (Ptr)

data AudioUnit

data AudioUnitList

foreign import data AudioUnitCons :: AudioUnit -> AudioUnitList -> AudioUnitList

foreign import data AudioUnitNil :: AudioUnitList

data AudioUnitRef (ptr :: Ptr)
  = AudioUnitRef Int

class GetPointer (audioUnit :: AudioUnit) (ptr :: Ptr) | audioUnit -> ptr

instance getPointerSinOsc :: GetPointer (TSinOsc ptr) ptr

instance getPointerHighpass :: GetPointer (THighpass ptr) ptr

instance getPointerGain :: GetPointer (TGain ptr) ptr

instance getPointerSpeaker :: GetPointer (TSpeaker ptr) ptr

---
foreign import data TAllpass :: Ptr -> AudioUnit

foreign import data TBandpass :: Ptr -> AudioUnit

foreign import data TConstant :: Ptr -> AudioUnit

foreign import data TConvolver :: Ptr -> AudioUnit

foreign import data TDelay :: Ptr -> AudioUnit

foreign import data TDup :: Ptr -> AudioUnit

foreign import data TDynamicsCompressor :: Ptr -> AudioUnit

foreign import data TGain :: Ptr -> AudioUnit

foreign import data THighpass :: Ptr -> AudioUnit

foreign import data THighshelf :: Ptr -> AudioUnit

foreign import data TLoopBuf :: Ptr -> AudioUnit

foreign import data TLowpass :: Ptr -> AudioUnit

foreign import data TLowshelf :: Ptr -> AudioUnit

foreign import data TMicrophone :: Ptr -> AudioUnit

foreign import data TNotch :: Ptr -> AudioUnit

foreign import data TPeaking :: Ptr -> AudioUnit

foreign import data TPeriodicOsc :: Ptr -> AudioUnit

foreign import data TPlayBuf :: Ptr -> AudioUnit

foreign import data TRecorder :: Ptr -> AudioUnit

foreign import data TSawtoothOsc :: Ptr -> AudioUnit

foreign import data TSinOsc :: Ptr -> AudioUnit

foreign import data TSpeaker :: Ptr -> AudioUnit

foreign import data TSquareOsc :: Ptr -> AudioUnit

foreign import data TStereoPanner :: Ptr -> AudioUnit

foreign import data TTriangleOsc :: Ptr -> AudioUnit

foreign import data TWaveShaper :: Ptr -> AudioUnit

class AudioUnitEq (a :: AudioUnit) (b :: AudioUnit) (tf :: Type) | a b -> tf

instance audioUnitEqTAllpass :: AudioUnitEq (TAllpass idx) (TAllpass idx) True
else instance audioUnitEqTBandpass :: AudioUnitEq (TBandpass idx) (TBandpass idx) True
else instance audioUnitEqTConstant :: AudioUnitEq (TConstant idx) (TConstant idx) True
else instance audioUnitEqTConvolver :: AudioUnitEq (TConvolver idx) (TConvolver idx) True
else instance audioUnitEqTDelay :: AudioUnitEq (TDelay idx) (TDelay idx) True
else instance audioUnitEqTDup :: AudioUnitEq (TDup idx) (TDup idx) True
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
