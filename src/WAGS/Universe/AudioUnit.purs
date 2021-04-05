module WAGS.Universe.AudioUnit where

import Data.Typelevel.Bool (False, True)
import WAGS.Universe.Bin (Ptr)

data AudioUnit

data AudioUnitList

foreign import data AudioUnitCons :: AudioUnit -> AudioUnitList -> AudioUnitList

foreign import data AudioUnitNil :: AudioUnitList
foreign import data TSinOsc :: Ptr -> AudioUnit

foreign import data THighpass :: Ptr -> AudioUnit

foreign import data TGain :: Ptr -> AudioUnit

foreign import data TSpeaker :: Ptr -> AudioUnit

data AudioUnitRef (ptr :: Ptr)
  = AudioUnitRef Int

class AudioUnitEq (a :: AudioUnit) (b :: AudioUnit) (tf :: Type) | a b -> tf

instance audioUnitEqTSinOsc :: AudioUnitEq (TSinOsc idx) (TSinOsc idx) True
else instance audioUnitEqTHighpass :: AudioUnitEq (THighpass idx) (THighpass idx) True
else instance audioUnitEqTGain :: AudioUnitEq (TGain idx) (TGain idx) True
else instance audioUnitEqTSpeaker :: AudioUnitEq (TSpeaker idx) (TSpeaker idx) True
else instance audioUnitEqFalse :: AudioUnitEq a b False


class GetPointer (audioUnit :: AudioUnit) (ptr :: Ptr) | audioUnit -> ptr

instance getPointerSinOsc :: GetPointer (TSinOsc ptr) ptr

instance getPointerHighpass :: GetPointer (THighpass ptr) ptr

instance getPointerGain :: GetPointer (TGain ptr) ptr

instance getPointerSpeaker :: GetPointer (TSpeaker ptr) ptr
