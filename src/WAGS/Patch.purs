module WAGS.Patch where

import Prelude hiding (Ordering(..))

import Data.Maybe (Maybe, maybe)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested (type (/\))
import Data.Typelevel.Bool (False, True)
import Data.Typelevel.Num (class Nat, class Pos, toInt')
import Prim.Ordering (Ordering, LT, GT, EQ)
import Prim.RowList (class RowToList)
import Prim.RowList as RL
import Prim.Symbol as Sym
import Simple.JSON as JSON
import Type.Proxy (Proxy(..))
import WAGS.Control.Indexed (IxWAG(..))
import WAGS.Control.Types (WAG, unsafeUnWAG, unsafeWAG)
import WAGS.Graph.AudioUnit (OnOff(..))
import WAGS.Graph.AudioUnit as AU
import WAGS.Graph.Oversample (class IsOversample, reflectOversample)
import WAGS.Interpret (class AudioInterpret, connectXToY, destroyUnit, disconnectXFromY, makeAllpass, makeAnalyser, makeAudioWorkletNode, makeBandpass, makeConstant, makeDelay, makeDynamicsCompressor, makeGain, makeHighpass, makeHighshelf, makeLoopBufWithDeferredBuffer, makeLowpass, makeLowshelf, makeMicrophone, makeNotch, makePassthroughConvolver, makePeaking, makePeriodicOscWithDeferredOsc, makePlayBufWithDeferredBuffer, makeRecorder, makeSawtoothOsc, makeSinOsc, makeSpeaker, makeSquareOsc, makeStereoPanner, makeTriangleOsc, makeWaveShaper)
import WAGS.Util (class TypeEqualTF, class ValidateOutputChannelCount, toOutputChannelCount)
import WAGS.WebAPI (AnalyserNodeCb(..), BrowserFloatArray, BrowserMicrophone, MediaRecorderCb(..))

data ConnectXToY (x :: Symbol) (y :: Symbol)
  = ConnectXToY (Proxy x) (Proxy y)

data DisconnectXFromY (x :: Symbol) (y :: Symbol)
  = DisconnectXFromY (Proxy x) (Proxy y)

data DestroyUnit (x :: Symbol)
  = DestroyUnit (Proxy x)

data MakeAllpass (ptr :: Symbol)
  = MakeAllpass (Proxy ptr)

data MakeAnalyser (ptr :: Symbol)
  = MakeAnalyser (Proxy ptr)

data MakeAudioWorkletNode (ptr :: Symbol) (sym :: Symbol) (numberOfInputs :: Type) (numberOfOutputs :: Type) (outputChannelCount :: Type) (parameterData :: Row Type) (processorOptions :: Row Type)
  = MakeAudioWorkletNode (Proxy ptr) (Proxy sym) (Proxy numberOfInputs) (Proxy numberOfOutputs) (Proxy outputChannelCount) (Proxy parameterData) (Proxy processorOptions)

data MakeBandpass (ptr :: Symbol)
  = MakeBandpass (Proxy ptr)

data MakeConstant (ptr :: Symbol)
  = MakeConstant (Proxy ptr)

data MakeConvolver (ptr :: Symbol)
  = MakeConvolver (Proxy ptr)

data MakeDelay (ptr :: Symbol)
  = MakeDelay (Proxy ptr)

data MakeDynamicsCompressor (ptr :: Symbol)
  = MakeDynamicsCompressor (Proxy ptr)

data MakeGain (ptr :: Symbol)
  = MakeGain (Proxy ptr)

data MakeHighpass (ptr :: Symbol)
  = MakeHighpass (Proxy ptr)

data MakeHighshelf (ptr :: Symbol)
  = MakeHighshelf (Proxy ptr)

data MakeLoopBuf (ptr :: Symbol)
  = MakeLoopBuf (Proxy ptr)

data MakeLowpass (ptr :: Symbol)
  = MakeLowpass (Proxy ptr)

data MakeLowshelf (ptr :: Symbol)
  = MakeLowshelf (Proxy ptr)

data MakeMicrophone
  = MakeMicrophone

data MakeNotch (ptr :: Symbol)
  = MakeNotch (Proxy ptr)

data MakePeaking (ptr :: Symbol)
  = MakePeaking (Proxy ptr)

data MakePeriodicOsc (ptr :: Symbol)
  = MakePeriodicOsc (Proxy ptr)

data MakePlayBuf (ptr :: Symbol)
  = MakePlayBuf (Proxy ptr)

data MakeRecorder (ptr :: Symbol)
  = MakeRecorder (Proxy ptr)

data MakeSawtoothOsc (ptr :: Symbol)
  = MakeSawtoothOsc (Proxy ptr)

data MakeSinOsc (ptr :: Symbol)
  = MakeSinOsc (Proxy ptr)

data MakeSquareOsc (ptr :: Symbol)
  = MakeSquareOsc (Proxy ptr)

data MakeSpeaker
  = MakeSpeaker

data MakeStereoPanner (ptr :: Symbol)
  = MakeStereoPanner (Proxy ptr)

data MakeTriangleOsc (ptr :: Symbol)
  = MakeTriangleOsc (Proxy ptr)

data MakeWaveShaper (ptr :: Symbol) (oversample :: Type)
  = MakeWaveShaper (Proxy ptr) oversample

class CompInstr (a :: Type) (b :: Type) (o :: Ordering) | a b -> o

instance compInstrDcL :: CompInstr (DisconnectXFromY x y) z LT
else instance compInstrDcR :: CompInstr z (DisconnectXFromY x y) GT
else instance compInstrCoL :: CompInstr (ConnectXToY x y) z GT
else instance compInstrCoR :: CompInstr z (ConnectXToY x y) LT
else instance compInstrDsL :: CompInstr (DestroyUnit x) z LT
else instance compInstrDsR :: CompInstr z (DestroyUnit x) GT
else instance compEq :: CompInstr a b EQ

class GetLR (a :: Type) (b :: Type) (l :: Type) (r :: Type) | a b -> l r

instance getLRUnit :: GetLR a Unit Unit Unit

instance getLRCons :: (CompInstr a b cmp, GetLRCmp cmp a b c l r) => GetLR a (b /\ c) l r

class GetLRCmp (cmp :: Ordering) (a :: Type) (b :: Type) (c :: Type) (l :: Type) (r :: Type) | cmp a b c -> l r

instance getLRCmpEQ :: GetLR a c l r => GetLRCmp EQ a b c (b /\ l) r

instance getLRCmpLT :: GetLR a c l r => GetLRCmp LT a b c l (b /\ r)

instance getLRCmpGT :: GetLR a c l r => GetLRCmp GT a b c (b /\ l) r

class SortInstructions (i :: Type) (o :: Type) | i -> o

instance sortInstructionsUnit :: SortInstructions Unit Unit

instance sortInstructionsLR ::
  ( GetLR a b l' r'
  , SortInstructions l' l
  , SortInstructions r' r
  , HListAppend l (a /\ r) o
  ) =>
  SortInstructions (a /\ b) o

class HListAppend (l :: Type) (r :: Type) (o :: Type) | l r -> o

instance hListAppendCons :: HListAppend b c o => HListAppend (a /\ b) c (a /\ o)

instance hListAppendUnit :: HListAppend Unit c c

class DoCreate (sym :: Symbol) (i :: Type) (o :: Type) | sym i -> o

instance doCreateMakeAllpass :: DoCreate ptr AU.TAllpass (MakeAllpass ptr)

instance doCreateMakeAnalyser :: DoCreate ptr AU.TAnalyser (MakeAnalyser ptr)

instance doCreateMakeAudioWorkletNode :: DoCreate ptr (AU.TAudioWorkletNode sym numberOfInputs numberOfOutputs outputChannelCount parameterData processorOptions) (MakeAudioWorkletNode ptr sym numberOfInputs numberOfOutputs outputChannelCount parameterData processorOptions)

instance doCreateMakeBandpass :: DoCreate ptr AU.TBandpass (MakeBandpass ptr)

instance doCreateMakeConstant :: DoCreate ptr AU.TConstant (MakeConstant ptr)

instance doCreateMakeConvolver :: DoCreate ptr AU.TConvolver (MakeConvolver ptr)

instance doCreateMakeDelay :: DoCreate ptr AU.TDelay (MakeDelay ptr)

instance doCreateMakeDynamicsCompressor :: DoCreate ptr AU.TDynamicsCompressor (MakeDynamicsCompressor ptr)

instance doCreateMakeGain :: DoCreate ptr AU.TGain (MakeGain ptr)

instance doCreateMakeHighpass :: DoCreate ptr AU.THighpass (MakeHighpass ptr)

instance doCreateMakeHighshelf :: DoCreate ptr AU.THighshelf (MakeHighshelf ptr)

instance doCreateMakeLoopBuf :: DoCreate ptr AU.TLoopBuf (MakeLoopBuf ptr)

instance doCreateMakeLowpass :: DoCreate ptr AU.TLowpass (MakeLowpass ptr)

instance doCreateMakeLowshelf :: DoCreate ptr AU.TLowshelf (MakeLowshelf ptr)

instance doCreateMakeMicrophone :: DoCreate "microphone" AU.TMicrophone MakeMicrophone

instance doCreateMakeNotch :: DoCreate ptr AU.TNotch (MakeNotch ptr)

instance doCreateMakePeaking :: DoCreate ptr AU.TPeaking (MakePeaking ptr)

instance doCreateMakePeriodicOsc :: DoCreate ptr AU.TPeriodicOsc (MakePeriodicOsc ptr)

instance doCreateMakePlayBuf :: DoCreate ptr AU.TPlayBuf (MakePlayBuf ptr)

instance doCreateMakeRecorder :: DoCreate ptr AU.TRecorder (MakeRecorder ptr)

instance doCreateMakeSawtoothOsc :: DoCreate ptr AU.TSawtoothOsc (MakeSawtoothOsc ptr)

instance doCreateMakeSinOsc :: DoCreate ptr AU.TSinOsc (MakeSinOsc ptr)

instance doCreateMakeSquareOsc :: DoCreate ptr AU.TSquareOsc (MakeSquareOsc ptr)

instance doCreateMakeSpeaker :: DoCreate "speaker" AU.TSpeaker MakeSpeaker

instance doCreateMakeStereoPanner :: DoCreate ptr AU.TStereoPanner (MakeStereoPanner ptr)

instance doCreateMakeTriangleOsc :: DoCreate ptr AU.TTriangleOsc (MakeTriangleOsc ptr)

instance doCreateMakeWaveShaper :: DoCreate ptr (AU.TWaveShaper overdrive) (MakeWaveShaper ptr overdrive)

class DisconnectAll (to :: Symbol) (froms :: RL.RowList Type) (i :: Type) | to froms -> i

instance disconnectAllNil :: DisconnectAll to RL.Nil Unit

instance disconnectAllCons :: DisconnectAll to rest o => DisconnectAll to (RL.Cons from ignore rest) (DisconnectXFromY from to /\ o)

class ConnectAll (to :: Symbol) (froms :: RL.RowList Type) (i :: Type) | to froms -> i

instance connectAllNil :: ConnectAll to RL.Nil Unit

instance connectAllCons :: ConnectAll to rest o => ConnectAll to (RL.Cons from ignore rest) (ConnectXToY from to /\ o)

class OldToNew (symComp :: Ordering) (oldSymbol :: Symbol) (oldDef :: Type) (oldRest :: RL.RowList Type) (newSymbol :: Symbol) (newDef :: Type) (newRest :: RL.RowList Type) (instructions :: Type) (oldList :: RL.RowList Type) (newList :: RL.RowList Type) | symComp oldSymbol oldDef oldRest newSymbol newDef newRest -> instructions oldList newList

instance oldToNewLT ::
  ( RL.RowToList me meList
  , DisconnectAll destroy meList disconnectInstructions
  ) =>
  OldToNew LT destroy (u /\ { | me }) thanks newSymbol newDef newRest (DestroyUnit destroy /\ disconnectInstructions) thanks (RL.Cons newSymbol newDef newRest)

instance oldToNewGT ::
  ( RL.RowToList me meList
  , ConnectAll create meList connectInstructions
  , DoCreate create u c
  ) =>
  OldToNew GT oldSymbol oldDef oldRest create (u /\ { | me }) thanks (c /\ connectInstructions) (RL.Cons oldSymbol oldDef oldRest) thanks

instance oldToNewEQ ::
  ( TypeEqualTF oldDef newDef tf
  , OldToNewEq tf oldSymbol (oldDef /\ od) (newDef /\ nd) instructions
  ) =>
  OldToNew EQ oldSymbol (oldDef /\ od) oldRest newSymbol (newDef /\ nd) newRest instructions oldRest newRest

class ConnectAndDisconnect (symbol :: Symbol) (oldConnectionsList :: RL.RowList Type) (newConnectionsList :: RL.RowList Type) (instructions :: Type) | symbol oldConnectionsList newConnectionsList -> instructions

class ConnectAndDisconnect' (comp :: Ordering) (symbol :: Symbol) (old :: Symbol) (oldRest :: RL.RowList Type) (new :: Symbol) (newRest :: RL.RowList Type) (instructions :: Type) (oldCont :: RL.RowList Type) (newCont :: RL.RowList Type) | comp symbol old oldRest new newRest -> instructions oldCont newCont

instance connectAndDisconnectNilNil :: ConnectAndDisconnect symbol RL.Nil RL.Nil Unit

instance connectAndDisconnectConsNil :: ConnectAndDisconnect to rest RL.Nil o => ConnectAndDisconnect to (RL.Cons from ignore rest) RL.Nil (DisconnectXFromY from to /\ o)

instance connectAndDisconnectNilCons :: ConnectAndDisconnect to RL.Nil rest o => ConnectAndDisconnect to RL.Nil (RL.Cons from ignore rest) (ConnectXToY from to /\ o)

instance connectAndDisconnectConsCons ::
  ( Sym.Compare i0 i1 comp
  , ConnectAndDisconnect' comp to i0 rest0 i1 rest1 o oldNext newNext
  , ConnectAndDisconnect to oldNext newNext instr
  , HListAppend o instr oo
  ) =>
  ConnectAndDisconnect to (RL.Cons i0 ignore0 rest0) (RL.Cons i1 ignore1 rest1) oo

instance ltConnectAndDisconnect' :: ConnectAndDisconnect' LT to i0 rest0 i1 rest1 (DisconnectXFromY i0 to /\ Unit) rest0 (RL.Cons i1 Unit rest1)

instance gtConnectAndDisconnect' :: ConnectAndDisconnect' GT to i0 rest0 i1 rest1 (ConnectXToY i1 to /\ Unit) (RL.Cons i0 Unit rest0) rest1

instance eqConnectAndDisconnect' :: ConnectAndDisconnect' EQ to i0 rest0 i1 rest1 Unit rest0 rest1

class OldToNewEq (tf :: Type) (symbol :: Symbol) (oldDef :: Type) (newDef :: Type) (instructions :: Type) | tf symbol oldDef newDef -> instructions

instance oldToNewEQFalse ::
  ( RL.RowToList toDisconnect toDisconnectList
  , DisconnectAll symbol toDisconnectList disconnectInstructions
  , RL.RowToList toConnect toConnectList
  , ConnectAll symbol toConnectList connectInstructions
  , DoCreate symbol create c
  , HListAppend disconnectInstructions connectInstructions o
  ) =>
  OldToNewEq False symbol (destroy /\ { | toDisconnect }) (create /\ { | toConnect }) (DestroyUnit symbol /\ c /\ o)

instance oldToNewEQTrue ::
  ( RL.RowToList oldConnections oldConnectionsList
  , RL.RowToList newConnections newConnectionsList
  , ConnectAndDisconnect symbol oldConnectionsList newConnectionsList instructions
  ) =>
  OldToNewEq True symbol (u0 /\ { | oldConnections }) (u0 /\ { | newConnections }) instructions

class PatchRL (old :: RL.RowList Type) (new :: RL.RowList Type) (i :: Type) | old new -> i

instance patchRLNilNil :: PatchRL RL.Nil RL.Nil Unit

instance patchRLConsNil ::
  ( RL.RowToList me meList
  , DisconnectAll destroy meList disconnectInstructions
  , PatchRL thanks RL.Nil rest
  , HListAppend disconnectInstructions rest o
  ) =>
  PatchRL (RL.Cons destroy (u /\ { | me }) thanks) RL.Nil (DestroyUnit destroy /\ o)

instance patchRLNilCons ::
  ( RL.RowToList me meList
  , ConnectAll create meList connectInstructions
  , PatchRL RL.Nil thanks rest
  , HListAppend connectInstructions rest o
  , DoCreate create u c
  ) =>
  PatchRL RL.Nil (RL.Cons create (u /\ { | me }) thanks) (c /\ o)

instance patchRLConsCons ::
  ( Sym.Compare oldSymbol newSymbol symComp
  , OldToNew symComp oldSymbol oldDef oldRest newSymbol newDef newRest instructions oldList newList
  , PatchRL oldList newList rest
  , HListAppend instructions rest o
  ) =>
  PatchRL (RL.Cons oldSymbol oldDef oldRest) (RL.Cons newSymbol newDef newRest) o

class ToGraphEffects (i :: Type) where
  toGraphEffects
    :: forall audio engine
     . AudioInterpret audio engine
    => Proxy i
    -> { microphone :: Maybe BrowserMicrophone }
    -> { instructions :: Array (audio -> engine)
       }
    -> { instructions :: Array (audio -> engine)
       }

instance toGraphEffectsUnit :: ToGraphEffects Unit where
  toGraphEffects _ _ = identity

instance toGraphEffectsConnectXToY :: (IsSymbol from, IsSymbol to, ToGraphEffects rest) => ToGraphEffects (ConnectXToY from to /\ rest) where
  toGraphEffects _ cache i =
    toGraphEffects (Proxy :: _ rest) cache
      ( i
          { instructions = i.instructions <> [ connectXToY from' to' ]
          }
      )
    where
    from' = reflectSymbol (Proxy :: _ from)

    to' = reflectSymbol (Proxy :: _ to)

instance toGraphEffectsDisconnectXFromY :: (IsSymbol from, IsSymbol to, ToGraphEffects rest) => ToGraphEffects (DisconnectXFromY from to /\ rest) where
  toGraphEffects _ cache i =
    toGraphEffects (Proxy :: _ rest) cache
      ( i
          { instructions = i.instructions <> [ disconnectXFromY from' to' ]
          }
      )
    where
    from' = reflectSymbol (Proxy :: _ from)

    to' = reflectSymbol (Proxy :: _ to)

instance toGraphEffectsDestroyUnit :: (IsSymbol ptr, ToGraphEffects rest) => ToGraphEffects (DestroyUnit ptr /\ rest) where
  toGraphEffects _ cache i =
    toGraphEffects (Proxy :: _ rest) cache
      ( i
          { instructions = i.instructions <> [ destroyUnit ptr' ]
          }
      )
    where
    ptr' = reflectSymbol (Proxy :: _ ptr)

instance toGraphEffectsMakeAllpass :: (IsSymbol ptr, ToGraphEffects rest) => ToGraphEffects (MakeAllpass ptr /\ rest) where
  toGraphEffects _ cache i =
    toGraphEffects (Proxy :: _ rest) cache
      ( i
          { instructions = i.instructions <> [ makeAllpass ptr' (pure 440.0) (pure 1.0) ]
          }
      )
    where
    ptr' = reflectSymbol (Proxy :: _ ptr)

instance toGraphEffectsMakeAnalyser :: (IsSymbol ptr, ToGraphEffects rest) => ToGraphEffects (MakeAnalyser ptr /\ rest) where
  toGraphEffects _ cache i =
    toGraphEffects (Proxy :: _ rest) cache
      ( i ---------------------------------- can we make an analyser without assets?
          ---------------------------------- same goes for worklet
          ---------------------------------- the two present two different cases
          ----------------------------------------------- for analyser, we can set the callback after the fact
          ----------------------------------------------- for a custom audio worklet, we're 
          { instructions = i.instructions <> [ makeAnalyser ptr' (AnalyserNodeCb \_ -> pure (pure unit)) ]
          }
      )
    where
    ptr' = reflectSymbol (Proxy :: _ ptr)

class FillWithNothing

instance toGraphEffectsMakeAudioWorkletNode ::
  ( IsSymbol ptr
  , IsSymbol sym
  , Monoid { | processorOptions }
  , Nat numberOfInputs
  , Pos numberOfOutputs
  , ValidateOutputChannelCount numberOfOutputs outputChannelCount
  , JSON.WriteForeign { | processorOptions }
  , ToGraphEffects rest
  ) =>
  ToGraphEffects (MakeAudioWorkletNode ptr sym numberOfInputs numberOfOutputs outputChannelCount parameterData processorOptions /\ rest) where
  toGraphEffects _ cache i =
    toGraphEffects (Proxy :: _ rest) cache
      ( i
          { instructions = i.instructions <>
              [ makeAudioWorkletNode ptr' sym' $ JSON.writeImpl
                  { numberOfInputs: toInt' (Proxy :: _ numberOfInputs)
                  , numberOfOutputs: toInt' (Proxy :: _ numberOfOutputs)
                  , outputChannelCount: toOutputChannelCount (Proxy :: _ numberOfOutputs) (Proxy :: _ outputChannelCount)
                  , parameterData: JSON.writeImpl {}
                  , processorOptions: JSON.writeImpl (mempty :: { | processorOptions })
                  }
              ]
          }
      )
    where
    ptr' = reflectSymbol (Proxy :: _ ptr)

    sym' = reflectSymbol (Proxy :: _ sym)

instance toGraphEffectsMakeBandpass :: (IsSymbol ptr, ToGraphEffects rest) => ToGraphEffects (MakeBandpass ptr /\ rest) where
  toGraphEffects _ cache i =
    toGraphEffects (Proxy :: _ rest) cache
      ( i
          { instructions = i.instructions <> [ makeBandpass ptr' (pure 440.0) (pure 1.0) ]
          }
      )
    where
    ptr' = reflectSymbol (Proxy :: _ ptr)

instance toGraphEffectsMakeConstant :: (IsSymbol ptr, ToGraphEffects rest) => ToGraphEffects (MakeConstant ptr /\ rest) where
  toGraphEffects _ cache i =
    toGraphEffects (Proxy :: _ rest) cache
      ( i
          { instructions = i.instructions <> [ makeConstant ptr' (pure Off) (pure 0.0) ]
          }
      )
    where
    ptr' = reflectSymbol (Proxy :: _ ptr)

instance toGraphEffectsMakeConvolver :: (IsSymbol ptr, ToGraphEffects rest) => ToGraphEffects (MakeConvolver ptr /\ rest) where
  toGraphEffects _ cache i =
    toGraphEffects (Proxy :: _ rest) cache
      ( i
          { instructions = i.instructions <> [ makePassthroughConvolver ptr' ]
          }
      )
    where
    ptr' = reflectSymbol (Proxy :: _ ptr)

instance toGraphEffectsMakeDelay :: (IsSymbol ptr, ToGraphEffects rest) => ToGraphEffects (MakeDelay ptr /\ rest) where
  toGraphEffects _ cache i =
    toGraphEffects (Proxy :: _ rest) cache
      ( i
          { instructions = i.instructions <> [ makeDelay ptr' (pure 1.0) ]
          }
      )
    where
    ptr' = reflectSymbol (Proxy :: _ ptr)

instance toGraphEffectsMakeDynamicsCompressor :: (IsSymbol ptr, ToGraphEffects rest) => ToGraphEffects (MakeDynamicsCompressor ptr /\ rest) where
  toGraphEffects _ cache i =
    toGraphEffects (Proxy :: _ rest) cache
      ( i
          { instructions = i.instructions <> [ makeDynamicsCompressor ptr' (pure (-24.0)) (pure 30.0) (pure 12.0) (pure 0.003) (pure 0.25) ]
          }
      )
    where
    ptr' = reflectSymbol (Proxy :: _ ptr)

instance toGraphEffectsMakeGain :: (IsSymbol ptr, ToGraphEffects rest) => ToGraphEffects (MakeGain ptr /\ rest) where
  toGraphEffects _ cache i =
    toGraphEffects (Proxy :: _ rest) cache
      ( i
          { instructions = i.instructions <> [ makeGain ptr' (pure 0.0) ]
          }
      )
    where
    ptr' = reflectSymbol (Proxy :: _ ptr)

instance toGraphEffectsMakeHighpass :: (IsSymbol ptr, ToGraphEffects rest) => ToGraphEffects (MakeHighpass ptr /\ rest) where
  toGraphEffects _ cache i =
    toGraphEffects (Proxy :: _ rest) cache
      ( i
          { instructions = i.instructions <> [ makeHighpass ptr' (pure 440.0) (pure 1.0) ]
          }
      )
    where
    ptr' = reflectSymbol (Proxy :: _ ptr)

instance toGraphEffectsMakeHighshelf :: (IsSymbol ptr, ToGraphEffects rest) => ToGraphEffects (MakeHighshelf ptr /\ rest) where
  toGraphEffects _ cache i =
    toGraphEffects (Proxy :: _ rest) cache
      ( i
          { instructions = i.instructions <> [ makeHighshelf ptr' (pure 440.0) (pure 0.0) ]
          }
      )
    where
    ptr' = reflectSymbol (Proxy :: _ ptr)

instance toGraphEffectsMakeLoopBuf :: (IsSymbol ptr, ToGraphEffects rest) => ToGraphEffects (MakeLoopBuf ptr /\ rest) where
  toGraphEffects _ cache i =
    toGraphEffects (Proxy :: _ rest) cache
      ( i
          { instructions = i.instructions <> [ makeLoopBufWithDeferredBuffer ptr' ]
          }
      )
    where
    ptr' = reflectSymbol (Proxy :: _ ptr)

instance toGraphEffectsMakeLowpass :: (IsSymbol ptr, ToGraphEffects rest) => ToGraphEffects (MakeLowpass ptr /\ rest) where
  toGraphEffects _ cache i =
    toGraphEffects (Proxy :: _ rest) cache
      ( i
          { instructions = i.instructions <> [ makeLowpass ptr' (pure 440.0) (pure 1.0) ]
          }
      )
    where
    ptr' = reflectSymbol (Proxy :: _ ptr)

instance toGraphEffectsMakeLowshelf :: (IsSymbol ptr, ToGraphEffects rest) => ToGraphEffects (MakeLowshelf ptr /\ rest) where
  toGraphEffects _ cache i =
    toGraphEffects (Proxy :: _ rest) cache
      ( i
          { instructions = i.instructions <> [ makeLowshelf ptr' (pure 440.0) (pure 0.0) ]
          }
      )
    where
    ptr' = reflectSymbol (Proxy :: _ ptr)

constantNothing :: forall audio engine. AudioInterpret audio engine => audio -> engine
constantNothing = makeConstant "microphone" (pure On) (pure 0.0)

instance toGraphEffectsMakeMicrophone :: ToGraphEffects rest => ToGraphEffects (MakeMicrophone /\ rest) where
  toGraphEffects _ cache i =
    toGraphEffects (Proxy :: _ rest) cache
      ( i
          { instructions = i.instructions <> [ maybe constantNothing makeMicrophone cache.microphone ]
          }
      )

instance toGraphEffectsMakeNotch :: (IsSymbol ptr, ToGraphEffects rest) => ToGraphEffects (MakeNotch ptr /\ rest) where
  toGraphEffects _ cache i =
    toGraphEffects (Proxy :: _ rest) cache
      ( i
          { instructions = i.instructions <> [ makeNotch ptr' (pure 440.0) (pure 1.0) ]
          }
      )
    where
    ptr' = reflectSymbol (Proxy :: _ ptr)

instance toGraphEffectsMakePeaking :: (IsSymbol ptr, ToGraphEffects rest) => ToGraphEffects (MakePeaking ptr /\ rest) where
  toGraphEffects _ cache i =
    toGraphEffects (Proxy :: _ rest) cache
      ( i
          { instructions = i.instructions <> [ makePeaking ptr' (pure 440.0) (pure 1.0) (pure 0.0) ]
          }
      )
    where
    ptr' = reflectSymbol (Proxy :: _ ptr)

instance toGraphEffectsMakePeriodicOsc :: (IsSymbol ptr, ToGraphEffects rest) => ToGraphEffects (MakePeriodicOsc ptr /\ rest) where
  toGraphEffects _ cache i =
    toGraphEffects (Proxy :: _ rest) cache
      ( i
          { instructions = i.instructions <> [ makePeriodicOscWithDeferredOsc ptr' ]
          }
      )
    where
    ptr' = reflectSymbol (Proxy :: _ ptr)

instance toGraphEffectsMakePlayBuf :: (IsSymbol ptr, ToGraphEffects rest) => ToGraphEffects (MakePlayBuf ptr /\ rest) where
  toGraphEffects _ cache i =
    toGraphEffects (Proxy :: _ rest) cache
      ( i
          { instructions = i.instructions <> [ makePlayBufWithDeferredBuffer ptr' ]
          }
      )
    where
    ptr' = reflectSymbol (Proxy :: _ ptr)

instance toGraphEffectsMakeRecorder :: (IsSymbol ptr, ToGraphEffects rest) => ToGraphEffects (MakeRecorder ptr /\ rest) where
  toGraphEffects _ cache i =
    toGraphEffects (Proxy :: _ rest) cache
      ( i
          { instructions = i.instructions <> [ makeRecorder ptr' (MediaRecorderCb \_ -> pure unit) ]
          }
      )
    where
    ptr' = reflectSymbol (Proxy :: _ ptr)

instance toGraphEffectsMakeSawtoothOsc :: (IsSymbol ptr, ToGraphEffects rest) => ToGraphEffects (MakeSawtoothOsc ptr /\ rest) where
  toGraphEffects _ cache i =
    toGraphEffects (Proxy :: _ rest) cache
      ( i
          { instructions = i.instructions <> [ makeSawtoothOsc ptr' (pure Off) (pure 440.0) ]
          }
      )
    where
    ptr' = reflectSymbol (Proxy :: _ ptr)

instance toGraphEffectsMakeSinOsc :: (IsSymbol ptr, ToGraphEffects rest) => ToGraphEffects (MakeSinOsc ptr /\ rest) where
  toGraphEffects _ cache i =
    toGraphEffects (Proxy :: _ rest) cache
      ( i
          { instructions = i.instructions <> [ makeSinOsc ptr' (pure Off) (pure 440.0) ]
          }
      )
    where
    ptr' = reflectSymbol (Proxy :: _ ptr)

instance toGraphEffectsMakeSquareOsc :: (IsSymbol ptr, ToGraphEffects rest) => ToGraphEffects (MakeSquareOsc ptr /\ rest) where
  toGraphEffects _ cache i =
    toGraphEffects (Proxy :: _ rest) cache
      ( i
          { instructions = i.instructions <> [ makeSquareOsc ptr' (pure Off) (pure 440.0) ]
          }
      )
    where
    ptr' = reflectSymbol (Proxy :: _ ptr)

instance toGraphEffectsMakeSpeaker :: (ToGraphEffects rest) => ToGraphEffects (MakeSpeaker /\ rest) where
  toGraphEffects _ cache i =
    toGraphEffects (Proxy :: _ rest) cache
      ( i
          { instructions = i.instructions <> [ makeSpeaker ]
          }
      )

instance toGraphEffectsMakeStereoPanner :: (IsSymbol ptr, ToGraphEffects rest) => ToGraphEffects (MakeStereoPanner ptr /\ rest) where
  toGraphEffects _ cache i =
    toGraphEffects (Proxy :: _ rest) cache
      ( i
          { instructions = i.instructions <> [ makeStereoPanner ptr' (pure 0.0) ]
          }
      )
    where
    ptr' = reflectSymbol (Proxy :: _ ptr)

instance toGraphEffectsMakeTriangleOsc :: (IsSymbol ptr, ToGraphEffects rest) => ToGraphEffects (MakeTriangleOsc ptr /\ rest) where
  toGraphEffects _ cache i =
    toGraphEffects (Proxy :: _ rest) cache
      ( i
          { instructions = i.instructions <> [ makeTriangleOsc ptr' (pure Off) (pure 440.0) ]
          }
      )
    where
    ptr' = reflectSymbol (Proxy :: _ ptr)

foreign import emptyFloatArray :: BrowserFloatArray

instance toGraphEffectsMakeWaveShaper :: (IsSymbol ptr, IsOversample oversample, Monoid oversample, ToGraphEffects rest) => ToGraphEffects (MakeWaveShaper ptr oversample /\ rest) where
  toGraphEffects _ cache i =
    toGraphEffects (Proxy :: _ rest) cache
      ( i
          { instructions = i.instructions <> [ makeWaveShaper ptr' emptyFloatArray oversample' ]
          }
      )
    where
    ptr' = reflectSymbol (Proxy :: _ ptr)

    oversample' = reflectOversample (mempty :: oversample)

ipatch
  :: forall audio engine proof res g0 g1
   . Patch g0 g1
  => AudioInterpret audio engine
  => { microphone :: Maybe BrowserMicrophone }
  -> IxWAG audio engine proof res g0 g1 Unit
ipatch cache = IxWAG \i -> patch cache (i $> unit)

type PatchSig g0 g1 =
  forall audio engine proof res a
   . AudioInterpret audio engine
  => { microphone :: Maybe BrowserMicrophone }
  -> WAG audio engine proof res g0 a
  -> WAG audio engine proof res g1 a

type PatchSigRes g0 g1 res =
  forall audio engine proof a
   . AudioInterpret audio engine
  => Monoid res
  => { microphone :: Maybe BrowserMicrophone }
  -> WAG audio engine proof res g0 a
  -> WAG audio engine proof res g1 a

class Patch g0 g1 where
  -- | Take any frame from `g0` to `g1`. The compiler automatically determines the necessary operations to perform the transformation.
  patch
    :: forall audio engine proof res a
     . AudioInterpret audio engine
    => { microphone :: Maybe BrowserMicrophone }
    -> WAG audio engine proof res g0 a
    -> WAG audio engine proof res g1 a

instance patchAll ::
  ( RowToList old oldList
  , RowToList new newList
  , PatchRL oldList newList instructions'
  , SortInstructions instructions' instructions
  , ToGraphEffects instructions
  ) =>
  Patch old new where
  patch cache w =
    unsafeWAG
      { context:
          i
            { instructions = n.instructions
            }
      , value
      }
    where
    { context: i@{ instructions }, value } = unsafeUnWAG w

    n = toGraphEffects (Proxy :: _ (instructions)) cache { instructions }
