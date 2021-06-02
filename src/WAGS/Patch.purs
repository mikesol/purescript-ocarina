module WAGS.Patch where

import Prelude hiding (Ordering(..))

import Data.Map as M
import Data.Set as S
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested (type (/\))
import Data.Typelevel.Bool (False, True)
import Prim.Ordering (Ordering, LT, GT, EQ)
import Prim.RowList (class RowToList)
import Prim.RowList as RL
import Prim.Symbol as Sym
import Type.Proxy (Proxy(..))
import WAGS.Control.Indexed (IxWAG(..))
import WAGS.Control.Types (WAG, unsafeUnWAG, unsafeWAG)
import WAGS.Graph.AudioUnit (OnOff(..))
import WAGS.Graph.AudioUnit as AU
import WAGS.Graph.Oversample (class IsOversample, reflectOversample)
import WAGS.Graph.Parameter (param)
import WAGS.Interpret (class AudioInterpret, connectXToY, destroyUnit, disconnectXFromY, makeAllpass, makeBandpass, makeConstant, makeConvolver, makeDelay, makeDynamicsCompressor, makeGain, makeHighpass, makeHighshelf, makeLoopBufWithDeferredBuffer, makeLowpass, makeLowshelf, makeMicrophone, makeNotch, makePeaking, makePeriodicOscWithDeferredOsc, makePlayBufWithDeferredBuffer, makeRecorder, makeSawtoothOsc, makeSinOsc, makeSpeaker, makeSquareOsc, makeStereoPanner, makeTriangleOsc, makeWaveShaper)
import WAGS.Rendered as Rendered
import WAGS.Util (class TypeEqualTF)

data ConnectXToY (x :: Symbol) (y :: Symbol)
  = ConnectXToY (Proxy x) (Proxy y)

data DisconnectXFromY (x :: Symbol) (y :: Symbol)
  = DisconnectXFromY (Proxy x) (Proxy y)

data DestroyUnit (x :: Symbol)
  = DestroyUnit (Proxy x)

data MakeAllpass (ptr :: Symbol)
  = MakeAllpass (Proxy ptr)

data MakeBandpass (ptr :: Symbol)
  = MakeBandpass (Proxy ptr)

data MakeConstant (ptr :: Symbol)
  = MakeConstant (Proxy ptr)

data MakeConvolver (ptr :: Symbol) (sym :: Symbol)
  = MakeConvolver (Proxy ptr) (Proxy sym)

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

data MakeRecorder (ptr :: Symbol) (sym :: Symbol)
  = MakeRecorder (Proxy ptr) (Proxy sym)

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

data MakeWaveShaper (ptr :: Symbol) (sym :: Symbol) (oversample :: Type)
  = MakeWaveShaper (Proxy ptr) (Proxy sym) oversample

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

instance doCreateMakeBandpass :: DoCreate ptr AU.TBandpass (MakeBandpass ptr)

instance doCreateMakeConstant :: DoCreate ptr AU.TConstant (MakeConstant ptr)

instance doCreateMakeConvolver :: DoCreate ptr (AU.TConvolver sym) (MakeConvolver ptr sym)

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

instance doCreateMakeRecorder :: DoCreate ptr (AU.TRecorder sym) (MakeRecorder ptr sym)

instance doCreateMakeSawtoothOsc :: DoCreate ptr AU.TSawtoothOsc (MakeSawtoothOsc ptr)

instance doCreateMakeSinOsc :: DoCreate ptr AU.TSinOsc (MakeSinOsc ptr)

instance doCreateMakeSquareOsc :: DoCreate ptr AU.TSquareOsc (MakeSquareOsc ptr)

instance doCreateMakeSpeaker :: DoCreate "speaker" AU.TSpeaker MakeSpeaker

instance doCreateMakeStereoPanner :: DoCreate ptr AU.TStereoPanner (MakeStereoPanner ptr)

instance doCreateMakeTriangleOsc :: DoCreate ptr AU.TTriangleOsc (MakeTriangleOsc ptr)

instance doCreateMakeWaveShaper :: DoCreate ptr (AU.TWaveShaper sym overdrive) (MakeWaveShaper ptr sym overdrive)

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
  toGraphEffects ::
    forall audio engine.
    AudioInterpret audio engine =>
    Proxy i ->
    { internalNodes :: M.Map String Rendered.AnAudioUnit
    , internalEdges :: M.Map String (S.Set String)
    , instructions :: Array (audio -> engine)
    } ->
    { internalNodes :: M.Map String Rendered.AnAudioUnit
    , internalEdges :: M.Map String (S.Set String)
    , instructions :: Array (audio -> engine)
    }

instance toGraphEffectsUnit :: ToGraphEffects Unit where
  toGraphEffects _ = identity

instance toGraphEffectsConnectXToY :: (IsSymbol from, IsSymbol to, ToGraphEffects rest) => ToGraphEffects (ConnectXToY from to /\ rest) where
  toGraphEffects _ i =
    toGraphEffects (Proxy :: _ rest)
      ( i
          { internalEdges = M.insertWith S.union to' (S.singleton from') i.internalEdges
          , instructions = i.instructions <> [ connectXToY from' to' ]
          }
      )
    where
    from' = reflectSymbol (Proxy :: _ from)

    to' = reflectSymbol (Proxy :: _ to)

instance toGraphEffectsDisconnectXFromY :: (IsSymbol from, IsSymbol to, ToGraphEffects rest) => ToGraphEffects (DisconnectXFromY from to /\ rest) where
  toGraphEffects _ i =
    toGraphEffects (Proxy :: _ rest)
      ( i
          { internalEdges = M.insertWith S.difference to' (S.singleton from') (i.internalEdges)
          , instructions = i.instructions <> [ disconnectXFromY from' to' ]
          }
      )
    where
    from' = reflectSymbol (Proxy :: _ from)

    to' = reflectSymbol (Proxy :: _ to)

instance toGraphEffectsDestroyUnit :: (IsSymbol ptr, ToGraphEffects rest) => ToGraphEffects (DestroyUnit ptr /\ rest) where
  toGraphEffects _ i =
    toGraphEffects (Proxy :: _ rest)
      ( i
          { internalNodes = M.delete ptr' i.internalNodes
          , internalEdges = M.delete ptr' i.internalEdges
          , instructions = i.instructions <> [ destroyUnit ptr' ]
          }
      )
    where
    ptr' = reflectSymbol (Proxy :: _ ptr)

instance toGraphEffectsMakeAllpass :: (IsSymbol ptr, ToGraphEffects rest) => ToGraphEffects (MakeAllpass ptr /\ rest) where
  toGraphEffects _ i =
    toGraphEffects (Proxy :: _ rest)
      ( i
          { internalNodes = M.insert ptr' (Rendered.AAllpass (param 440.0) (param 1.0)) i.internalNodes
          , instructions = i.instructions <> [ makeAllpass ptr' (param 440.0) (param 1.0) ]
          }
      )
    where
    ptr' = reflectSymbol (Proxy :: _ ptr)

instance toGraphEffectsMakeBandpass :: (IsSymbol ptr, ToGraphEffects rest) => ToGraphEffects (MakeBandpass ptr /\ rest) where
  toGraphEffects _ i =
    toGraphEffects (Proxy :: _ rest)
      ( i
          { internalNodes = M.insert ptr' (Rendered.ABandpass (param 440.0) (param 1.0)) i.internalNodes
          , instructions = i.instructions <> [ makeBandpass ptr' (param 440.0) (param 1.0) ]
          }
      )
    where
    ptr' = reflectSymbol (Proxy :: _ ptr)

instance toGraphEffectsMakeConstant :: (IsSymbol ptr, ToGraphEffects rest) => ToGraphEffects (MakeConstant ptr /\ rest) where
  toGraphEffects _ i =
    toGraphEffects (Proxy :: _ rest)
      ( i
          { internalNodes = M.insert ptr' (Rendered.AConstant Off (param 0.0)) i.internalNodes
          , instructions = i.instructions <> [ makeConstant ptr' Off (param 0.0) ]
          }
      )
    where
    ptr' = reflectSymbol (Proxy :: _ ptr)

instance toGraphEffectsMakeConvolver :: (IsSymbol ptr, IsSymbol sym, ToGraphEffects rest) => ToGraphEffects (MakeConvolver ptr sym /\ rest) where
  toGraphEffects _ i =
    toGraphEffects (Proxy :: _ rest)
      ( i
          { internalNodes = M.insert ptr' (Rendered.AConvolver sym') i.internalNodes
          , instructions = i.instructions <> [ makeConvolver ptr' sym' ]
          }
      )
    where
    ptr' = reflectSymbol (Proxy :: _ ptr)

    sym' = reflectSymbol (Proxy :: _ sym)

instance toGraphEffectsMakeDelay :: (IsSymbol ptr, ToGraphEffects rest) => ToGraphEffects (MakeDelay ptr /\ rest) where
  toGraphEffects _ i =
    toGraphEffects (Proxy :: _ rest)
      ( i
          { internalNodes = M.insert ptr' (Rendered.ADelay (param 1.0)) i.internalNodes
          , instructions = i.instructions <> [ makeDelay ptr' (param 1.0) ]
          }
      )
    where
    ptr' = reflectSymbol (Proxy :: _ ptr)

instance toGraphEffectsMakeDynamicsCompressor :: (IsSymbol ptr, ToGraphEffects rest) => ToGraphEffects (MakeDynamicsCompressor ptr /\ rest) where
  toGraphEffects _ i =
    toGraphEffects (Proxy :: _ rest)
      ( i
          { internalNodes = M.insert ptr' (Rendered.ADynamicsCompressor (param (-24.0)) (param 30.0) (param 12.0) (param 0.003) (param 0.25)) i.internalNodes
          , instructions = i.instructions <> [ makeDynamicsCompressor ptr' (param (-24.0)) (param 30.0) (param 12.0) (param 0.003) (param 0.25) ]
          }
      )
    where
    ptr' = reflectSymbol (Proxy :: _ ptr)

instance toGraphEffectsMakeGain :: (IsSymbol ptr, ToGraphEffects rest) => ToGraphEffects (MakeGain ptr /\ rest) where
  toGraphEffects _ i =
    toGraphEffects (Proxy :: _ rest)
      ( i
          { internalNodes = M.insert ptr' (Rendered.AGain (param 0.0)) i.internalNodes
          , instructions = i.instructions <> [ makeGain ptr' (param 0.0) ]
          }
      )
    where
    ptr' = reflectSymbol (Proxy :: _ ptr)

instance toGraphEffectsMakeHighpass :: (IsSymbol ptr, ToGraphEffects rest) => ToGraphEffects (MakeHighpass ptr /\ rest) where
  toGraphEffects _ i =
    toGraphEffects (Proxy :: _ rest)
      ( i
          { internalNodes = M.insert ptr' (Rendered.AHighpass (param 440.0) (param 1.0)) i.internalNodes
          , instructions = i.instructions <> [ makeHighpass ptr' (param 440.0) (param 1.0) ]
          }
      )
    where
    ptr' = reflectSymbol (Proxy :: _ ptr)

instance toGraphEffectsMakeHighshelf :: (IsSymbol ptr, ToGraphEffects rest) => ToGraphEffects (MakeHighshelf ptr /\ rest) where
  toGraphEffects _ i =
    toGraphEffects (Proxy :: _ rest)
      ( i
          { internalNodes = M.insert ptr' (Rendered.AHighshelf (param 440.0) (param 0.0)) i.internalNodes
          , instructions = i.instructions <> [ makeHighshelf ptr' (param 440.0) (param 0.0) ]
          }
      )
    where
    ptr' = reflectSymbol (Proxy :: _ ptr)

instance toGraphEffectsMakeLoopBuf :: (IsSymbol ptr, ToGraphEffects rest) => ToGraphEffects (MakeLoopBuf ptr /\ rest) where
  toGraphEffects _ i =
    toGraphEffects (Proxy :: _ rest)
      ( i
          { internalNodes = M.insert ptr' (Rendered.ALoopBuf "" Off (param 1.0) 0.0 0.0) i.internalNodes
          , instructions = i.instructions <> [ makeLoopBufWithDeferredBuffer ptr' ]
          }
      )
    where
    ptr' = reflectSymbol (Proxy :: _ ptr)

instance toGraphEffectsMakeLowpass :: (IsSymbol ptr, ToGraphEffects rest) => ToGraphEffects (MakeLowpass ptr /\ rest) where
  toGraphEffects _ i =
    toGraphEffects (Proxy :: _ rest)
      ( i
          { internalNodes = M.insert ptr' (Rendered.ALowpass (param 440.0) (param 1.0)) i.internalNodes
          , instructions = i.instructions <> [ makeLowpass ptr' (param 440.0) (param 1.0) ]
          }
      )
    where
    ptr' = reflectSymbol (Proxy :: _ ptr)

instance toGraphEffectsMakeLowshelf :: (IsSymbol ptr, ToGraphEffects rest) => ToGraphEffects (MakeLowshelf ptr /\ rest) where
  toGraphEffects _ i =
    toGraphEffects (Proxy :: _ rest)
      ( i
          { internalNodes = M.insert ptr' (Rendered.ALowshelf (param 440.0) (param 0.0)) i.internalNodes
          , instructions = i.instructions <> [ makeLowshelf ptr' (param 440.0) (param 0.0) ]
          }
      )
    where
    ptr' = reflectSymbol (Proxy :: _ ptr)

instance toGraphEffectsMakeMicrophone :: (ToGraphEffects rest) => ToGraphEffects (MakeMicrophone /\ rest) where
  toGraphEffects _ i =
    toGraphEffects (Proxy :: _ rest)
      ( i
          { internalNodes = M.insert "microphone" (Rendered.AMicrophone) i.internalNodes
          , instructions = i.instructions <> [ makeMicrophone ]
          }
      )

instance toGraphEffectsMakeNotch :: (IsSymbol ptr, ToGraphEffects rest) => ToGraphEffects (MakeNotch ptr /\ rest) where
  toGraphEffects _ i =
    toGraphEffects (Proxy :: _ rest)
      ( i
          { internalNodes = M.insert ptr' (Rendered.ANotch (param 440.0) (param 1.0)) i.internalNodes
          , instructions = i.instructions <> [ makeNotch ptr' (param 440.0) (param 1.0) ]
          }
      )
    where
    ptr' = reflectSymbol (Proxy :: _ ptr)

instance toGraphEffectsMakePeaking :: (IsSymbol ptr, ToGraphEffects rest) => ToGraphEffects (MakePeaking ptr /\ rest) where
  toGraphEffects _ i =
    toGraphEffects (Proxy :: _ rest)
      ( i
          { internalNodes = M.insert ptr' (Rendered.APeaking (param 440.0) (param 1.0) (param 0.0)) i.internalNodes
          , instructions = i.instructions <> [ makePeaking ptr' (param 440.0) (param 1.0) (param 0.0) ]
          }
      )
    where
    ptr' = reflectSymbol (Proxy :: _ ptr)

instance toGraphEffectsMakePeriodicOsc :: (IsSymbol ptr, ToGraphEffects rest) => ToGraphEffects (MakePeriodicOsc ptr /\ rest) where
  toGraphEffects _ i =
    toGraphEffects (Proxy :: _ rest)
      ( i
          { internalNodes = M.insert ptr' (Rendered.APeriodicOsc "" Off (param 440.0)) i.internalNodes
          , instructions = i.instructions <> [ makePeriodicOscWithDeferredOsc ptr' ]
          }
      )
    where
    ptr' = reflectSymbol (Proxy :: _ ptr)

instance toGraphEffectsMakePlayBuf :: (IsSymbol ptr, ToGraphEffects rest) => ToGraphEffects (MakePlayBuf ptr /\ rest) where
  toGraphEffects _ i =
    toGraphEffects (Proxy :: _ rest)
      ( i
          { internalNodes = M.insert ptr' (Rendered.APlayBuf "" 0.0 Off (param 1.0)) i.internalNodes
          , instructions = i.instructions <> [ makePlayBufWithDeferredBuffer ptr' ]
          }
      )
    where
    ptr' = reflectSymbol (Proxy :: _ ptr)

instance toGraphEffectsMakeRecorder :: (IsSymbol ptr, IsSymbol sym, ToGraphEffects rest) => ToGraphEffects (MakeRecorder ptr sym /\ rest) where
  toGraphEffects _ i =
    toGraphEffects (Proxy :: _ rest)
      ( i
          { internalNodes = M.insert ptr' (Rendered.ARecorder sym') i.internalNodes
          , instructions = i.instructions <> [ makeRecorder ptr' sym' ]
          }
      )
    where
    ptr' = reflectSymbol (Proxy :: _ ptr)

    sym' = reflectSymbol (Proxy :: _ sym)

instance toGraphEffectsMakeSawtoothOsc :: (IsSymbol ptr, ToGraphEffects rest) => ToGraphEffects (MakeSawtoothOsc ptr /\ rest) where
  toGraphEffects _ i =
    toGraphEffects (Proxy :: _ rest)
      ( i
          { internalNodes = M.insert ptr' (Rendered.ASawtoothOsc Off (param 440.0)) i.internalNodes
          , instructions = i.instructions <> [ makeSawtoothOsc ptr' Off (param 440.0) ]
          }
      )
    where
    ptr' = reflectSymbol (Proxy :: _ ptr)

instance toGraphEffectsMakeSinOsc :: (IsSymbol ptr, ToGraphEffects rest) => ToGraphEffects (MakeSinOsc ptr /\ rest) where
  toGraphEffects _ i =
    toGraphEffects (Proxy :: _ rest)
      ( i
          { internalNodes = M.insert ptr' (Rendered.ASinOsc Off (param 440.0)) i.internalNodes
          , instructions = i.instructions <> [ makeSinOsc ptr' Off (param 440.0) ]
          }
      )
    where
    ptr' = reflectSymbol (Proxy :: _ ptr)

instance toGraphEffectsMakeSquareOsc :: (IsSymbol ptr, ToGraphEffects rest) => ToGraphEffects (MakeSquareOsc ptr /\ rest) where
  toGraphEffects _ i =
    toGraphEffects (Proxy :: _ rest)
      ( i
          { internalNodes = M.insert ptr' (Rendered.ASquareOsc Off (param 440.0)) i.internalNodes
          , instructions = i.instructions <> [ makeSquareOsc ptr' Off (param 440.0) ]
          }
      )
    where
    ptr' = reflectSymbol (Proxy :: _ ptr)

instance toGraphEffectsMakeSpeaker :: (ToGraphEffects rest) => ToGraphEffects (MakeSpeaker /\ rest) where
  toGraphEffects _ i =
    toGraphEffects (Proxy :: _ rest)
      ( i
          { internalNodes = M.insert "speaker" Rendered.ASpeaker i.internalNodes
          , instructions = i.instructions <> [ makeSpeaker ]
          }
      )

instance toGraphEffectsMakeStereoPanner :: (IsSymbol ptr, ToGraphEffects rest) => ToGraphEffects (MakeStereoPanner ptr /\ rest) where
  toGraphEffects _ i =
    toGraphEffects (Proxy :: _ rest)
      ( i
          { internalNodes = M.insert ptr' (Rendered.AStereoPanner (param 0.0)) i.internalNodes
          , instructions = i.instructions <> [ makeStereoPanner ptr' (param 0.0) ]
          }
      )
    where
    ptr' = reflectSymbol (Proxy :: _ ptr)

instance toGraphEffectsMakeTriangleOsc :: (IsSymbol ptr, ToGraphEffects rest) => ToGraphEffects (MakeTriangleOsc ptr /\ rest) where
  toGraphEffects _ i =
    toGraphEffects (Proxy :: _ rest)
      ( i
          { internalNodes = M.insert ptr' (Rendered.ATriangleOsc Off (param 440.0)) i.internalNodes
          , instructions = i.instructions <> [ makeTriangleOsc ptr' Off (param 440.0) ]
          }
      )
    where
    ptr' = reflectSymbol (Proxy :: _ ptr)

instance toGraphEffectsMakeWaveShaper :: (IsSymbol ptr, IsSymbol sym, IsOversample oversample, Monoid oversample, ToGraphEffects rest) => ToGraphEffects (MakeWaveShaper ptr sym oversample /\ rest) where
  toGraphEffects _ i =
    toGraphEffects (Proxy :: _ rest)
      ( i
          { internalNodes = M.insert ptr' (Rendered.AWaveShaper ptr' oversample') i.internalNodes
          , instructions = i.instructions <> [ makeWaveShaper ptr' sym' oversample' ]
          }
      )
    where
    ptr' = reflectSymbol (Proxy :: _ ptr)

    sym' = reflectSymbol (Proxy :: _ sym)

    oversample' = reflectOversample (mempty :: oversample)

ipatch ::
    forall audio engine proof res g0 g1.
    Patch g0 g1 =>
    AudioInterpret audio engine =>
    IxWAG audio engine proof res { | g0 } { | g1 } Unit
ipatch = IxWAG \i -> patch (i $> unit)

class Patch g0 g1 where
  -- | Take any frame from `g0` to `g1`. The compiler automatically determines the necessary operations to perform the transformation.
  patch ::
    forall audio engine proof res a.
    AudioInterpret audio engine => WAG audio engine proof res { | g0 } a -> WAG audio engine proof res { | g1 } a

instance patchAll ::
  ( RowToList old oldList
  , RowToList new newList
  , PatchRL oldList newList instructions'
  , SortInstructions instructions' instructions
  , ToGraphEffects instructions
  ) =>
  Patch old new where
  patch w =
    unsafeWAG
      { context:
          i
            { internalNodes = n.internalNodes
            , internalEdges = n.internalEdges
            , instructions = n.instructions
            }
      , value
      }
    where
    { context: i@{ internalNodes, internalEdges, instructions }, value } = unsafeUnWAG w

    n = toGraphEffects (Proxy :: _ instructions) { internalNodes, internalEdges, instructions }
