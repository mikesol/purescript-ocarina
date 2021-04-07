module WAGS.Change where

import Prelude

import Control.Monad.Indexed.Qualified as Ix
import Control.Monad.State (gets, modify_)
import Data.Identity (Identity(..))
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple, snd)
import Data.Tuple.Nested ((/\), type (/\))
import Type.Proxy (Proxy(..))
import WAGS.Control.Types (Frame(..))
import WAGS.Create (class Create)
import WAGS.Graph.Constructors (Dup(..), Gain(..), Highpass(..), SinOsc(..), Speaker(..))
import WAGS.Graph.Decorators (Focus(..))
import WAGS.Graph.Parameter (AudioParameter(..), defaultParam, param)
import WAGS.Rendered (AnAudioUnit(..), Instruction(..))
import WAGS.Universe.AudioUnit (AudioUnitRef, TGain, THighpass, TSinOsc, TSpeaker)
import WAGS.Universe.Bin (class BinSub, class BinToInt, BinL, D0, Ptr, PtrListCons, PtrListNil, toInt')
import WAGS.Universe.EdgeProfile (EdgeProfile, ManyEdges, NoEdge, SingleEdge)
import WAGS.Universe.Graph (class GraphToNodeList, InitialGraph)
import WAGS.Universe.Node (Node, NodeC, NodeList, NodeListCons, NodeListNil)
import WAGS.Universe.Skolems (class GetSkolemFromRecursiveArgument, class ToSkolemizedFunction, SkolemListCons, SkolemPairC, toSkolemizedFunction)
import WAGS.Universe.Universe (Universe, UniverseC)
import WAGS.Validation (class AssertSingleton, class EdgeProfileChooseGreater, class NodeListAppend, class TerminalIdentityEdge)

class ChangeInstructions (g :: Type) where
  changeInstructions :: Int -> g -> AnAudioUnit -> Maybe (Array Instruction /\ AnAudioUnit)

instance changeInstructionsSinOsc :: SetterVal a => ChangeInstructions (SinOsc a) where
  changeInstructions idx (SinOsc a) = case _ of
    ASinOsc prm@(AudioParameter prm') ->
      Just $ setterVal a
        # \f ->
            let
              iv' = f prm

              AudioParameter iv = iv'
            in
              (if iv.param == prm'.param then [] else [ SetFrequency idx iv.param iv.timeOffset iv.transition ]) /\ ASinOsc iv'
    _ -> Nothing

instance changeInstructionsHighpass :: (SetterVal a, SetterVal b) => ChangeInstructions (Highpass a b c) where
  changeInstructions idx (Highpass a b _) = case _ of
    AHighpass va@(AudioParameter va') vb@(AudioParameter vb') ->
      let
        sa = setterVal a

        aiv' = sa va

        freqChanges = let AudioParameter aiv = aiv' in if aiv.param == va'.param then [] else [ SetFrequency idx aiv.param aiv.timeOffset aiv.transition ]

        sb = setterVal b

        biv' = sb vb

        qChanges = let AudioParameter biv = biv' in if biv.param == vb'.param then [] else [ SetQ idx biv.param biv.timeOffset biv.transition ]
      in
        Just
          $ (freqChanges <> qChanges)
          /\ AHighpass aiv' biv'
    _ -> Nothing

instance changeInstructionsGain :: SetterVal a => ChangeInstructions (Gain a b) where
  changeInstructions idx (Gain a _) fromMap = case fromMap of
    AGain prm@(AudioParameter prm') ->
      Just $ setterVal a
        # \f ->
            let
              iv' = f prm

              AudioParameter iv = iv'
            in
              (if iv.param == prm'.param then [] else [ SetGain idx iv.param iv.timeOffset iv.transition ]) /\ AGain iv'
    _ -> Nothing

instance changeInstructionsSpeaker :: ChangeInstructions (Speaker a) where
  changeInstructions _ _ _ = Nothing

class SetterVal a where
  setterVal :: a -> AudioParameter -> AudioParameter

instance setterValNumber :: SetterVal Number where
  setterVal = const <<< AudioParameter <<< defaultParam { param = _ }

instance setterValAudioParameter :: SetterVal AudioParameter where
  setterVal = const

instance setterValTuple :: SetterVal (Tuple a (AudioParameter -> AudioParameter)) where
  setterVal = snd

instance setterValTupleN :: SetterVal (Tuple a (AudioParameter -> Number)) where
  setterVal = map param <<< snd

instance setterValFunction :: SetterVal (AudioParameter -> AudioParameter) where
  setterVal = identity

instance setterValFunctionN :: SetterVal (AudioParameter -> Number) where
  setterVal = map param

change ::
  forall edge a i env proof.
  TerminalIdentityEdge i edge =>
  Change edge a i =>
  a -> Frame env proof i i Unit
change = change' (Proxy :: _ edge)

changeAt ::
  forall ptr a i env proof.
  Change (SingleEdge ptr) a i =>
  AudioUnitRef ptr -> a -> Frame env proof i i Unit
changeAt _ = change' (Proxy :: _ (SingleEdge ptr))

class Change (p :: EdgeProfile) (a :: Type) (o :: Universe) where
  change' :: forall env proof. Proxy p -> a -> Frame env proof o o Unit

class ModifyRes (tag :: Type) (p :: Ptr) (i :: Node) (mod :: NodeList) (plist :: EdgeProfile) | tag p i -> mod plist

instance modifyResSinOsc :: ModifyRes (SinOsc a) p (NodeC (TSinOsc p) e) (NodeListCons (NodeC (TSinOsc p) e) NodeListNil) e
else instance modifyResHighpass :: ModifyRes (Highpass a b c) p (NodeC (THighpass p) e) (NodeListCons (NodeC (THighpass p) e) NodeListNil) e
else instance modifyResGain :: ModifyRes (Gain a b) p (NodeC (TGain p) e) (NodeListCons (NodeC (TGain p) e) NodeListNil) e
else instance modifyResSpeaker :: ModifyRes (Speaker a) p (NodeC (TSpeaker p) e) (NodeListCons (NodeC (TSpeaker p) e) NodeListNil) e
else instance modifyResMiss :: ModifyRes tag p n NodeListNil NoEdge

class Modify' (tag :: Type) (p :: Ptr) (i :: NodeList) (mod :: NodeList) (nextP :: EdgeProfile) | tag p i -> mod nextP

instance modifyNil :: Modify' tag p NodeListNil NodeListNil NoEdge

instance modifyCons ::
  ( ModifyRes tag p head headResAsList headPlist
  , Modify' tag p tail tailResAsList tailPlist
  , NodeListAppend headResAsList tailResAsList o
  , EdgeProfileChooseGreater headPlist tailPlist plist
  ) =>
  Modify' tag p (NodeListCons head tail) o plist

class Modify (tag :: Type) (p :: Ptr) (i :: Universe) (nextP :: EdgeProfile) | tag p i -> nextP

instance modify ::
  ( GraphToNodeList ig il
  , Modify' tag p il mod nextP
  , AssertSingleton mod x
  ) =>
  Modify tag p (UniverseC i ig cb sk) nextP

changeAudioUnit ::
  forall g env proof (inuniv :: Universe) (p :: BinL) (nextP :: EdgeProfile) univ.
  ChangeInstructions g =>
  BinToInt p =>
  Modify g p inuniv nextP =>
  Proxy (Proxy p /\ Proxy nextP /\ Proxy inuniv) -> g -> Frame env proof univ inuniv Unit
changeAudioUnit _ g =
  Frame
    $ do
        let
          ptr = toInt' (Proxy :: _ p)
        anAudioUnit' <- M.lookup ptr <$> gets _.internalNodes
        case anAudioUnit' of
          Just (anAudioUnit) -> case changeInstructions ptr g anAudioUnit of
            Just (instr /\ au) ->
              modify_
                ( \i ->
                    i
                      { internalNodes = M.insert ptr (au) i.internalNodes
                      , instructions = i.instructions <> instr
                      }
                )
            Nothing -> pure unit
          Nothing -> pure unit

instance changeNoEdge ::
  Change NoEdge g inuniv where
  change' _ _ = Frame $ (pure unit)

instance changeSkolem ::
  Change (SingleEdge p) (Proxy skolem) inuniv where
  change' _ _ = Frame $ (pure unit)

instance changeIdentity :: Change (SingleEdge p) x inuniv => Change (SingleEdge p) (Identity x) inuniv where
  change' p (Identity x) = change' p x

instance changeFocus :: Change (SingleEdge p) x inuniv => Change (SingleEdge p) (Focus x) inuniv where
  change' p (Focus x) = change' p x

instance changeMany2 ::
  ( Change (SingleEdge p) x inuniv
  , Change (ManyEdges a b) y inuniv
  ) =>
  Change (ManyEdges p (PtrListCons a b)) (x /\ y) inuniv where
  change' _ (x /\ y) = Ix.do
    (change' :: forall env proof. Proxy (SingleEdge p) -> x -> Frame env proof inuniv inuniv Unit) Proxy x
    (change' :: forall env proof. Proxy (ManyEdges a b) -> y -> Frame env proof inuniv inuniv Unit) Proxy y

instance changeMany1 ::
  Change (SingleEdge p) a inuniv =>
  Change (ManyEdges p PtrListNil) (a /\ Unit) inuniv where
  change' _ (a /\ _) = (change' :: forall env proof. Proxy (SingleEdge p) -> a -> Frame env proof inuniv inuniv Unit) Proxy a

instance changeSinOsc ::
  ( SetterVal a
  , BinToInt p
  , Modify (SinOsc a) p inuniv nextP
  ) =>
  Change (SingleEdge p) (SinOsc a) inuniv where
  change' _ = changeAudioUnit (Proxy :: Proxy ((Proxy p) /\ (Proxy nextP) /\ Proxy inuniv))

instance changeHighpass ::
  ( SetterVal a
  , SetterVal b
  , BinToInt p
  , GetSkolemFromRecursiveArgument fc skolem
  , ToSkolemizedFunction fc skolem c
  , Modify (Highpass a b c) p inuniv nextP
  , Change nextP c inuniv
  ) =>
  Change (SingleEdge p) (Highpass a b fc) inuniv where
  change' _ (Highpass a b fc) =
    let
      c = (((toSkolemizedFunction :: fc -> (Proxy skolem -> c)) fc) Proxy)
    in
      Ix.do
        changeAudioUnit (Proxy :: Proxy (Proxy p /\ Proxy nextP /\ Proxy inuniv)) (Highpass a b c)
        (change' :: forall env proof. (Proxy nextP) -> c -> Frame env proof inuniv inuniv Unit) Proxy c

instance changeDup ::
  ( Create
      a
      (UniverseC D0 InitialGraph changeBit (SkolemListCons (SkolemPairC skolem D0) skolems))
      (UniverseC outptr grapho changeBit (SkolemListCons (SkolemPairC skolem D0) skolems))
      ignore
  , BinToInt p
  , BinToInt outptr
  , BinToInt continuation
  , BinSub p outptr continuation
  , Change (SingleEdge p) b inuniv
  , Change (SingleEdge continuation) a inuniv
  ) =>
  Change (SingleEdge p) (Dup a (Proxy skolem -> b)) inuniv where
  change' _ (Dup a f) = Ix.do
    (change' :: forall env proof. (Proxy (SingleEdge p)) -> b -> Frame env proof inuniv inuniv Unit) Proxy (f Proxy)
    (change' :: forall env proof. (Proxy (SingleEdge continuation)) -> a -> Frame env proof inuniv inuniv Unit) Proxy a

instance changeGain ::
  ( SetterVal a
  , BinToInt p
  , GetSkolemFromRecursiveArgument fb skolem
  , ToSkolemizedFunction fb skolem b
  , Modify (Gain a b) p inuniv nextP
  , Change nextP b inuniv
  ) =>
  Change (SingleEdge p) (Gain a fb) inuniv where
  change' _ (Gain a fb) =
    let
      b = (((toSkolemizedFunction :: fb -> (Proxy skolem -> b)) fb) Proxy)
    in
      Ix.do
        changeAudioUnit (Proxy :: Proxy (Proxy p /\ Proxy nextP /\ Proxy inuniv)) (Gain a b)
        (change' :: forall env proof. (Proxy nextP) -> b -> Frame env proof inuniv inuniv Unit) Proxy b

instance changeSpeaker ::
  ( BinToInt p
  , GetSkolemFromRecursiveArgument fa skolem
  , ToSkolemizedFunction fa skolem a
  , Modify (Speaker a) p inuniv nextP
  , Change nextP a inuniv
  ) =>
  Change (SingleEdge p) (Speaker fa) inuniv where
  change' _ (Speaker fa) =
    let
      a = (((toSkolemizedFunction :: fa -> (Proxy skolem -> a)) fa) Proxy)
    in
      Ix.do
        changeAudioUnit (Proxy :: Proxy (Proxy p /\ Proxy nextP /\ Proxy inuniv)) (Speaker a)
        (change' :: forall env proof. (Proxy nextP) -> a -> Frame env proof inuniv inuniv Unit) Proxy a
