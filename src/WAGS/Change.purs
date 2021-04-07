module WAGS.Change where

import Prelude
import Control.Monad.Indexed.Qualified as Ix
import Control.Monad.State (gets, modify_)
import Data.Identity (Identity(..))
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple, snd)
import Data.Tuple.Nested ((/\), type (/\))
import Type.Data.Peano (Succ)
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
import WAGS.Universe.Graph (class GraphToNodeList, Graph, InitialGraph)
import WAGS.Universe.Node (Node, NodeC, NodeList, NodeListCons, NodeListNil)
import WAGS.Universe.Skolems (class GetSkolemFromRecursiveArgument, class ToSkolemizedFunction, SkolemListCons, SkolemPairC, toSkolemizedFunction)
import WAGS.Universe.Universe (UniverseC)
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
  forall edge a currentIdx graph changeBit skolems env proof.
  TerminalIdentityEdge graph edge =>
  Change edge a graph =>
  a -> Frame env proof (UniverseC currentIdx graph changeBit skolems) (UniverseC currentIdx graph (Succ changeBit) skolems) Unit
change = change' (Proxy :: _ edge)

changeAt ::
  forall ptr a env proof currentIdx graph changeBit skolems.
  Change (SingleEdge ptr) a graph =>
  AudioUnitRef ptr -> a -> Frame env proof (UniverseC currentIdx graph changeBit skolems) (UniverseC currentIdx graph (Succ changeBit) skolems) Unit
changeAt _ = change' (Proxy :: _ (SingleEdge ptr))

type ChangeType (p :: EdgeProfile) (a :: Type) (grapho :: Graph)
  = forall env proof ptr changeBit skolems. Proxy p -> a -> Frame env proof (UniverseC ptr grapho changeBit skolems) (UniverseC ptr grapho (Succ changeBit) skolems) Unit

type ChangesType (a :: Type) (g :: Graph)
  = forall env proof ptr changeBit skolems. a -> Frame env proof (UniverseC ptr g changeBit skolems) (UniverseC ptr g (Succ changeBit) skolems) Unit

class Changes (a :: Type) (g :: Graph) where
  changes :: forall env proof ptr changeBit skolems. a -> Frame env proof (UniverseC ptr g changeBit skolems) (UniverseC ptr g (Succ changeBit) skolems) Unit

data ChangeInstruction a b
  = ChangeInstruction a b

instance changesUnit :: Changes Unit g where
  changes _ = Frame (pure unit)
else instance changesPx :: Change p a graph => Changes (ChangeInstruction (Proxy p) a) graph where
  changes (ChangeInstruction p a) = (change' :: ChangeType p a graph) p a
else instance changesTp :: (Changes x graph, Changes y graph) => Changes (Tuple x y) graph where
  changes (x /\ y) = Frame (x' *> y')
    where
    Frame x' = (changes :: ChangesType x graph) x

    Frame y' = (changes :: ChangesType y graph) y
else instance changesSingle :: (TerminalIdentityEdge graph edge, Change edge a graph) => Changes a graph where
  changes a = change a

class Change (p :: EdgeProfile) (a :: Type) (grapho :: Graph) where
  change' :: forall env proof ptr changeBit skolems. Proxy p -> a -> Frame env proof (UniverseC ptr grapho changeBit skolems) (UniverseC ptr grapho (Succ changeBit) skolems) Unit

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

class Modify (tag :: Type) (p :: Ptr) (i :: Graph) (nextP :: EdgeProfile) | tag p i -> nextP

instance modify ::
  ( GraphToNodeList ig il
  , Modify' tag p il mod nextP
  , AssertSingleton mod x
  ) =>
  Modify tag p ig nextP

changeAudioUnit ::
  forall g env proof currentIdx (igraph :: Graph) changeBit skolems (p :: BinL) (nextP :: EdgeProfile) univ.
  ChangeInstructions g =>
  BinToInt p =>
  Modify g p igraph nextP =>
  Proxy (Proxy p /\ Proxy nextP /\ Proxy igraph) -> g -> Frame env proof univ (UniverseC currentIdx igraph changeBit skolems) Unit
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
  Change NoEdge g igraph where
  change' _ _ = Frame $ (pure unit)

instance changeSkolem ::
  Change (SingleEdge p) (Proxy skolem) igraph where
  change' _ _ = Frame $ (pure unit)

instance changeIdentity :: Change (SingleEdge p) x igraph => Change (SingleEdge p) (Identity x) igraph where
  change' p (Identity x) = change' p x

instance changeFocus :: Change (SingleEdge p) x igraph => Change (SingleEdge p) (Focus x) igraph where
  change' p (Focus x) = change' p x

instance changeMany2 ::
  ( Change (SingleEdge p) x igraph
  , Change (ManyEdges a b) y igraph
  ) =>
  Change (ManyEdges p (PtrListCons a b)) (x /\ y) igraph where
  change' _ (x /\ y) = Frame (_1 *> _2)
    where
    Frame _1 = (change' :: ChangeType (SingleEdge p) x igraph) Proxy x

    Frame _2 = (change' :: ChangeType (ManyEdges a b) y igraph) Proxy y

instance changeMany1 ::
  Change (SingleEdge p) a igraph =>
  Change (ManyEdges p PtrListNil) (a /\ Unit) igraph where
  change' _ (a /\ _) = (change' :: ChangeType (SingleEdge p) a igraph) Proxy a

instance changeSinOsc ::
  ( SetterVal a
  , BinToInt p
  , Modify (SinOsc a) p igraph nextP
  ) =>
  Change (SingleEdge p) (SinOsc a) igraph where
  change' _ = changeAudioUnit (Proxy :: Proxy ((Proxy p) /\ (Proxy nextP) /\ Proxy igraph))

instance changeHighpass ::
  ( SetterVal a
  , SetterVal b
  , BinToInt p
  , GetSkolemFromRecursiveArgument fc skolem
  , ToSkolemizedFunction fc skolem c
  , Modify (Highpass a b c) p igraph nextP
  , Change nextP c igraph
  ) =>
  Change (SingleEdge p) (Highpass a b fc) igraph where
  change' _ (Highpass a b fc) =
    let
      c = (((toSkolemizedFunction :: fc -> (Proxy skolem -> c)) fc) Proxy)
    in
      Ix.do
        changeAudioUnit (Proxy :: Proxy (Proxy p /\ Proxy nextP /\ Proxy igraph)) (Highpass a b c)
        (change' :: ChangeType nextP c igraph) Proxy c

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
  , Change (SingleEdge p) b igraph
  , Change (SingleEdge continuation) a igraph
  ) =>
  Change (SingleEdge p) (Dup a (Proxy skolem -> b)) igraph where
  change' _ (Dup a f) = Frame (_1 *> _2)
    where
    Frame _1 = (change' :: ChangeType (SingleEdge p) b igraph) Proxy (f Proxy)

    Frame _2 = (change' :: ChangeType (SingleEdge continuation) a igraph) Proxy a

instance changeGain ::
  ( SetterVal a
  , BinToInt p
  , GetSkolemFromRecursiveArgument fb skolem
  , ToSkolemizedFunction fb skolem b
  , Modify (Gain a b) p igraph nextP
  , Change nextP b igraph
  ) =>
  Change (SingleEdge p) (Gain a fb) igraph where
  change' _ (Gain a fb) =
    let
      b = (((toSkolemizedFunction :: fb -> (Proxy skolem -> b)) fb) Proxy)
    in
      Ix.do
        changeAudioUnit (Proxy :: Proxy (Proxy p /\ Proxy nextP /\ Proxy igraph)) (Gain a b)
        (change' :: ChangeType nextP b igraph) Proxy b

instance changeSpeaker ::
  ( BinToInt p
  , GetSkolemFromRecursiveArgument fa skolem
  , ToSkolemizedFunction fa skolem a
  , Modify (Speaker a) p igraph nextP
  , Change nextP a igraph
  ) =>
  Change (SingleEdge p) (Speaker fa) igraph where
  change' _ (Speaker fa) =
    let
      a = (((toSkolemizedFunction :: fa -> (Proxy skolem -> a)) fa) Proxy)
    in
      Ix.do
        changeAudioUnit (Proxy :: Proxy (Proxy p /\ Proxy nextP /\ Proxy igraph)) (Speaker a)
        (change' :: ChangeType nextP a igraph) Proxy a
