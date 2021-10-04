module WAGS.Tumult.Reconciliation where

import Prelude

import Data.List (List(..), (:))
import Data.List as List
import Data.Newtype (unwrap)
import Data.Tuple (uncurry)
import WAGS.Rendered (Instruction(..))
import Foreign.Object as Object
import Data.Set (Set)
import Data.Set as Set

reconcileTumult :: Set Instruction -> Set Instruction -> Set Instruction
reconcileTumult new old = go primus secondus Set.empty
  where
  primus = List.fromFoldable new
  secondus = List.fromFoldable old
  go :: List Instruction -> List Instruction -> Set Instruction -> Set Instruction
  go Nil Nil set = set
  go l0@(ConnectXToY x0 y0 : rest0) l1@(ConnectXToY x1 y1 : rest1) set
    | x0 < x1 = go rest0 l1 $ Set.insert (ConnectXToY x0 y0) set
    | x0 > x1 = go l0 rest1 $ Set.insert (DisconnectXFromY x1 y1) set
    | y0 < y1 = go rest0 l1 $ Set.insert (ConnectXToY x0 y0) set
    | y0 > y1 = go l0 rest1 $ Set.insert (DisconnectXFromY x1 y1) set
    | otherwise = go rest0 rest1 set
  go (ConnectXToY x0 y0 : rest0) l1 set = go rest0 l1 $ Set.insert (ConnectXToY x0 y0) set
  go l0 (ConnectXToY x1 y1 : rest1) set = go l0 rest1 $ Set.insert (DisconnectXFromY x1 y1) set
  go l0@(vA@(MakeAllpass ptr0 valA0 valB0) : rest0) l1@(MakeAllpass ptr1 _ _ : rest1) set
    | ptr0 < ptr1 = go rest0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
    | otherwise = go rest0 rest1 (Set.insert (SetFrequency ptr0 valA0) $ Set.insert (SetQ ptr0 valB0) set)
  go (vA@(MakeAllpass _ _ _) : rest0) l1 set =
    go rest0 l1 (Set.insert vA set)
  go l0 (MakeAllpass ptr1 _ _ : rest1) set =
    go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
  go l0@(vA@(MakeAnalyser ptr0 valA0) : rest0) l1@(MakeAnalyser ptr1 _ : rest1) set
    | ptr0 < ptr1 = go rest0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
    | otherwise = go rest0 rest1 (Set.insert (SetAnalyserNodeCb ptr0 valA0) set)
  go (vA@(MakeAnalyser _ _) : rest0) rest1 set =
    go rest0 rest1 (Set.insert vA set)
  go rest0 (MakeAnalyser ptr1 _ : rest1) set =
    go rest0 rest1 (Set.insert (DestroyUnit ptr1) set)
  go l0@(vA@(MakeAudioWorkletNode ptr0 valA0) : rest0) l1@(MakeAudioWorkletNode ptr1 _ : rest1) set
    | ptr0 < ptr1 = go rest0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
    | otherwise =
        let
          fn = uncurry $ SetAudioWorkletParameter ptr0
        in
          go rest0 rest1
            $ Set.union
              ( Set.fromFoldable
                  $ (identity :: Array ~> Array)
                  $ map fn
                  $ Object.toUnfoldable (unwrap valA0).parameterData
              )
              set
  go (vA@(MakeAudioWorkletNode _ _) : rest0) rest1 set =
    go rest0 rest1 (Set.insert vA set)
  go rest0 (MakeAudioWorkletNode ptr1 _ : rest1) set =
    go rest0 rest1 (Set.insert (DestroyUnit ptr1) set)
  go l0@(vA@(MakeBandpass ptr0 valA0 valB0) : rest0) l1@(MakeBandpass ptr1 _ _ : rest1) set
    | ptr0 < ptr1 = go rest0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
    | otherwise = go rest0 rest1 (Set.insert (SetFrequency ptr0 valA0) $ Set.insert (SetQ ptr0 valB0) set)
  go (vA@(MakeBandpass _ _ _) : rest0) l1 set =
    go rest0 l1 (Set.insert vA set)
  go l0 (MakeBandpass ptr1 _ _ : rest1) set =
    go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
  go l0@(vA@(MakeConstant ptr0 valA0 valB0) : rest0) l1@(MakeConstant ptr1 _ _ : rest1) set
    | ptr0 < ptr1 = go rest0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
    | otherwise = go rest0 rest1 (Set.insert (SetOnOff ptr0 valA0) $ Set.insert (SetOffset ptr0 valB0) set)
  go (vA@(MakeConstant _ _ _) : rest0) l1 set =
    go rest0 l1 (Set.insert vA set)
  go l0 (MakeConstant ptr1 _ _ : rest1) set =
    go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
  go l0@(vA@(MakePassthroughConvolver ptr0) : rest0) l1@(MakePassthroughConvolver ptr1 : rest1) set
    | ptr0 < ptr1 = go rest0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
    | otherwise = go rest0 rest1 set
  go (vA@(MakePassthroughConvolver _) : rest0) l1 set =
    go rest0 l1 (Set.insert vA set)
  go l0 (MakePassthroughConvolver ptr1 : rest1) set =
    go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
  go l0@(vA@(MakeConvolver ptr0 _) : rest0) l1@(MakeConvolver ptr1 _ : rest1) set
    | ptr0 < ptr1 = go rest0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
    | otherwise = go rest0 rest1 set
  go (vA@(MakeConvolver _ _) : rest0) l1 set =
    go rest0 l1 (Set.insert vA set)
  go l0 (MakeConvolver ptr1 _ : rest1) set =
    go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
  go l0@(vA@(MakeDelay ptr0 valA0) : rest0) l1@(MakeDelay ptr1 _ : rest1) set
    | ptr0 < ptr1 = go rest0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
    | otherwise = go rest0 rest1 (Set.insert (SetDelay ptr0 valA0) set)
  go (vA@(MakeDelay _ _) : rest0) l1 set =
    go rest0 l1 (Set.insert vA set)
  go l0 (MakeDelay ptr1 _ : rest1) set =
    go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
  go l0@(vA@(MakeDynamicsCompressor ptr0 valA0 valB0 valC0 valD0 valE0) : rest0) l1@(MakeDynamicsCompressor ptr1 _ _ _ _ _ : rest1) set
    | ptr0 < ptr1 = go rest0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
    | otherwise = go rest0 rest1
        $ Set.insert (SetThreshold ptr0 valA0)
        $ Set.insert (SetKnee ptr0 valB0)
        $ Set.insert (SetRatio ptr0 valC0)
        $ Set.insert (SetAttack ptr0 valD0)
        $ Set.insert (SetRelease ptr0 valE0) set
  go (vA@(MakeDynamicsCompressor _ _ _ _ _ _) : rest0) l1 set =
    go rest0 l1 (Set.insert vA set)
  go l0 (MakeDynamicsCompressor ptr1 _ _ _ _ _ : rest1) set =
    go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
  go l0@(vA@(MakeGain ptr0 valA0) : rest0) l1@(MakeGain ptr1 _ : rest1) set
    | ptr0 < ptr1 = go rest0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
    | otherwise = go rest0 rest1 (Set.insert (SetGain ptr0 valA0) set)
  go (vA@(MakeGain _ _) : rest0) l1 set =
    go rest0 l1 (Set.insert vA set)
  go l0 (MakeGain ptr1 _ : rest1) set =
    go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
  go l0@(vA@(MakeHighpass ptr0 valA0 valB0) : rest0) l1@(MakeHighpass ptr1 _ _ : rest1) set
    | ptr0 < ptr1 = go rest0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
    | otherwise = go rest0 rest1 (Set.insert (SetFrequency ptr0 valA0) $ Set.insert (SetQ ptr0 valB0) set)
  go (vA@(MakeHighpass _ _ _) : rest0) l1 set =
    go rest0 l1 (Set.insert vA set)
  go l0 (MakeHighpass ptr1 _ _ : rest1) set =
    go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
  go l0@(vA@(MakeHighshelf ptr0 valA0 valB0) : rest0) l1@(MakeHighshelf ptr1 _ _ : rest1) set
    | ptr0 < ptr1 = go rest0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
    | otherwise = go rest0 rest1 (Set.insert (SetFrequency ptr0 valA0) $ Set.insert (SetGain ptr0 valB0) set)
  go (vA@(MakeHighshelf _ _ _) : rest0) l1 set =
    go rest0 l1 (Set.insert vA set)
  go l0 (MakeHighshelf ptr1 _ _ : rest1) set =
    go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
  go l0@(vA@(MakeInput ptr0 _) : rest0) l1@(MakeInput ptr1 _ : rest1) set
    | ptr0 < ptr1 = go rest0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
    | otherwise = go rest0 rest1 set
  go (vA@(MakeInput _ _) : rest0) l1 set =
    go rest0 l1 (Set.insert vA set)
  go l0 (MakeInput ptr1 _ : rest1) set =
    go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
  go l0@(vA@(MakeLoopBuf ptr0 valA0 valB0 valC0 valD0 valE0) : rest0) l1@(MakeLoopBuf ptr1 _ _ _ _ _ : rest1) set
    | ptr0 < ptr1 = go rest0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
    | otherwise = go rest0 rest1
        $ Set.insert (SetBuffer ptr0 valA0)
        $ Set.insert (SetOnOff ptr0 valB0)
        $ Set.insert (SetPlaybackRate ptr0 valC0)
        $ Set.insert (SetLoopStart ptr0 valD0)
        $ Set.insert (SetLoopEnd ptr0 valE0) set
  go (vA@(MakeLoopBuf _ _ _ _ _ _) : rest0) l1 set =
    go rest0 l1 (Set.insert vA set)
  go l0 (MakeLoopBuf ptr1 _ _ _ _ _ : rest1) set =
    go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
  go l0@(vA@(MakeLoopBufWithDeferredBuffer ptr0) : rest0) l1@(MakeLoopBufWithDeferredBuffer ptr1 : rest1) set
    | ptr0 < ptr1 = go rest0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
    | otherwise = go rest0 rest1 set
  go (vA@(MakeLoopBufWithDeferredBuffer _) : rest0) l1 set =
    go rest0 l1 (Set.insert vA set)
  go l0 (MakeLoopBufWithDeferredBuffer ptr1 : rest1) set =
    go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
  go l0@(vA@(MakeLowpass ptr0 valA0 valB0) : rest0) l1@(MakeLowpass ptr1 _ _ : rest1) set
    | ptr0 < ptr1 = go rest0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
    | otherwise = go rest0 rest1 (Set.insert (SetFrequency ptr0 valA0) $ Set.insert (SetQ ptr0 valB0) set)
  go (vA@(MakeLowpass _ _ _) : rest0) l1 set =
    go rest0 l1 (Set.insert vA set)
  go l0 (MakeLowpass ptr1 _ _ : rest1) set =
    go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
  go l0@(vA@(MakeLowshelf ptr0 valA0 valB0) : rest0) l1@(MakeLowshelf ptr1 _ _ : rest1) set
    | ptr0 < ptr1 = go rest0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
    | otherwise = go rest0 rest1 (Set.insert (SetFrequency ptr0 valA0) $ Set.insert (SetGain ptr0 valB0) set)
  go (vA@(MakeLowshelf _ _ _) : rest0) l1 set =
    go rest0 l1 (Set.insert vA set)
  go l0 (MakeLowshelf ptr1 _ _ : rest1) set =
    go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
  go (MakeMicrophone _ : rest0) (MakeMicrophone _ : rest1) set = go rest0 rest1 set
  go (vA@(MakeMicrophone _) : rest0) l1 set =
    go rest0 l1 (Set.insert vA set)
  go l0 (MakeMicrophone _ : rest1) set =
    go l0 rest1 (Set.insert (DestroyUnit "microphone") set)
  go l0@(vA@(MakeNotch ptr0 valA0 valB0) : rest0) l1@(MakeNotch ptr1 _ _ : rest1) set
    | ptr0 < ptr1 = go rest0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
    | otherwise = go rest0 rest1 (Set.insert (SetFrequency ptr0 valA0) $ Set.insert (SetQ ptr0 valB0) set)
  go (vA@(MakeNotch _ _ _) : rest0) l1 set =
    go rest0 l1 (Set.insert vA set)
  go l0 (MakeNotch ptr1 _ _ : rest1) set =
    go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
  go l0@(vA@(MakePeaking ptr0 valA0 valB0 valC0) : rest0) l1@(MakePeaking ptr1 _ _ _ : rest1) set
    | ptr0 < ptr1 = go rest0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
    | otherwise = go rest0 rest1
        $ Set.insert (SetFrequency ptr0 valA0)
        $ Set.insert (SetQ ptr0 valB0)
        $ Set.insert (SetGain ptr0 valC0) set
  go (vA@(MakePeaking _ _ _ _) : rest0) l1 set =
    go rest0 l1 (Set.insert vA set)
  go l0 (MakePeaking ptr1 _ _ _ : rest1) set =
    go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
  go l0@(vA@(MakePeriodicOscWithDeferredOsc ptr0) : rest0) l1@(MakePeriodicOscWithDeferredOsc ptr1 : rest1) set
    | ptr0 < ptr1 = go rest0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
    | otherwise = go rest0 rest1 set
  go (vA@(MakePeriodicOscWithDeferredOsc _) : rest0) l1 set =
    go rest0 l1 (Set.insert vA set)
  go l0 (MakePeriodicOscWithDeferredOsc ptr1 : rest1) set =
    go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
  go l0@(vA@(MakePeriodicOsc ptr0 valA0 valB0 valC0) : rest0) l1@(MakePeriodicOsc ptr1 _ _ _ : rest1) set
    | ptr0 < ptr1 = go rest0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
    | otherwise = go rest0 rest1 $ Set.insert (SetPeriodicOsc ptr0 valA0) $ Set.insert (SetOnOff ptr0 valB0) $ Set.insert (SetFrequency ptr0 valC0) set
  go (vA@(MakePeriodicOsc _ _ _ _) : rest0) l1 set =
    go rest0 l1 (Set.insert vA set)
  go l0 (MakePeriodicOsc ptr1 _ _ _ : rest1) set =
    go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
  go l0@(vA@(MakePlayBuf ptr0 valA0 valB0 valC0 valD0) : rest0) l1@(MakePlayBuf ptr1 _ _ _ _ : rest1) set
    | ptr0 < ptr1 = go rest0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
    | otherwise = go rest0 rest1
        $ Set.insert (SetBuffer ptr0 valA0)
        $ Set.insert (SetBufferOffset ptr0 valB0)
        $ Set.insert (SetOnOff ptr0 valC0)
        $ Set.insert (SetPlaybackRate ptr0 valD0) set
  go (vA@(MakePlayBuf _ _ _ _ _) : rest0) l1 set =
    go rest0 l1 (Set.insert vA set)
  go l0 (MakePlayBuf ptr1 _ _ _ _ : rest1) set =
    go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
  go l0@(vA@(MakePlayBufWithDeferredBuffer ptr0) : rest0) l1@(MakePlayBufWithDeferredBuffer ptr1 : rest1) set
    | ptr0 < ptr1 = go rest0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
    | otherwise = go rest0 rest1 set
  go (vA@(MakePlayBufWithDeferredBuffer _) : rest0) l1 set =
    go rest0 l1 (Set.insert vA set)
  go l0 (MakePlayBufWithDeferredBuffer ptr1 : rest1) set =
    go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
  go l0@(vA@(MakeRecorder ptr0 _) : rest0) l1@(MakeRecorder ptr1 _ : rest1) set
    | ptr0 < ptr1 = go rest0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
    | otherwise = go rest0 rest1 set
  go (vA@(MakeRecorder _ _) : rest0) l1 set =
    go rest0 l1 (Set.insert vA set)
  go l0 (MakeRecorder ptr1 _ : rest1) set =
    go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
  go l0@(vA@(MakeSawtoothOsc ptr0 valA0 valB0) : rest0) l1@(MakeSawtoothOsc ptr1 _ _ : rest1) set
    | ptr0 < ptr1 = go rest0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
    | otherwise = go rest0 rest1 $ Set.insert (SetOnOff ptr0 valA0) $ Set.insert (SetFrequency ptr0 valB0) set
  go (vA@(MakeSawtoothOsc _ _ _) : rest0) l1 set =
    go rest0 l1 (Set.insert vA set)
  go l0 (MakeSawtoothOsc ptr1 _ _ : rest1) set =
    go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
  go l0@(vA@(MakeSinOsc ptr0 valA0 valB0) : rest0) l1@(MakeSinOsc ptr1 _ _ : rest1) set
    | ptr0 < ptr1 = go rest0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
    | otherwise = go rest0 rest1 $ Set.insert (SetOnOff ptr0 valA0) $ Set.insert (SetFrequency ptr0 valB0) set
  go (vA@(MakeSinOsc _ _ _) : rest0) l1 set =
    go rest0 l1 (Set.insert vA set)
  go l0 (MakeSinOsc ptr1 _ _ : rest1) set =
    go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
  go l0@(vA@(MakeSquareOsc ptr0 valA0 valB0) : rest0) l1@(MakeSquareOsc ptr1 _ _ : rest1) set
    | ptr0 < ptr1 = go rest0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
    | otherwise = go rest0 rest1 $ Set.insert (SetOnOff ptr0 valA0) $ Set.insert (SetFrequency ptr0 valB0) set
  go (vA@(MakeSquareOsc _ _ _) : rest0) l1 set =
    go rest0 l1 (Set.insert vA set)
  go l0 (MakeSquareOsc ptr1 _ _ : rest1) set =
    go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
  go (MakeSpeaker : rest0) (MakeSpeaker : rest1) set = go rest0 rest1 set
  go (vA@(MakeSpeaker) : rest0) l1 set =
    go rest0 l1 (Set.insert vA set)
  go l0 (MakeSpeaker : rest1) set =
    go l0 rest1 (Set.insert (DestroyUnit "speaker") set)
  go l0@(vA@(MakeStereoPanner ptr0 valA0) : rest0) l1@(MakeStereoPanner ptr1 _ : rest1) set
    | ptr0 < ptr1 = go rest0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
    | otherwise = go rest0 rest1 (Set.insert (SetPan ptr0 valA0) set)
  go (vA@(MakeStereoPanner _ _) : rest0) l1 set =
    go rest0 l1 (Set.insert vA set)
  go l0 (MakeStereoPanner ptr1 _ : rest1) set =
    go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
  go l0@(vA@(MakeTriangleOsc ptr0 valA0 valB0) : rest0) l1@(MakeTriangleOsc ptr1 _ _ : rest1) set
    | ptr0 < ptr1 = go rest0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
    | otherwise = go rest0 rest1 $ Set.insert (SetOnOff ptr0 valA0) $ Set.insert (SetFrequency ptr0 valB0) set
  go (vA@(MakeTriangleOsc _ _ _) : rest0) l1 set =
    go rest0 l1 (Set.insert vA set)
  go l0 (MakeTriangleOsc ptr1 _ _ : rest1) set =
    go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
  go l0@(vA@(MakeWaveShaper ptr0 _ _) : rest0) l1@(MakeWaveShaper ptr1 _ _ : rest1) set
    | ptr0 < ptr1 = go rest0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
    | otherwise = go rest0 rest1 set
  go (vA@(MakeWaveShaper _ _ _) : rest0) l1 set =
    go rest0 l1 (Set.insert vA set)
  go l0 (MakeWaveShaper ptr1 _ _ : rest1) set =
    go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
  -- todo: find implementation for subgraph and tumult
  -- issue is that, because we are building a flat list, we cannot adequately
  -- create the nesting necessary for subgraphs
  -- need to change the data structure
  go l0@(vA@(MakeSubgraph ptr0 _) : rest0) l1@(MakeSubgraph ptr1 _ : rest1) set
    | ptr0 < ptr1 = go rest0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
    | otherwise = go rest0 rest1 set
  go (vA@(MakeSubgraph _ _) : rest0) l1 set =
    go rest0 l1 (Set.insert vA set)
  go l0 (MakeSubgraph ptr1 _ : rest1) set =
    go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
  go l0@(vA@(MakeSubgraphWithDeferredScene ptr0) : rest0) l1@(MakeSubgraphWithDeferredScene ptr1 : rest1) set
    | ptr0 < ptr1 = go rest0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
    | otherwise = go rest0 rest1 set
  go (vA@(MakeSubgraphWithDeferredScene _) : rest0) l1 set =
    go rest0 l1 (Set.insert vA set)
  go l0 (MakeSubgraphWithDeferredScene ptr1 : rest1) set =
    go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
  go l0@(vA@(MakeTumult ptr0 _ _) : rest0) l1@(MakeTumult ptr1 _ _ : rest1) set
    | ptr0 < ptr1 = go rest0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
    | otherwise = go rest0 rest1 set
  go (vA@(MakeTumult _ _ _) : rest0) l1 set =
    go rest0 l1 (Set.insert vA set)
  go l0 (MakeTumult ptr1 _ _ : rest1) set =
    go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
  go l0@(vA@(MakeTumultWithDeferredGraph ptr0) : rest0) l1@(MakeTumultWithDeferredGraph ptr1 : rest1) set
    | ptr0 < ptr1 = go rest0 l1 (Set.insert vA set)
    | ptr1 < ptr0 = go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
    | otherwise = go rest0 rest1 set
  go (vA@(MakeTumultWithDeferredGraph _) : rest0) l1 set =
    go rest0 l1 (Set.insert vA set)
  go l0 (MakeTumultWithDeferredGraph ptr1 : rest1) set =
    go l0 rest1 (Set.insert (DestroyUnit ptr1) set)
  go (DestroyUnit x0 : rest0) l1 set = go rest0 l1 $ Set.insert (DestroyUnit x0) set
  go l0 (DestroyUnit _ : rest1) set = go l0 rest1 set
  go (DisconnectXFromY x0 y0 : rest0) l1 set = go rest0 l1 $ Set.insert (DisconnectXFromY x0 y0) set
  go l0 (DisconnectXFromY _ _ : rest1) set = go l0 rest1 set
  go (SetAnalyserNodeCb x0 y0 : rest0) l1 set = go rest0 l1 $ Set.insert (SetAnalyserNodeCb x0 y0) set
  go l0 (SetAnalyserNodeCb _ _ : rest1) set = go l0 rest1 set
  go (SetMediaRecorderCb x0 y0 : rest0) l1 set = go rest0 l1 $ Set.insert (SetMediaRecorderCb x0 y0) set
  go l0 (SetMediaRecorderCb _ _ : rest1) set = go l0 rest1 set
  go (SetAudioWorkletParameter x0 y0 z0 : rest0) l1 set = go rest0 l1 $ Set.insert (SetAudioWorkletParameter x0 y0 z0) set
  go l0 (SetAudioWorkletParameter _ _ _ : rest1) set = go l0 rest1 set
  go (SetBuffer x0 y0 : rest0) l1 set = go rest0 l1 $ Set.insert (SetBuffer x0 y0) set
  go l0 (SetBuffer _ _ : rest1) set = go l0 rest1 set
  go (SetConvolverBuffer x0 y0 : rest0) l1 set = go rest0 l1 $ Set.insert (SetConvolverBuffer x0 y0) set
  go l0 (SetConvolverBuffer _ _ : rest1) set = go l0 rest1 set
  go (SetPeriodicOsc x0 y0 : rest0) l1 set = go rest0 l1 $ Set.insert (SetPeriodicOsc x0 y0) set
  go l0 (SetPeriodicOsc _ _ : rest1) set = go l0 rest1 set
  go (SetOnOff x0 y0 : rest0) l1 set = go rest0 l1 $ Set.insert (SetOnOff x0 y0) set
  go l0 (SetOnOff _ _ : rest1) set = go l0 rest1 set
  go (SetBufferOffset x0 y0 : rest0) l1 set = go rest0 l1 $ Set.insert (SetBufferOffset x0 y0) set
  go l0 (SetBufferOffset _ _ : rest1) set = go l0 rest1 set
  go (SetLoopStart x0 y0 : rest0) l1 set = go rest0 l1 $ Set.insert (SetLoopStart x0 y0) set
  go l0 (SetLoopStart _ _ : rest1) set = go l0 rest1 set
  go (SetLoopEnd x0 y0 : rest0) l1 set = go rest0 l1 $ Set.insert (SetLoopEnd x0 y0) set
  go l0 (SetLoopEnd _ _ : rest1) set = go l0 rest1 set
  go (SetRatio x0 y0 : rest0) l1 set = go rest0 l1 $ Set.insert (SetRatio x0 y0) set
  go l0 (SetRatio _ _ : rest1) set = go l0 rest1 set
  go (SetOffset x0 y0 : rest0) l1 set = go rest0 l1 $ Set.insert (SetOffset x0 y0) set
  go l0 (SetOffset _ _ : rest1) set = go l0 rest1 set
  go (SetAttack x0 y0 : rest0) l1 set = go rest0 l1 $ Set.insert (SetAttack x0 y0) set
  go l0 (SetAttack _ _ : rest1) set = go l0 rest1 set
  go (SetGain x0 y0 : rest0) l1 set = go rest0 l1 $ Set.insert (SetGain x0 y0) set
  go l0 (SetGain _ _ : rest1) set = go l0 rest1 set
  go (SetQ x0 y0 : rest0) l1 set = go rest0 l1 $ Set.insert (SetQ x0 y0) set
  go l0 (SetQ _ _ : rest1) set = go l0 rest1 set
  go (SetPan x0 y0 : rest0) l1 set = go rest0 l1 $ Set.insert (SetPan x0 y0) set
  go l0 (SetPan _ _ : rest1) set = go l0 rest1 set
  go (SetThreshold x0 y0 : rest0) l1 set = go rest0 l1 $ Set.insert (SetThreshold x0 y0) set
  go l0 (SetThreshold _ _ : rest1) set = go l0 rest1 set
  go (SetRelease x0 y0 : rest0) l1 set = go rest0 l1 $ Set.insert (SetRelease x0 y0) set
  go l0 (SetRelease _ _ : rest1) set = go l0 rest1 set
  go (SetKnee x0 y0 : rest0) l1 set = go rest0 l1 $ Set.insert (SetKnee x0 y0) set
  go l0 (SetKnee _ _ : rest1) set = go l0 rest1 set
  go (SetDelay x0 y0 : rest0) l1 set = go rest0 l1 $ Set.insert (SetDelay x0 y0) set
  go l0 (SetDelay _ _ : rest1) set = go l0 rest1 set
  go (SetPlaybackRate x0 y0 : rest0) l1 set = go rest0 l1 $ Set.insert (SetPlaybackRate x0 y0) set
  go l0 (SetPlaybackRate _ _ : rest1) set = go l0 rest1 set
  go (SetFrequency x0 y0 : rest0) l1 set = go rest0 l1 $ Set.insert (SetFrequency x0 y0) set
  go l0 (SetFrequency _ _ : rest1) set = go l0 rest1 set
  go (SetWaveShaperCurve x0 y0 : rest0) l1 set = go rest0 l1 $ Set.insert (SetWaveShaperCurve x0 y0) set
  go l0 (SetWaveShaperCurve _ _ : rest1) set = go l0 rest1 set
  go (SetInput x0 y0 : rest0) l1 set = go rest0 l1 $ Set.insert (SetInput x0 y0) set
  go l0 (SetInput _ _ : rest1) set = go l0 rest1 set
  go (SetSubgraph x0 y0 : rest0) l1 set = go rest0 l1 $ Set.insert (SetSubgraph x0 y0) set
  go l0 (SetSubgraph _ _ : rest1) set = go l0 rest1 set
  go (SetTumult x0 y0 z0 : rest0) l1 set = go rest0 l1 $ Set.insert (SetTumult x0 y0 z0) set
  go l0 (SetTumult _ _ _ : rest1) set = go l0 rest1 set