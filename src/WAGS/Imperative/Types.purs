-- | Type definitions for the imperative graph builder.
module WAGS.Imperative.Types where

import Prim.Boolean (True, False)

-- | A poly-kinded pair of types.
data TypePair :: forall k l. k -> l -> Type
data TypePair a b

infixr 6 type TypePair as \/

-- | An audio node with an `id`.
data GraphUnit :: Symbol -> Node -> Type
data GraphUnit id node = GraphUnit

-- | Determines that a node has inputs.
class HasInput :: Node -> Constraint
class HasInput node

-- | Determines that a node has outputs.
class HasOutput :: Node -> Constraint
class HasOutput node

-- | Determines that a node makes sound.
class HasSound :: Node -> Boolean -> Constraint
class HasSound node tOrF | node -> tOrF

-- | The kind of audio nodes.
data Node

foreign import data Allpass :: Node

instance hasInputAllpass :: HasInput Allpass
instance hasOutputAllpass :: HasOutput Allpass
instance hasSoundAllpass :: HasSound Allpass False

foreign import data Analyser :: Node

instance hasInputAnalyser :: HasInput Analyser
instance hasSoundAnalyser :: HasSound Analyser False

foreign import data Bandpass :: Node

instance hasInputBandpass :: HasInput Bandpass
instance hasOutputBandpass :: HasOutput Bandpass
instance hasSoundBandpass :: HasSound Bandpass False

foreign import data Constant :: Node

instance hasInputConstant :: HasInput Constant
instance hasOutputConstant :: HasOutput Constant
instance hasSoundConstant :: HasSound Constant False

foreign import data Convolver :: Node

instance hasInputConvolver :: HasInput Convolver
instance hasOutputConvolver :: HasOutput Convolver
instance hasSoundConvolver :: HasSound Convolver False

foreign import data Delay :: Node

instance hasInputDelay :: HasInput Delay
instance hasOutputDelay :: HasOutput Delay
instance hasSoundDelay :: HasSound Delay False

foreign import data DynamicsCompressor :: Node

instance hasInputDynamicsCompressor :: HasInput DynamicsCompressor
instance hasOutputDynamicsCompressor :: HasOutput DynamicsCompressor
instance hasSoundDynamicsCompressor :: HasSound DynamicsCompressor False

foreign import data Gain :: Node

instance hasInputGain :: HasInput Gain
instance hasOutputGain :: HasOutput Gain
instance hasSoundGain :: HasSound Gain False

foreign import data Highpass :: Node

instance hasInputHighpass :: HasInput Highpass
instance hasOutputHighpass :: HasOutput Highpass
instance hasSoundHighpass :: HasSound Highpass False

foreign import data Highshelf :: Node

instance hasInputHighshelf :: HasInput Highshelf
instance hasOutputHighshelf :: HasOutput Highshelf
instance hasSoundHighshelf :: HasSound Highshelf False

foreign import data LoopBuf :: Node

instance hasOutputLoopBuf :: HasOutput LoopBuf
instance hasSoundLoopBuf :: HasSound LoopBuf True

foreign import data Lowpass :: Node

instance hasInputLowpass :: HasInput Lowpass
instance hasOutputLowpass :: HasOutput Lowpass
instance hasSoundLowpass :: HasSound Lowpass False

foreign import data Lowshelf :: Node

instance hasInputLowshelf :: HasInput Lowshelf
instance hasOutputLowshelf :: HasOutput Lowshelf
instance hasSoundLowshelf :: HasSound Lowshelf False

foreign import data MediaElement :: Node

instance hasOutputMediaElement :: HasOutput MediaElement
instance hasSoundMediaElement :: HasSound MediaElement True

foreign import data Microphone :: Node

instance hasOutputMicrophone :: HasOutput Microphone
instance hasSoundMicrophone :: HasSound Microphone True

foreign import data Notch :: Node

instance hasInputNotch :: HasInput Notch
instance hasOutputNotch :: HasOutput Notch
instance hasSoundNotch :: HasSound Notch False

foreign import data Peaking :: Node

instance hasInputPeaking :: HasInput Peaking
instance hasOutputPeaking :: HasOutput Peaking
instance hasSoundPeaking :: HasSound Peaking False

foreign import data PeriodicOsc :: Node

instance hasOutputPeriodicOsc :: HasOutput PeriodicOsc
instance hasSoundPeriodicOsc :: HasSound PeriodicOsc True

foreign import data PlayBuf :: Node

instance hasOutputPlayBuf :: HasOutput PlayBuf
instance hasSoundPlayBuf :: HasSound PlayBuf True

foreign import data SawtoothOsc :: Node

instance hasOutputSawtoothOsc :: HasOutput SawtoothOsc
instance hasSoundSawtoothOsc :: HasSound SawtoothOsc True

foreign import data SinOsc :: Node

instance hasOutputSinOsc :: HasOutput SinOsc
instance hasSoundSinOsc :: HasSound SinOsc True

foreign import data Speaker :: Node

instance hasInputSpeaker :: HasInput Speaker
instance hasOutputSpeaker :: HasOutput Speaker
instance hasSoundSpeaker :: HasSound Speaker False

foreign import data SquareOsc :: Node

instance hasOutputSquareOsc :: HasOutput SquareOsc
instance hasSoundSquareOsc :: HasSound SquareOsc True

foreign import data TriangleOsc :: Node

instance hasOutputTriangleOsc :: HasOutput TriangleOsc
instance hasSoundTriangleOsc :: HasSound TriangleOsc True
