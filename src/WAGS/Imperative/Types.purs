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

foreign import data Speaker :: Node

instance hasInputSpeaker :: HasInput Speaker
instance hasOutputSpeaker :: HasOutput Speaker
instance hasSoundSpeaker :: HasSound Speaker False

foreign import data Gain :: Node

instance hasInputGain :: HasInput Gain
instance hasOutputGain :: HasOutput Gain
instance hasSoundGain :: HasSound Gain False

foreign import data SinOsc :: Node

instance hasOutputSinOsc :: HasOutput SinOsc
instance hasSoundSinOsc :: HasSound SinOsc True

foreign import data PlayBuf :: Node

instance hasOutputPlayBuf :: HasOutput PlayBuf
instance hasSoundPlayBuf :: HasSound PlayBuf True
