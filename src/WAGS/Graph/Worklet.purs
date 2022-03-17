module WAGS.Graph.Worklet
  ( AudioWorkletNodeRequest(..)
  , AudioWorkletNodeResponse
  ) where

data AudioWorkletNodeRequest
  (node :: Symbol)
  (numberOfInputs :: Type)
  (numberOfOutputs :: Type)
  (outputChannelCount :: Type)
  (parameterData :: Row Type)
  (processorOptions :: Row Type) = AudioWorkletNodeRequest

data AudioWorkletNodeResponse
  (node :: Symbol)
  (numberOfInputs :: Type)
  (numberOfOutputs :: Type)
  (outputChannelCount :: Type)
  (parameterData :: Row Type)
  (processorOptions :: Row Type)