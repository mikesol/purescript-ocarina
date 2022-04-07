module WAGS.Tumult.Instructions where

import Prelude

import Data.Variant (inj)
import Type.Proxy (Proxy(..))
import WAGS.Core as C

iDisconnectXFromY :: C.DisconnectXFromY' -> C.Instruction
iDisconnectXFromY = C.Instruction <<< inj (Proxy :: Proxy "disconnectXFromY")

iDestroyUnit :: C.DestroyUnit' -> C.Instruction
iDestroyUnit = C.Instruction <<< inj (Proxy :: Proxy "destroyUnit")

iMakeAllpass :: C.MakeAllpass' -> C.Instruction
iMakeAllpass = C.Instruction <<< inj (Proxy :: Proxy "makeAllpass")

iMakeAnalyser :: C.MakeAnalyser -> C.Instruction
iMakeAnalyser = C.Instruction <<< inj (Proxy :: Proxy "makeAnalyser")

iMakeAudioWorkletNode :: C.MakeAudioWorkletNode' -> C.Instruction
iMakeAudioWorkletNode = C.Instruction <<< inj
  (Proxy :: Proxy "makeAudioWorkletNode")

iMakeBandpass :: C.MakeBandpass' -> C.Instruction
iMakeBandpass = C.Instruction <<< inj (Proxy :: Proxy "makeBandpass")

iMakeConstant :: C.MakeConstant' -> C.Instruction
iMakeConstant = C.Instruction <<< inj (Proxy :: Proxy "makeConstant")

iMakeConvolver :: C.MakeConvolver -> C.Instruction
iMakeConvolver = C.Instruction <<< inj (Proxy :: Proxy "makeConvolver")

iMakeDelay :: C.MakeDelay' -> C.Instruction
iMakeDelay = C.Instruction <<< inj (Proxy :: Proxy "makeDelay")

iMakeDynamicsCompressor :: C.MakeDynamicsCompressor' -> C.Instruction
iMakeDynamicsCompressor = C.Instruction <<< inj
  (Proxy :: Proxy "makeDynamicsCompressor")

iMakeGain :: C.MakeGain' -> C.Instruction
iMakeGain = C.Instruction <<< inj (Proxy :: Proxy "makeGain")

iMakeHighpass :: C.MakeHighpass' -> C.Instruction
iMakeHighpass = C.Instruction <<< inj (Proxy :: Proxy "makeHighpass")

iMakeHighshelf :: C.MakeHighshelf' -> C.Instruction
iMakeHighshelf = C.Instruction <<< inj (Proxy :: Proxy "makeHighshelf")

iMakeInput :: C.MakeTumultInput -> C.Instruction
iMakeInput = C.Instruction <<< inj (Proxy :: Proxy "makeInput")

iMakeLoopBuf :: C.MakeLoopBuf' -> C.Instruction
iMakeLoopBuf = C.Instruction <<< inj (Proxy :: Proxy "makeLoopBuf")

iMakeLowpass :: C.MakeLowpass' -> C.Instruction
iMakeLowpass = C.Instruction <<< inj (Proxy :: Proxy "makeLowpass")

iMakeLowshelf :: C.MakeLowshelf' -> C.Instruction
iMakeLowshelf = C.Instruction <<< inj (Proxy :: Proxy "makeLowshelf")

iMakeMediaElement :: C.MakeMediaElement -> C.Instruction
iMakeMediaElement = C.Instruction <<< inj (Proxy :: Proxy "makeMediaElement")

iMakeMicrophone :: C.MakeMicrophone -> C.Instruction
iMakeMicrophone = C.Instruction <<< inj (Proxy :: Proxy "makeMicrophone")

iMakeNotch :: C.MakeNotch' -> C.Instruction
iMakeNotch = C.Instruction <<< inj (Proxy :: Proxy "makeNotch")

iMakePeaking :: C.MakePeaking' -> C.Instruction
iMakePeaking = C.Instruction <<< inj (Proxy :: Proxy "makePeaking")

iMakePeriodicOsc :: C.MakePeriodicOsc' -> C.Instruction
iMakePeriodicOsc = C.Instruction <<< inj (Proxy :: Proxy "makePeriodicOsc")

iMakePlayBuf :: C.MakePlayBuf' -> C.Instruction
iMakePlayBuf = C.Instruction <<< inj (Proxy :: Proxy "makePlayBuf")

iMakeRecorder :: C.MakeRecorder -> C.Instruction
iMakeRecorder = C.Instruction <<< inj (Proxy :: Proxy "makeRecorder")

iMakeSawtoothOsc :: C.MakeSawtoothOsc' -> C.Instruction
iMakeSawtoothOsc = C.Instruction <<< inj (Proxy :: Proxy "makeSawtoothOsc")

iMakeSinOsc :: C.MakeSinOsc' -> C.Instruction
iMakeSinOsc = C.Instruction <<< inj (Proxy :: Proxy "makeSinOsc")

iMakeSquareOsc :: C.MakeSquareOsc' -> C.Instruction
iMakeSquareOsc = C.Instruction <<< inj (Proxy :: Proxy "makeSquareOsc")

iMakeStereoPanner :: C.MakeStereoPanner' -> C.Instruction
iMakeStereoPanner = C.Instruction <<< inj (Proxy :: Proxy "makeStereoPanner")

iMakeTriangleOsc :: C.MakeTriangleOsc' -> C.Instruction
iMakeTriangleOsc = C.Instruction <<< inj (Proxy :: Proxy "makeTriangleOsc")

iMakeWaveShaper :: C.MakeWaveShaper -> C.Instruction
iMakeWaveShaper = C.Instruction <<< inj (Proxy :: Proxy "makeWaveShaper")

iConnectXToY :: C.ConnectXToY' -> C.Instruction
iConnectXToY = C.Instruction <<< inj (Proxy :: Proxy "connectXToY")

iSetAnalyserNodeCb :: C.SetAnalyserNodeCb -> C.Instruction
iSetAnalyserNodeCb = C.Instruction <<< inj (Proxy :: Proxy "setAnalyserNodeCb")

iSetMediaRecorderCb :: C.SetMediaRecorderCb -> C.Instruction
iSetMediaRecorderCb = C.Instruction <<< inj
  (Proxy :: Proxy "setMediaRecorderCb")

iSetAudioWorkletParameter :: C.SetAudioWorkletParameter -> C.Instruction
iSetAudioWorkletParameter = C.Instruction <<< inj
  (Proxy :: Proxy "setAudioWorkletParameter")

iSetBuffer :: C.SetBuffer -> C.Instruction
iSetBuffer = C.Instruction <<< inj (Proxy :: Proxy "setBuffer")

iSetConvolverBuffer :: C.SetConvolverBuffer -> C.Instruction
iSetConvolverBuffer = C.Instruction <<< inj
  (Proxy :: Proxy "setConvolverBuffer")

iSetPeriodicOsc :: C.SetPeriodicOsc -> C.Instruction
iSetPeriodicOsc = C.Instruction <<< inj (Proxy :: Proxy "setPeriodicOsc")

iSetOnOff :: C.SetOnOff -> C.Instruction
iSetOnOff = C.Instruction <<< inj (Proxy :: Proxy "setOnOff")

iSetBufferOffset :: C.SetBufferOffset -> C.Instruction
iSetBufferOffset = C.Instruction <<< inj (Proxy :: Proxy "setBufferOffset")

iSetLoopStart :: C.SetLoopStart -> C.Instruction
iSetLoopStart = C.Instruction <<< inj (Proxy :: Proxy "setLoopStart")

iSetLoopEnd :: C.SetLoopEnd -> C.Instruction
iSetLoopEnd = C.Instruction <<< inj (Proxy :: Proxy "setLoopEnd")

iSetRatio :: C.SetRatio -> C.Instruction
iSetRatio = C.Instruction <<< inj (Proxy :: Proxy "setRatio")

iSetOffset :: C.SetOffset -> C.Instruction
iSetOffset = C.Instruction <<< inj (Proxy :: Proxy "setOffset")

iSetAttack :: C.SetAttack -> C.Instruction
iSetAttack = C.Instruction <<< inj (Proxy :: Proxy "setAttack")

iSetGain :: C.SetGain -> C.Instruction
iSetGain = C.Instruction <<< inj (Proxy :: Proxy "setGain")

iSetQ :: C.SetQ -> C.Instruction
iSetQ = C.Instruction <<< inj (Proxy :: Proxy "setQ")

iSetPan :: C.SetPan -> C.Instruction
iSetPan = C.Instruction <<< inj (Proxy :: Proxy "setPan")

iSetThreshold :: C.SetThreshold -> C.Instruction
iSetThreshold = C.Instruction <<< inj (Proxy :: Proxy "setThreshold")

iSetRelease :: C.SetRelease -> C.Instruction
iSetRelease = C.Instruction <<< inj (Proxy :: Proxy "setRelease")

iSetKnee :: C.SetKnee -> C.Instruction
iSetKnee = C.Instruction <<< inj (Proxy :: Proxy "setKnee")

iSetDelay :: C.SetDelay -> C.Instruction
iSetDelay = C.Instruction <<< inj (Proxy :: Proxy "setDelay")

iSetPlaybackRate :: C.SetPlaybackRate -> C.Instruction
iSetPlaybackRate = C.Instruction <<< inj (Proxy :: Proxy "setPlaybackRate")

iSetFrequency :: C.SetFrequency -> C.Instruction
iSetFrequency = C.Instruction <<< inj (Proxy :: Proxy "setFrequency")

iSetWaveShaperCurve :: C.SetWaveShaperCurve -> C.Instruction
iSetWaveShaperCurve = C.Instruction <<< inj
  (Proxy :: Proxy "setWaveShaperCurve")
