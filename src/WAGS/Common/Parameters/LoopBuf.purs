module WAGS.Common.Parameters.LoopBuf where

import Prelude

import Data.Variant.Maybe (Maybe, just, nothing)
import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import WAGS.Core as Core
import WAGS.WebAPI (BrowserAudioBuffer)

data LoopBufOptions = LoopBufOptions

instance
  ConvertOption LoopBufOptions
    "playbackRate"
    Core.InitialAudioParameter
    Core.InitialAudioParameter where
  convertOption _ _ = identity

instance ConvertOption LoopBufOptions "duration" Number (Maybe Number) where
  convertOption _ _ = just

instance ConvertOption LoopBufOptions "loopStart" Number Number where
  convertOption _ _ = identity

instance ConvertOption LoopBufOptions "loopEnd" Number Number where
  convertOption _ _ = identity

instance
  ConvertOption LoopBufOptions "buffer" BrowserAudioBuffer BrowserAudioBuffer where
  convertOption _ _ = identity

type LoopBufOptional =
  ( loopStart :: Number
  , loopEnd :: Number
  , playbackRate :: Core.InitialAudioParameter
  , duration :: Maybe Number
  )

type LoopBufAll =
  ( buffer :: BrowserAudioBuffer
  | LoopBufOptional
  )

defaultLoopBuf :: { | LoopBufOptional }
defaultLoopBuf =
  { loopStart: 0.0
  , loopEnd: 0.0
  , playbackRate: 1.0
  , duration: nothing
  }

class InitialLoopBuf i where
  toInitializeLoopBuf :: i -> Core.InitializeLoopBuf

instance InitialLoopBuf Core.InitializeLoopBuf where
  toInitializeLoopBuf = identity

instance InitialLoopBuf BrowserAudioBuffer where
  toInitializeLoopBuf = toInitializeLoopBuf <<< { buffer: _ }

instance
  ConvertOptionsWithDefaults LoopBufOptions { | LoopBufOptional } { | provided }
    { | LoopBufAll } =>
  InitialLoopBuf { | provided } where
  toInitializeLoopBuf provided = Core.InitializeLoopBuf
    (convertOptionsWithDefaults LoopBufOptions defaultLoopBuf provided)
