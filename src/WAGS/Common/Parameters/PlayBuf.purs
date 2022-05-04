module WAGS.Common.Parameters.PlayBuf where

import Prelude

import Data.Variant.Maybe (Maybe, just, nothing)
import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import WAGS.Core as Core
import WAGS.WebAPI (BrowserAudioBuffer)

data PlayBufOptions = PlayBufOptions

instance
  ConvertOption PlayBufOptions
    "playbackRate"
    Core.InitialAudioParameter
    Core.InitialAudioParameter where
  convertOption _ _ = identity

instance ConvertOption PlayBufOptions "duration" Number (Maybe Number) where
  convertOption _ _ = just

instance ConvertOption PlayBufOptions "bufferOffset" Number Number where
  convertOption _ _ = identity

instance
  ConvertOption PlayBufOptions "buffer" BrowserAudioBuffer BrowserAudioBuffer where
  convertOption _ _ = identity

type PlayBufOptional =
  ( bufferOffset :: Number
  , playbackRate :: Core.InitialAudioParameter
  , duration :: Maybe Number
  )

type PlayBufAll =
  ( buffer :: BrowserAudioBuffer
  | PlayBufOptional
  )

defaultPlayBuf :: { | PlayBufOptional }
defaultPlayBuf =
  { bufferOffset: 0.0
  , playbackRate: 1.0
  , duration: nothing
  }

class InitialPlayBuf i where
  toInitializePlayBuf :: i -> Core.InitializePlayBuf

instance InitialPlayBuf Core.InitializePlayBuf where
  toInitializePlayBuf = identity

instance InitialPlayBuf BrowserAudioBuffer where
  toInitializePlayBuf = toInitializePlayBuf <<< { buffer: _ }

instance
  ConvertOptionsWithDefaults PlayBufOptions { | PlayBufOptional } { | provided }
    { | PlayBufAll } =>
  InitialPlayBuf { | provided } where
  toInitializePlayBuf provided = Core.InitializePlayBuf
    (convertOptionsWithDefaults PlayBufOptions defaultPlayBuf provided)
