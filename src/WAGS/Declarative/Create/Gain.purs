module WAGS.Declarative.Create.Gain where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Either (Either(..))
import Data.Variant (inj, match)
import Data.Variant.Maybe (just, nothing)
import Effect.AVar as AVar
import Effect.Exception (throwException)
import FRP.Event (Event, bang, keepLatest, makeEvent, subscribe)
import Type.Proxy (Proxy(..))
import WAGS.Common.Parameters.Gain as Parameters
import WAGS.Core as Core

gain
  :: forall i aud (outputChannels :: Type) lock payload
   . Parameters.InitialGain i
  => Core.Mix aud (Core.Audible outputChannels lock payload)
  => i
  -> Event (Core.Gain lock payload)
  -> aud
  -> Core.Node outputChannels lock payload
gain i' atts elts = Core.Node go
  where
  Core.InitializeGain i = Parameters.toInitializeGain i'
  go parent di@(Core.AudioInterpret { ids, deleteFromCache, makeGain, setGain }) = makeEvent \k -> do
    me <- ids
    parent.raiseId me
    map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
      bang
        ( makeGain
            { id: me, parent: parent.parent, scope: parent.scope, gain: i.gain }
        )
        <|>
          ( keepLatest $ map
              ( \(Core.Gain e) -> match
                  { gain: tmpResolveAU parent.scope di (setGain <<< { id: me, gain: _ })
                  }
                  e
              )
              atts
          )
        <|> Core.__internalWagsFlatten (just me) parent.scope di (Core.mix elts)

gain_
  :: forall i aud (outputChannels :: Type) lock payload
   . Parameters.InitialGain i
  => Core.Mix aud (Core.Audible outputChannels lock payload)
  => i
  -> aud
  -> Core.Node outputChannels lock payload
gain_ i a = gain i empty a

-- TODO
--
-- this function is copied between two files with the sole difference that this version wraps its
-- argument in a gain node the reason for this is that, otherwise, we'd have to write additional
-- machinery for all generators (ie sine wave oscillators) to listen to when they turn on and off
-- and reconnect fresh generators whenever something turns on again by doing it this way, all
-- generators go to a gain node, so we can use code we've already written the downside is that we
-- have an extra gain node for every audio parameter which can add up so we definitely want to
-- delete this and use Common.resolveAU as soon as we can correctly attach and detach generators
tmpResolveAU
  :: forall lock payload. String -> Core.AudioInterpret payload -> (Core.FFIAudioParameter -> payload) -> Core.AudioParameter lock payload -> Event payload
tmpResolveAU = go
  where
  cncl = Core.FFIAudioParameter <<< inj (Proxy :: _ "cancel")
  ev = Core.FFIAudioParameter <<< inj (Proxy :: _ "envelope")
  nmc = Core.FFIAudioParameter <<< inj (Proxy :: _ "numeric")
  sdn = Core.FFIAudioParameter <<< inj (Proxy :: _ "sudden")
  ut = Core.FFIAudioParameter <<< inj (Proxy :: _ "unit")
  go scope di f (Core.AudioParameter a) = match
    { numeric: bang <<< f <<< nmc
    , envelope: bang <<< f <<< ev
    , cancel: bang <<< f <<< cncl
    , sudden: bang <<< f <<< sdn
    , unit: \(Core.AudioUnit { u }) ->
        let
          Core.Node n = gain_ 1.0 u
        in
          makeEvent \k -> do
            av <- AVar.empty
            subscribe
              ( n { parent: nothing, scope: scope, raiseId: \x -> void $ AVar.tryPut x av } di <|> makeEvent \k2 -> do
                  void $ AVar.take av case _ of
                    Left e -> throwException e
                    -- only do the connection if not silence
                    Right i -> k2 (f (ut (Core.FFIAudioUnit { i })))
                  pure (pure unit)
              )
              k
    }
    a
