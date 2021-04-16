module WAGS.Example.WTK.Types where

import Prelude
import Data.DateTime.Instant (Instant)
import Data.Identity (Identity(..))
import Data.List (List)
import Data.Set as S
import Data.Tuple.Nested ((/\), type (/\))
import FRP.Event.MIDI (MIDIEventInTime)
import Record (set)
import Type.Proxy (Proxy(..))
import WAGS (Decorating', Focus(..), Gain(..), GetSetAP, OnOff(..), SinOsc(..), Speaker(..), defaultGetSetAP)

data Key
  = K0
  | K1
  | K2
  | K3
  | K4
  | K5
  | K6
  | K7
  | K8
  | K9

type KeyUnit
  = Gain GetSetAP (SinOsc GetSetAP)

type KlavierType k0 k1 k2 k3 k4 k5 k6 k7 k8 k9
  = { k0 :: Decorating' k0
    , k1 :: Decorating' k1
    , k2 :: Decorating' k2
    , k3 :: Decorating' k3
    , k4 :: Decorating' k4
    , k5 :: Decorating' k5
    , k6 :: Decorating' k6
    , k7 :: Decorating' k7
    , k8 :: Decorating' k8
    , k9 :: Decorating' k9
    } ->
    Speaker
      ( Gain GetSetAP
          ( k0 KeyUnit
              /\ k1 KeyUnit
              /\ k2 KeyUnit
              /\ k3 KeyUnit
              /\ k4 KeyUnit
              /\ k5 KeyUnit
              /\ k6 KeyUnit
              /\ k7 KeyUnit
              /\ k8 KeyUnit
              /\ k9 KeyUnit
              /\ Unit
          )
      )

klavierIdentity =
  { k0: Identity
  , k1: Identity
  , k2: Identity
  , k3: Identity
  , k4: Identity
  , k5: Identity
  , k6: Identity
  , k7: Identity
  , k8: Identity
  , k9: Identity
  }

cursors =
  { k0: fullKeyboard (set (Proxy :: _ "k0") Focus klavierIdentity)
  , k1: fullKeyboard (set (Proxy :: _ "k1") Focus klavierIdentity)
  , k2: fullKeyboard (set (Proxy :: _ "k2") Focus klavierIdentity)
  , k3: fullKeyboard (set (Proxy :: _ "k3") Focus klavierIdentity)
  , k4: fullKeyboard (set (Proxy :: _ "k4") Focus klavierIdentity)
  , k5: fullKeyboard (set (Proxy :: _ "k5") Focus klavierIdentity)
  , k6: fullKeyboard (set (Proxy :: _ "k6") Focus klavierIdentity)
  , k7: fullKeyboard (set (Proxy :: _ "k7") Focus klavierIdentity)
  , k8: fullKeyboard (set (Proxy :: _ "k8") Focus klavierIdentity)
  , k9: fullKeyboard (set (Proxy :: _ "k9") Focus klavierIdentity)
  }

initialKey :: KeyUnit
initialKey = Gain (defaultGetSetAP 0.0) (SinOsc Off (defaultGetSetAP 440.0))

fullKeyboard :: forall k0 k1 k2 k3 k4 k5 k6 k7 k8 k9. KlavierType k0 k1 k2 k3 k4 k5 k6 k7 k8 k9
fullKeyboard { k0, k1, k2, k3, k4, k5, k6, k7, k8, k9 } =
  Speaker
    ( Gain (defaultGetSetAP 1.0)
        ( k0 initialKey
            /\ k1 initialKey
            /\ k2 initialKey
            /\ k3 initialKey
            /\ k4 initialKey
            /\ k5 initialKey
            /\ k6 initialKey
            /\ k7 initialKey
            /\ k8 initialKey
            /\ k9 initialKey
            /\ unit
        )
    )

type Trigger = List { time :: Instant, value :: MIDIEventInTime }

type KeyInfo
  = { startU :: KeyUnit
    , endU :: KeyUnit
    , sustainU :: Number -> KeyUnit
    , startT :: Number
    , cps :: Number
    , keyDuration :: Number
    , i :: Int
    , k :: Key
    }

type MakeRenderingEnv
  = Boolean ->
    Trigger ->
    Number ->
    List Key ->
    List KeyInfo ->
    { onsets :: List KeyInfo
    , newCurrentKeys :: List KeyInfo
    , notesOff :: S.Set Int
    , newAvailableKeys :: List Key
    , futureCurrentKeys :: List KeyInfo
    , futureAvailableKeys :: List Key
    }