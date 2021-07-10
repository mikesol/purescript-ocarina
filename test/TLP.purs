module Test.TLP where

import Prelude
import Data.Tuple.Nested (type (/\))
import Data.Typelevel.Num (D0, D1)
import Type.Proxy (Proxy(..))
import WAGS.Change (ichange)
import WAGS.Control.Indexed (IxWAG)
import WAGS.Create (icreate)
import WAGS.Create.Optionals (speaker, sinOsc, highpass)
import WAGS.CreateT (class CreateT)
import WAGS.Graph.AudioUnit (THighpass, TSinOsc, TSpeaker)
import WAGS.Interpret (class AudioInterpret)
import WAGS.Patch (class Patch, ipatch)
import WAGS.Util (class AutoIncrementingInsert)

testAII0 :: Proxy (D0 /\ ((TSinOsc /\ D0) /\ Unit))
testAII0 = Proxy :: forall val omap. AutoIncrementingInsert TSinOsc Unit val omap => Proxy (val /\ omap)

testAII1 :: Proxy (D1 /\ ((TSinOsc /\ D1) /\ Unit))
testAII1 = Proxy :: forall val omap. AutoIncrementingInsert TSinOsc ((TSinOsc /\ D0) /\ Unit) val omap => Proxy (val /\ omap)

testAII2 :: Proxy (D0 /\ ((TSinOsc /\ D0) /\ (THighpass /\ D0) /\ Unit))
testAII2 = Proxy :: forall val omap. AutoIncrementingInsert THighpass ((TSinOsc /\ D0) /\ Unit) val omap => Proxy (val /\ omap)

testAII3 :: Proxy (D1 /\ ((TSinOsc /\ D0) /\ (THighpass /\ D1) /\ Unit))
testAII3 = Proxy :: forall val omap. AutoIncrementingInsert THighpass ((TSinOsc /\ D0) /\ (THighpass /\ D0) /\ Unit) val omap => Proxy (val /\ omap)

testAII4 :: Proxy (D1 /\ ((TSinOsc /\ D1) /\ (THighpass /\ D0) /\ Unit))
testAII4 = Proxy :: forall val omap. AutoIncrementingInsert TSinOsc ((TSinOsc /\ D0) /\ (THighpass /\ D0) /\ Unit) val omap => Proxy (val /\ omap)

-----
type C0
  = ( speaker :: TSpeaker /\ { speaker_SinOsc_D0 :: Unit }
    , speaker_SinOsc_D0 :: TSinOsc /\ {}
    )

testC0Cr ::
  forall (audio :: Type) (engine :: Type) (proof :: Type) (res :: Type).
  AudioInterpret audio engine =>
  IxWAG audio engine proof res {} { | C0 } Unit
testC0Cr = icreate (speaker (sinOsc 440.0))

testC0CrT ::
  forall r (audio :: Type) (engine :: Type) (proof :: Type) (res :: Type).
  AudioInterpret audio engine =>
  CreateT r () C0 =>
  Patch () C0 =>
  { | r } ->
  IxWAG audio engine proof res {} { | C0 } Unit
testC0CrT _ = ipatch

testC0CrT' ::
  forall (audio :: Type) (engine :: Type) (proof :: Type) (res :: Type).
  AudioInterpret audio engine =>
  IxWAG audio engine proof res {} { | C0 } Unit
testC0CrT' = testC0CrT (speaker (sinOsc 440.0))

testC0Ch ::
  forall (audio :: Type) (engine :: Type) (proof :: Type) (res :: Type).
  AudioInterpret audio engine =>
  IxWAG audio engine proof res { | C0 } { | C0 } Unit
testC0Ch = ichange (speaker (sinOsc 440.0))

-----
type C1
  = ( speaker :: TSpeaker /\ { speaker_Highpass_D0 :: Unit }
    , speaker_SinOsc_D0 :: TSinOsc /\ {}
    , speaker_Highpass_D0 :: THighpass /\ { speaker_SinOsc_D0 :: Unit }
    )

testC1Cr ::
  forall (audio :: Type) (engine :: Type) (proof :: Type) (res :: Type).
  AudioInterpret audio engine =>
  IxWAG audio engine proof res {} { | C1 } Unit
testC1Cr = icreate (speaker (highpass 440.0 (sinOsc 440.0)))

testC1CrT ::
  forall r (audio :: Type) (engine :: Type) (proof :: Type) (res :: Type).
  AudioInterpret audio engine =>
  CreateT r () C1 =>
  Patch () C1 =>
  { | r } ->
  IxWAG audio engine proof res {} { | C1 } Unit
testC1CrT _ = ipatch

testC1CrT' ::
  forall (audio :: Type) (engine :: Type) (proof :: Type) (res :: Type).
  AudioInterpret audio engine =>
  IxWAG audio engine proof res {} { | C1 } Unit
testC1CrT' = testC1CrT (speaker (highpass 440.0 (sinOsc 440.0)))

testC1Ch ::
  forall (audio :: Type) (engine :: Type) (proof :: Type) (res :: Type).
  AudioInterpret audio engine =>
  IxWAG audio engine proof res { | C1 } { | C1 } Unit
testC1Ch = ichange (speaker (highpass 440.0 (sinOsc 440.0)))

-----
type C2
  = ( speaker :: TSpeaker /\ { speaker_Highpass_D0 :: Unit }
    , speaker_SinOsc_D0 :: TSinOsc /\ {}
    , speaker_Highpass_D0 :: THighpass /\ { speaker_Highpass_D1 :: Unit }
    , speaker_Highpass_D1 :: THighpass /\ { speaker_SinOsc_D0 :: Unit }
    )

testC2Cr ::
  forall (audio :: Type) (engine :: Type) (proof :: Type) (res :: Type).
  AudioInterpret audio engine =>
  IxWAG audio engine proof res {} { | C2 } Unit
testC2Cr = icreate (speaker (highpass 440.0 (highpass 440.0(sinOsc 440.0))))

testC2CrT ::
  forall r (audio :: Type) (engine :: Type) (proof :: Type) (res :: Type).
  AudioInterpret audio engine =>
  CreateT r () C2 =>
  Patch () C2 =>
  { | r } ->
  IxWAG audio engine proof res {} { | C2 } Unit
testC2CrT _ = ipatch

testC2CrT' ::
  forall (audio :: Type) (engine :: Type) (proof :: Type) (res :: Type).
  AudioInterpret audio engine =>
  IxWAG audio engine proof res {} { | C2 } Unit
testC2CrT' = testC2CrT (speaker (highpass 440.0 (highpass 440.0(sinOsc 440.0))))

testC2Ch ::
  forall (audio :: Type) (engine :: Type) (proof :: Type) (res :: Type).
  AudioInterpret audio engine =>
  IxWAG audio engine proof res { | C2 } { | C2 } Unit
testC2Ch = ichange (speaker (highpass 440.0 (highpass 440.0(sinOsc 440.0))))

---

-----
type C3
  = ( speaker :: TSpeaker /\ { hello :: Unit, world :: Unit }
    , hello :: TSinOsc /\ {}
    , world :: TSinOsc /\ {}
    )

testC3Cr ::
  forall (audio :: Type) (engine :: Type) (proof :: Type) (res :: Type).
  AudioInterpret audio engine =>
  IxWAG audio engine proof res {} { | C3 } Unit
testC3Cr = icreate (speaker { hello: sinOsc 440.0, world: sinOsc 440.0 } )

testC3CrT ::
  forall r (audio :: Type) (engine :: Type) (proof :: Type) (res :: Type).
  AudioInterpret audio engine =>
  CreateT r () C3 =>
  Patch () C3 =>
  { | r } ->
  IxWAG audio engine proof res {} { | C3 } Unit
testC3CrT _ = ipatch

testC3CrT' ::
  forall (audio :: Type) (engine :: Type) (proof :: Type) (res :: Type).
  AudioInterpret audio engine =>
  IxWAG audio engine proof res {} { | C3 } Unit
testC3CrT' = testC3CrT (speaker { hello: sinOsc 440.0, world: sinOsc 440.0 })

testC3Ch ::
  forall (audio :: Type) (engine :: Type) (proof :: Type) (res :: Type).
  AudioInterpret audio engine =>
  IxWAG audio engine proof res { | C3 } { | C3 } Unit
testC3Ch = ichange (speaker { hello: sinOsc 440.0, world: sinOsc 440.0 })

-----
type C4
  = ( speaker :: TSpeaker /\ { hello :: Unit, world :: Unit }
    , hello :: THighpass /\ { hello_SinOsc_D0 :: Unit }
    , hello_SinOsc_D0 :: TSinOsc /\ {}
    , world :: TSinOsc /\ {}
    )

testC4Cr ::
  forall (audio :: Type) (engine :: Type) (proof :: Type) (res :: Type).
  AudioInterpret audio engine =>
  IxWAG audio engine proof res {} { | C4 } Unit
testC4Cr = icreate (speaker { hello: (highpass 440.0 (sinOsc 440.0)), world: sinOsc 440.0 } )

testC4CrT ::
  forall r (audio :: Type) (engine :: Type) (proof :: Type) (res :: Type).
  AudioInterpret audio engine =>
  CreateT r () C4 =>
  Patch () C4 =>
  { | r } ->
  IxWAG audio engine proof res {} { | C4 } Unit
testC4CrT _ = ipatch

testC4CrT' ::
  forall (audio :: Type) (engine :: Type) (proof :: Type) (res :: Type).
  AudioInterpret audio engine =>
  IxWAG audio engine proof res {} { | C4 } Unit
testC4CrT' = testC4CrT (speaker { hello: (highpass 440.0 (sinOsc 440.0)), world: sinOsc 440.0 })

testC4Ch ::
  forall (audio :: Type) (engine :: Type) (proof :: Type) (res :: Type).
  AudioInterpret audio engine =>
  IxWAG audio engine proof res { | C4 } { | C4 } Unit
testC4Ch = ichange (speaker { hello: (highpass 440.0 (sinOsc 440.0)), world: sinOsc 440.0 })