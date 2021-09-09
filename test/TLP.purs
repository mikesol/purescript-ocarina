module Test.TLP where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\))
import Data.Typelevel.Num (D0, D1, D3)
import Data.Vec as Vec
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import WAGS.Change (ichange)
import WAGS.Control.Indexed (IxWAG)
import WAGS.Create (icreate)
import WAGS.Create.Optionals (speaker, sinOsc, highpass, playBuf, gain)
import WAGS.CreateT (class CreateT)
import WAGS.Graph.AudioUnit (OnOff(..), TGain, THighpass, TPlayBuf, TSinOsc, TSpeaker)
import WAGS.Interpret (class AudioInterpret)
import WAGS.Patch (class Patch, ipatch)
import WAGS.Template (fromTemplate)
import WAGS.Util (class AutoIncrementingInsert)
import WAGS.WebAPI (BrowserAudioBuffer)

unsafeBuffer :: BrowserAudioBuffer
unsafeBuffer = unsafeCoerce unit

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
  =
  ( speaker :: TSpeaker /\ { speaker_SinOsc_D0 :: Unit }
  , speaker_SinOsc_D0 :: TSinOsc /\ {}
  )

testC0Cr
  :: forall (audio :: Type) (engine :: Type) (proof :: Type) (res :: Type)
   . AudioInterpret audio engine
  => IxWAG audio engine proof res () C0 Unit
testC0Cr = icreate (speaker (sinOsc 440.0))

testC0CrT
  :: forall r (audio :: Type) (engine :: Type) (proof :: Type) (res :: Type)
   . AudioInterpret audio engine
  => CreateT r () C0
  => Patch () C0
  => { | r }
  -> IxWAG audio engine proof res () C0 Unit
testC0CrT _ = ipatch { microphone: Nothing }

testC0CrT'
  :: forall (audio :: Type) (engine :: Type) (proof :: Type) (res :: Type)
   . AudioInterpret audio engine
  => IxWAG audio engine proof res () C0 Unit
testC0CrT' = testC0CrT (speaker (sinOsc 440.0))

testC0Ch
  :: forall (audio :: Type) (engine :: Type) (proof :: Type) (res :: Type)
   . AudioInterpret audio engine
  => IxWAG audio engine proof res C0 C0 Unit
testC0Ch = ichange (speaker (sinOsc 440.0))

-----
type C1
  =
  ( speaker :: TSpeaker /\ { speaker_Highpass_D0 :: Unit }
  , speaker_SinOsc_D0 :: TSinOsc /\ {}
  , speaker_Highpass_D0 :: THighpass /\ { speaker_SinOsc_D0 :: Unit }
  )

testC1Cr
  :: forall (audio :: Type) (engine :: Type) (proof :: Type) (res :: Type)
   . AudioInterpret audio engine
  => IxWAG audio engine proof res () C1 Unit
testC1Cr = icreate (speaker (highpass 440.0 (sinOsc 440.0)))

testC1CrT
  :: forall r (audio :: Type) (engine :: Type) (proof :: Type) (res :: Type)
   . AudioInterpret audio engine
  => CreateT r () C1
  => Patch () C1
  => { | r }
  -> IxWAG audio engine proof res () C1 Unit
testC1CrT _ = ipatch { microphone: Nothing }

testC1CrT'
  :: forall (audio :: Type) (engine :: Type) (proof :: Type) (res :: Type)
   . AudioInterpret audio engine
  => IxWAG audio engine proof res () C1 Unit
testC1CrT' = testC1CrT (speaker (highpass 440.0 (sinOsc 440.0)))

testC1Ch
  :: forall (audio :: Type) (engine :: Type) (proof :: Type) (res :: Type)
   . AudioInterpret audio engine
  => IxWAG audio engine proof res C1 C1 Unit
testC1Ch = ichange (speaker (highpass 440.0 (sinOsc 440.0)))

-----
type C2
  =
  ( speaker :: TSpeaker /\ { speaker_Highpass_D0 :: Unit }
  , speaker_SinOsc_D0 :: TSinOsc /\ {}
  , speaker_Highpass_D0 :: THighpass /\ { speaker_Highpass_D1 :: Unit }
  , speaker_Highpass_D1 :: THighpass /\ { speaker_SinOsc_D0 :: Unit }
  )

testC2Cr
  :: forall (audio :: Type) (engine :: Type) (proof :: Type) (res :: Type)
   . AudioInterpret audio engine
  => IxWAG audio engine proof res () C2 Unit
testC2Cr = icreate (speaker (highpass 440.0 (highpass 440.0 (sinOsc 440.0))))

testC2CrT
  :: forall r (audio :: Type) (engine :: Type) (proof :: Type) (res :: Type)
   . AudioInterpret audio engine
  => CreateT r () C2
  => Patch () C2
  => { | r }
  -> IxWAG audio engine proof res () C2 Unit
testC2CrT _ = ipatch { microphone: Nothing }

testC2CrT'
  :: forall (audio :: Type) (engine :: Type) (proof :: Type) (res :: Type)
   . AudioInterpret audio engine
  => IxWAG audio engine proof res () C2 Unit
testC2CrT' = testC2CrT (speaker (highpass 440.0 (highpass 440.0 (sinOsc 440.0))))

testC2Ch
  :: forall (audio :: Type) (engine :: Type) (proof :: Type) (res :: Type)
   . AudioInterpret audio engine
  => IxWAG audio engine proof res C2 C2 Unit
testC2Ch = ichange (speaker (highpass 440.0 (highpass 440.0 (sinOsc 440.0))))

---
-----
type C3
  =
  ( speaker :: TSpeaker /\ { hello :: Unit, world :: Unit }
  , hello :: TSinOsc /\ {}
  , world :: TSinOsc /\ {}
  )

testC3Cr
  :: forall (audio :: Type) (engine :: Type) (proof :: Type) (res :: Type)
   . AudioInterpret audio engine
  => IxWAG audio engine proof res () C3 Unit
testC3Cr = icreate (speaker { hello: sinOsc 440.0, world: sinOsc 440.0 })

testC3CrT
  :: forall r (audio :: Type) (engine :: Type) (proof :: Type) (res :: Type)
   . AudioInterpret audio engine
  => CreateT r () C3
  => Patch () C3
  => { | r }
  -> IxWAG audio engine proof res () C3 Unit
testC3CrT _ = ipatch { microphone: Nothing }

testC3CrT'
  :: forall (audio :: Type) (engine :: Type) (proof :: Type) (res :: Type)
   . AudioInterpret audio engine
  => IxWAG audio engine proof res () C3 Unit
testC3CrT' = testC3CrT (speaker { hello: sinOsc 440.0, world: sinOsc 440.0 })

testC3Ch
  :: forall (audio :: Type) (engine :: Type) (proof :: Type) (res :: Type)
   . AudioInterpret audio engine
  => IxWAG audio engine proof res C3 C3 Unit
testC3Ch = ichange (speaker { hello: sinOsc 440.0, world: sinOsc 440.0 })

-----
type C4
  =
  ( speaker :: TSpeaker /\ { hello :: Unit, world :: Unit }
  , hello :: THighpass /\ { hello_SinOsc_D0 :: Unit }
  , hello_SinOsc_D0 :: TSinOsc /\ {}
  , world :: TSinOsc /\ {}
  )

testC4Cr
  :: forall (audio :: Type) (engine :: Type) (proof :: Type) (res :: Type)
   . AudioInterpret audio engine
  => IxWAG audio engine proof res () C4 Unit
testC4Cr = icreate (speaker { hello: (highpass 440.0 (sinOsc 440.0)), world: sinOsc 440.0 })

testC4CrT
  :: forall r (audio :: Type) (engine :: Type) (proof :: Type) (res :: Type)
   . AudioInterpret audio engine
  => CreateT r () C4
  => Patch () C4
  => { | r }
  -> IxWAG audio engine proof res () C4 Unit
testC4CrT _ = ipatch { microphone: Nothing }

testC4CrT'
  :: forall (audio :: Type) (engine :: Type) (proof :: Type) (res :: Type)
   . AudioInterpret audio engine
  => IxWAG audio engine proof res () C4 Unit
testC4CrT' = testC4CrT (speaker { hello: (highpass 440.0 (sinOsc 440.0)), world: sinOsc 440.0 })

testC4Ch
  :: forall (audio :: Type) (engine :: Type) (proof :: Type) (res :: Type)
   . AudioInterpret audio engine
  => IxWAG audio engine proof res C4 C4 Unit
testC4Ch = ichange (speaker { hello: (highpass 440.0 (sinOsc 440.0)), world: sinOsc 440.0 })

----
type C5
  =
  ( busFor_D1_hello ::
      TGain
        /\
          { busFor_D1_hello_PlayBuf_D0 :: Unit
          }
  , busFor_D1_hello_PlayBuf_D0 :: TPlayBuf /\ {}
  , busFor_D2_hello ::
      TGain
        /\
          { busFor_D2_hello_PlayBuf_D0 :: Unit
          }
  , busFor_D2_hello_PlayBuf_D0 :: TPlayBuf /\ {}
  , busFor_D3_hello ::
      TGain
        /\
          { busFor_D3_hello_PlayBuf_D0 :: Unit
          }
  , busFor_D3_hello_PlayBuf_D0 :: TPlayBuf /\ {}
  , speaker ::
      TSpeaker
        /\
          { speaker_Gain_D0 :: Unit
          }
  , speaker_Gain_D0 ::
      TGain
        /\
          { busFor_D1_hello :: Unit
          , busFor_D2_hello :: Unit
          , busFor_D3_hello :: Unit
          }
  )

testC5Cr
  :: forall (audio :: Type) (engine :: Type) (proof :: Type) (res :: Type)
   . AudioInterpret audio engine
  => IxWAG audio engine proof res () C5 Unit
testC5Cr =
  icreate
    $ speaker
      ( fromTemplate (Proxy :: _ "hello") ((Vec.fill (const unit)) :: Vec.Vec D3 Unit) \_ _ ->
          gain 0.0
            ( playBuf
                { playbackRate: 1.0
                , onOff: On
                }
                unsafeBuffer
            )
      )

testC5CrT
  :: forall r (audio :: Type) (engine :: Type) (proof :: Type) (res :: Type)
   . AudioInterpret audio engine
  => CreateT r () C5
  => Patch () C5
  => { | r }
  -> IxWAG audio engine proof res () C5 Unit
testC5CrT _ = ipatch { microphone: Nothing }

testC5CrT'
  :: forall (audio :: Type) (engine :: Type) (proof :: Type) (res :: Type)
   . AudioInterpret audio engine
  => IxWAG audio engine proof res () C5 Unit
testC5CrT' =
  testC5CrT
    $ speaker
      ( fromTemplate (Proxy :: _ "hello") ((Vec.fill (const unit)) :: Vec.Vec D3 Unit) \_ _ ->
          gain 0.0
            ( playBuf
                { playbackRate: 1.0
                , onOff: On
                }
                unsafeBuffer
            )
      )

testC5Ch
  :: forall (audio :: Type) (engine :: Type) (proof :: Type) (res :: Type)
   . AudioInterpret audio engine
  => IxWAG audio engine proof res C5 C5 Unit
testC5Ch =
  ichange
    $ speaker
      ( fromTemplate (Proxy :: _ "hello") ((Vec.fill (const unit)) :: Vec.Vec D3 Unit) \_ _ ->
          gain 0.0
            ( playBuf
                { playbackRate: 1.0
                , onOff: On
                }
                unsafeBuffer
            )
      )

-----
type C6
  =
  ( busFor_foo_hello ::
      TGain
        /\
          { busFor_foo_hello_PlayBuf_D0 :: Unit
          }
  , busFor_foo_hello_PlayBuf_D0 :: TPlayBuf /\ {}
  , busFor_bar_hello ::
      TGain
        /\
          { busFor_bar_hello_PlayBuf_D0 :: Unit
          }
  , busFor_bar_hello_PlayBuf_D0 :: TPlayBuf /\ {}
  , busFor_baz_hello ::
      TGain
        /\
          { busFor_baz_hello_PlayBuf_D0 :: Unit
          }
  , busFor_baz_hello_PlayBuf_D0 :: TPlayBuf /\ {}
  , speaker ::
      TSpeaker
        /\
          { speaker_Gain_D0 :: Unit
          }
  , speaker_Gain_D0 ::
      TGain
        /\
          { busFor_foo_hello :: Unit
          , busFor_bar_hello :: Unit
          , busFor_baz_hello :: Unit
          }
  )

testC6Cr
  :: forall (audio :: Type) (engine :: Type) (proof :: Type) (res :: Type)
   . AudioInterpret audio engine
  => IxWAG audio engine proof res () C6 Unit
testC6Cr =
  icreate
    $ speaker
      ( fromTemplate (Proxy :: _ "hello") { foo: unit, bar: unit, baz: unit } \_ _ ->
          gain 0.0
            ( playBuf
                { playbackRate: 1.0
                , onOff: On
                }
                unsafeBuffer
            )
      )

testC6CrT
  :: forall r (audio :: Type) (engine :: Type) (proof :: Type) (res :: Type)
   . AudioInterpret audio engine
  => CreateT r () C6
  => Patch () C6
  => { | r }
  -> IxWAG audio engine proof res () C6 Unit
testC6CrT _ = ipatch { microphone: Nothing }

testC6CrT'
  :: forall (audio :: Type) (engine :: Type) (proof :: Type) (res :: Type)
   . AudioInterpret audio engine
  => IxWAG audio engine proof res () C6 Unit
testC6CrT' =
  testC6CrT
    $ speaker
      ( fromTemplate (Proxy :: _ "hello") { foo: unit, bar: unit, baz: unit } \_ _ ->
          gain 0.0
            ( playBuf
                { playbackRate: 1.0
                , onOff: On
                }
                unsafeBuffer
            )
      )

testC6Ch
  :: forall (audio :: Type) (engine :: Type) (proof :: Type) (res :: Type)
   . AudioInterpret audio engine
  => IxWAG audio engine proof res C6 C6 Unit
testC6Ch =
  ichange
    $ speaker
      ( fromTemplate (Proxy :: _ "hello") { foo: unit, bar: unit, baz: unit } \_ _ ->
          gain 0.0
            ( playBuf
                { playbackRate: 1.0
                , onOff: On
                }
                unsafeBuffer
            )
      )
