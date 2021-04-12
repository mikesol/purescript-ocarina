module Test.WTK where

import Prelude
import Control.Applicative.Indexed (imap)
import Data.Either (Either(..))
import Data.Functor.Indexed (ivoid)
import Data.Identity (Identity(..))
import Data.List (List(..), (:))
import Data.Map as M
import Data.Set as S
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested ((/\), type (/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Prim.Row (class Cons)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Type.Data.Peano (Succ)
import Type.Proxy (Proxy(..))
import WAGS (class Change, class Changes, class Connect, class Create, class Cursor, class GraphIsRenderable, AnAudioUnit(..), AudioParameter, AudioUnitRef(..), Focus(..), FrameT, Instruction(..), SceneT, SinOsc(..), SingleEdge, UniverseC, branch, change, changeAt, create, cursor, env, freeze, gain, highpass, isHere, loop, mix, oneFrame', param, proof, runThunkableWithCount, sinOsc, speaker, start, thunkThunkable, wait, withProof, (@>), (@|>))
import WAGS.Change (ChangeInstruction(..), changes)
import WAGS.Control.Qualified as Ix
import WAGS.Interpret (class AudioInterpret)

type Keyboard k0 k1 k2 k3 k4 k5 k6 k7 k8 k9 k10 k11 k12 k13 k14 k15 k16 k17 k18 k19 k20 k21 k22 k23 k24 k25 k26 k27 k28 k29 k30 k31 k32 k33 k34 k35 k36 k37 k38 k39 k40 k41 k42 k43 k44 k45 k46 k47 k48 k49 k50 k51 k52 k53 k54 k55 k56 k57 k58 k59
  = { k0 :: Tuple (AudioUnitRef k0) Number
    , k1 :: Tuple (AudioUnitRef k1) Number
    , k2 :: Tuple (AudioUnitRef k2) Number
    , k3 :: Tuple (AudioUnitRef k3) Number
    , k4 :: Tuple (AudioUnitRef k4) Number
    , k5 :: Tuple (AudioUnitRef k5) Number
    , k6 :: Tuple (AudioUnitRef k6) Number
    , k7 :: Tuple (AudioUnitRef k7) Number
    , k8 :: Tuple (AudioUnitRef k8) Number
    , k9 :: Tuple (AudioUnitRef k9) Number
    , k10 :: Tuple (AudioUnitRef k10) Number
    , k11 :: Tuple (AudioUnitRef k11) Number
    , k12 :: Tuple (AudioUnitRef k12) Number
    , k13 :: Tuple (AudioUnitRef k13) Number
    , k14 :: Tuple (AudioUnitRef k14) Number
    , k15 :: Tuple (AudioUnitRef k15) Number
    , k16 :: Tuple (AudioUnitRef k16) Number
    , k17 :: Tuple (AudioUnitRef k17) Number
    , k18 :: Tuple (AudioUnitRef k18) Number
    , k19 :: Tuple (AudioUnitRef k19) Number
    , k20 :: Tuple (AudioUnitRef k20) Number
    , k21 :: Tuple (AudioUnitRef k21) Number
    , k22 :: Tuple (AudioUnitRef k22) Number
    , k23 :: Tuple (AudioUnitRef k23) Number
    , k24 :: Tuple (AudioUnitRef k24) Number
    , k25 :: Tuple (AudioUnitRef k25) Number
    , k26 :: Tuple (AudioUnitRef k26) Number
    , k27 :: Tuple (AudioUnitRef k27) Number
    , k28 :: Tuple (AudioUnitRef k28) Number
    , k29 :: Tuple (AudioUnitRef k29) Number
    , k30 :: Tuple (AudioUnitRef k30) Number
    , k31 :: Tuple (AudioUnitRef k31) Number
    , k32 :: Tuple (AudioUnitRef k32) Number
    , k33 :: Tuple (AudioUnitRef k33) Number
    , k34 :: Tuple (AudioUnitRef k34) Number
    , k35 :: Tuple (AudioUnitRef k35) Number
    , k36 :: Tuple (AudioUnitRef k36) Number
    , k37 :: Tuple (AudioUnitRef k37) Number
    , k38 :: Tuple (AudioUnitRef k38) Number
    , k39 :: Tuple (AudioUnitRef k39) Number
    , k40 :: Tuple (AudioUnitRef k40) Number
    , k41 :: Tuple (AudioUnitRef k41) Number
    , k42 :: Tuple (AudioUnitRef k42) Number
    , k43 :: Tuple (AudioUnitRef k43) Number
    , k44 :: Tuple (AudioUnitRef k44) Number
    , k45 :: Tuple (AudioUnitRef k45) Number
    , k46 :: Tuple (AudioUnitRef k46) Number
    , k47 :: Tuple (AudioUnitRef k47) Number
    , k48 :: Tuple (AudioUnitRef k48) Number
    , k49 :: Tuple (AudioUnitRef k49) Number
    , k50 :: Tuple (AudioUnitRef k50) Number
    , k51 :: Tuple (AudioUnitRef k51) Number
    , k52 :: Tuple (AudioUnitRef k52) Number
    , k53 :: Tuple (AudioUnitRef k53) Number
    , k54 :: Tuple (AudioUnitRef k54) Number
    , k55 :: Tuple (AudioUnitRef k55) Number
    , k56 :: Tuple (AudioUnitRef k56) Number
    , k57 :: Tuple (AudioUnitRef k57) Number
    , k58 :: Tuple (AudioUnitRef k58) Number
    , k59 :: Tuple (AudioUnitRef k59) Number
    }

type KeyboardTrigger
  = { k0 :: Boolean
    , k1 :: Boolean
    , k2 :: Boolean
    , k3 :: Boolean
    , k4 :: Boolean
    , k5 :: Boolean
    , k6 :: Boolean
    , k7 :: Boolean
    , k8 :: Boolean
    , k9 :: Boolean
    , k10 :: Boolean
    , k11 :: Boolean
    , k12 :: Boolean
    , k13 :: Boolean
    , k14 :: Boolean
    , k15 :: Boolean
    , k16 :: Boolean
    , k17 :: Boolean
    , k18 :: Boolean
    , k19 :: Boolean
    , k20 :: Boolean
    , k21 :: Boolean
    , k22 :: Boolean
    , k23 :: Boolean
    , k24 :: Boolean
    , k25 :: Boolean
    , k26 :: Boolean
    , k27 :: Boolean
    , k28 :: Boolean
    , k29 :: Boolean
    , k30 :: Boolean
    , k31 :: Boolean
    , k32 :: Boolean
    , k33 :: Boolean
    , k34 :: Boolean
    , k35 :: Boolean
    , k36 :: Boolean
    , k37 :: Boolean
    , k38 :: Boolean
    , k39 :: Boolean
    , k40 :: Boolean
    , k41 :: Boolean
    , k42 :: Boolean
    , k43 :: Boolean
    , k44 :: Boolean
    , k45 :: Boolean
    , k46 :: Boolean
    , k47 :: Boolean
    , k48 :: Boolean
    , k49 :: Boolean
    , k50 :: Boolean
    , k51 :: Boolean
    , k52 :: Boolean
    , k53 :: Boolean
    , k54 :: Boolean
    , k55 :: Boolean
    , k56 :: Boolean
    , k57 :: Boolean
    , k58 :: Boolean
    , k59 :: Boolean
    }

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
  | K10
  | K11
  | K12
  | K13
  | K14
  | K15
  | K16
  | K17
  | K18
  | K19
  | K20
  | K21
  | K22
  | K23
  | K24
  | K25
  | K26
  | K27
  | K28
  | K29
  | K30
  | K31
  | K32
  | K33
  | K34
  | K35
  | K36
  | K37
  | K38
  | K39
  | K40
  | K41
  | K42
  | K43
  | K44
  | K45
  | K46
  | K47
  | K48
  | K49
  | K50
  | K51
  | K52
  | K53
  | K54
  | K55
  | K56
  | K57
  | K58
  | K59

playKeys ::
  forall buildInfo k0 k1 k2 k3 k4 k5 k6 k7 k8 k9 k10 k11 k12 k13 k14 k15 k16 k17 k18 k19 k20 k21 k22 k23 k24 k25 k26 k27 k28 k29 k30 k31 k32 k33 k34 k35 k36 k37 k38 k39 k40 k41 k42 k43 k44 k45 k46 k47 k48 k49 k50 k51 k52 k53 k54 k55 k56 k57 k58 k59 incoming env audio engine proof m currentIdx graph j skolems.
  Monad m =>
  AudioInterpret audio engine =>
  Change (SingleEdge k0) buildInfo graph =>
  Change (SingleEdge k1) buildInfo graph =>
  Change (SingleEdge k2) buildInfo graph =>
  Change (SingleEdge k3) buildInfo graph =>
  Change (SingleEdge k4) buildInfo graph =>
  Change (SingleEdge k5) buildInfo graph =>
  Change (SingleEdge k6) buildInfo graph =>
  Change (SingleEdge k7) buildInfo graph =>
  Change (SingleEdge k8) buildInfo graph =>
  Change (SingleEdge k9) buildInfo graph =>
  Change (SingleEdge k10) buildInfo graph =>
  Change (SingleEdge k11) buildInfo graph =>
  Change (SingleEdge k12) buildInfo graph =>
  Change (SingleEdge k13) buildInfo graph =>
  Change (SingleEdge k14) buildInfo graph =>
  Change (SingleEdge k15) buildInfo graph =>
  Change (SingleEdge k16) buildInfo graph =>
  Change (SingleEdge k17) buildInfo graph =>
  Change (SingleEdge k18) buildInfo graph =>
  Change (SingleEdge k19) buildInfo graph =>
  Change (SingleEdge k20) buildInfo graph =>
  Change (SingleEdge k21) buildInfo graph =>
  Change (SingleEdge k22) buildInfo graph =>
  Change (SingleEdge k23) buildInfo graph =>
  Change (SingleEdge k24) buildInfo graph =>
  Change (SingleEdge k25) buildInfo graph =>
  Change (SingleEdge k26) buildInfo graph =>
  Change (SingleEdge k27) buildInfo graph =>
  Change (SingleEdge k28) buildInfo graph =>
  Change (SingleEdge k29) buildInfo graph =>
  Change (SingleEdge k30) buildInfo graph =>
  Change (SingleEdge k31) buildInfo graph =>
  Change (SingleEdge k32) buildInfo graph =>
  Change (SingleEdge k33) buildInfo graph =>
  Change (SingleEdge k34) buildInfo graph =>
  Change (SingleEdge k35) buildInfo graph =>
  Change (SingleEdge k36) buildInfo graph =>
  Change (SingleEdge k37) buildInfo graph =>
  Change (SingleEdge k38) buildInfo graph =>
  Change (SingleEdge k39) buildInfo graph =>
  Change (SingleEdge k40) buildInfo graph =>
  Change (SingleEdge k41) buildInfo graph =>
  Change (SingleEdge k42) buildInfo graph =>
  Change (SingleEdge k43) buildInfo graph =>
  Change (SingleEdge k44) buildInfo graph =>
  Change (SingleEdge k45) buildInfo graph =>
  Change (SingleEdge k46) buildInfo graph =>
  Change (SingleEdge k47) buildInfo graph =>
  Change (SingleEdge k48) buildInfo graph =>
  Change (SingleEdge k49) buildInfo graph =>
  Change (SingleEdge k50) buildInfo graph =>
  Change (SingleEdge k51) buildInfo graph =>
  Change (SingleEdge k52) buildInfo graph =>
  Change (SingleEdge k53) buildInfo graph =>
  Change (SingleEdge k54) buildInfo graph =>
  Change (SingleEdge k55) buildInfo graph =>
  Change (SingleEdge k56) buildInfo graph =>
  Change (SingleEdge k57) buildInfo graph =>
  Change (SingleEdge k58) buildInfo graph =>
  Change (SingleEdge k59) buildInfo graph =>
  Changes incoming graph =>
  Proxy graph -> incoming -> List Key -> (Key -> buildInfo) -> Keyboard k0 k1 k2 k3 k4 k5 k6 k7 k8 k9 k10 k11 k12 k13 k14 k15 k16 k17 k18 k19 k20 k21 k22 k23 k24 k25 k26 k27 k28 k29 k30 k31 k32 k33 k34 k35 k36 k37 k38 k39 k40 k41 k42 k43 k44 k45 k46 k47 k48 k49 k50 k51 k52 k53 k54 k55 k56 k57 k58 k59 -> FrameT env audio engine proof m (UniverseC currentIdx graph j skolems) (UniverseC currentIdx graph (Succ j) skolems) Unit
-- needs to be explicit
-- otherwise it does not know what instance of changes to pick
playKeys proxyGraph incoming Nil buildInfo keyboard = changes incoming

playKeys proxyGraph incoming (a : b) buildInfo keyboard = case a of
  K0 -> playKeys proxyGraph (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k0)) (buildInfo K0)) incoming) b buildInfo keyboard
  K1 -> playKeys proxyGraph (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k1)) (buildInfo K1)) incoming) b buildInfo keyboard
  K2 -> playKeys proxyGraph (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k2)) (buildInfo K2)) incoming) b buildInfo keyboard
  K3 -> playKeys proxyGraph (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k3)) (buildInfo K3)) incoming) b buildInfo keyboard
  K4 -> playKeys proxyGraph (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k4)) (buildInfo K4)) incoming) b buildInfo keyboard
  K5 -> playKeys proxyGraph (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k5)) (buildInfo K5)) incoming) b buildInfo keyboard
  K6 -> playKeys proxyGraph (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k6)) (buildInfo K6)) incoming) b buildInfo keyboard
  K7 -> playKeys proxyGraph (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k7)) (buildInfo K7)) incoming) b buildInfo keyboard
  K8 -> playKeys proxyGraph (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k8)) (buildInfo K8)) incoming) b buildInfo keyboard
  K9 -> playKeys proxyGraph (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k9)) (buildInfo K9)) incoming) b buildInfo keyboard
  K10 -> playKeys proxyGraph (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k10)) (buildInfo K10)) incoming) b buildInfo keyboard
  K11 -> playKeys proxyGraph (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k11)) (buildInfo K11)) incoming) b buildInfo keyboard
  K12 -> playKeys proxyGraph (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k12)) (buildInfo K12)) incoming) b buildInfo keyboard
  K13 -> playKeys proxyGraph (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k13)) (buildInfo K13)) incoming) b buildInfo keyboard
  K14 -> playKeys proxyGraph (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k14)) (buildInfo K14)) incoming) b buildInfo keyboard
  K15 -> playKeys proxyGraph (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k15)) (buildInfo K15)) incoming) b buildInfo keyboard
  K16 -> playKeys proxyGraph (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k16)) (buildInfo K16)) incoming) b buildInfo keyboard
  K17 -> playKeys proxyGraph (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k17)) (buildInfo K17)) incoming) b buildInfo keyboard
  K18 -> playKeys proxyGraph (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k18)) (buildInfo K18)) incoming) b buildInfo keyboard
  K19 -> playKeys proxyGraph (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k19)) (buildInfo K19)) incoming) b buildInfo keyboard
  K20 -> playKeys proxyGraph (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k20)) (buildInfo K20)) incoming) b buildInfo keyboard
  K21 -> playKeys proxyGraph (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k21)) (buildInfo K21)) incoming) b buildInfo keyboard
  K22 -> playKeys proxyGraph (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k22)) (buildInfo K22)) incoming) b buildInfo keyboard
  K23 -> playKeys proxyGraph (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k23)) (buildInfo K23)) incoming) b buildInfo keyboard
  K24 -> playKeys proxyGraph (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k24)) (buildInfo K24)) incoming) b buildInfo keyboard
  K25 -> playKeys proxyGraph (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k25)) (buildInfo K25)) incoming) b buildInfo keyboard
  K26 -> playKeys proxyGraph (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k26)) (buildInfo K26)) incoming) b buildInfo keyboard
  K27 -> playKeys proxyGraph (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k27)) (buildInfo K27)) incoming) b buildInfo keyboard
  K28 -> playKeys proxyGraph (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k28)) (buildInfo K28)) incoming) b buildInfo keyboard
  K29 -> playKeys proxyGraph (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k29)) (buildInfo K29)) incoming) b buildInfo keyboard
  K30 -> playKeys proxyGraph (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k30)) (buildInfo K30)) incoming) b buildInfo keyboard
  K31 -> playKeys proxyGraph (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k31)) (buildInfo K31)) incoming) b buildInfo keyboard
  K32 -> playKeys proxyGraph (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k32)) (buildInfo K32)) incoming) b buildInfo keyboard
  K33 -> playKeys proxyGraph (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k33)) (buildInfo K33)) incoming) b buildInfo keyboard
  K34 -> playKeys proxyGraph (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k34)) (buildInfo K34)) incoming) b buildInfo keyboard
  K35 -> playKeys proxyGraph (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k35)) (buildInfo K35)) incoming) b buildInfo keyboard
  K36 -> playKeys proxyGraph (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k36)) (buildInfo K36)) incoming) b buildInfo keyboard
  K37 -> playKeys proxyGraph (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k37)) (buildInfo K37)) incoming) b buildInfo keyboard
  K38 -> playKeys proxyGraph (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k38)) (buildInfo K38)) incoming) b buildInfo keyboard
  K39 -> playKeys proxyGraph (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k39)) (buildInfo K39)) incoming) b buildInfo keyboard
  K40 -> playKeys proxyGraph (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k40)) (buildInfo K40)) incoming) b buildInfo keyboard
  K41 -> playKeys proxyGraph (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k41)) (buildInfo K41)) incoming) b buildInfo keyboard
  K42 -> playKeys proxyGraph (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k42)) (buildInfo K42)) incoming) b buildInfo keyboard
  K43 -> playKeys proxyGraph (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k43)) (buildInfo K43)) incoming) b buildInfo keyboard
  K44 -> playKeys proxyGraph (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k44)) (buildInfo K44)) incoming) b buildInfo keyboard
  K45 -> playKeys proxyGraph (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k45)) (buildInfo K45)) incoming) b buildInfo keyboard
  K46 -> playKeys proxyGraph (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k46)) (buildInfo K46)) incoming) b buildInfo keyboard
  K47 -> playKeys proxyGraph (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k47)) (buildInfo K47)) incoming) b buildInfo keyboard
  K48 -> playKeys proxyGraph (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k48)) (buildInfo K48)) incoming) b buildInfo keyboard
  K49 -> playKeys proxyGraph (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k49)) (buildInfo K49)) incoming) b buildInfo keyboard
  K50 -> playKeys proxyGraph (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k50)) (buildInfo K50)) incoming) b buildInfo keyboard
  K51 -> playKeys proxyGraph (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k51)) (buildInfo K51)) incoming) b buildInfo keyboard
  K52 -> playKeys proxyGraph (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k52)) (buildInfo K52)) incoming) b buildInfo keyboard
  K53 -> playKeys proxyGraph (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k53)) (buildInfo K53)) incoming) b buildInfo keyboard
  K54 -> playKeys proxyGraph (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k54)) (buildInfo K54)) incoming) b buildInfo keyboard
  K55 -> playKeys proxyGraph (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k55)) (buildInfo K55)) incoming) b buildInfo keyboard
  K56 -> playKeys proxyGraph (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k56)) (buildInfo K56)) incoming) b buildInfo keyboard
  K57 -> playKeys proxyGraph (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k57)) (buildInfo K57)) incoming) b buildInfo keyboard
  K58 -> playKeys proxyGraph (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k58)) (buildInfo K58)) incoming) b buildInfo keyboard
  K59 -> playKeys proxyGraph (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k59)) (buildInfo K59)) incoming) b buildInfo keyboard