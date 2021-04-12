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
  Proxy (Proxy graph /\ Proxy k0 /\ Proxy k1 /\ Proxy k2 /\ Proxy k3 /\ Proxy k4 /\ Proxy k5 /\ Proxy k6 /\ Proxy k7 /\ Proxy k8 /\ Proxy k9 /\ Proxy k10 /\ Proxy k11 /\ Proxy k12 /\ Proxy k13 /\ Proxy k14 /\ Proxy k15 /\ Proxy k16 /\ Proxy k17 /\ Proxy k18 /\ Proxy k19 /\ Proxy k20 /\ Proxy k21 /\ Proxy k22 /\ Proxy k23 /\ Proxy k24 /\ Proxy k25 /\ Proxy k26 /\ Proxy k27 /\ Proxy k28 /\ Proxy k29 /\ Proxy k30 /\ Proxy k31 /\ Proxy k32 /\ Proxy k33 /\ Proxy k34 /\ Proxy k35 /\ Proxy k36 /\ Proxy k37 /\ Proxy k38 /\ Proxy k39 /\ Proxy k40 /\ Proxy k41 /\ Proxy k42 /\ Proxy k43 /\ Proxy k44 /\ Proxy k45 /\ Proxy k46 /\ Proxy k47 /\ Proxy k48 /\ Proxy k49 /\ Proxy k50 /\ Proxy k51 /\ Proxy k52 /\ Proxy k53 /\ Proxy k54 /\ Proxy k55 /\ Proxy k56 /\ Proxy k57 /\ Proxy k58 /\ Proxy k59) -> incoming -> List Key -> (Key -> buildInfo) -> FrameT env audio engine proof m (UniverseC currentIdx graph j skolems) (UniverseC currentIdx graph (Succ j) skolems) Unit
-- then need key sustaing ctor and coff ctor
-- finally add start/stop to change
playKeys proxyForTypes incoming Nil keyStartCtor = changes incoming

playKeys proxyForTypes incoming (a : b) keyStartCtor = case a of
  K0 -> playKeys proxyForTypes (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k0)) (keyStartCtor K0)) incoming) b keyStartCtor
  K1 -> playKeys proxyForTypes (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k1)) (keyStartCtor K1)) incoming) b keyStartCtor
  K2 -> playKeys proxyForTypes (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k2)) (keyStartCtor K2)) incoming) b keyStartCtor
  K3 -> playKeys proxyForTypes (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k3)) (keyStartCtor K3)) incoming) b keyStartCtor
  K4 -> playKeys proxyForTypes (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k4)) (keyStartCtor K4)) incoming) b keyStartCtor
  K5 -> playKeys proxyForTypes (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k5)) (keyStartCtor K5)) incoming) b keyStartCtor
  K6 -> playKeys proxyForTypes (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k6)) (keyStartCtor K6)) incoming) b keyStartCtor
  K7 -> playKeys proxyForTypes (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k7)) (keyStartCtor K7)) incoming) b keyStartCtor
  K8 -> playKeys proxyForTypes (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k8)) (keyStartCtor K8)) incoming) b keyStartCtor
  K9 -> playKeys proxyForTypes (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k9)) (keyStartCtor K9)) incoming) b keyStartCtor
  K10 -> playKeys proxyForTypes (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k10)) (keyStartCtor K10)) incoming) b keyStartCtor
  K11 -> playKeys proxyForTypes (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k11)) (keyStartCtor K11)) incoming) b keyStartCtor
  K12 -> playKeys proxyForTypes (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k12)) (keyStartCtor K12)) incoming) b keyStartCtor
  K13 -> playKeys proxyForTypes (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k13)) (keyStartCtor K13)) incoming) b keyStartCtor
  K14 -> playKeys proxyForTypes (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k14)) (keyStartCtor K14)) incoming) b keyStartCtor
  K15 -> playKeys proxyForTypes (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k15)) (keyStartCtor K15)) incoming) b keyStartCtor
  K16 -> playKeys proxyForTypes (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k16)) (keyStartCtor K16)) incoming) b keyStartCtor
  K17 -> playKeys proxyForTypes (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k17)) (keyStartCtor K17)) incoming) b keyStartCtor
  K18 -> playKeys proxyForTypes (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k18)) (keyStartCtor K18)) incoming) b keyStartCtor
  K19 -> playKeys proxyForTypes (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k19)) (keyStartCtor K19)) incoming) b keyStartCtor
  K20 -> playKeys proxyForTypes (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k20)) (keyStartCtor K20)) incoming) b keyStartCtor
  K21 -> playKeys proxyForTypes (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k21)) (keyStartCtor K21)) incoming) b keyStartCtor
  K22 -> playKeys proxyForTypes (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k22)) (keyStartCtor K22)) incoming) b keyStartCtor
  K23 -> playKeys proxyForTypes (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k23)) (keyStartCtor K23)) incoming) b keyStartCtor
  K24 -> playKeys proxyForTypes (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k24)) (keyStartCtor K24)) incoming) b keyStartCtor
  K25 -> playKeys proxyForTypes (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k25)) (keyStartCtor K25)) incoming) b keyStartCtor
  K26 -> playKeys proxyForTypes (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k26)) (keyStartCtor K26)) incoming) b keyStartCtor
  K27 -> playKeys proxyForTypes (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k27)) (keyStartCtor K27)) incoming) b keyStartCtor
  K28 -> playKeys proxyForTypes (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k28)) (keyStartCtor K28)) incoming) b keyStartCtor
  K29 -> playKeys proxyForTypes (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k29)) (keyStartCtor K29)) incoming) b keyStartCtor
  K30 -> playKeys proxyForTypes (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k30)) (keyStartCtor K30)) incoming) b keyStartCtor
  K31 -> playKeys proxyForTypes (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k31)) (keyStartCtor K31)) incoming) b keyStartCtor
  K32 -> playKeys proxyForTypes (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k32)) (keyStartCtor K32)) incoming) b keyStartCtor
  K33 -> playKeys proxyForTypes (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k33)) (keyStartCtor K33)) incoming) b keyStartCtor
  K34 -> playKeys proxyForTypes (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k34)) (keyStartCtor K34)) incoming) b keyStartCtor
  K35 -> playKeys proxyForTypes (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k35)) (keyStartCtor K35)) incoming) b keyStartCtor
  K36 -> playKeys proxyForTypes (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k36)) (keyStartCtor K36)) incoming) b keyStartCtor
  K37 -> playKeys proxyForTypes (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k37)) (keyStartCtor K37)) incoming) b keyStartCtor
  K38 -> playKeys proxyForTypes (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k38)) (keyStartCtor K38)) incoming) b keyStartCtor
  K39 -> playKeys proxyForTypes (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k39)) (keyStartCtor K39)) incoming) b keyStartCtor
  K40 -> playKeys proxyForTypes (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k40)) (keyStartCtor K40)) incoming) b keyStartCtor
  K41 -> playKeys proxyForTypes (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k41)) (keyStartCtor K41)) incoming) b keyStartCtor
  K42 -> playKeys proxyForTypes (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k42)) (keyStartCtor K42)) incoming) b keyStartCtor
  K43 -> playKeys proxyForTypes (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k43)) (keyStartCtor K43)) incoming) b keyStartCtor
  K44 -> playKeys proxyForTypes (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k44)) (keyStartCtor K44)) incoming) b keyStartCtor
  K45 -> playKeys proxyForTypes (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k45)) (keyStartCtor K45)) incoming) b keyStartCtor
  K46 -> playKeys proxyForTypes (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k46)) (keyStartCtor K46)) incoming) b keyStartCtor
  K47 -> playKeys proxyForTypes (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k47)) (keyStartCtor K47)) incoming) b keyStartCtor
  K48 -> playKeys proxyForTypes (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k48)) (keyStartCtor K48)) incoming) b keyStartCtor
  K49 -> playKeys proxyForTypes (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k49)) (keyStartCtor K49)) incoming) b keyStartCtor
  K50 -> playKeys proxyForTypes (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k50)) (keyStartCtor K50)) incoming) b keyStartCtor
  K51 -> playKeys proxyForTypes (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k51)) (keyStartCtor K51)) incoming) b keyStartCtor
  K52 -> playKeys proxyForTypes (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k52)) (keyStartCtor K52)) incoming) b keyStartCtor
  K53 -> playKeys proxyForTypes (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k53)) (keyStartCtor K53)) incoming) b keyStartCtor
  K54 -> playKeys proxyForTypes (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k54)) (keyStartCtor K54)) incoming) b keyStartCtor
  K55 -> playKeys proxyForTypes (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k55)) (keyStartCtor K55)) incoming) b keyStartCtor
  K56 -> playKeys proxyForTypes (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k56)) (keyStartCtor K56)) incoming) b keyStartCtor
  K57 -> playKeys proxyForTypes (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k57)) (keyStartCtor K57)) incoming) b keyStartCtor
  K58 -> playKeys proxyForTypes (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k58)) (keyStartCtor K58)) incoming) b keyStartCtor
  K59 -> playKeys proxyForTypes (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k59)) (keyStartCtor K59)) incoming) b keyStartCtor