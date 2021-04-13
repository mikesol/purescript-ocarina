module WAGS.Example.WTK where

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
import Type.Data.Peano (Succ)
import Type.Proxy (Proxy(..))
import WAGS (class Change, class Changes, class Connect, class Create, class Cursor, class GraphIsRenderable, AnAudioUnit(..), AudioParameter, AudioUnitRef(..), Decorating(..), Focus(..), FrameT, Gain(..), GetSetAP, Instruction(..), OnOff(..), SceneT, SinOsc(..), SingleEdge, Speaker(..), UniverseC, branch, change, changeAt, create, cursor, defaultGetSetAP, dk, env, freeze, gain, highpass, isHere, loop, mix, oneFrame', param, proof, runThunkableWithCount, sinOsc, speaker, start, thunkThunkable, wait, withProof, (@>), (@|>))
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
  { proxyForTypes :: Proxy (Proxy graph /\ Proxy k0 /\ Proxy k1 /\ Proxy k2 /\ Proxy k3 /\ Proxy k4 /\ Proxy k5 /\ Proxy k6 /\ Proxy k7 /\ Proxy k8 /\ Proxy k9 /\ Proxy k10 /\ Proxy k11 /\ Proxy k12 /\ Proxy k13 /\ Proxy k14 /\ Proxy k15 /\ Proxy k16 /\ Proxy k17 /\ Proxy k18 /\ Proxy k19 /\ Proxy k20 /\ Proxy k21 /\ Proxy k22 /\ Proxy k23 /\ Proxy k24 /\ Proxy k25 /\ Proxy k26 /\ Proxy k27 /\ Proxy k28 /\ Proxy k29 /\ Proxy k30 /\ Proxy k31 /\ Proxy k32 /\ Proxy k33 /\ Proxy k34 /\ Proxy k35 /\ Proxy k36 /\ Proxy k37 /\ Proxy k38 /\ Proxy k39 /\ Proxy k40 /\ Proxy k41 /\ Proxy k42 /\ Proxy k43 /\ Proxy k44 /\ Proxy k45 /\ Proxy k46 /\ Proxy k47 /\ Proxy k48 /\ Proxy k49 /\ Proxy k50 /\ Proxy k51 /\ Proxy k52 /\ Proxy k53 /\ Proxy k54 /\ Proxy k55 /\ Proxy k56 /\ Proxy k57 /\ Proxy k58 /\ Proxy k59)
  , currentTime :: Number
  , keyDuration :: Number
  , keyStartCtor :: Key -> buildInfo
  , keySustainCtor :: Number -> Key -> buildInfo
  , keyEndCtor :: Key -> buildInfo
  } ->
  incoming -> List Key -> List (Tuple Number Key) -> FrameT env audio engine proof m (UniverseC currentIdx graph j skolems) (UniverseC currentIdx graph (Succ j) skolems) Unit
-- finally add start/stop to change
playKeys rec incoming Nil Nil = changes incoming

playKeys rec@{ currentTime, keyDuration, keySustainCtor, keyEndCtor } incoming Nil ((Tuple onset a) : b) = case a of
  K0 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k0)) ((if currentTime - onset > keyDuration then keyEndCtor else keySustainCtor (currentTime - onset)) K0)) incoming) Nil b
  K1 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k1)) ((if currentTime - onset > keyDuration then keyEndCtor else keySustainCtor (currentTime - onset)) K1)) incoming) Nil b
  K2 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k2)) ((if currentTime - onset > keyDuration then keyEndCtor else keySustainCtor (currentTime - onset)) K2)) incoming) Nil b
  K3 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k3)) ((if currentTime - onset > keyDuration then keyEndCtor else keySustainCtor (currentTime - onset)) K3)) incoming) Nil b
  K4 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k4)) ((if currentTime - onset > keyDuration then keyEndCtor else keySustainCtor (currentTime - onset)) K4)) incoming) Nil b
  K5 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k5)) ((if currentTime - onset > keyDuration then keyEndCtor else keySustainCtor (currentTime - onset)) K5)) incoming) Nil b
  K6 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k6)) ((if currentTime - onset > keyDuration then keyEndCtor else keySustainCtor (currentTime - onset)) K6)) incoming) Nil b
  K7 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k7)) ((if currentTime - onset > keyDuration then keyEndCtor else keySustainCtor (currentTime - onset)) K7)) incoming) Nil b
  K8 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k8)) ((if currentTime - onset > keyDuration then keyEndCtor else keySustainCtor (currentTime - onset)) K8)) incoming) Nil b
  K9 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k9)) ((if currentTime - onset > keyDuration then keyEndCtor else keySustainCtor (currentTime - onset)) K9)) incoming) Nil b
  K10 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k10)) ((if currentTime - onset > keyDuration then keyEndCtor else keySustainCtor (currentTime - onset)) K10)) incoming) Nil b
  K11 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k11)) ((if currentTime - onset > keyDuration then keyEndCtor else keySustainCtor (currentTime - onset)) K11)) incoming) Nil b
  K12 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k12)) ((if currentTime - onset > keyDuration then keyEndCtor else keySustainCtor (currentTime - onset)) K12)) incoming) Nil b
  K13 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k13)) ((if currentTime - onset > keyDuration then keyEndCtor else keySustainCtor (currentTime - onset)) K13)) incoming) Nil b
  K14 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k14)) ((if currentTime - onset > keyDuration then keyEndCtor else keySustainCtor (currentTime - onset)) K14)) incoming) Nil b
  K15 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k15)) ((if currentTime - onset > keyDuration then keyEndCtor else keySustainCtor (currentTime - onset)) K15)) incoming) Nil b
  K16 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k16)) ((if currentTime - onset > keyDuration then keyEndCtor else keySustainCtor (currentTime - onset)) K16)) incoming) Nil b
  K17 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k17)) ((if currentTime - onset > keyDuration then keyEndCtor else keySustainCtor (currentTime - onset)) K17)) incoming) Nil b
  K18 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k18)) ((if currentTime - onset > keyDuration then keyEndCtor else keySustainCtor (currentTime - onset)) K18)) incoming) Nil b
  K19 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k19)) ((if currentTime - onset > keyDuration then keyEndCtor else keySustainCtor (currentTime - onset)) K19)) incoming) Nil b
  K20 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k20)) ((if currentTime - onset > keyDuration then keyEndCtor else keySustainCtor (currentTime - onset)) K20)) incoming) Nil b
  K21 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k21)) ((if currentTime - onset > keyDuration then keyEndCtor else keySustainCtor (currentTime - onset)) K21)) incoming) Nil b
  K22 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k22)) ((if currentTime - onset > keyDuration then keyEndCtor else keySustainCtor (currentTime - onset)) K22)) incoming) Nil b
  K23 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k23)) ((if currentTime - onset > keyDuration then keyEndCtor else keySustainCtor (currentTime - onset)) K23)) incoming) Nil b
  K24 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k24)) ((if currentTime - onset > keyDuration then keyEndCtor else keySustainCtor (currentTime - onset)) K24)) incoming) Nil b
  K25 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k25)) ((if currentTime - onset > keyDuration then keyEndCtor else keySustainCtor (currentTime - onset)) K25)) incoming) Nil b
  K26 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k26)) ((if currentTime - onset > keyDuration then keyEndCtor else keySustainCtor (currentTime - onset)) K26)) incoming) Nil b
  K27 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k27)) ((if currentTime - onset > keyDuration then keyEndCtor else keySustainCtor (currentTime - onset)) K27)) incoming) Nil b
  K28 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k28)) ((if currentTime - onset > keyDuration then keyEndCtor else keySustainCtor (currentTime - onset)) K28)) incoming) Nil b
  K29 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k29)) ((if currentTime - onset > keyDuration then keyEndCtor else keySustainCtor (currentTime - onset)) K29)) incoming) Nil b
  K30 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k30)) ((if currentTime - onset > keyDuration then keyEndCtor else keySustainCtor (currentTime - onset)) K30)) incoming) Nil b
  K31 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k31)) ((if currentTime - onset > keyDuration then keyEndCtor else keySustainCtor (currentTime - onset)) K31)) incoming) Nil b
  K32 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k32)) ((if currentTime - onset > keyDuration then keyEndCtor else keySustainCtor (currentTime - onset)) K32)) incoming) Nil b
  K33 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k33)) ((if currentTime - onset > keyDuration then keyEndCtor else keySustainCtor (currentTime - onset)) K33)) incoming) Nil b
  K34 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k34)) ((if currentTime - onset > keyDuration then keyEndCtor else keySustainCtor (currentTime - onset)) K34)) incoming) Nil b
  K35 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k35)) ((if currentTime - onset > keyDuration then keyEndCtor else keySustainCtor (currentTime - onset)) K35)) incoming) Nil b
  K36 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k36)) ((if currentTime - onset > keyDuration then keyEndCtor else keySustainCtor (currentTime - onset)) K36)) incoming) Nil b
  K37 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k37)) ((if currentTime - onset > keyDuration then keyEndCtor else keySustainCtor (currentTime - onset)) K37)) incoming) Nil b
  K38 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k38)) ((if currentTime - onset > keyDuration then keyEndCtor else keySustainCtor (currentTime - onset)) K38)) incoming) Nil b
  K39 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k39)) ((if currentTime - onset > keyDuration then keyEndCtor else keySustainCtor (currentTime - onset)) K39)) incoming) Nil b
  K40 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k40)) ((if currentTime - onset > keyDuration then keyEndCtor else keySustainCtor (currentTime - onset)) K40)) incoming) Nil b
  K41 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k41)) ((if currentTime - onset > keyDuration then keyEndCtor else keySustainCtor (currentTime - onset)) K41)) incoming) Nil b
  K42 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k42)) ((if currentTime - onset > keyDuration then keyEndCtor else keySustainCtor (currentTime - onset)) K42)) incoming) Nil b
  K43 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k43)) ((if currentTime - onset > keyDuration then keyEndCtor else keySustainCtor (currentTime - onset)) K43)) incoming) Nil b
  K44 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k44)) ((if currentTime - onset > keyDuration then keyEndCtor else keySustainCtor (currentTime - onset)) K44)) incoming) Nil b
  K45 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k45)) ((if currentTime - onset > keyDuration then keyEndCtor else keySustainCtor (currentTime - onset)) K45)) incoming) Nil b
  K46 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k46)) ((if currentTime - onset > keyDuration then keyEndCtor else keySustainCtor (currentTime - onset)) K46)) incoming) Nil b
  K47 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k47)) ((if currentTime - onset > keyDuration then keyEndCtor else keySustainCtor (currentTime - onset)) K47)) incoming) Nil b
  K48 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k48)) ((if currentTime - onset > keyDuration then keyEndCtor else keySustainCtor (currentTime - onset)) K48)) incoming) Nil b
  K49 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k49)) ((if currentTime - onset > keyDuration then keyEndCtor else keySustainCtor (currentTime - onset)) K49)) incoming) Nil b
  K50 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k50)) ((if currentTime - onset > keyDuration then keyEndCtor else keySustainCtor (currentTime - onset)) K50)) incoming) Nil b
  K51 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k51)) ((if currentTime - onset > keyDuration then keyEndCtor else keySustainCtor (currentTime - onset)) K51)) incoming) Nil b
  K52 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k52)) ((if currentTime - onset > keyDuration then keyEndCtor else keySustainCtor (currentTime - onset)) K52)) incoming) Nil b
  K53 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k53)) ((if currentTime - onset > keyDuration then keyEndCtor else keySustainCtor (currentTime - onset)) K53)) incoming) Nil b
  K54 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k54)) ((if currentTime - onset > keyDuration then keyEndCtor else keySustainCtor (currentTime - onset)) K54)) incoming) Nil b
  K55 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k55)) ((if currentTime - onset > keyDuration then keyEndCtor else keySustainCtor (currentTime - onset)) K55)) incoming) Nil b
  K56 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k56)) ((if currentTime - onset > keyDuration then keyEndCtor else keySustainCtor (currentTime - onset)) K56)) incoming) Nil b
  K57 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k57)) ((if currentTime - onset > keyDuration then keyEndCtor else keySustainCtor (currentTime - onset)) K57)) incoming) Nil b
  K58 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k58)) ((if currentTime - onset > keyDuration then keyEndCtor else keySustainCtor (currentTime - onset)) K58)) incoming) Nil b
  K59 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k59)) ((if currentTime - onset > keyDuration then keyEndCtor else keySustainCtor (currentTime - onset)) K59)) incoming) Nil b

playKeys rec@{ keyStartCtor } incoming (a : b) currentPlaying = case a of
  K0 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k0)) (keyStartCtor K0)) incoming) b currentPlaying
  K1 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k1)) (keyStartCtor K1)) incoming) b currentPlaying
  K2 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k2)) (keyStartCtor K2)) incoming) b currentPlaying
  K3 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k3)) (keyStartCtor K3)) incoming) b currentPlaying
  K4 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k4)) (keyStartCtor K4)) incoming) b currentPlaying
  K5 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k5)) (keyStartCtor K5)) incoming) b currentPlaying
  K6 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k6)) (keyStartCtor K6)) incoming) b currentPlaying
  K7 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k7)) (keyStartCtor K7)) incoming) b currentPlaying
  K8 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k8)) (keyStartCtor K8)) incoming) b currentPlaying
  K9 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k9)) (keyStartCtor K9)) incoming) b currentPlaying
  K10 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k10)) (keyStartCtor K10)) incoming) b currentPlaying
  K11 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k11)) (keyStartCtor K11)) incoming) b currentPlaying
  K12 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k12)) (keyStartCtor K12)) incoming) b currentPlaying
  K13 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k13)) (keyStartCtor K13)) incoming) b currentPlaying
  K14 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k14)) (keyStartCtor K14)) incoming) b currentPlaying
  K15 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k15)) (keyStartCtor K15)) incoming) b currentPlaying
  K16 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k16)) (keyStartCtor K16)) incoming) b currentPlaying
  K17 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k17)) (keyStartCtor K17)) incoming) b currentPlaying
  K18 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k18)) (keyStartCtor K18)) incoming) b currentPlaying
  K19 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k19)) (keyStartCtor K19)) incoming) b currentPlaying
  K20 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k20)) (keyStartCtor K20)) incoming) b currentPlaying
  K21 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k21)) (keyStartCtor K21)) incoming) b currentPlaying
  K22 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k22)) (keyStartCtor K22)) incoming) b currentPlaying
  K23 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k23)) (keyStartCtor K23)) incoming) b currentPlaying
  K24 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k24)) (keyStartCtor K24)) incoming) b currentPlaying
  K25 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k25)) (keyStartCtor K25)) incoming) b currentPlaying
  K26 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k26)) (keyStartCtor K26)) incoming) b currentPlaying
  K27 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k27)) (keyStartCtor K27)) incoming) b currentPlaying
  K28 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k28)) (keyStartCtor K28)) incoming) b currentPlaying
  K29 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k29)) (keyStartCtor K29)) incoming) b currentPlaying
  K30 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k30)) (keyStartCtor K30)) incoming) b currentPlaying
  K31 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k31)) (keyStartCtor K31)) incoming) b currentPlaying
  K32 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k32)) (keyStartCtor K32)) incoming) b currentPlaying
  K33 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k33)) (keyStartCtor K33)) incoming) b currentPlaying
  K34 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k34)) (keyStartCtor K34)) incoming) b currentPlaying
  K35 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k35)) (keyStartCtor K35)) incoming) b currentPlaying
  K36 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k36)) (keyStartCtor K36)) incoming) b currentPlaying
  K37 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k37)) (keyStartCtor K37)) incoming) b currentPlaying
  K38 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k38)) (keyStartCtor K38)) incoming) b currentPlaying
  K39 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k39)) (keyStartCtor K39)) incoming) b currentPlaying
  K40 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k40)) (keyStartCtor K40)) incoming) b currentPlaying
  K41 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k41)) (keyStartCtor K41)) incoming) b currentPlaying
  K42 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k42)) (keyStartCtor K42)) incoming) b currentPlaying
  K43 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k43)) (keyStartCtor K43)) incoming) b currentPlaying
  K44 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k44)) (keyStartCtor K44)) incoming) b currentPlaying
  K45 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k45)) (keyStartCtor K45)) incoming) b currentPlaying
  K46 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k46)) (keyStartCtor K46)) incoming) b currentPlaying
  K47 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k47)) (keyStartCtor K47)) incoming) b currentPlaying
  K48 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k48)) (keyStartCtor K48)) incoming) b currentPlaying
  K49 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k49)) (keyStartCtor K49)) incoming) b currentPlaying
  K50 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k50)) (keyStartCtor K50)) incoming) b currentPlaying
  K51 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k51)) (keyStartCtor K51)) incoming) b currentPlaying
  K52 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k52)) (keyStartCtor K52)) incoming) b currentPlaying
  K53 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k53)) (keyStartCtor K53)) incoming) b currentPlaying
  K54 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k54)) (keyStartCtor K54)) incoming) b currentPlaying
  K55 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k55)) (keyStartCtor K55)) incoming) b currentPlaying
  K56 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k56)) (keyStartCtor K56)) incoming) b currentPlaying
  K57 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k57)) (keyStartCtor K57)) incoming) b currentPlaying
  K58 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k58)) (keyStartCtor K58)) incoming) b currentPlaying
  K59 -> playKeys rec (Tuple (ChangeInstruction (Proxy :: Proxy (SingleEdge k59)) (keyStartCtor K59)) incoming) b currentPlaying

keyToPitch :: Key -> Number
keyToPitch = case _ of
  K0 -> 69.295658
  K1 -> 73.416192
  K2 -> 77.781746
  K3 -> 82.406889
  K4 -> 87.307058
  K5 -> 92.498606
  K6 -> 97.998859
  K7 -> 103.826174
  K8 -> 110.000000
  K9 -> 116.540940
  K10 -> 123.470825
  K11 -> 130.812783
  K12 -> 138.591315
  K13 -> 146.832384
  K14 -> 155.563492
  K15 -> 164.813778
  K16 -> 174.614116
  K17 -> 184.997211
  K18 -> 195.997718
  K19 -> 207.652349
  K20 -> 220.000000
  K21 -> 233.081881
  K22 -> 246.941651
  K23 -> 261.625565
  K24 -> 277.182631
  K25 -> 293.664768
  K26 -> 311.126984
  K27 -> 329.627557
  K28 -> 349.228231
  K29 -> 369.994423
  K30 -> 391.995436
  K31 -> 415.304698
  K32 -> 440.000000
  K33 -> 466.163762
  K34 -> 493.883301
  K35 -> 523.251131
  K36 -> 554.365262
  K37 -> 587.329536
  K38 -> 622.253967
  K39 -> 659.255114
  K40 -> 698.456463
  K41 -> 739.988845
  K42 -> 783.990872
  K43 -> 830.609395
  K44 -> 880.000000
  K45 -> 932.327523
  K46 -> 987.766603
  K47 -> 1046.502261
  K48 -> 1108.730524
  K49 -> 1174.659072
  K50 -> 1244.507935
  K51 -> 1318.510228
  K52 -> 1396.912926
  K53 -> 1479.977691
  K54 -> 1567.981744
  K55 -> 1661.218790
  K56 -> 1760.000000
  K57 -> 1864.655046
  K58 -> 1975.533205
  K59 -> 2093.004522

type KlavierType k0 k1 k2 k3 k4 k5 k6 k7 k8 k9 k10 k11 k12 k13 k14 k15 k16 k17 k18 k19 k20 k21 k22 k23 k24 k25 k26 k27 k28 k29 k30 k31 k32 k33 k34 k35 k36 k37 k38 k39 k40 k41 k42 k43 k44 k45 k46 k47 k48 k49 k50 k51 k52 k53 k54 k55 k56 k57 k58 k59
  = { k0 :: Decorating k0
    , k1 :: Decorating k1
    , k2 :: Decorating k2
    , k3 :: Decorating k3
    , k4 :: Decorating k4
    , k5 :: Decorating k5
    , k6 :: Decorating k6
    , k7 :: Decorating k7
    , k8 :: Decorating k8
    , k9 :: Decorating k9
    , k10 :: Decorating k10
    , k11 :: Decorating k11
    , k12 :: Decorating k12
    , k13 :: Decorating k13
    , k14 :: Decorating k14
    , k15 :: Decorating k15
    , k16 :: Decorating k16
    , k17 :: Decorating k17
    , k18 :: Decorating k18
    , k19 :: Decorating k19
    , k20 :: Decorating k20
    , k21 :: Decorating k21
    , k22 :: Decorating k22
    , k23 :: Decorating k23
    , k24 :: Decorating k24
    , k25 :: Decorating k25
    , k26 :: Decorating k26
    , k27 :: Decorating k27
    , k28 :: Decorating k28
    , k29 :: Decorating k29
    , k30 :: Decorating k30
    , k31 :: Decorating k31
    , k32 :: Decorating k32
    , k33 :: Decorating k33
    , k34 :: Decorating k34
    , k35 :: Decorating k35
    , k36 :: Decorating k36
    , k37 :: Decorating k37
    , k38 :: Decorating k38
    , k39 :: Decorating k39
    , k40 :: Decorating k40
    , k41 :: Decorating k41
    , k42 :: Decorating k42
    , k43 :: Decorating k43
    , k44 :: Decorating k44
    , k45 :: Decorating k45
    , k46 :: Decorating k46
    , k47 :: Decorating k47
    , k48 :: Decorating k48
    , k49 :: Decorating k49
    , k50 :: Decorating k50
    , k51 :: Decorating k51
    , k52 :: Decorating k52
    , k53 :: Decorating k53
    , k54 :: Decorating k54
    , k55 :: Decorating k55
    , k56 :: Decorating k56
    , k57 :: Decorating k57
    , k58 :: Decorating k58
    , k59 :: Decorating k59
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
              /\ k10 KeyUnit
              /\ k11 KeyUnit
              /\ k12 KeyUnit
              /\ k13 KeyUnit
              /\ k14 KeyUnit
              /\ k15 KeyUnit
              /\ k16 KeyUnit
              /\ k17 KeyUnit
              /\ k18 KeyUnit
              /\ k19 KeyUnit
              /\ k20 KeyUnit
              /\ k21 KeyUnit
              /\ k22 KeyUnit
              /\ k23 KeyUnit
              /\ k24 KeyUnit
              /\ k25 KeyUnit
              /\ k26 KeyUnit
              /\ k27 KeyUnit
              /\ k28 KeyUnit
              /\ k29 KeyUnit
              /\ k30 KeyUnit
              /\ k31 KeyUnit
              /\ k32 KeyUnit
              /\ k33 KeyUnit
              /\ k34 KeyUnit
              /\ k35 KeyUnit
              /\ k36 KeyUnit
              /\ k37 KeyUnit
              /\ k38 KeyUnit
              /\ k39 KeyUnit
              /\ k40 KeyUnit
              /\ k41 KeyUnit
              /\ k42 KeyUnit
              /\ k43 KeyUnit
              /\ k44 KeyUnit
              /\ k45 KeyUnit
              /\ k46 KeyUnit
              /\ k47 KeyUnit
              /\ k48 KeyUnit
              /\ k49 KeyUnit
              /\ k50 KeyUnit
              /\ k51 KeyUnit
              /\ k52 KeyUnit
              /\ k53 KeyUnit
              /\ k54 KeyUnit
              /\ k55 KeyUnit
              /\ k56 KeyUnit
              /\ k57 KeyUnit
              /\ k58 KeyUnit
              /\ k59 KeyUnit
              /\ Unit
          )
      )

type KeyUnit
  = Gain GetSetAP (SinOsc GetSetAP)

initialKey :: Key -> KeyUnit
initialKey key = Gain (defaultGetSetAP 0.0) (SinOsc Off (defaultGetSetAP (keyToPitch key)))

fullKeyboard :: forall k0 k1 k2 k3 k4 k5 k6 k7 k8 k9 k10 k11 k12 k13 k14 k15 k16 k17 k18 k19 k20 k21 k22 k23 k24 k25 k26 k27 k28 k29 k30 k31 k32 k33 k34 k35 k36 k37 k38 k39 k40 k41 k42 k43 k44 k45 k46 k47 k48 k49 k50 k51 k52 k53 k54 k55 k56 k57 k58 k59. KlavierType k0 k1 k2 k3 k4 k5 k6 k7 k8 k9 k10 k11 k12 k13 k14 k15 k16 k17 k18 k19 k20 k21 k22 k23 k24 k25 k26 k27 k28 k29 k30 k31 k32 k33 k34 k35 k36 k37 k38 k39 k40 k41 k42 k43 k44 k45 k46 k47 k48 k49 k50 k51 k52 k53 k54 k55 k56 k57 k58 k59
fullKeyboard { k0, k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, k20, k21, k22, k23, k24, k25, k26, k27, k28, k29, k30, k31, k32, k33, k34, k35, k36, k37, k38, k39, k40, k41, k42, k43, k44, k45, k46, k47, k48, k49, k50, k51, k52, k53, k54, k55, k56, k57, k58, k59 } =
  Speaker
    ( Gain (defaultGetSetAP 1.0)
        ( dk k0 (initialKey K0)
            /\ dk k1 (initialKey K1)
            /\ dk k2 (initialKey K2)
            /\ dk k3 (initialKey K3)
            /\ dk k4 (initialKey K4)
            /\ dk k5 (initialKey K5)
            /\ dk k6 (initialKey K6)
            /\ dk k7 (initialKey K7)
            /\ dk k8 (initialKey K8)
            /\ dk k9 (initialKey K9)
            /\ dk k10 (initialKey K10)
            /\ dk k11 (initialKey K11)
            /\ dk k12 (initialKey K12)
            /\ dk k13 (initialKey K13)
            /\ dk k14 (initialKey K14)
            /\ dk k15 (initialKey K15)
            /\ dk k16 (initialKey K16)
            /\ dk k17 (initialKey K17)
            /\ dk k18 (initialKey K18)
            /\ dk k19 (initialKey K19)
            /\ dk k20 (initialKey K20)
            /\ dk k21 (initialKey K21)
            /\ dk k22 (initialKey K22)
            /\ dk k23 (initialKey K23)
            /\ dk k24 (initialKey K24)
            /\ dk k25 (initialKey K25)
            /\ dk k26 (initialKey K26)
            /\ dk k27 (initialKey K27)
            /\ dk k28 (initialKey K28)
            /\ dk k29 (initialKey K29)
            /\ dk k30 (initialKey K30)
            /\ dk k31 (initialKey K31)
            /\ dk k32 (initialKey K32)
            /\ dk k33 (initialKey K33)
            /\ dk k34 (initialKey K34)
            /\ dk k35 (initialKey K35)
            /\ dk k36 (initialKey K36)
            /\ dk k37 (initialKey K37)
            /\ dk k38 (initialKey K38)
            /\ dk k39 (initialKey K39)
            /\ dk k40 (initialKey K40)
            /\ dk k41 (initialKey K41)
            /\ dk k42 (initialKey K42)
            /\ dk k43 (initialKey K43)
            /\ dk k44 (initialKey K44)
            /\ dk k45 (initialKey K45)
            /\ dk k46 (initialKey K46)
            /\ dk k47 (initialKey K47)
            /\ dk k48 (initialKey K48)
            /\ dk k49 (initialKey K49)
            /\ dk k50 (initialKey K50)
            /\ dk k51 (initialKey K51)
            /\ dk k52 (initialKey K52)
            /\ dk k53 (initialKey K53)
            /\ dk k54 (initialKey K54)
            /\ dk k55 (initialKey K55)
            /\ dk k56 (initialKey K56)
            /\ dk k57 (initialKey K57)
            /\ dk k58 (initialKey K58)
            /\ dk k59 (initialKey K59)
            /\ unit
        )
    )
