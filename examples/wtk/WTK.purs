module WAGS.Example.WTK where

import Prelude
import Control.Applicative.Indexed (imap)
import Data.Compactable (compact)
import Data.Either (Either(..))
import Data.Functor.Indexed (ivoid)
import Data.Identity (Identity(..))
import Data.List (List(..), (:), filter)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Set as S
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested ((/\), type (/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import FRP.Event.MIDI (MIDIEvent(..))
import Prim.Row (class Cons)
import Type.Data.Peano (Succ)
import Record (modify)
import Type.Proxy (Proxy(..))
import WAGS (class Change, class Changes, class Connect, class Create, class Cursor, class GraphIsRenderable, AnAudioUnit(..), AudioParameter, AudioUnitRef(..), Decorating(..), FFIAudio(..), Focus(..), Frame0, FrameT, Gain(..), GetSetAP, Instruction(..), OnOff(..), Scene, SceneI, SceneT, SinOsc(..), SingleEdge, Speaker(..), UniverseC, branch, change, changeAt, create, cursor, defaultGetSetAP, dk, env, freeze, gain, highpass, isHere, loop, mix, oneFrame', param, proof, runThunkableWithCount, sinOsc, speaker, start, thunkThunkable, wait, withProof, (@>), (@|>), graph)
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

noteToKey :: Int -> Maybe Key
noteToKey = case _ of
  36 -> Just K0
  37 -> Just K1
  38 -> Just K2
  39 -> Just K3
  40 -> Just K4
  41 -> Just K5
  42 -> Just K6
  43 -> Just K7
  44 -> Just K8
  45 -> Just K9
  46 -> Just K10
  47 -> Just K11
  48 -> Just K12
  49 -> Just K13
  50 -> Just K14
  51 -> Just K15
  52 -> Just K16
  53 -> Just K17
  54 -> Just K18
  55 -> Just K19
  56 -> Just K20
  57 -> Just K21
  58 -> Just K22
  59 -> Just K23
  60 -> Just K24
  61 -> Just K25
  62 -> Just K26
  63 -> Just K27
  64 -> Just K28
  65 -> Just K29
  66 -> Just K30
  67 -> Just K31
  68 -> Just K32
  69 -> Just K33
  70 -> Just K34
  71 -> Just K35
  72 -> Just K36
  73 -> Just K37
  74 -> Just K38
  75 -> Just K39
  76 -> Just K40
  77 -> Just K41
  78 -> Just K42
  79 -> Just K43
  80 -> Just K44
  81 -> Just K45
  82 -> Just K46
  83 -> Just K47
  84 -> Just K48
  85 -> Just K49
  86 -> Just K50
  87 -> Just K51
  88 -> Just K52
  89 -> Just K53
  90 -> Just K54
  91 -> Just K55
  92 -> Just K56
  93 -> Just K57
  94 -> Just K58
  95 -> Just K59
  _ -> Nothing

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
  { graphProxy :: Proxy graph
  , audioRefs :: AudioUnitRef k0 /\ AudioUnitRef k1 /\ AudioUnitRef k2 /\ AudioUnitRef k3 /\ AudioUnitRef k4 /\ AudioUnitRef k5 /\ AudioUnitRef k6 /\ AudioUnitRef k7 /\ AudioUnitRef k8 /\ AudioUnitRef k9 /\ AudioUnitRef k10 /\ AudioUnitRef k11 /\ AudioUnitRef k12 /\ AudioUnitRef k13 /\ AudioUnitRef k14 /\ AudioUnitRef k15 /\ AudioUnitRef k16 /\ AudioUnitRef k17 /\ AudioUnitRef k18 /\ AudioUnitRef k19 /\ AudioUnitRef k20 /\ AudioUnitRef k21 /\ AudioUnitRef k22 /\ AudioUnitRef k23 /\ AudioUnitRef k24 /\ AudioUnitRef k25 /\ AudioUnitRef k26 /\ AudioUnitRef k27 /\ AudioUnitRef k28 /\ AudioUnitRef k29 /\ AudioUnitRef k30 /\ AudioUnitRef k31 /\ AudioUnitRef k32 /\ AudioUnitRef k33 /\ AudioUnitRef k34 /\ AudioUnitRef k35 /\ AudioUnitRef k36 /\ AudioUnitRef k37 /\ AudioUnitRef k38 /\ AudioUnitRef k39 /\ AudioUnitRef k40 /\ AudioUnitRef k41 /\ AudioUnitRef k42 /\ AudioUnitRef k43 /\ AudioUnitRef k44 /\ AudioUnitRef k45 /\ AudioUnitRef k46 /\ AudioUnitRef k47 /\ AudioUnitRef k48 /\ AudioUnitRef k49 /\ AudioUnitRef k50 /\ AudioUnitRef k51 /\ AudioUnitRef k52 /\ AudioUnitRef k53 /\ AudioUnitRef k54 /\ AudioUnitRef k55 /\ AudioUnitRef k56 /\ AudioUnitRef k57 /\ AudioUnitRef k58 /\ AudioUnitRef k59
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
  = { k0 :: (forall a. a -> k0 a)
    , k1 :: (forall a. a -> k1 a)
    , k2 :: (forall a. a -> k2 a)
    , k3 :: (forall a. a -> k3 a)
    , k4 :: (forall a. a -> k4 a)
    , k5 :: (forall a. a -> k5 a)
    , k6 :: (forall a. a -> k6 a)
    , k7 :: (forall a. a -> k7 a)
    , k8 :: (forall a. a -> k8 a)
    , k9 :: (forall a. a -> k9 a)
    , k10 :: (forall a. a -> k10 a)
    , k11 :: (forall a. a -> k11 a)
    , k12 :: (forall a. a -> k12 a)
    , k13 :: (forall a. a -> k13 a)
    , k14 :: (forall a. a -> k14 a)
    , k15 :: (forall a. a -> k15 a)
    , k16 :: (forall a. a -> k16 a)
    , k17 :: (forall a. a -> k17 a)
    , k18 :: (forall a. a -> k18 a)
    , k19 :: (forall a. a -> k19 a)
    , k20 :: (forall a. a -> k20 a)
    , k21 :: (forall a. a -> k21 a)
    , k22 :: (forall a. a -> k22 a)
    , k23 :: (forall a. a -> k23 a)
    , k24 :: (forall a. a -> k24 a)
    , k25 :: (forall a. a -> k25 a)
    , k26 :: (forall a. a -> k26 a)
    , k27 :: (forall a. a -> k27 a)
    , k28 :: (forall a. a -> k28 a)
    , k29 :: (forall a. a -> k29 a)
    , k30 :: (forall a. a -> k30 a)
    , k31 :: (forall a. a -> k31 a)
    , k32 :: (forall a. a -> k32 a)
    , k33 :: (forall a. a -> k33 a)
    , k34 :: (forall a. a -> k34 a)
    , k35 :: (forall a. a -> k35 a)
    , k36 :: (forall a. a -> k36 a)
    , k37 :: (forall a. a -> k37 a)
    , k38 :: (forall a. a -> k38 a)
    , k39 :: (forall a. a -> k39 a)
    , k40 :: (forall a. a -> k40 a)
    , k41 :: (forall a. a -> k41 a)
    , k42 :: (forall a. a -> k42 a)
    , k43 :: (forall a. a -> k43 a)
    , k44 :: (forall a. a -> k44 a)
    , k45 :: (forall a. a -> k45 a)
    , k46 :: (forall a. a -> k46 a)
    , k47 :: (forall a. a -> k47 a)
    , k48 :: (forall a. a -> k48 a)
    , k49 :: (forall a. a -> k49 a)
    , k50 :: (forall a. a -> k50 a)
    , k51 :: (forall a. a -> k51 a)
    , k52 :: (forall a. a -> k52 a)
    , k53 :: (forall a. a -> k53 a)
    , k54 :: (forall a. a -> k54 a)
    , k55 :: (forall a. a -> k55 a)
    , k56 :: (forall a. a -> k56 a)
    , k57 :: (forall a. a -> k57 a)
    , k58 :: (forall a. a -> k58 a)
    , k59 :: (forall a. a -> k59 a)
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
  , k10: Identity
  , k11: Identity
  , k12: Identity
  , k13: Identity
  , k14: Identity
  , k15: Identity
  , k16: Identity
  , k17: Identity
  , k18: Identity
  , k19: Identity
  , k20: Identity
  , k21: Identity
  , k22: Identity
  , k23: Identity
  , k24: Identity
  , k25: Identity
  , k26: Identity
  , k27: Identity
  , k28: Identity
  , k29: Identity
  , k30: Identity
  , k31: Identity
  , k32: Identity
  , k33: Identity
  , k34: Identity
  , k35: Identity
  , k36: Identity
  , k37: Identity
  , k38: Identity
  , k39: Identity
  , k40: Identity
  , k41: Identity
  , k42: Identity
  , k43: Identity
  , k44: Identity
  , k45: Identity
  , k46: Identity
  , k47: Identity
  , k48: Identity
  , k49: Identity
  , k50: Identity
  , k51: Identity
  , k52: Identity
  , k53: Identity
  , k54: Identity
  , k55: Identity
  , k56: Identity
  , k57: Identity
  , k58: Identity
  , k59: Identity
  }

type KeyUnit
  = Gain GetSetAP (SinOsc GetSetAP)

fullKeyboard :: forall k0 k1 k2 k3 k4 k5 k6 k7 k8 k9 k10 k11 k12 k13 k14 k15 k16 k17 k18 k19 k20 k21 k22 k23 k24 k25 k26 k27 k28 k29 k30 k31 k32 k33 k34 k35 k36 k37 k38 k39 k40 k41 k42 k43 k44 k45 k46 k47 k48 k49 k50 k51 k52 k53 k54 k55 k56 k57 k58 k59. KlavierType k0 k1 k2 k3 k4 k5 k6 k7 k8 k9 k10 k11 k12 k13 k14 k15 k16 k17 k18 k19 k20 k21 k22 k23 k24 k25 k26 k27 k28 k29 k30 k31 k32 k33 k34 k35 k36 k37 k38 k39 k40 k41 k42 k43 k44 k45 k46 k47 k48 k49 k50 k51 k52 k53 k54 k55 k56 k57 k58 k59
fullKeyboard { k0, k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, k20, k21, k22, k23, k24, k25, k26, k27, k28, k29, k30, k31, k32, k33, k34, k35, k36, k37, k38, k39, k40, k41, k42, k43, k44, k45, k46, k47, k48, k49, k50, k51, k52, k53, k54, k55, k56, k57, k58, k59 } =
  Speaker
    ( Gain (defaultGetSetAP 1.0)
        ( k0 (initialKey K0)
            /\ k1 (initialKey K1)
            /\ k2 (initialKey K2)
            /\ k3 (initialKey K3)
            /\ k4 (initialKey K4)
            /\ k5 (initialKey K5)
            /\ k6 (initialKey K6)
            /\ k7 (initialKey K7)
            /\ k8 (initialKey K8)
            /\ k9 (initialKey K9)
            /\ k10 (initialKey K10)
            /\ k11 (initialKey K11)
            /\ k12 (initialKey K12)
            /\ k13 (initialKey K13)
            /\ k14 (initialKey K14)
            /\ k15 (initialKey K15)
            /\ k16 (initialKey K16)
            /\ k17 (initialKey K17)
            /\ k18 (initialKey K18)
            /\ k19 (initialKey K19)
            /\ k20 (initialKey K20)
            /\ k21 (initialKey K21)
            /\ k22 (initialKey K22)
            /\ k23 (initialKey K23)
            /\ k24 (initialKey K24)
            /\ k25 (initialKey K25)
            /\ k26 (initialKey K26)
            /\ k27 (initialKey K27)
            /\ k28 (initialKey K28)
            /\ k29 (initialKey K29)
            /\ k30 (initialKey K30)
            /\ k31 (initialKey K31)
            /\ k32 (initialKey K32)
            /\ k33 (initialKey K33)
            /\ k34 (initialKey K34)
            /\ k35 (initialKey K35)
            /\ k36 (initialKey K36)
            /\ k37 (initialKey K37)
            /\ k38 (initialKey K38)
            /\ k39 (initialKey K39)
            /\ k40 (initialKey K40)
            /\ k41 (initialKey K41)
            /\ k42 (initialKey K42)
            /\ k43 (initialKey K43)
            /\ k44 (initialKey K44)
            /\ k45 (initialKey K45)
            /\ k46 (initialKey K46)
            /\ k47 (initialKey K47)
            /\ k48 (initialKey K48)
            /\ k49 (initialKey K49)
            /\ k50 (initialKey K50)
            /\ k51 (initialKey K51)
            /\ k52 (initialKey K52)
            /\ k53 (initialKey K53)
            /\ k54 (initialKey K54)
            /\ k55 (initialKey K55)
            /\ k56 (initialKey K56)
            /\ k57 (initialKey K57)
            /\ k58 (initialKey K58)
            /\ k59 (initialKey K59)
            /\ unit
        )
    )

keyDur :: Number
keyDur = 1.6

initialKey :: Key -> KeyUnit
initialKey key = Gain (defaultGetSetAP 0.0) (SinOsc Off (defaultGetSetAP (keyToPitch key)))

keyStart :: Key -> KeyUnit
keyStart key = Gain (defaultGetSetAP 0.0) (SinOsc On (defaultGetSetAP (keyToPitch key)))

calcSlope :: Number -> Number -> Number -> Number -> Number -> Number
calcSlope x0 y0 x1 y1 x =
  if x1 == x0 || y1 == y0 then
    y0
  else
    let
      m = (y1 - y0) / (x1 - x0)

      b = y0 - m * x0
    in
      m * x + b

asdr :: Number -> Number
asdr n
  | n <= 0.0 = 0.0
  | n < 0.25 = calcSlope 0.0 0.0 0.25 0.5 n
  | n < 0.5 = calcSlope 0.25 0.5 0.5 0.1 n
  | n < keyDur = calcSlope 0.5 0.1 keyDur 0.0 n
  | otherwise = 0.0

keySustain :: Number -> Key -> KeyUnit
keySustain nSecLive key =
  Gain
    (defaultGetSetAP (asdr nSecLive))
    (SinOsc On (defaultGetSetAP (keyToPitch key)))

keyEnd :: Key -> KeyUnit
keyEnd key = Gain (defaultGetSetAP 0.0) (SinOsc Off (defaultGetSetAP (keyToPitch key)))

midiEventsToOnsets :: List MIDIEvent -> List Key
midiEventsToOnsets = compact <<< go Nil
  where
  go acc Nil = acc

  go acc (a : b) =
    go
      ( Cons
          ( case a of
              NoteOn _ note _ -> noteToKey note
              _ -> Nothing
          )
          acc
      )
      b

piece :: Scene (SceneI (List MIDIEvent) Unit) FFIAudio (Effect Unit) Frame0
piece =
  Ix.do
    start
    ivoid $ create $ fullKeyboard klavierIdentity
    k0 <- cursor $ fullKeyboard (modify (Proxy :: _ "k0") (const Focus) klavierIdentity)
    k1 <- cursor $ fullKeyboard (modify (Proxy :: _ "k1") (const Focus) klavierIdentity)
    k2 <- cursor $ fullKeyboard (modify (Proxy :: _ "k2") (const Focus) klavierIdentity)
    k3 <- cursor $ fullKeyboard (modify (Proxy :: _ "k3") (const Focus) klavierIdentity)
    k4 <- cursor $ fullKeyboard (modify (Proxy :: _ "k4") (const Focus) klavierIdentity)
    k5 <- cursor $ fullKeyboard (modify (Proxy :: _ "k5") (const Focus) klavierIdentity)
    k6 <- cursor $ fullKeyboard (modify (Proxy :: _ "k6") (const Focus) klavierIdentity)
    k7 <- cursor $ fullKeyboard (modify (Proxy :: _ "k7") (const Focus) klavierIdentity)
    k8 <- cursor $ fullKeyboard (modify (Proxy :: _ "k8") (const Focus) klavierIdentity)
    k9 <- cursor $ fullKeyboard (modify (Proxy :: _ "k9") (const Focus) klavierIdentity)
    k10 <- cursor $ fullKeyboard (modify (Proxy :: _ "k10") (const Focus) klavierIdentity)
    k11 <- cursor $ fullKeyboard (modify (Proxy :: _ "k11") (const Focus) klavierIdentity)
    k12 <- cursor $ fullKeyboard (modify (Proxy :: _ "k12") (const Focus) klavierIdentity)
    k13 <- cursor $ fullKeyboard (modify (Proxy :: _ "k13") (const Focus) klavierIdentity)
    k14 <- cursor $ fullKeyboard (modify (Proxy :: _ "k14") (const Focus) klavierIdentity)
    k15 <- cursor $ fullKeyboard (modify (Proxy :: _ "k15") (const Focus) klavierIdentity)
    k16 <- cursor $ fullKeyboard (modify (Proxy :: _ "k16") (const Focus) klavierIdentity)
    k17 <- cursor $ fullKeyboard (modify (Proxy :: _ "k17") (const Focus) klavierIdentity)
    k18 <- cursor $ fullKeyboard (modify (Proxy :: _ "k18") (const Focus) klavierIdentity)
    k19 <- cursor $ fullKeyboard (modify (Proxy :: _ "k19") (const Focus) klavierIdentity)
    k20 <- cursor $ fullKeyboard (modify (Proxy :: _ "k20") (const Focus) klavierIdentity)
    k21 <- cursor $ fullKeyboard (modify (Proxy :: _ "k21") (const Focus) klavierIdentity)
    k22 <- cursor $ fullKeyboard (modify (Proxy :: _ "k22") (const Focus) klavierIdentity)
    k23 <- cursor $ fullKeyboard (modify (Proxy :: _ "k23") (const Focus) klavierIdentity)
    k24 <- cursor $ fullKeyboard (modify (Proxy :: _ "k24") (const Focus) klavierIdentity)
    k25 <- cursor $ fullKeyboard (modify (Proxy :: _ "k25") (const Focus) klavierIdentity)
    k26 <- cursor $ fullKeyboard (modify (Proxy :: _ "k26") (const Focus) klavierIdentity)
    k27 <- cursor $ fullKeyboard (modify (Proxy :: _ "k27") (const Focus) klavierIdentity)
    k28 <- cursor $ fullKeyboard (modify (Proxy :: _ "k28") (const Focus) klavierIdentity)
    k29 <- cursor $ fullKeyboard (modify (Proxy :: _ "k29") (const Focus) klavierIdentity)
    k30 <- cursor $ fullKeyboard (modify (Proxy :: _ "k30") (const Focus) klavierIdentity)
    k31 <- cursor $ fullKeyboard (modify (Proxy :: _ "k31") (const Focus) klavierIdentity)
    k32 <- cursor $ fullKeyboard (modify (Proxy :: _ "k32") (const Focus) klavierIdentity)
    k33 <- cursor $ fullKeyboard (modify (Proxy :: _ "k33") (const Focus) klavierIdentity)
    k34 <- cursor $ fullKeyboard (modify (Proxy :: _ "k34") (const Focus) klavierIdentity)
    k35 <- cursor $ fullKeyboard (modify (Proxy :: _ "k35") (const Focus) klavierIdentity)
    k36 <- cursor $ fullKeyboard (modify (Proxy :: _ "k36") (const Focus) klavierIdentity)
    k37 <- cursor $ fullKeyboard (modify (Proxy :: _ "k37") (const Focus) klavierIdentity)
    k38 <- cursor $ fullKeyboard (modify (Proxy :: _ "k38") (const Focus) klavierIdentity)
    k39 <- cursor $ fullKeyboard (modify (Proxy :: _ "k39") (const Focus) klavierIdentity)
    k40 <- cursor $ fullKeyboard (modify (Proxy :: _ "k40") (const Focus) klavierIdentity)
    k41 <- cursor $ fullKeyboard (modify (Proxy :: _ "k41") (const Focus) klavierIdentity)
    k42 <- cursor $ fullKeyboard (modify (Proxy :: _ "k42") (const Focus) klavierIdentity)
    k43 <- cursor $ fullKeyboard (modify (Proxy :: _ "k43") (const Focus) klavierIdentity)
    k44 <- cursor $ fullKeyboard (modify (Proxy :: _ "k44") (const Focus) klavierIdentity)
    k45 <- cursor $ fullKeyboard (modify (Proxy :: _ "k45") (const Focus) klavierIdentity)
    k46 <- cursor $ fullKeyboard (modify (Proxy :: _ "k46") (const Focus) klavierIdentity)
    k47 <- cursor $ fullKeyboard (modify (Proxy :: _ "k47") (const Focus) klavierIdentity)
    k48 <- cursor $ fullKeyboard (modify (Proxy :: _ "k48") (const Focus) klavierIdentity)
    k49 <- cursor $ fullKeyboard (modify (Proxy :: _ "k49") (const Focus) klavierIdentity)
    k50 <- cursor $ fullKeyboard (modify (Proxy :: _ "k50") (const Focus) klavierIdentity)
    k51 <- cursor $ fullKeyboard (modify (Proxy :: _ "k51") (const Focus) klavierIdentity)
    k52 <- cursor $ fullKeyboard (modify (Proxy :: _ "k52") (const Focus) klavierIdentity)
    k53 <- cursor $ fullKeyboard (modify (Proxy :: _ "k53") (const Focus) klavierIdentity)
    k54 <- cursor $ fullKeyboard (modify (Proxy :: _ "k54") (const Focus) klavierIdentity)
    k55 <- cursor $ fullKeyboard (modify (Proxy :: _ "k55") (const Focus) klavierIdentity)
    k56 <- cursor $ fullKeyboard (modify (Proxy :: _ "k56") (const Focus) klavierIdentity)
    k57 <- cursor $ fullKeyboard (modify (Proxy :: _ "k57") (const Focus) klavierIdentity)
    k58 <- cursor $ fullKeyboard (modify (Proxy :: _ "k58") (const Focus) klavierIdentity)
    k59 <- cursor $ fullKeyboard (modify (Proxy :: _ "k59") (const Focus) klavierIdentity)
    myProof <- proof
    withProof myProof $ Right { audioRefs: k0 /\ k1 /\ k2 /\ k3 /\ k4 /\ k5 /\ k6 /\ k7 /\ k8 /\ k9 /\ k10 /\ k11 /\ k12 /\ k13 /\ k14 /\ k15 /\ k16 /\ k17 /\ k18 /\ k19 /\ k20 /\ k21 /\ k22 /\ k23 /\ k24 /\ k25 /\ k26 /\ k27 /\ k28 /\ k29 /\ k30 /\ k31 /\ k32 /\ k33 /\ k34 /\ k35 /\ k36 /\ k37 /\ k38 /\ k39 /\ k40 /\ k41 /\ k42 /\ k43 /\ k44 /\ k45 /\ k46 /\ k47 /\ k48 /\ k49 /\ k50 /\ k51 /\ k52 /\ k53 /\ k54 /\ k55 /\ k56 /\ k57 /\ k58 /\ k59, currentKeys: (Nil :: List (Tuple Number Key)) }
    @> loop
        ( \{ audioRefs, currentKeys } -> Ix.do
            { time, trigger } <- env
            graphProxy <- graph
            let
              onsets = midiEventsToOnsets trigger
            ( playKeys
                { graphProxy
                , audioRefs
                , currentTime: time
                , keyDuration: keyDur
                , keyStartCtor: keyStart
                , keySustainCtor: keySustain
                , keyEndCtor: keyEnd
                }
                unit
                onsets
                currentKeys
            )
              $> { audioRefs, 
              currentKeys: (filter (\(Tuple t _) -> time - t > keyDur) currentKeys) <> map (Tuple time) onsets }
        )
