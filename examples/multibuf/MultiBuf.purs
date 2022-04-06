module WAGS.Example.MultiBuf where

import Prelude

import Control.Alt ((<|>))
import Control.Alternative (class Plus)
import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree, deferCofree)
import Control.Comonad.Cofree.Class (unwrapCofree)
import Control.Plus (empty)
import Data.Array as Array
import Data.Either (either)
import Data.Exists (Exists, mkExists)
import Data.Foldable (for_, oneOf)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Identity (Identity(..))
import Data.Map (Map, fromFoldable, insert, union, values)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Profunctor (lcmap)
import Data.Tuple (Tuple(..), swap)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Typelevel.Num (D2)
import Deku.Attribute (cb, (:=))
import Deku.Control (deku, text, text_)
import Deku.Core (SubgraphF(..))
import Deku.DOM as DOM
import Deku.Interpret (effectfulDOMInterpret, makeFFIDOMSnapshot)
import Deku.Subgraph (SubgraphAction(..), subgraph)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import FRP.Behavior (sample_)
import FRP.Event (class IsEvent, keepLatest, mapAccum, subscribe)
import FRP.Event.Time (interval)
import WAGS.Control (gain__, playBuf, singleton, speaker2)
import WAGS.Core (GainInput, Subgraph(..))
import WAGS.Example.Utils (RaiseCancellation)
import WAGS.Interpret (close, context, decodeAudioDataFromUri, effectfulAudioInterpret, makeFFIAudioSnapshot, writeHead)
import WAGS.Parameter (ACTime, AudioOnOff(..), WriteHead, _on)
import WAGS.Properties (onOff)
import WAGS.Subgraph as Sg
import WAGS.WebAPI (AudioContext, BrowserAudioBuffer)
import Web.HTML (window)
import Web.HTML.HTMLDocument (body)
import Web.HTML.HTMLElement (toElement)
import Web.HTML.Window (document)

unrollCofree
  :: forall a
   . Int
  -> Cofree Identity a
  -> { head :: Array a, rest :: Cofree Identity a }
unrollCofree 0 cf = { head: [], rest: cf }
unrollCofree n cf =
  let
    h = extract cf
    t = unwrap $ unwrapCofree cf
    v = unrollCofree (n - 1) t
  in
    { head: Array.cons h v.head, rest: v.rest }

type Acc0 =
  { cf :: Cofree Identity (Int /\ Number)
  , prevs :: Map Number Int
  }

type Acc1 =
  { head :: Array (Int /\ Number)
  , no :: Map Number Int
  }

acc :: Acc0
acc =
  let
    f i x = deferCofree \_ -> (i /\ x) /\ Identity
      (f (i + 1) (x + 0.25))
  in
    { cf: f 0 0.0, prevs: empty }

accLoop :: Number -> Acc0 -> Acc0 /\ Acc1
accLoop time { cf, prevs } =
  { cf: rest, prevs: union yes $ fromFoldable (map swap head) } /\ { head, no }
  where
  { head, rest } = unrollCofree 20 cf
  { yes, no } = foldlWithIndex
    ( \k yn v ->
        if k < (time - 2.0) then yn { no = insert k v yn.no }
        else yn { yes = insert k v yn.yes }
    )
    { yes: empty, no: empty }
    prevs

sg
  :: KickSnare
  -> forall event payload
   . IsEvent event
  => Subgraph Int (Number /\ ACTime) D2 () () event payload
sg ks = Subgraph go
  where
  go i n = gain__ 1.0 empty
    ( playBuf (if i `mod` 2 == 0 then ks.kick else ks.snare)
        ( n # map \(t /\ { lookAhead }) -> onOff $ AudioOnOff
            { onOff: _on
            , timeOffset: lookAhead + t
            }
        )
    )

sgActionMaker
  :: forall event
   . Plus event
  => Applicative event
  => ACTime /\ Acc1
  -> event (Int /\ Sg.SubgraphAction (Number /\ ACTime))
sgActionMaker (ac /\ { head, no }) =
  oneOf (map (\(i /\ n) -> pure $ i /\ Sg.InsertOrUpdate (n /\ ac)) head) <|>
    oneOf
      (map (\i -> pure $ i /\ Sg.Remove) $ values no)

scene
  :: forall event payload
   . IsEvent event
  => KickSnare
  -> WriteHead event
  -> GainInput D2 () () event payload
scene ks wh =
  let
    mapped = mapAccum
      (\a b -> let b' /\ c = accLoop a.abstractTime b in b' /\ (a /\ c))
      wh
      acc
  in
    singleton
      ( Sg.subgraph (keepLatest (map sgActionMaker mapped))
          (\({} :: {}) -> sg ks)
      )

type UIAction = Maybe { unsub :: Effect Unit, ctx :: AudioContext }
type KickSnare = { kick :: BrowserAudioBuffer, snare :: BrowserAudioBuffer }

type Init = { kick :: BrowserAudioBuffer, snare :: BrowserAudioBuffer }

initializeMultiBuf :: Aff Init
initializeMultiBuf = do
  audioCtx <- liftEffect context
  kick <- decodeAudioDataFromUri
    audioCtx
    "https://freesound.org/data/previews/344/344757_1676145-hq.mp3"
  snare <- decodeAudioDataFromUri
    audioCtx
    "https://freesound.org/data/previews/387/387186_7255534-hq.mp3"
  pure { kick, snare }

multiBuf
  :: forall index event payload
   . IsEvent event
  => KickSnare
  -> RaiseCancellation
  -> index
  -> Exists (SubgraphF index Unit event payload)
multiBuf ks rc _ = mkExists $ SubgraphF \push -> lcmap (map (either (const Nothing) identity)) \event ->
  DOM.div_
    [ DOM.h1_ [ text_ "Multi Buf" ]
    , DOM.button
        ( map
            ( \i -> DOM.OnClick := cb
                ( const $
                    maybe
                      ( do
                          ctx <- context
                          ffi2 <- makeFFIAudioSnapshot ctx
                          let wh = writeHead 0.04 ctx
                          unsub <- subscribe
                            ( speaker2
                                ( scene ks
                                    ( sample_ wh
                                        (pure unit <|> (interval 4900 $> unit))
                                    )
                                )
                                effectfulAudioInterpret
                            )
                            ((#) ffi2)
                          rc $ Just { unsub, ctx }
                          push $ Just { unsub, ctx }
                      )
                      ( \{ unsub, ctx } -> do
                          rc Nothing
                          unsub
                          close ctx
                          push Nothing
                      )
                      i
                )
            )
            event
        )
        [ text
            (map (maybe "Turn on" (const "Turn off")) event)
        ]
    ]

main :: Effect Unit
main = launchAff_ do
  init <- initializeMultiBuf
  liftEffect do
    b' <- window >>= document >>= body
    for_ (toElement <$> b') \elt -> do
      ffi <- makeFFIDOMSnapshot
      let
        evt = deku elt
          ( subgraph (pure (Tuple unit (InsertOrUpdate unit)))
              (multiBuf init (const $ pure unit))
          )
          effectfulDOMInterpret
      _ <- subscribe evt \i -> i ffi
      pure unit
