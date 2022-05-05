module WAGS.Control where

import Prelude

import Control.Alt ((<|>))
import Control.Comonad (extract)
import Control.Plus (empty)
import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Data.Either (Either(..))
import Data.Foldable (for_, oneOf)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Homogeneous (class HomogeneousRowLabels)
import Data.Homogeneous.Variant (homogeneous)
import Data.Int (pow)
import Data.Profunctor (lcmap)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Typelevel.Num (class Lt, class Nat, class Pos, class Pred, D1, D2, d0, pred, toInt)
import Data.Variant (Unvariant(..), inj, match, unvariant)
import Data.Variant.Maybe (Maybe, just, nothing)
import Data.Vec (Vec, index, singleton, toArray)
import Effect (Effect, foreachE)
import Effect.AVar (tryPut)
import Effect.AVar as AVar
import Effect.Exception (throwException)
import FRP.Event (Event, bang, keepLatest, makeEvent, subscribe)
import Foreign.Object (fromHomogeneous)
import Simple.JSON as JSON
import Type.Proxy (Proxy(..))
import Type.Row.Homogeneous (class Homogeneous)
import Unsafe.Coerce (unsafeCoerce)
import WAGS.Common as Common
import WAGS.Core (ChannelCountMode(..), ChannelInterpretation(..), Po2(..), __internalWagsFlatten, mix)
import WAGS.Core as Core
import WAGS.WebAPI (BrowserAudioBuffer)

-- -- audio worklet

class
  ValidateOutputChannelCount
    (numberOfOutputs :: Type)
    (outputChannelCount :: Type) where
  toOutputChannelCount
    :: numberOfOutputs
    -> outputChannelCount
    -> Array Int

instance validateOutputChannelCountD1 ::
  Pos n =>
  ValidateOutputChannelCount D1 n where
  toOutputChannelCount _ n = [ toInt n ]
else instance validateOutputChannelCountN ::
  ( Pred x xMinus1
  , Pos n
  , ValidateOutputChannelCount xMinus1 r
  ) =>
  ValidateOutputChannelCount x (n /\ r) where
  toOutputChannelCount x (n /\ r) = [ toInt n ] <> toOutputChannelCount
    (pred x)
    r

-- __audioWorklet
--   :: forall name numberOfInputs numberOfOutputs outputChannelCount parameterData
--        parameterDataRL
--        processorOptions lock payload
--    . IsSymbol name
--   => Nat numberOfInputs
--   => Pos numberOfOutputs
--   => ValidateOutputChannelCount numberOfOutputs outputChannelCount
--   => Homogeneous parameterData Core.InitialAudioParameter
--   => HomogeneousRowLabels parameterData (Core.AudioParameter lock payload) parameterDataRL
--   => JSON.WriteForeign { | processorOptions }
--   => Core.InitializeAudioWorkletNode name numberOfInputs numberOfOutputs
--        outputChannelCount
--        parameterData
--        processorOptions
--   -> Event (Core.AudioWorkletNode parameterData)
--   -> Core.Node numberOfOutputs lock payload
--   -> Core.Node numberOfOutputs lock payload
-- __audioWorklet (Core.InitializeAudioWorkletNode i) atts elt = Core.Node go
--   where
--   go
--     parent
--     di@
--       ( Core.AudioInterpret
--           { ids, deleteFromCache, makeAudioWorkletNode, setAudioWorkletParameter }
--       ) =
--     makeEvent \k -> do
--       me <- ids
--       parent.raiseId me
--       map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
--         bang
--           ( makeAudioWorkletNode
--               { id: me
--               , parent: parent.parent
--               , scope: parent.scope
--               , options:
--                   Core.AudioWorkletNodeOptions_
--                     { name: reflectSymbol (Proxy :: _ name)
--                     , numberOfInputs: toInt i.numberOfInputs
--                     , numberOfOutputs: toInt i.numberOfOutputs
--                     , outputChannelCount: toOutputChannelCount
--                         i.numberOfOutputs
--                         i.outputChannelCount
--                     , parameterData: fromHomogeneous i.parameterData
--                     , processorOptions: JSON.writeImpl i.processorOptions
--                     }
--               }
--           )
--           <|>
--             ( keepLatest $ map
--                 ( \(Core.AudioWorkletNode e) -> tmpResolveAU parent.scope di
--                     ( \paramValue -> setAudioWorkletParameter
--                         { id: me
--                         , paramName: (let Unvariant e' = unvariant e in e')
--                             (\sym _ -> reflectSymbol sym)
--                         , paramValue
--                         }
--                     )
--                     (extract (homogeneous e))
--                 )
--                 atts
--             )
--           <|> __internalWagsFlatten (just me) parent.scope di (mix elt)

-- audioWorklet
--   :: forall name numberOfInputs numberOfOutputs outputChannelCount parameterData
--        parameterDataRL
--        processorOptions lock payload
--    . IsSymbol name
--   => Nat numberOfInputs
--   => Pos numberOfOutputs
--   => ValidateOutputChannelCount numberOfOutputs outputChannelCount
--   => Homogeneous parameterData Core.InitialAudioParameter
--   => HomogeneousRowLabels parameterData (Core.AudioParameter lock payload) parameterDataRL
--   => JSON.WriteForeign { | processorOptions }
--   => Core.InitializeAudioWorkletNode name numberOfInputs numberOfOutputs
--        outputChannelCount
--        parameterData
--        processorOptions
--   -> Event (Core.AudioWorkletNode parameterData)
--   -> Core.Node numberOfOutputs lock payload
--   -> Core.Node numberOfOutputs lock payload
-- audioWorklet = __audioWorklet

-- -- iirFilter

-- iirFilter
--   :: forall i aud (feedforward :: Type) (feedback :: Type) (outputChannels :: Type) lock
--        payload
--    . Lt D2 feedforward
--   => Lt D2 feedback
--   => Core.Mix aud (Core.Audible outputChannels lock payload)
--   => Common.InitialIIRFilter i feedforward feedback
--   => Core.Mix aud (Core.Audible outputChannels lock payload)
--   => i
--   -> aud
--   -> Core.Node outputChannels lock payload
-- iirFilter = iirFilter' (Proxy :: _ feedforward) (Proxy :: _ feedback)

-- iirFilter'
--   :: forall i aud proxy (feedforward :: Type) (feedback :: Type) (outputChannels :: Type) lock
--        payload
--    . Lt D2 feedforward
--   => Lt D2 feedback
--   => Common.InitialIIRFilter i feedforward feedback
--   => Core.Mix aud (Core.Audible outputChannels lock payload)
--   => proxy feedforward
--   -> proxy feedback
--   -> i
--   -> aud
--   -> Core.Node outputChannels lock payload
-- iirFilter' fwd bk i' elts = Core.Node go
--   where
--   Core.InitializeIIRFilter i = Common.toInitializeIIRFilter i' fwd bk
--   go
--     parent
--     di@
--       ( Core.AudioInterpret
--           { ids
--           , deleteFromCache
--           , makeIIRFilter
--           }
--       ) =
--     makeEvent \k -> do
--       me <- ids
--       parent.raiseId me
--       map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
--         bang
--           ( makeIIRFilter
--               { id: me
--               , parent: parent.parent
--               , scope: parent.scope
--               , feedforward: toArray i.feedforward
--               , feedback: toArray i.feedback
--               }
--           )
--           <|> __internalWagsFlatten (just me) parent.scope di (mix elts)

-- -- recorder
-- recorder
--   :: forall i outputChannels lock payload
--    . Common.InitialRecorder i
--   => i
--   -> Core.Node outputChannels lock payload
--   -> Core.Node outputChannels lock payload
-- recorder i' elt = Core.Node go
--   where
--   Core.InitializeRecorder i = Common.toInitializeRecorder i'
--   go parent di@(Core.AudioInterpret { ids, deleteFromCache, makeRecorder }) =
--     makeEvent \k -> do
--       me <- ids
--       parent.raiseId me
--       map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
--         bang
--           ( makeRecorder
--               { id: me, parent: parent.parent, scope: parent.scope, cb: i.cb }
--           )
--           <|> __internalWagsFlatten (just me) parent.scope di (mix elt)

-- speaker
speaker
  :: forall aud (outputChannels :: Type) lock payload
   . Core.Mix aud (Core.Audible outputChannels lock payload)
  => aud
  -> Core.AudioInterpret payload
  -> Event payload
speaker elts di@(Core.AudioInterpret { ids, makeSpeaker }) = makeEvent \k -> do
  id <- ids
  k (makeSpeaker { id })
  subscribe (__internalWagsFlatten (just id) "toplevel" di (mix elts)) k

speaker2
  :: forall aud lock payload
   . Core.Mix aud (Core.Audible D2 lock payload)
  => aud
  -> Core.AudioInterpret payload
  -> Event payload
speaker2 = speaker

-- ----------

-- newtype MutAr a = MutAr (Array a)

-- foreign import mutAr :: forall a. Array a -> Effect (MutAr a)
-- foreign import unsafeUpdateMutAr :: forall a. Int -> a -> MutAr a -> Effect Unit
-- foreign import readAr :: forall a. MutAr a -> Effect (Array a)

-- -- todo: this is almost literally a copy-paste now of Deku
-- -- are the libraries close enough now where we can merge the two functions into one?
-- -- the only differences I can see are:
-- -- - the deconstruction and reconstruction of Node
-- -- - the name of the connection function
-- internalFan
--   :: forall n outputChannels lock0 lock1 payload
--    . Boolean
--   -> (String -> String)
--   -> Vec n (Core.Node outputChannels lock0 payload)
--   -> ( Vec n (Core.Node outputChannels lock1 payload)
--        -> (Core.Node outputChannels lock0 payload -> Core.Node outputChannels lock1 payload)
--        -> Core.Audible outputChannels lock1 payload
--      )
--   -> Core.Node outputChannels lock0 payload
-- internalFan isGlobal scopeF gaga closure = Core.Node go
--   where
--   go psr di@(Core.AudioInterpret { deleteFromCache }) = makeEvent \k -> do
--     av <- mutAr (map (const "") $ toArray gaga)
--     let
--       actualized = oneOf $ mapWithIndex
--         ( \ix (Core.Node gogo) ->
--             gogo
--               { parent: just "@fan@"
--               , scope: scopeF psr.scope
--               , raiseId: \id -> unsafeUpdateMutAr ix id av
--               }
--               di
--         )
--         gaga
--     u0 <- subscribe actualized k
--     av2 <- AVar.empty
--     let
--       asIds :: Array String -> Vec n String
--       asIds = unsafeCoerce
--     idz <- asIds <$> readAr av
--     let
--       -- we never connect or disconnect the referentially opaque node
--       -- instead, it is always managed inside a referentially transparent node
--       -- that can be properly connected and disconnected
--       injectable = map
--         ( \id -> Core.Node
--             \{ parent, raiseId } (Core.AudioInterpret { connectXToY }) ->
--               makeEvent \k2 -> do
--                 raiseId id
--                 for_ parent \p' ->
--                   k2 (connectXToY { from: id, to: p' })
--                 pure (pure unit)
--         )
--         idz
--       realized = __internalWagsFlatten psr.parent psr.scope di
--         ((unsafeCoerce :: Core.Audible _ _ _ -> Core.Audible _ _ _) (closure injectable (\(Core.Node q) -> Core.Node q)))
--     u <- subscribe realized k
--     void $ tryPut u av2
--     -- cancel immediately, as it should be run synchronously
--     -- so if this actually does something then we have a problem
--     pure do
--       u0
--       when (not isGlobal) $ foreachE (toArray idz) \id -> k
--         (deleteFromCache { id })
--       cncl2 <- AVar.take av2 \q -> case q of
--         Right usu -> usu
--         Left e -> throwException e
--       -- cancel immediately, as it should be run synchronously
--       -- so if this actually does something then we have a problem
--       cncl2

-- globalFan
--   :: forall n outputChannels lock payload
--    . Vec n (Core.Node outputChannels lock payload)
--   -> (Vec n (Core.Node outputChannels lock payload) -> Core.Audible outputChannels lock payload)
--   -> Core.Node outputChannels lock payload
-- globalFan e f = internalFan true (const "@fan@") e (\x _ -> f x)

-- globalFan1
--   :: forall outputChannels lock payload
--    . Core.Node outputChannels lock payload
--   -> (Core.Node outputChannels lock payload -> Core.Audible outputChannels lock payload)
--   -> Core.Node outputChannels lock payload
-- globalFan1 e f = globalFan (singleton e) (lcmap (flip index d0) f)

-- fan
--   :: forall n outputChannels lock0 payload
--    . Vec n (Core.Node outputChannels lock0 payload)
--   -> ( forall lock1
--         . Vec n (Core.Node outputChannels lock1 payload)
--        -> (Core.Node outputChannels lock0 payload -> Core.Node outputChannels lock1 payload)
--        -> Core.Audible outputChannels lock1 payload
--      )
--   -> Core.Node outputChannels lock0 payload
-- fan e = internalFan false identity e

-- fan1
--   :: forall outputChannels lock0 payload
--    . Core.Node outputChannels lock0 payload
--   -> ( forall lock1
--         . Core.Node outputChannels lock1 payload
--        -> (Core.Node outputChannels lock0 payload -> Core.Node outputChannels lock1 payload)
--        -> Core.Audible outputChannels lock1 payload
--      )
--   -> Core.Node outputChannels lock0 payload
-- fan1 e f = fan (singleton e) (lcmap (flip index d0) f)

-- ---- fix
-- fix
--   :: forall outputChannels lock payload
--    . (Core.Node outputChannels lock payload -> Core.Node outputChannels lock payload)
--   -> Core.Node outputChannels lock payload
-- fix f = Core.Node go
--   where
--   go i di@(Core.AudioInterpret { connectXToY }) = makeEvent \k -> do
--     av <- AVar.empty
--     let
--       Core.Node nn = f $ Core.Node \ii _ -> makeEvent \k -> do
--         void $ AVar.read av case _ of
--           Left e -> throwException e
--           -- only do the connection if not silence
--           Right r -> for_ ii.parent \p' ->
--             when (r /= p') (ii.raiseId r *> k (connectXToY { from: r, to: p' }))
--         pure (pure unit)
--     subscribe
--       ( nn
--           { parent: i.parent
--           , scope: i.scope
--           , raiseId: \s -> do
--               i.raiseId s
--               void $ tryPut s av
--           }
--           di
--       )
--       k

-- silence
--   :: forall outputChannels lock payload
--    . Core.Node outputChannels lock payload
-- silence = fix identity

-- -----
-- -- starts work on merge
-- -- merge
-- --   :: forall i n lock payload
-- --    . IsEvent event
-- --   => Pos n
-- --   => Vec n (Core.Node D1 lock payload)
-- --   -> Core.Node n lock payload
-- -- merge elts = Core.Node go
-- --   where
-- --   go
-- --     parent
-- --     di@
-- --       ( Core.AudioInterpret
-- --           { ids
-- --           , scope
-- --           , makeMerger
-- --           }
-- --       ) =
-- --     keepLatest
-- --       ( (sample_ ids (bang unit)) <#> \me ->
-- --           bang
-- --             ( makeMerger
-- --                 { id: me
-- --                 , parent: parent.parent
-- --                 , scope: parent.scope
-- --                 }
-- --             )
-- --             <|> oneOf
-- --               ( ( mapWithIndex
-- --                     -- parent needs to accept an ix for this to work
-- --                     ( \ix (id /\ elt) -> (((\y -> let Core.Node x = y in x) elt) (Parent me ix) di)
-- --                     )
-- --                     elts
-- --                 )
-- --               )
-- --       )

-- -- TODO
-- -- this function is copied between two files
-- -- with the sole difference that this version wraps its argument in a gain node
-- -- the reason for this is that, otherwise, we'd have to write additional machinery
-- -- for all generators (ie sine wave oscillators) to listen to when they turn on and off and reconnect fresh generators whenever something turns on again
-- -- by doing it this way, all generators go to a gain node, so we can use code we've already written
-- -- the downside is that we have an extra gain node for every audio parameter
-- -- which can add up
-- -- so we definitely want to delete this and use Common.resolveAU
-- -- as soon as we can correctly attach and detach generators
-- tmpResolveAU :: forall lock payload. String -> Core.AudioInterpret payload -> (Core.FFIAudioParameter -> payload) -> Core.AudioParameter lock payload -> Event payload
-- tmpResolveAU = go
--   where
--   cncl = Core.FFIAudioParameter <<< inj (Proxy :: _ "cancel")
--   ev = Core.FFIAudioParameter <<< inj (Proxy :: _ "envelope")
--   nmc = Core.FFIAudioParameter <<< inj (Proxy :: _ "numeric")
--   sdn = Core.FFIAudioParameter <<< inj (Proxy :: _ "sudden")
--   ut = Core.FFIAudioParameter <<< inj (Proxy :: _ "unit")
--   go scope di f (Core.AudioParameter a) = match
--     { numeric: bang <<< f <<< nmc
--     , envelope: bang <<< f <<< ev
--     , cancel: bang <<< f <<< cncl
--     , sudden: bang <<< f <<< sdn
--     , unit: \(Core.AudioUnit { u }) ->
--         let
--           Core.Node n = gain_ 1.0 u
--         in
--           makeEvent \k -> do
--             av <- AVar.empty
--             subscribe
--               ( n { parent: nothing, scope: scope, raiseId: \x -> void $ AVar.tryPut x av } di <|> makeEvent \k2 -> do
--                   void $ AVar.take av case _ of
--                     Left e -> throwException e
--                     -- only do the connection if not silence
--                     Right i -> k2 (f (ut (Core.FFIAudioUnit { i })))
--                   pure (pure unit)
--               )
--               k
--     }
--     a
