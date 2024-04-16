module Bolson.Control
  ( flatten
  , globalPortalComplexComplex
  , globalPortalSimpleComplex
  , globalPortalComplexSimple
  , portalComplexComplex
  , portalSimpleComplex
  , portalComplexSimple
  , fixComplexComplex
  , behaving
  , Flatten
  , Portal
  , PortalComplex
  , PortalSimple
  , Fix
  ) where

import Prelude

import Bolson.Core (Child(..), DynamicChildren(..), Element(..), Entity(..), FixedChildren(..), PSR, Scope(..))
import Control.Lazy as Lazy
import Control.Monad.ST.Class (liftST)
import Control.Monad.ST.Global as Global
import Control.Monad.ST.Global as Region
import Control.Monad.ST.Internal (ST)
import Control.Monad.ST.Internal as Ref
import Control.Monad.ST.Internal as ST
import Control.Plus (empty)
import Data.Array as Array
import Data.FastVect.FastVect (toArray, Vect)
import Data.Filterable (compact)
import Data.Foldable (foldl, for_)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List ((:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Tuple (fst, snd)
import FRP.Event (Event, EventfulProgram, createPure, justMany, justNone, justOne, makeEvent, merge)
import FRP.Event.Class (once)
import FRP.Poll (Poll, poll, sample, sample_)
import Foreign.Object as Object
import Prim.Int (class Compare)
import Prim.Ordering (GT)
import Prim.Row (class Lacks)
import Record.Builder as RB
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

----

type Neg1 = -1

newtype MutAr a = MutAr (Array a)

foreign import mutAr :: forall s a. Array a -> ST s (MutAr a)
foreign import unsafeUpdateMutAr :: forall s a. Int -> a -> MutAr a -> ST s Unit
foreign import readAr :: forall s a. MutAr a -> ST s (Array a)

type Portal logic specialization interpreter obj1 obj2 r payload =
  { giveNewParent ::
      interpreter
      -> { id :: String
         , parent :: String
         , deferralPath :: List.List Int
         , scope :: Scope
         , raiseId :: String -> ST.ST Region.Global Unit
         | r
         }
      -> Entity logic (obj1 payload)
      -> specialization
      -> payload
  , wrapElt ::
      Entity logic (obj1 payload)
      -> Entity logic (obj1 payload)
  , deleteFromCache :: interpreter -> { id :: String } -> payload
  , fromEltO1 :: Element interpreter r payload -> obj1 payload
  , fromEltO2 :: Element interpreter r payload -> obj2 payload
  , toElt :: obj1 payload -> Element interpreter r payload
  }

type PortalComplex logic specialization interpreter obj1 obj2 r payload =
  { giveNewParent ::
      interpreter
      -> { id :: String
         , parent :: String
         , deferralPath :: List.List Int
         , scope :: Scope
         , raiseId :: String -> ST.ST Region.Global Unit
         | r
         }
      -> Entity logic (obj1 payload)
      -> specialization
      -> payload
  , wrapElt ::
      Entity logic (obj1 payload)
      -> Entity logic (obj1 payload)
  , deferPayload :: interpreter -> List.List Int -> payload -> payload
  , deleteFromCache :: interpreter -> { id :: String } -> payload
  , fromEltO1 :: Element interpreter r payload -> obj1 payload
  , fromEltO2 :: Element interpreter r payload -> obj2 payload
  , toEltO1 :: obj1 payload -> Element interpreter r payload
  , toEltO2 :: obj2 payload -> Element interpreter r payload
  }

type PortalSimple logic specialization interpreter obj1 obj2 r payload =
  { giveNewParent ::
      interpreter
      -> { id :: String
         , parent :: String
         , deferralPath :: List.List Int
         , scope :: Scope
         , raiseId :: String -> ST.ST Region.Global Unit
         | r
         }
      -> Entity logic (obj1 payload)
      -> specialization
      -> payload
  , deleteFromCache :: interpreter -> { id :: String } -> payload
  , fromEltO1 :: Element interpreter r payload -> obj1 payload
  , fromEltO2 :: Element interpreter r payload -> obj2 payload
  , toElt :: obj1 payload -> Element interpreter r payload
  }

behaving'
  :: forall a
   . ( forall b
        . (forall c. Event c -> (c -> EventfulProgram b) -> ST Global.Global (ST Global.Global Unit))
       -> Event (a -> b)
       -> (a -> EventfulProgram b)
       -> (Event b -> EventfulProgram b)
       -> EventfulProgram b
     )
  -> Poll a
behaving' iii = poll \e -> makeEvent \subscribe -> do
  urf <- Ref.new (pure unit)
  ugh <- subscribe e \f -> do
    iii subscribe e (f >>> justOne) \z -> justNone do
      acsu <- subscribe z justOne
      void $ Ref.modify (_ *> acsu) urf
  pure do
    liftST $ join (Ref.read urf)
    ugh

behaving
  :: forall a
   . ( forall b
        . Event (a -> b)
       -> (a -> EventfulProgram b)
       -> (Event b -> EventfulProgram b)
       -> EventfulProgram b
     )
  -> Poll a
behaving iii = behaving' \_ -> iii

internalPortalSimpleComplex
  :: forall n r logic obj1 obj2 specialization interpreter payload
   . Compare n Neg1 GT
  => Lacks "id" r
  => Lacks "raiseId" r
  => Boolean
  -> (Scope -> Scope)
  -> Flatten logic interpreter obj2 r payload
  -> PortalSimple logic specialization interpreter obj1 obj2 r payload
  -> Vect n (obj1 payload)
  -> ( Vect n (specialization -> (obj1 payload))
       -> Entity logic (obj2 payload)
     )
  -> Entity logic (obj2 payload)
internalPortalSimpleComplex
  isGlobal
  scopeF
  flatArgs
  { giveNewParent
  , deleteFromCache
  , fromEltO1
  , fromEltO2
  , toElt
  }
  toBeam
  closure = Element' $ fromEltO2 $ Element go
  where
  go psr interpreter = behaving' \fullSub e kx subscribe -> do
    -- we initialize a mutable array with empty ids and empty elements
    -- for each element in the portal vector
    av <- justNone $ mutAr
      ( toArray toBeam $>
          { id: ""
          , entity: Element'
              (fromEltO1 (Element \_ _ -> poll \_ -> empty))
          }
      )
    arct <- justNone $ Ref.new 0
    let
      cont = do
        -- this is the id we'll use for deferred unloading
        let
          asIds
            :: Array { id :: String, entity :: Entity logic (obj1 payload) }
            -> Vect n { id :: String, entity :: Entity logic (obj1 payload) }
          asIds = unsafeCoerce
        -- now, when we read the ids, we will have all of the ids of the "beamable" elements in the vector
        -- this is because the left-bind above that produces actualized' triggers all of the `raiseId` in the elements
        idz <- justNone (asIds <$> (readAr av))
        -- here's the bait and switch: instead of injecting the beamables into the closure,
        -- we inject completely empty elements
        -- they have no moving parts, so it's an empty event
        -- the only thing they do is signal that they're
        -- in fact from the portal (the raiseId)
        -- and provide a side-effect to run immediately upon subscription, meaning the give-new-parent
        let
          injectable = map
            ( \{ id, entity } specialization -> fromEltO1 $ Element
                \psr2 itp -> behaving \_ kxkx _ -> do
                  justNone $ psr2.raiseId id
                  for_
                    ( compact
                        [ psr2.parent <#> \pt ->
                            ( giveNewParent itp
                                ( RB.build
                                    ( RB.insert (Proxy :: _ "id") id >>>
                                        RB.modify (Proxy :: _ "parent")
                                          (const pt)
                                    )
                                    psr2
                                )
                                entity
                                specialization
                            )
                        ]
                    )
                    kxkx
            )
            idz
          -- now, the elements are simply the evaluation of the closure

          realized = flatten flatArgs (closure injectable) psr interpreter

        subscribe (sample realized e)
        -- When we unsubscribe from the portal, we want to delete everything
        -- with one of the ids we created.
        when (not isGlobal) do
          for_ (toArray idz) \{ id } -> do
            kx (flatArgs.deferPayload interpreter psr.deferralPath (deleteFromCache interpreter { id }))

    -- We intercept all of the elements in the portal vector
    -- and turn them into instructions and events.
    --
    -- This is very much like flatten on its simplest branch.
    --
    -- Crucially, when an id is raised, we update mutAr
    -- with the entity so we know what things can be beamed around.
    --
    -- We'll need this later when we actually do the beaming.
    --
    -- We also give the framework the option to wrap the element
    -- so that we are dealing with a singleton (Element'), otherwise it gets too thorny.
    let
      actualized = mapWithIndex
        ( \ix entity -> toElt entity # \(Element elt) -> sample
            ( elt
                ( psr
                    { parent = Nothing
                    , scope = scopeF psr.scope
                    , raiseId = \id -> do
                        unsafeUpdateMutAr ix { id, entity: Element' entity } av
                        ii <- Ref.read arct
                        if ii + 1 == Array.length (toArray toBeam) then do
                               ugh <- createPure
                               ughhh <- fullSub ugh.event \_ -> cont
                               ugh.push unit
                               ughhh
                        else void $ Ref.modify (add 1) arct
                    }
                )
                interpreter
            )
            e
        )
        (toArray toBeam)
    subscribe (merge actualized)

internalPortalComplexComplex
  :: forall n r logic obj1 obj2 specialization interpreter payload
   . Compare n Neg1 GT
  => Lacks "id" r
  => Lacks "raiseId" r
  => Boolean
  -> (Scope -> Scope)
  -> Flatten logic interpreter obj2 r payload
  -> Portal logic specialization interpreter obj1 obj2 r payload
  -> Vect n (Entity logic (obj1 payload))
  -> ( Vect n (specialization -> Entity logic (obj1 payload))
       -> Entity logic (obj2 payload)
     )
  -> Entity logic (obj2 payload)
internalPortalComplexComplex
  isGlobal
  scopeF
  flatArgs
  { giveNewParent
  , deleteFromCache
  , fromEltO1
  , fromEltO2
  , wrapElt
  , toElt
  }
  toBeam
  closure = Element' $ fromEltO2 $ Element go
  where
  go psr interpreter = behaving' \fullSub e kx subscribe -> do
    -- we initialize a mutable array with empty ids and empty elements
    -- for each element in the portal vector
    av <- justNone $ mutAr
      ( toArray toBeam $>
          { id: ""
          , entity: Element'
              (fromEltO1 (Element \_ _ -> poll \_ -> empty))
          }
      )
    arct <- justNone $ Ref.new 0
    let
      cont = do
        -- this is the id we'll use for deferred unloading
        let
          asIds
            :: Array { id :: String, entity :: Entity logic (obj1 payload) }
            -> Vect n { id :: String, entity :: Entity logic (obj1 payload) }
          asIds = unsafeCoerce
        -- now, when we read the ids, we will have all of the ids of the "beamable" elements in the vector
        -- this is because the left-bind above that produces actualized' triggers all of the `raiseId` in the elements
        idz <- justNone (asIds <$> (readAr av))
        -- here's the bait and switch: instead of injecting the beamables into the closure,
        -- we inject completely empty elements
        -- they have no moving parts, so it's an empty event
        -- the only thing they do is signal that they're
        -- in fact from the portal (the raiseId)
        -- and provide a side-effect to run immediately upon subscription, meaning the give-new-parent
        let
          injectable = map
            ( \{ id, entity } specialization -> Element' $ fromEltO1 $ Element
                \psr2 itp -> behaving \_ kxkx _ -> do
                  justNone $ psr2.raiseId id
                  for_
                    ( compact
                        [ psr2.parent <#> \pt ->
                            ( giveNewParent itp
                                ( RB.build
                                    ( RB.insert (Proxy :: _ "id") id >>>
                                        RB.modify (Proxy :: _ "parent")
                                          (const pt)
                                    )
                                    psr2
                                )
                                entity
                                specialization
                            )
                        ]
                    )
                    kxkx
            )
            idz
          -- now, the elements are simply the evaluation of the closure
          realized = flatten flatArgs (closure (injectable)) psr interpreter

        subscribe (sample realized e)
        -- When we unsubscribe from the portal, we want to delete everything
        -- with one of the ids we created.
        when (not isGlobal) do
          for_ (toArray idz) \{ id } -> do
            kx (flatArgs.deferPayload interpreter psr.deferralPath (deleteFromCache interpreter { id }))
    -- We intercept all of the elements in the portal vector
    -- and turn them into instructions and events.
    --
    -- This is very much like flatten on its simplest branch.
    --
    -- Crucially, when an id is raised, we update mutAr
    -- with the entity so we know what things can be beamed around.
    --
    -- We'll need this later when we actually do the beaming.
    --
    -- We also give the framework the option to wrap the element
    -- so that we are dealing with a singleton (Element'), otherwise it gets too thorny.
    let
      actualized = mapWithIndex
        ( \ix -> Lazy.fix \ff entity -> case entity of
            Element' beamable -> toElt beamable # \(Element elt) -> sample
              ( elt
                  ( psr
                      { parent = Nothing
                      , scope = scopeF psr.scope
                      , raiseId = \id -> do
                          unsafeUpdateMutAr ix { id, entity } av
                          ii <- Ref.read arct
                          if ii + 1 == Array.length (toArray toBeam) then do
                               ugh <- createPure
                               ughhh <- fullSub ugh.event \_ -> cont
                               ugh.push unit
                               ughhh
                          else void $ Ref.modify (add 1) arct
                      }
                  )
                  interpreter
              )
              e
            _ -> ff (wrapElt entity)
        )
        (toArray toBeam)
    subscribe (merge actualized)

internalPortalComplexSimple
  :: forall n r logic obj1 obj2 specialization interpreter payload
   . Compare n Neg1 GT
  => Lacks "id" r
  => Lacks "raiseId" r
  => Boolean
  -> (Scope -> Scope)
  -> PortalComplex logic specialization interpreter obj1 obj2 r payload
  -> Vect n (Entity logic (obj1 payload))
  -> (Vect n (specialization -> Entity logic (obj1 payload)) -> obj2 payload)
  -> obj2 payload
internalPortalComplexSimple
  isGlobal
  scopeF
  { giveNewParent
  , deleteFromCache
  , deferPayload
  , fromEltO1
  , fromEltO2
  , wrapElt
  , toEltO1
  , toEltO2
  }
  toBeam
  closure = fromEltO2 $ Element go
  where
  go psr interpreter = behaving' \fullSub e kx subscribe -> do
    -- we initialize a mutable array with empty ids and empty elements
    -- for each element in the portal vector
    av <- justNone $ mutAr
      ( toArray toBeam $>
          { id: ""
          , entity: Element'
              (fromEltO1 (Element \_ _ -> poll \_ -> empty))
          }
      )
    arct <- justNone $ Ref.new 0
    let
      cont = do
        -- this is the id we'll use for deferred unloading
        let
          asIds
            :: Array { id :: String, entity :: Entity logic (obj1 payload) }
            -> Vect n { id :: String, entity :: Entity logic (obj1 payload) }
          asIds = unsafeCoerce
        -- now, when we read the ids, we will have all of the ids of the "beamable" elements in the vector
        -- this is because the left-bind above that produces actualized' triggers all of the `raiseId` in the elements
        idz <- justNone (asIds <$> (readAr av))
        -- here's the bait and switch: instead of injecting the beamables into the closure,
        -- we inject completely empty elements
        -- they have no moving parts, so it's an empty event
        -- the only thing they do is signal that they're
        -- in fact from the portal (the raiseId)
        -- and provide a side-effect to run immediately upon subscription, meaning the give-new-parent
        let
          injectable = map
            ( \{ id, entity } specialization -> Element' $ fromEltO1 $ Element
                \psr2 itp -> poll \ne -> makeEvent \sub2 -> sub2 ne
                  \ff ->
                    do
                      justNone $ liftST $ psr2.raiseId id
                      justMany $ map ff $
                        ( compact
                            [ psr2.parent <#> \pt ->
                                ( giveNewParent itp
                                    ( RB.build
                                        ( RB.insert (Proxy :: _ "id") id >>>
                                            RB.modify (Proxy :: _ "parent")
                                              (const pt)
                                        )
                                        psr2
                                    )
                                    entity
                                    specialization
                                )
                            ]
                        )
            )
            idz
          -- now, the elements are simply the evaluation of the closure
          Element realized = toEltO2 (closure (injectable))
        subscribe (sample (realized psr interpreter) e)
        -- When we unsubscribe from the portal, we want to delete everything
        -- with one of the ids we created.
        when (not isGlobal) do
          for_ (toArray idz) \{ id } -> do
            kx (deferPayload interpreter psr.deferralPath (deleteFromCache interpreter { id }))
    -- We intercept all of the elements in the portal vector
    -- and turn them into instructions and events.
    --
    -- This is very much like flatten on its simplest branch.
    --
    -- Crucially, when an id is raised, we update mutAr
    -- with the entity so we know what things can be beamed around.
    --
    -- We'll need this later when we actually do the beaming.
    --
    -- We also give the framework the option to wrap the element
    -- so that we are dealing with a singleton (Element'), otherwise it gets too thorny.
    let
      actualized = mapWithIndex
        ( \ix -> Lazy.fix \ff entity -> case entity of
            Element' beamable -> toEltO1 beamable # \(Element elt) -> sample
              ( elt
                  ( psr
                      { parent = Nothing
                      , scope = scopeF psr.scope
                      , raiseId = \id -> do
                          unsafeUpdateMutAr ix { id, entity } av
                          ii <- Ref.read arct
                          if ii + 1 == Array.length (toArray toBeam) then do
                               ugh <- createPure
                               ughhh <- fullSub ugh.event \_ -> cont
                               ugh.push unit
                               ughhh
                          else void $ Ref.modify (add 1) arct
                      }
                  )
                  interpreter
              )
              e
            _ -> ff (wrapElt entity)
        )
        (toArray toBeam)
    subscribe (merge actualized)

globalPortalComplexComplex
  :: forall n r logic obj1 obj2 specialization interpreter payload
   . Compare n Neg1 GT
  => Lacks "id" r
  => Lacks "raiseId" r
  => Flatten logic interpreter obj2 r payload
  -> Portal logic specialization interpreter obj1 obj2 r payload
  -> Vect n (Entity logic (obj1 payload))
  -> ( Vect n (specialization -> Entity logic (obj1 payload))
       -> Entity logic (obj2 payload)
     )
  -> Entity logic (obj2 payload)
globalPortalComplexComplex
  flatArgs
  portalArgs
  toBeam
  closure = internalPortalComplexComplex true (const Global) flatArgs
  portalArgs
  toBeam
  closure

globalPortalSimpleComplex
  :: forall n r logic obj1 obj2 specialization interpreter payload
   . Compare n Neg1 GT
  => Lacks "id" r
  => Lacks "raiseId" r
  => Flatten logic interpreter obj2 r payload
  -> PortalSimple logic specialization interpreter obj1 obj2 r
       payload
  -> Vect n (obj1 payload)
  -> ( Vect n (specialization -> obj1 payload)
       -> Entity logic (obj2 payload)
     )
  -> Entity logic (obj2 payload)
globalPortalSimpleComplex
  flatArgs
  portalArgs
  toBeam
  closure = internalPortalSimpleComplex true (const Global) flatArgs
  portalArgs
  toBeam
  closure

globalPortalComplexSimple
  :: forall n r logic obj1 obj2 specialization interpreter payload
   . Compare n Neg1 GT
  => Lacks "id" r
  => Lacks "raiseId" r
  => PortalComplex logic specialization interpreter obj1 obj2 r
       payload
  -> Vect n (Entity logic (obj1 payload))
  -> ( Vect n (specialization -> Entity logic (obj1 payload))
       -> obj2 payload
     )
  -> obj2 payload
globalPortalComplexSimple
  portalArgs
  toBeam
  closure = internalPortalComplexSimple true (const Global)
  portalArgs
  toBeam
  closure

portalComplexComplex
  :: forall n r logic obj1 obj2 specialization interpreter payload
   . Compare n Neg1 GT
  => Lacks "id" r
  => Lacks "raiseId" r
  => Flatten logic interpreter obj2 r payload
  -> Portal logic specialization interpreter obj1 obj2 r payload
  -> Vect n (Entity logic (obj1 payload))
  -> ( Vect n (specialization -> Entity logic (obj1 payload))
       -> Entity logic (obj2 payload)
     )
  -> Entity logic (obj2 payload)
portalComplexComplex
  flatArgs
  portalArgs
  toBeam
  closure = internalPortalComplexComplex false identity flatArgs
  portalArgs
  toBeam
  closure

portalSimpleComplex
  :: forall n r logic obj1 obj2 specialization interpreter payload
   . Compare n Neg1 GT
  => Lacks "id" r
  => Lacks "raiseId" r
  => Flatten logic interpreter obj2 r payload
  -> PortalSimple logic specialization interpreter obj1 obj2 r payload
  -> Vect n (obj1 payload)
  -> ( Vect n (specialization -> obj1 payload)
       -> Entity logic (obj2 payload)
     )
  -> Entity logic (obj2 payload)
portalSimpleComplex
  flatArgs
  portalArgs
  toBeam
  closure = internalPortalSimpleComplex false identity flatArgs
  portalArgs
  toBeam
  closure

portalComplexSimple
  :: forall n r logic obj1 obj2 specialization interpreter payload
   . Compare n Neg1 GT
  => Lacks "id" r
  => Lacks "raiseId" r
  => PortalComplex logic specialization interpreter obj1 obj2 r
       payload
  -> Vect n (Entity logic (obj1 payload))
  -> ( Vect n (specialization -> Entity logic (obj1 payload))
       -> obj2 payload
     )
  -> obj2 payload
portalComplexSimple
  portalArgs
  toBeam
  closure = internalPortalComplexSimple false identity
  portalArgs
  toBeam
  closure

data Stage = Listening | Closed

type Flatten logic interpreter obj r payload =
  { doLogic :: logic -> interpreter -> String -> payload
  , ids :: interpreter -> ST Region.Global Int
  , disconnectElement ::
      interpreter
      -> { id :: String, parent :: String, scope :: Scope }
      -> payload
  , deferPayload :: interpreter -> List.List Int -> payload -> payload
  , forcePayload :: interpreter -> List.List Int -> payload
  , toElt :: obj payload -> Element interpreter r payload
  }

flatten
  :: forall r obj logic interpreter payload
   . Flatten logic interpreter obj r payload
  -> Entity logic (obj payload)
  -> PSR r
  -> interpreter
  -> Poll payload
flatten
  flatArgs@
    { doLogic
    , ids
    , deferPayload
    , forcePayload
    , disconnectElement
    , toElt
    }
  etty
  psr
  interpreter = case etty of
  FixedChildren' (FixedChildren fc) -> poll \e ->
    merge $ map (\ex -> sample (flatten flatArgs ex psr interpreter) e) fc
  Element' e -> element (toElt e)
  -- todo: cancelInner is preventing this from using `behaving`
  -- is there a way to fix that?
  DynamicChildren' (DynamicChildren children) -> poll \e0 -> makeEvent
    \subscribe -> do
      urf <- Ref.new (pure unit)
      cancelInner <- liftST $ Ref.new Object.empty
      ugh <- subscribe e0 \f -> do
        -- fireId1 is only needed for global clean up
        -- if we clean the dyn and removes haven't been called, this will pick it up
        fireId1 <- justNone $ liftST $ ids interpreter
        justOne $ f
          ( deferPayload interpreter psr.deferralPath
              (forcePayload interpreter $ List.snoc psr.deferralPath fireId1)
          )
        justNone do
          eepp <- createPure
          unsubscribe <- subscribe (sample_ children e0) \k -> justNone (eepp.push k)
          let memoKids = { unsubscribe, event: eepp.event }
          void $ Ref.modify (_ *> memoKids.unsubscribe) urf
          cancelOuter <- subscribe memoKids.event \inner -> justNone do
            fireId2 <- liftST $ ids interpreter
            -- holds the previous id
            myUnsubId <- liftST $ ids interpreter
            myUnsub <- liftST $ Ref.new (pure unit)
            eltsUnsubId <- liftST $ ids interpreter
            eltsUnsub <- liftST $ Ref.new (pure unit)
            myIds <- liftST $ Ref.new []
            myScope <- liftST $ Local <$>
              ( case psr.scope of
                  Global -> show <$> ids interpreter
                  Local l -> pure l <> pure "!" <> show <$> ids interpreter
              )
            stageRef <- liftST $ Ref.new Listening
            void $ liftST $ Ref.write Listening stageRef
            -- for the inner dyn, we pass it a singleton via `once`
            let
              evt = sample
                ( flatten
                    flatArgs
                    (snd inner)
                    ( psr
                        { scope = myScope
                        , deferralPath = psr.deferralPath <>
                            (fireId1 : fireId2 : List.Nil)
                        , raiseId = \id -> do
                            void $ Ref.modify (append [ id ]) myIds
                        }
                    )
                    interpreter
                )
                (once memoKids.event $> identity)
            c1 <- liftST $ subscribe evt (justOne <<< f)
            void $ liftST $ Ref.modify (Object.insert (show eltsUnsubId) c1)
              cancelInner
            void $ liftST $ Ref.write c1 eltsUnsub
            c0 <- liftST $ subscribe (sample_ (fst inner) (once memoKids.event))
              \kid' -> do
                stage <- justNone $ Ref.read stageRef
                case kid', stage of
                  Logic lgc, Listening -> do
                    cid <- justNone $ Ref.read myIds
                    for_ cid (justOne <<< f <<< doLogic lgc interpreter)
                  Remove, Listening -> do
                    void $ justNone $ Ref.write Closed stageRef
                    idRef <- justNone $ Ref.read myIds
                    for_ idRef \old ->
                      for_ psr.parent \pnt -> justOne $ f
                        ( disconnectElement interpreter
                            { id: old, parent: pnt, scope: myScope }
                        )
                    -- we force after the disconnect element
                    -- because assumedly the forcing has clean-up-y stuff
                    -- so we want to disconnect before we clean up, lest
                    -- we try to disconnect something that has already been deleted
                    justOne $ f $ forcePayload interpreter $ psr.deferralPath <>
                      (fireId1 : fireId2 : List.Nil)
                    myu <- justNone $ Ref.read myUnsub
                    justNone myu
                    eltu <- justNone $ Ref.read eltsUnsub
                    justNone eltu
                    void $ justNone $ Ref.modify
                      (Object.delete $ show myUnsubId)
                      cancelInner
                    void $ justNone $ Ref.modify
                      (Object.delete $ show eltsUnsubId)
                      cancelInner
                  _, _ -> pure unit
            void $ liftST $ Ref.write c0 myUnsub
            void $ liftST $ Ref.modify (Object.insert (show myUnsubId) c0)
              cancelInner
          void $ Ref.modify (_ *> cancelOuter) urf

      pure do
        liftST $ join (Ref.read urf)
        ugh
        (Ref.read cancelInner) >>= foldl (*>) (pure unit)

  where
  element (Element e) = e psr interpreter

type Fix interpreter obj r payload =
  { connectToParent ::
      interpreter -> { id :: String, parent :: String } -> payload
  , fromElt :: Element interpreter r payload -> obj payload
  }

fixComplexComplex
  :: forall r obj logic interpreter payload
   . Flatten logic interpreter obj r payload
  -> Fix interpreter obj r payload
  -> (Entity logic (obj payload) -> Entity logic (obj payload))
  -> Entity logic (obj payload)
fixComplexComplex
  flatArgs
  { connectToParent, fromElt }
  fx = Element' $ fromElt $ Element go
  where
  go i interpret = poll \ez -> makeEvent \subex -> do
    urf <- Ref.new (pure unit)
    uu <- subex ez \_ -> do
      av <- justNone $ Ref.new Nothing
      let
        nn = fx $ Element' $ fromElt $ Element \ii _ -> poll \e ->
          makeEvent \subscribe -> subscribe e \f -> do
            av' <- justNone $ Ref.read av
            case av', ii.parent of
              Just r, Just p'
                | r /= p' -> justOne $ f $ connectToParent interpret
                    { id: r, parent: p' }
              _, _ -> pure unit
      ud <- justNone $ subex
        ( sample
            ( flatten flatArgs
                nn
                ( i
                    { parent = i.parent
                    , scope = i.scope
                    , raiseId = \s -> do
                        i.raiseId s
                        void $ Ref.write (Just s) av
                    }
                )
                interpret
            )
            ez
        )
        justOne
      void $ justNone $ Ref.modify (_ *> ud) urf
    pure do
      join (Ref.read urf)
      uu
