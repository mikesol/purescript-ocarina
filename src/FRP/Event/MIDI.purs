module FRP.Event.MIDI
  ( MIDIAccess(..)
  , MIDIEvent(..)
  , MIDIEventInTime(..)
  , midi
  , midiAccess
  ) where

import Prelude
import Control.Promise (Promise)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Foldable (traverse_)
import Data.List (List)
import Data.Map as M
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (wrap)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import FRP.Event (Event, makeEvent)
import Foreign.Object as O
import Web.Event.Event as WE
import Web.Event.EventTarget (EventTarget, addEventListener, eventListener, removeEventListener)
import Web.Internal.FFI (unsafeReadProtoTagged)

-- | Represents a single MIDI event.
data MIDIEvent
  = NoteOff Int Int Int
  | NoteOn Int Int Int
  | Polytouch Int Int Int
  | ControlChange Int Int Int
  | ProgramChange Int Int
  | Aftertouch Int Int
  | Pitchwheel Int Int

-- | Represents a MIDI event with a timestamp.
type MIDIEventInTime
  = { timeStamp :: Number
    , event :: MIDIEvent
    }

-- | The Web API's [MIDIAccess](https://developer.mozilla.org/en-US/docs/Web/API/MIDIAccess).
foreign import data MIDIAccess :: Type

-- | The Web API's [MIDIMessageEvent](https://developer.mozilla.org/en-US/docs/Web/API/MIDIMessageEvent).
foreign import data MIDIMessageEvent :: Type

-- | Get the [MIDIAccess](https://developer.mozilla.org/en-US/docs/Web/API/MIDIAccess) from the browser.
foreign import midiAccess :: Effect (Promise MIDIAccess)

foreign import toTargetMap :: MIDIAccess -> Effect (O.Object EventTarget)

foreign import toMIDIEvent_ ::
  (Int -> Int -> Int -> MIDIEvent) ->
  (Int -> Int -> Int -> MIDIEvent) ->
  (Int -> Int -> Int -> MIDIEvent) ->
  (Int -> Int -> Int -> MIDIEvent) ->
  (Int -> Int -> MIDIEvent) ->
  (Int -> Int -> MIDIEvent) ->
  (Int -> Int -> MIDIEvent) ->
  Maybe MIDIEvent ->
  (MIDIEvent -> Maybe MIDIEvent) ->
  ArrayBuffer ->
  Effect (Maybe MIDIEvent)

foreign import getData_ :: (Maybe ArrayBuffer) -> (ArrayBuffer -> Maybe ArrayBuffer) -> MIDIMessageEvent -> Effect (Maybe ArrayBuffer)

foreign import getTimeStamp_ :: (Maybe Number) -> (Number -> Maybe Number) -> MIDIMessageEvent -> Effect (Maybe Number)

getData :: MIDIMessageEvent -> Effect (Maybe ArrayBuffer)
getData = getData_ Nothing Just

getTimeStamp :: MIDIMessageEvent -> Effect (Maybe Number)
getTimeStamp = getTimeStamp_ Nothing Just

toMIDIEvent :: ArrayBuffer -> Effect (Maybe MIDIEvent)
toMIDIEvent =
  toMIDIEvent_
    NoteOff
    NoteOn
    Polytouch
    ControlChange
    ProgramChange
    Aftertouch
    Pitchwheel
    Nothing
    Just

fromEvent :: WE.Event -> Maybe MIDIMessageEvent
fromEvent = unsafeReadProtoTagged "MIDIMessageEvent"

-- | After having acquired the [MIDIAccess](https://developer.mozilla.org/en-US/docs/Web/API/MIDIAccess) from the browser, use it to create a streamed event of type `Event MIDIEventInTime`.
midi :: MIDIAccess -> Event MIDIEventInTime
midi midiAccess_ =
  makeEvent \push -> do
    targetMap <- toTargetMap midiAccess_ >>= pure <<< M.fromFoldable <<< (O.toUnfoldable :: O.Object EventTarget -> List (Tuple String EventTarget))
    let
      makeListener inputName =
        eventListener \e -> do
          fromEvent e
            # traverse_ \me -> do
                data__ <- getData me
                timeStamp__ <- getTimeStamp me
                midiEvent__ <- maybe (pure Nothing) toMIDIEvent data__
                let
                  toAdd_ =
                    ( do
                        timeStamp_ <- timeStamp__
                        midiEvent_ <- midiEvent__
                        pure
                          { timeStamp: timeStamp_
                          , event: midiEvent_
                          }
                    )
                case toAdd_ of
                  Nothing -> pure unit
                  Just toAdd -> push toAdd
    listeners <-
      sequence
        $ M.mapMaybeWithKey
            ( \k v ->
                Just
                  ( do
                      listener <- makeListener k
                      _ <-
                        addEventListener
                          (wrap "midimessage")
                          listener
                          false
                          v
                      pure (Tuple listener v)
                  )
            )
            targetMap
    let
      dispose = do
        _ <-
          sequence
            $ M.mapMaybeWithKey
                ( \k (Tuple l v) ->
                    Just
                      ( removeEventListener
                          (wrap "midimessage")
                          l
                          false
                          v
                      )
                )
                listeners
        pure unit
    pure dispose
