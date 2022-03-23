# Sketch of the PR

## Updating the onOff parameter of playbuf to take `PlayBufOnOff` instead of `AudioOnOff`.

```purescript
newtype PlaybufOnOff = PlaybuffOnOff (Variant
    -- starts is the start time
    -- next is an array of diffs in time
    -- { starts: 2.0, next: [1.0,0.5,0.5]}
    -- 2.0, 3.0, 3.5, 4.0
    (ons :: { starts :: Number, next :: Array Number } -- we turn on one buffer many times
    , off :: Number -- at a time, shut everything down
    , offOn :: { starts :: Number, next :: Array Number } -- at a time, shut everything down and start again
))
```

# Current status

name. | already playing | not playing.                 |
------------------------------------------------------ |
on.   | no-op           | turn on                      |
off.  | turns off       | no-op                        |
offOn | turns off then on again immediately | turns on |
--------------------------------------------------------

# New PR just for playbuf

name. | already playing | not playing.                 |
------------------------------------------------------ |
on.   | no-op                                                           | turn on & schedules  |
off.  | turns off & unschedules                                         | no-op                |
offOn | turns off & unschedules then on again immediately and schedules | turns on & schedules |
------------------------------------------------------------------------------------------------

We could want to schedule more sounds if we're already playing.

Change this in `Graph/AudioUnits.purs` for the newtype `PlayBuf`. Specifically, its `onOff` will have this type.
Once we do that, a bunch of stuff will turn read and it'll be type whack-a-mole.

At a certain point, you'll reach the ffi for `makePlayBuf` and `setOnOff`. Even here, you'll be able to use `NonEmptyArray` as it is just a newtype around `Array`.

As you're doing this, you'll need to create one new record in the variant in `Rendered.purs` called `setPlaybuffOnOff`, as `setOnOff` is also used for oscillators and loopbuf.

Once you create `setPlaybuffOnOff`, you'll have another game of type whack-a-mole for the variant in `Reconciliation.purs` and `Interpret.purs`. The solution will be to just copy and paste the boilerplate from `setOnOff`.

## Updating the JS


Once the FFI gets it, we will want to use the `ended` event emitted by the [`AudioScheduledSourceNode`](https://developer.mozilla.org/en-US/docs/Web/API/AudioScheduledSourceNode#events).

We will take the array, use it to schedule the first buffer. Then, on the `ended` event, in the listener, we'll schedule the next buffer. Then, in that buffer's listener, we'll schedule the next buffer, and so forth and so on.`

# Proposal 2

We keep the old on-off but we add a new parameter.
The new parameter is event spacing after current time.

```purescript
spacing :: { start :: Number, next :: Number }
```

## Example 1
We call it at 0 seconds with a spacing of { start: 0.0, next: [1.0, 1.0]}
Then, at 2.5 seconds, we call it with a spacing of { start: 0.5, next: [1.0, 1.0, 1.0]}
This will produce [0, 1, 2, 3, 4, 5, 6].

## Example 2
However, the behavior would also be this:

We call it at 0 seconds with a spacing of { start: 0.0, next: [1.0, 1.0, 1.0, 1.0]}
Then, at 2.5 seconds, we call it with a spacing of { start: 0.5, next: [1.0, 1.0, 1.0]}.
If we didn't cancel, it would produce [0, 1, 2, 3, 3, 4, 4, 5, 6]
This will produce [0, 1, 2, 3, 4, 5, 6].

One advantage of erasing the previous and putting new is that it is consistent with sound theory in general. Meaning that when we change the frequency of an oscillator, we don't keep the old one and add the new one simultaneously.

```purescript
newtype PlaybufOnOff = PlaybuffOnOff (Variant
    -- starts is the start time
    -- next is an array of diffs in time
    -- { starts: 2.0, next: [1.0,0.5,0.5]}
    -- 2.0, 3.0, 3.5, 4.0
    (on :: { starts :: Number, next :: Array Number } -- we turn on one buffer many times
    , off :: Number -- at a time, shut everything down
    , offOn :: { starts :: Number, next :: Array Number } -- at a time, shut everything down and start again
))
```

```purescript
argument = { buffer, onOff: AudioOnOff { onOff: _offOn, timeOffset: unwrap offset } }
-- https://github.com/mikesol/rhythm-game-sketch/blob/188031f6234b25ff4be574bf38a10954e27be304/src/Feedback/Acc.purs#L80
argument = { buffer, timeOffset }

argument = { buffer: sampleOne, onOff: PlaybufOnOff ({ starts: 0.0, next: []  }) }
argument = { buffer: sampleTwo, onOff: PlaybufOnOff ({ starts: 2.0, next: [ ] }) }
```

```purescript
argument = { onOff: PlaybufOnOff ({ starts: {t:0.0,b:buffer0}, next: [{t:2.0,b:buffer1}]  }) }
```

PROBLEM: buffer is currently decoupled from on off.

We probably want to change buffer to be 100% coupled with the on-off mechanism. This would mean rolling it into the playbuffOnOff type, which seems safe as there is no such thing as turning on a playbuf without a buffer.

It doesn't really make sense to change a buffer midstream, meaning that when we schedule something to turn on, we know in advance what we want the buffer to be.


###### Problem

Background
- buffer0 lasts 10m
- buffer1 lasts 8m

```purescript
ichange (Proxy :: _ "ptr") { onOff: PlaybufOnOff ({ starts: {t:0.0,b:buffer0,bufferOffset:0.0}, next: [{t:2.0,b:buffer1,bufferOffset:0.0}]  }) }
```

#### final candidate?
```purescript
-- time buffer offset
type TBO = {t :: Number, b :: AudioBufferNode, o :: Number }
newtype PlaybufOnOff = PlaybuffOnOff (Variant
    -- starts is the start time
    -- next is an array of diffs in time
    -- { starts: 2.0, next: [1.0,0.5,0.5]}
    -- 2.0, 3.0, 3.5, 4.0
    (ons :: { starts :: TBO, next :: Array TBO } -- we turn on one buffer many times
    , off :: Number -- at a time, shut everything down
    , offOn :: { starts :: TBO, next :: Array TBO } -- at a time, shut everything down and start again
))
type PlayBuf' =
  ( onOff :: PlaybufOnOff
  -- applies to the currently playing buffer
  -- this can take an envelope
  , playbackRate :: AudioParameter
  )
```

We _always_ distribute the playback rate just like it was a volume or frequency or q-value of a filter.
This requires a bit of accounting because we need to do array splice in the case of multiple samples, but that is pretty fast in JS-land.

When we change it, we haven't yet called the future samples, so they don't have to worry about anything and will just get the most recent value once they start.

We schedule three sounds.
Each sound lasts three minutes.
At 1.5 minutes, we change the envelope of the playbackRate.

## Another option

Create multi-buf. This is an entirely separate node that exists for the purpose of scheduling multiple buffers.