# Adding audio units

When adding or modifying audio units in this library, there are several files you'll need to consult. This document attempts to keep the most recent up-to-date list of those files.

| file | action |
| ------- | ---- |
| `Ocarina.purs` | Top-level imports |
| `Ocarina/Change.purs` | Changing a unit's values, ie the frequency of an oscillator or the playback rate of a playbuf |
| `Ocarina/Create.purs` | Creating new audio units |
| `Ocarina/Cursor.purs` | Making a cursor that points to an audio unit |
| `Ocarina/Destroy.purs` | Destroying an audio unit
| `Ocarina/Graph/Constructors.purs` | Type-level representations of audio units |
| `Ocarina/Rebase.purs` | Reset the pointers in an audio graph |
| `Ocarina/Rendered.purs` | Low-level instructions for rendering an audio graph |
| `Ocarina/Universe/AudioUnit.purs` | Constructors of audio units to be parsed by type classes |
| `Ocarina/Validation.purs` | Validation of the correctness of audio graphs |