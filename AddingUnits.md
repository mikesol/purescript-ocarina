# Adding audio units

When adding or modifying audio units in this library, there are several files you'll need to consult. This document attempts to keep the most recent up-to-date list of those files.

| file | action |
| ------- | ---- |
| `WAGS.purs` | Top-level imports |
| `WAGS/Change.purs` | Changing a unit's values, ie the frequency of an oscillator or the playback rate of a playbuf |
| `WAGS/Create.purs` | Creating new audio units |
| `WAGS/Cursor.purs` | Making a cursor that points to an audio unit |
| `WAGS/Destroy.purs` | Destroying an audio unit
| `WAGS/Graph/Constructors.purs` | Type-level representations of audio units |
| `WAGS/Rebase.purs` | Reset the pointers in an audio graph |
| `WAGS/Rendered.purs` | Low-level instructions for rendering an audio graph |
| `WAGS/Universe/AudioUnit.purs` | Constructors of audio units to be parsed by type classes |
| `WAGS/Validation.purs` | Validation of the correctness of audio graphs |