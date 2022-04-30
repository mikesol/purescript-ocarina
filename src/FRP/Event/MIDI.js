export function midiAccess() {
  return navigator.requestMIDIAccess()
}

const mkMidiDevices = function (devices) {
  return function (mk) {
    return function () {
      const res = [];
      for (let device of devices.entries()) {
        const portID = device[0];
        const dev = device[1];
        res.push(mk(portID)(dev.manufacturer)(dev.name))
      }
      return res;
    }
  }
}

export function midiInputDevices_(midiAccess) {
  return mkMidiDevices(midiAccess.inputs)
}

export function midiOutputDevices_(midiAccess) {
  return mkMidiDevices(midiAccess.outputs)
}

export function getData_(nothing) {
  return function (just) {
    return function (e) {
      return function () {
        return e.data ? just(e.data) : nothing;
      };
    };
  };
}

export function getTimeStamp_(nothing) {
  return function (just) {
    return function (e) {
      return function () {
        return e.timeStamp ? just(e.timeStamp) : nothing;
      };
    };
  };
}

export function toTargetMap(midiAccess) {
  return function () {
    var o = {};
    var a = Array.from(midiAccess.inputs);
    for (var i = 0; i < a.length; i++) {
      o[a[i][0]] = a[i][1];
    }
    return o;
  };
}

export function toMIDIEvent_(NoteOff) {
  return function (NoteOn) {
    return function (Polytouch) {
      return function (ControlChange) {
        return function (ProgramChange) {
          return function (Aftertouch) {
            return function (Pitchwheel) {
              return function (Nothing) {
                return function (Just) {
                  return function (a) {
                    return function () {
                      return a[0] >= 128 && a[0] <= 143
                        ? Just(NoteOff(a[0] - 128)(a[1])(a[2]))
                        : a[0] >= 144 && a[0] <= 159
                        ? Just(NoteOn(a[0] - 144)(a[1])(a[2]))
                        : a[0] >= 160 && a[0] <= 175
                        ? Just(Polytouch(a[0] - 160)(a[1])(a[2]))
                        : a[0] >= 176 && a[0] <= 191
                        ? Just(ControlChange(a[0] - 176)(a[1])(a[2]))
                        : a[0] >= 192 && a[0] <= 207
                        ? Just(ProgramChange(a[0] - 192)(a[1]))
                        : a[0] >= 208 && a[0] <= 223
                        ? Just(Aftertouch(a[0] - 208)(a[1]))
                        : a[0] >= 224 && a[0] <= 239
                        ? Just(Pitchwheel(a[0] - 224)(a[1]))
                        : Nothing;
                    };
                  };
                };
              };
            };
          };
        };
      };
    };
  };
}
