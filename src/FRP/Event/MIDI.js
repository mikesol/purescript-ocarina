exports.midiAccess = function () {
  return navigator.requestMIDIAccess();
};

exports.getData_ = function (nothing) {
  return function (just) {
    return function (e) {
      return function () {
        return e.data ? just(e.data) : nothing;
      };
    };
  };
};

exports.getTimeStamp_ = function (nothing) {
  return function (just) {
    return function (e) {
      return function () {
        return e.timeStamp ? just(e.timeStamp) : nothing;
      };
    };
  };
};

exports.toTargetMap = function (midiAccess) {
  return function () {
    var o = {};
    var a = Array.from(midiAccess.inputs);
    for (var i = 0; i < a.length; i++) {
      o[a[i][0]] = a[i][1];
    }
    return o;
  };
};
exports.toMIDIEvent_ = function (NoteOff) {
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
};
