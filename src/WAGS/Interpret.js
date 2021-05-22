exports.context = function () {
  return new (window.AudioContext || window.webkitAudioContext)();
};
exports.makeUnitCache = function () {
  return {};
};
exports.close = function (audioCtx) {
  return function () {
    audioCtx.close();
  };
};
var genericStarter = function (unit, name, param) {
  unit[name].value = param.param;
};
var genericSetter = function (unit, name, timeToSet, param) {
  if (param.transition === "Immediately") {
    if (param.cancel) {
      unit[name].cancelScheduledValues();
    } else {
      unit[name].value = param.param;
    }
  } else {
    if (param.cancel) {
      unit[name].cancelScheduledValues(timeToSet + param.timeOffset);
    } else {
      unit[name][
        param.transition === "NoRamp"
          ? "setValueAtTime"
          : param.transition === "LinearRamp"
          ? "linearRampToValueAtTime"
          : param.transition === "ExponentialRamp"
          ? "exponentialRampToValueAtTime"
          : "linearRampToValueAtTime"
      ](param.param, timeToSet + param.timeOffset);
    }
  }
};
var connectXToY = function (x) {
  return function (y) {
    return function (state) {
      return function () {
        state.units[x].main.connect(state.units[y].main);
        state.units[x].outgoing.push(y);
        state.units[y].incoming.push(x);
        if (state.units[y].se) {
          state.units[x].main.connect(state.units[y].se);
        }
      };
    };
  };
};
exports.connectXToY_ = connectXToY;
var disconnectXFromY = function (x) {
  return function (y) {
    return function (state) {
      return function () {
        state.units[x].main.disconnect(state.units[y].main);
        state.units[x].outgoing = state.units[x].outgoing.filter(function (i) {
          i !== y;
        });
        state.units[y].incoming = state.units[y].incoming.filter(function (i) {
          i !== x;
        });
        if (state.units[y].se) {
          state.units[x].main.disconnect(state.units[y].se);
        }
      };
    };
  };
};
exports.disconnectXFromY_ = disconnectXFromY;
exports.destroyUnit_ = function (ptr) {
  return function (state) {
    return function () {
      // hack for recorder
      if (state.units[ptr].recorder) {
        state.units[ptr].recorder.stop();
      }
      delete state.units[ptr];
    };
  };
};
exports.rebaseAllUnits_ = function (toRebase) {
  return function (state) {
    return function () {
      var newCache = {};
      for (var i = 0; i < toRebase.length; i++) {
        var trb = toRebase[i];
        newCache[trb.to] = state.units[trb.from];
      }
      var propsO = Object.getOwnPropertyNames(state.units);
      for (var i = 0; i < propsO.length; i++) {
        delete state.units[propsO[i]];
      }
      var propsN = Object.getOwnPropertyNames(newCache);
      for (var i = 0; i < propsN.length; i++) {
        state.units[propsN[i]] = newCache[propsN[i]];
      }
    };
  };
};
exports.renderAudio = function (ffiAudio) {
  return function (arrayToApply) {
    return function () {
      for (var i = 0; i < arrayToApply.length; i++) {
        arrayToApply[i](ffiAudio)();
      }
    };
  };
};
exports.getAudioClockTime = function (ctx) {
  return function () {
    return ctx.currentTime;
  };
};
exports.makeAllpass_ = function (ptr) {
  return function (a) {
    return function (b) {
      return function (state) {
        return function () {
          state.units[ptr] = {
            outgoing: [],
            incoming: [],
            main: state.context.createBiquadFilter(),
          };
          state.units[ptr].main.type = "allpass";
          genericStarter(state.units[ptr].main, "frequency", a);
          genericStarter(state.units[ptr].main, "Q", b);
        };
      };
    };
  };
};
exports.makeBandpass_ = function (ptr) {
  return function (a) {
    return function (b) {
      return function (state) {
        return function () {
          state.units[ptr] = {
            outgoing: [],
            incoming: [],
            main: state.context.createBiquadFilter(),
          };
          state.units[ptr].main.type = "bandpass";
          genericStarter(state.units[ptr].main, "frequency", a);
          genericStarter(state.units[ptr].main, "Q", b);
        };
      };
    };
  };
};
exports.makeConstant_ = function (ptr) {
  return function (onOff) {
    return function (a) {
      return function (state) {
        return function () {
          var createFunction = function () {
            var unit = state.context.createConstantSource();
            return unit;
          };
          state.units[ptr] = {
            outgoing: [],
            incoming: [],
            resumeClosure: {
              offset: function (i) {
                genericStarter(i, "offset", a);
              },
            },
            createFunction: createFunction,
            main: createFunction(),
          };
          genericStarter(state.units[ptr].main, "offset", a);
          if (onOff) {
            state.units[ptr].main.start();
          }
        };
      };
    };
  };
};
exports.makeConvolver_ = function (ptr) {
  return function (a) {
    return function (state) {
      return function () {
        state.units[ptr] = {
          outgoing: [],
          incoming: [],
          main: state.context.createConvolver(),
        };
        state.units[ptr].main.buffer = state.buffers[a];
      };
    };
  };
};
exports.makeDelay_ = function (ptr) {
  return function (a) {
    return function (state) {
      return function () {
        state.units[ptr] = {
          outgoing: [],
          incoming: [],
          main: state.context.createDelay(),
        };
        genericStarter(state.units[ptr].main, "delayTime", a);
      };
    };
  };
};
exports.makeDynamicsCompressor_ = function (ptr) {
  return function (a) {
    return function (b) {
      return function (c) {
        return function (d) {
          return function (e) {
            return function (state) {
              return function () {
                state.units[ptr] = {
                  main: state.context.createDynamicsCompressor(),
                  outgoing: [],
                  incoming: [],
                };
                genericStarter(state.units[ptr].main, "threshold", a);
                genericStarter(state.units[ptr].main, "knee", b);
                genericStarter(state.units[ptr].main, "ratio", c);
                genericStarter(state.units[ptr].main, "attack", d);
                genericStarter(state.units[ptr].main, "release", e);
              };
            };
          };
        };
      };
    };
  };
};
exports.makeGain_ = function (ptr) {
  return function (a) {
    return function (state) {
      return function () {
        state.units[ptr] = {
          outgoing: [],
          incoming: [],
          main: state.context.createGain(),
        };
        genericStarter(state.units[ptr].main, "gain", a);
      };
    };
  };
};
exports.makeHighpass_ = function (ptr) {
  return function (a) {
    return function (b) {
      return function (state) {
        return function () {
          state.units[ptr] = {
            outgoing: [],
            incoming: [],
            main: state.context.createBiquadFilter(),
          };
          state.units[ptr].main.type = "highpass";
          genericStarter(state.units[ptr].main, "frequency", a);
          genericStarter(state.units[ptr].main, "Q", b);
        };
      };
    };
  };
};
exports.makeHighshelf_ = function (ptr) {
  return function (a) {
    return function (b) {
      return function (state) {
        return function () {
          state.units[ptr] = {
            outgoing: [],
            incoming: [],
            main: state.context.createBiquadFilter(),
          };
          state.units[ptr].main.type = "highshelf";
          genericStarter(state.units[ptr].main, "frequency", a);
          genericStarter(state.units[ptr].main, "gain", b);
        };
      };
    };
  };
};
exports.makeLoopBufWithDeferredBuffer_ = function (ptr) {
  return function (state) {
    return function () {
      var createFunction = function () {
        var unit = state.context.createBufferSource();
        unit.loop = true;
        return unit;
      };
      state.units[ptr] = {
        outgoing: [],
        incoming: [],
        createFunction: createFunction,
        resumeClosure: {},
        main: createFunction(),
      };
    };
  };
};
exports.makeLoopBuf_ = function (ptr) {
  return function (a) {
    return function (onOff) {
      return function (b) {
        return function (c) {
          return function (d) {
            return function (state) {
              return function () {
                var createFunction = function () {
                  var unit = state.context.createBufferSource();
                  unit.loop = true;
                  return unit;
                };
                state.units[ptr] = {
                  outgoing: [],
                  incoming: [],
                  buffer: a,
                  createFunction: createFunction,
                  resumeClosure: {
                    playbackRate: function (i) {
                      genericStarter(i, "playbackRate", d);
                    },
                  },
                  main: createFunction(),
                };
                genericStarter(state.units[ptr].main, "playbackRate", d);
                if (onOff) {
                  state.units[ptr].main.buffer = state.buffers[a];
                  state.units[ptr].main.start(
                    state.writeHead + b.timeOffset,
                    c
                  );
                }
              };
            };
          };
        };
      };
    };
  };
};
exports.makeLowpass_ = function (ptr) {
  return function (a) {
    return function (b) {
      return function (state) {
        return function () {
          state.units[ptr] = {
            outgoing: [],
            incoming: [],
            main: state.context.createBiquadFilter(),
          };
          state.units[ptr].main.type = "lowpass";
          genericStarter(state.units[ptr].main, "frequency", a);
          genericStarter(state.units[ptr].main, "Q", b);
        };
      };
    };
  };
};
exports.makeLowshelf_ = function (ptr) {
  return function (a) {
    return function (b) {
      return function (state) {
        return function () {
          state.units[ptr] = {
            outgoing: [],
            incoming: [],
            main: state.context.createBiquadFilter(),
          };
          state.units[ptr].main.type = "lowshelf";
          genericStarter(state.units[ptr].main, "frequency", a);
          genericStarter(state.units[ptr].main, "gain", b);
        };
      };
    };
  };
};
exports.makeMicrophone_ = function (state) {
  return function () {
    if (state.microphone === null) {
      throw "Trying to use a microphone when no microphone is available.";
    }
    state.units["microphone"] = {
      main: state.context.createMediaStreamSource(state.microphone),
      outgoing: [],
      incoming: [],
    };
  };
};

exports.makeNotch_ = function (ptr) {
  return function (a) {
    return function (b) {
      return function (state) {
        return function () {
          state.units[ptr] = {
            outgoing: [],
            incoming: [],
            main: state.context.createBiquadFilter(),
          };
          state.units[ptr].main.type = "notch";
          genericStarter(state.units[ptr].main, "frequency", a);
          genericStarter(state.units[ptr].main, "Q", b);
        };
      };
    };
  };
};
exports.makePeaking_ = function (ptr) {
  return function (a) {
    return function (b) {
      return function (c) {
        return function (state) {
          return function () {
            state.units[ptr] = {
              outgoing: [],
              incoming: [],
              main: state.context.createBiquadFilter(),
            };
            state.units[ptr].main.type = "peaking";
            genericStarter(state.units[ptr].main, "frequency", a);
            genericStarter(state.units[ptr].main, "Q", b);
            genericStarter(state.units[ptr].main, "gain", c);
          };
        };
      };
    };
  };
};
exports.makePeriodicOscWithDeferredOsc_ = function (ptr) {
  return function (state) {
    return function () {
      var createFunction = function () {
        var unit = state.context.createOscillator();
        return unit;
      };
      state.units[ptr] = {
        outgoing: [],
        incoming: [],
        createFunction: createFunction,
        resumeClosure: {},
        main: createFunction(),
      };
    };
  };
};
exports.makePeriodicOsc_ = function (ptr) {
  return function (a) {
    return function (onOff) {
      return function (b) {
        return function (state) {
          return function () {
            var createFunction = function () {
              var unit = state.context.createOscillator();
              return unit;
            };
            state.units[ptr] = {
              outgoing: [],
              incoming: [],
              createFunction: createFunction,
              resumeClosure: {
                frequency: function (i) {
                  genericStarter(i, "frequency", b);
                },
                periodicOsc: function (i) {
                  i.setPeriodicWave(state.periodicWaves[a]);
                },
              },
              main: createFunction(),
            };
            genericStarter(state.units[ptr].main, "frequency", b);
            if (onOff) {
              state.units[ptr].main.setPeriodicWave(state.periodicWaves[a]);
              state.units[ptr].main.start(state.writeHead + b.timeOffset);
            }
          };
        };
      };
    };
  };
};
exports.makePlayBufWithDeferredBuffer_ = function (ptr) {
  return function (state) {
    return function () {
      var createFunction = function () {
        var unit = state.context.createBufferSource();
        return unit;
      };
      state.units[ptr] = {
        outgoing: [],
        incoming: [],
        createFunction: createFunction,
        resumeClosure: {},
        main: createFunction(),
      };
    };
  };
};
exports.makePlayBuf_ = function (ptr) {
  return function (a) {
    return function (b) {
      return function (onOff) {
        return function (c) {
          return function (state) {
            return function () {
              var createFunction = function () {
                var unit = state.context.createBufferSource();
                return unit;
              };
              state.units[ptr] = {
                outgoing: [],
                incoming: [],
                buffer: a,
                bufferOffset: b,
                createFunction: createFunction,
                resumeClosure: {
                  playbackRate: function (i) {
                    genericStarter(i, "playbackRate", c);
                  },
                },
                main: createFunction(),
              };
              genericStarter(state.units[ptr].main, "playbackRate", c);
              if (onOff) {
                state.units[ptr].main.buffer = state.buffers[a];
                state.units[ptr].main.start(state.writeHead + c.timeOffset, b);
              }
            };
          };
        };
      };
    };
  };
};
exports.makeRecorder_ = function (ptr) {
  return function (a) {
    return function (state) {
      return function () {
        var mediaRecorderSideEffectFn = state.recorders[a];
        var dest = state.context.createMediaStreamDestination();
        var mediaRecorder = new MediaRecorder(dest.stream);
        mediaRecorderSideEffectFn(mediaRecorder)();
        mediaRecorder.start();
        state.units[ptr] = {
          outgoing: [],
          incoming: [],
          recorder: mediaRecorder,
          main: state.context.createGain(),
          se: dest,
        };
      };
    };
  };
};
exports.makeSawtoothOsc_ = function (ptr) {
  return function (onOff) {
    return function (a) {
      return function (state) {
        return function () {
          var createFunction = function () {
            var unit = state.context.createOscillator();
            unit.type = "sawtooth";
            return unit;
          };
          state.units[ptr] = {
            outgoing: [],
            incoming: [],
            createFunction: createFunction,
            resumeClosure: {
              frequency: function (i) {
                genericStarter(i, "frequency", a);
              },
            },
            main: createFunction(),
          };
          genericStarter(state.units[ptr].main, "frequency", a);
          if (onOff) {
            state.units[ptr].main.start(state.writeHead + a.timeOffset);
          }
        };
      };
    };
  };
};
exports.makeSinOsc_ = function (ptr) {
  return function (onOff) {
    return function (a) {
      return function (state) {
        return function () {
          var createFunction = function () {
            var unit = state.context.createOscillator();
            unit.type = "sine";
            return unit;
          };
          state.units[ptr] = {
            outgoing: [],
            incoming: [],
            createFunction: createFunction,
            resumeClosure: {
              frequency: function (i) {
                genericStarter(i, "frequency", a);
              },
            },
            main: createFunction(),
          };
          genericStarter(state.units[ptr].main, "frequency", a);
          if (onOff) {
            state.units[ptr].main.start(state.writeHead + a.timeOffset);
          }
        };
      };
    };
  };
};
exports.makeSpeaker_ = function (state) {
  return function () {
    state.units["speaker"] = {
      outgoing: [],
      incoming: [],
      main: state.context.createGain(),
      se: state.context.destination,
    };
  };
};

exports.makeSquareOsc_ = function (ptr) {
  return function (onOff) {
    return function (a) {
      return function (state) {
        return function () {
          var createFunction = function () {
            var unit = state.context.createOscillator();
            unit.type = "square";
            return unit;
          };
          state.units[ptr] = {
            outgoing: [],
            incoming: [],
            createFunction: createFunction,
            resumeClosure: {
              frequency: function (i) {
                genericStarter(i, "frequency", a);
              },
            },
            main: createFunction(),
          };
          genericStarter(state.units[ptr].main, "frequency", a);
          if (onOff) {
            state.units[ptr].main.start(state.writeHead + a.timeOffset);
          }
        };
      };
    };
  };
};
exports.makeStereoPanner_ = function (ptr) {
  return function (a) {
    return function (state) {
      return function () {
        state.units[ptr] = {
          outgoing: [],
          incoming: [],
          main: state.context.createStereoPanner(),
        };
        genericStarter(state.units[ptr].main, "pan", a);
      };
    };
  };
};
exports.makeTriangleOsc_ = function (ptr) {
  return function (onOff) {
    return function (a) {
      return function (state) {
        return function () {
          var createFunction = function () {
            var unit = state.context.createOscillator();
            unit.type = "triangle";
            return unit;
          };
          state.units[ptr] = {
            outgoing: [],
            incoming: [],
            createFunction: createFunction,
            resumeClosure: {
              frequency: function (i) {
                genericStarter(i, "frequency", a);
              },
            },
            main: createFunction(),
          };
          genericStarter(state.units[ptr].main, "frequency", a);
          if (onOff) {
            state.units[ptr].main.start(state.writeHead + a.timeOffset);
          }
        };
      };
    };
  };
};
exports.makeWaveShaper_ = function (ptr) {
  return function (a) {
    return function (b) {
      return function (state) {
        return function () {
          state.units[ptr] = {
            outgoing: [],
            incoming: [],
            main: state.context.createWaveShaper(),
          };
          state.units[ptr].main.curve = state.floatArrays[a];
          state.units[ptr].main.oversample = b;
        };
      };
    };
  };
};
exports.setBuffer_ = function (ptr) {
  return function (buffer) {
    return function (state) {
      return function () {
        state.units[ptr].resumeClosure.buffer = function (i) {
          i.buffer = buffer;
        };
      };
    };
  };
};
exports.setPeriodicOsc_ = function (ptr) {
  return function (periodicOsc) {
    return function (state) {
      return function () {
        state.units[ptr].resumeClosure.periodicOsc = function (i) {
          i.setPeriodicWave(state.periodicWaves[periodicOsc]);
        };
      };
    };
  };
};
exports.setOn_ = function (ptr) {
  return function (state) {
    return function () {
      if (state.units[ptr].periodicOsc) {
        state.units[ptr].main.setPeriodicWave(
          state.periodicWaves[state.units[ptr].periodicOsc]
        );
      }
      if (state.units[ptr].buffer) {
        state.units[ptr].main.buffer = state.buffers[state.units[ptr].buffer];
      }
      if (state.units[ptr].resumeClosure) {
        for (var key in state.units[ptr].resumeClosure) {
          if (state.units[ptr].resumeClosure.hasOwnProperty(key)) {
            state.units[ptr].resumeClosure[key](state.units[ptr].main);
          }
        }
      }
      if (state.units[ptr].bufferOffset) {
        state.units[ptr].main.start(undefined, state.units[ptr].bufferOffset);
      } else {
        state.units[ptr].main.start();
      }
    };
  };
};
exports.setOff_ = function (ptr) {
  return function (state) {
    return function () {
      state.units[ptr].main.stop();
      for (var i = 0; i < state.units[ptr].outgoing.length; i++) {
        state.units[ptr].main.disconnect(
          state.units[state.units[ptr].outgoing[i]].main
        );
        if (state.units[state.units[ptr].outgoing[i]].se) {
          state.units[ptr].main.disconnect(
            state.units[state.units[ptr].outgoing[i]].se
          );
        }
      }
      delete state.units[ptr].main;
      state.units[ptr].main = state.units[ptr].createFunction();
      for (var i = 0; i < state.units[ptr].outgoing.length; i++) {
        state.units[ptr].main.connect(
          state.units[state.units[ptr].outgoing[i]].main
        );
        if (state.units[state.units[ptr].outgoing[i]].se) {
          state.units[ptr].main.connect(
            state.units[state.units[ptr].outgoing[i]].se
          );
        }
      }
    };
  };
};
exports.setLoopStart_ = function (ptr) {
  return function (a) {
    return function (state) {
      return function () {
        state.units[ptr].main.loopStart = a;
      };
    };
  };
};
exports.setBufferOffset_ = function (ptr) {
  return function (a) {
    return function (state) {
      return function () {
        state.units[ptr].main.bufferOffset = a;
      };
    };
  };
};
exports.setLoopEnd_ = function (ptr) {
  return function (a) {
    return function (state) {
      return function () {
        state.units[ptr].main.loopEnd = a;
      };
    };
  };
};
exports.setRatio_ = function (ptr) {
  return function (a) {
    return function (state) {
      return function () {
        genericSetter(state.units[ptr].main, "ratio", state.writeHead, a);
      };
    };
  };
};
exports.setOffset_ = function (ptr) {
  return function (a) {
    return function (state) {
      return function () {
        genericSetter(state.units[ptr].main, "offset", state.writeHead, a);
        state.units[ptr].resumeClosure.offset = function (i) {
          genericStarter(i, "offset", a);
        };
      };
    };
  };
};
exports.setAttack_ = function (ptr) {
  return function (a) {
    return function (state) {
      return function () {
        genericSetter(state.units[ptr].main, "attack", state.writeHead, a);
      };
    };
  };
};
exports.setGain_ = function (ptr) {
  return function (a) {
    return function (state) {
      return function () {
        genericSetter(state.units[ptr].main, "gain", state.writeHead, a);
      };
    };
  };
};
exports.setQ_ = function (ptr) {
  return function (a) {
    return function (state) {
      return function () {
        genericSetter(state.units[ptr].main, "Q", state.writeHead, a);
      };
    };
  };
};
exports.setPan_ = function (ptr) {
  return function (a) {
    return function (state) {
      return function () {
        genericSetter(state.units[ptr].main, "pan", state.writeHead, a);
      };
    };
  };
};
exports.setThreshold_ = function (ptr) {
  return function (a) {
    return function (state) {
      return function () {
        genericSetter(state.units[ptr].main, "threshold", state.writeHead, a);
      };
    };
  };
};
exports.setRelease_ = function (ptr) {
  return function (a) {
    return function (state) {
      return function () {
        genericSetter(state.units[ptr].main, "release", state.writeHead, a);
      };
    };
  };
};
exports.setKnee_ = function (ptr) {
  return function (a) {
    return function (state) {
      return function () {
        genericSetter(state.units[ptr].main, "knee", state.writeHead, a);
      };
    };
  };
};
exports.setDelay_ = function (ptr) {
  return function (a) {
    return function (state) {
      return function () {
        genericSetter(state.units[ptr].main, "delayTime", state.writeHead, a);
      };
    };
  };
};
exports.setPlaybackRate_ = function (ptr) {
  return function (a) {
    return function (state) {
      return function () {
        genericSetter(
          state.units[ptr].main,
          "playbackRate",
          state.writeHead,
          a
        );
        state.units[ptr].resumeClosure.playbackRate = function (i) {
          genericStarter(i, "playbackRate", a);
        };
      };
    };
  };
};
exports.setFrequency_ = function (ptr) {
  return function (a) {
    return function (state) {
      return function () {
        genericSetter(state.units[ptr].main, "frequency", state.writeHead, a);
        state.units[ptr].resumeClosure.frequency = function (i) {
          genericStarter(i, "frequency", a);
        };
      };
    };
  };
};

exports.decodeAudioDataFromBase64EncodedString = function (ctx) {
  return function (s) {
    return function () {
      {
        function base64ToArrayBuffer(base64) {
          var binaryString = window.atob(base64);
          var len = binaryString.length;
          var bytes = new Uint8Array(len);
          for (var i = 0; i < len; i++) {
            bytes[i] = binaryString.charCodeAt(i);
          }
          return bytes.buffer;
        }
        return ctx.decodeAudioData(base64ToArrayBuffer(s));
      }
    };
  };
};
exports.decodeAudioDataFromUri = function (ctx) {
  return function (s) {
    return function () {
      {
        return fetch(s)
          .then(function (b) {
            return b.arrayBuffer();
          })
          .then(function (b) {
            return ctx.decodeAudioData(b);
          });
      }
    };
  };
};
exports.audioWorkletAddModule = function (ctx) {
  return function (s) {
    return function () {
      {
        return ctx.audioWorklet.addModule(s);
      }
    };
  };
};
exports.makeAudioBuffer = function (ctx) {
  return function (b) {
    return function () {
      var myArrayBuffer = ctx.createBuffer(
        b.value1.length,
        b.value1[0].length,
        b.value0
      );
      for (
        var channel = 0;
        channel < myArrayBuffer.numberOfChannels;
        channel++
      ) {
        var nowBuffering = myArrayBuffer.getChannelData(channel);
        for (var i = 0; i < myArrayBuffer.length; i++) {
          nowBuffering[i] = b.value1[channel][i];
        }
      }
      return myArrayBuffer;
    };
  };
};

exports.makePeriodicWaveImpl = function (ctx) {
  return function (real_) {
    return function (imag_) {
      return function () {
        var real = new Float32Array(real_.length);
        var imag = new Float32Array(imag_.length);
        for (var i = 0; i < real_.length; i++) {
          real[i] = real_[i];
        }
        for (var i = 0; i < imag_.length; i++) {
          imag[i] = imag_[i];
        }
        return ctx.createPeriodicWave(real, imag, {
          disableNormalization: true,
        });
      };
    };
  };
};

exports.makeFloatArray = function (fa) {
  return function () {
    var r = new Float32Array(fa.length);
    for (var i = 0; i < fa.length; i++) {
      r[i] = fa[i];
    }
    return r;
  };
};

exports.stopMediaRecorder = function (mediaRecorder) {
  return function () {
    mediaRecorder.stop();
  };
};

exports.isTypeSupported = function (mimeType) {
  return function () {
    return MediaRecorder.isTypeSupported(mimeType);
  };
};

exports.mediaRecorderToUrl = function (mimeType) {
  return function (handler) {
    return function (mediaRecorder) {
      var chunks = [];
      return function () {
        mediaRecorder.ondataavailable = function (evt) {
          chunks.push(evt.data);
        };

        mediaRecorder.onstop = function () {
          var blob = new Blob(chunks, { type: mimeType });
          handler(URL.createObjectURL(blob))();
          chunks = null;
        };
      };
    };
  };
};
exports.getBrowserMediaStreamImpl = function (audio) {
  return function (video) {
    return function () {
      return navigator.mediaDevices.getUserMedia({
        audio: audio,
        video: video,
      });
    };
  };
};
