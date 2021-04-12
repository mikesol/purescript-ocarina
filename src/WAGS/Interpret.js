/**
 * 
 * @param {data AudioParameterTransition
  = NoRamp
  | LinearRamp
  | ExponentialRamp
  | Immediately
} unit 
 * @param {*} param
 param 
 timeOffset
    , transition: show transition
    , forceSet
 */
var genericStarter = function (unit, name, param) {
  unit[name].value = param.param;
};
var genericSetter = function (unit, name, timeToSet, param) {
  if (param.transition === "Immediately") {
    unit[name].value = param.param;
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
};
exports.connectXToY_ = function (x) {
  return function (y) {
    return function (state) {
      return function () {
        state.cache[x].main.connect(state.cache[y].main);
        if (state.cache[y].se) {
          state.cache[x].main.connect(state.cache[y].se);
        }
      };
    };
  };
};
exports.disconnectXFromY_ = function (x) {
  return function (y) {
    return function (state) {
      return function () {
        state.cache[x].main.disconnect(state.cache[y].main);
        if (state.cache[y].se) {
          state.cache[x].main.disconnect(state.cache[y].se);
        }
      };
    };
  };
};
exports.destroyUnit_ = function (ptr) {
  return function (state) {
    return function () {
      delete state.cache[ptr];
    };
  };
};
exports.rebaseAllUnits_ = function (toRebase) {
  return function (state) {
    return function () {
      var newCache = {};
      for (var i = 0; i < toRebase.length; i++) {
        var trb = toRebase[i];
        newCache[trb.to] = state.cache[trb.from];
      }
      state.cache = newCache;
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
          state.cache[ptr] = { main: state.context.createBiquadFilter() };
          state.cache[ptr].main.type = "allpass";
          genericStarter(state.cache[ptr].main, "frequency", a);
          genericStarter(state.cache[ptr].main, "Q", b);
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
          state.cache[ptr] = { main: state.context.createBiquadFilter() };
          state.cache[ptr].main.type = "bandpass";
          genericStarter(state.cache[ptr].main, "frequency", a);
          genericStarter(state.cache[ptr].main, "Q", b);
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
          state.cache[ptr] = { main: state.context.createConstantSource() };
          genericStarter(state.cache[ptr].main, "offset", a);
          if (onOff) {
            state.cache[ptr].main.start();
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
        state.cache[ptr] = { main: state.context.createConvolver() };
        state.cache[ptr].main.buffer = state.buffers[a];
      };
    };
  };
};
exports.makeDelay_ = function (ptr) {
  return function (a) {
    return function (state) {
      return function () {
        state.cache[ptr] = { main: state.context.createDelay() };
        genericStarter(state.cache[ptr].main, "delayTime", a);
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
                state.cache[ptr] = {
                  main: state.context.createDynamicsCompressor(),
                };
                genericStarter(state.cache[ptr].main, "threshold", a);
                genericStarter(state.cache[ptr].main, "knee", b);
                genericStarter(state.cache[ptr].main, "ratio", c);
                genericStarter(state.cache[ptr].main, "attack", d);
                genericStarter(state.cache[ptr].main, "release", e);
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
        state.cache[ptr] = { main: state.context.createGain() };
        genericStarter(state.cache[ptr].main, "gain", a);
      };
    };
  };
};
exports.makeHighpass_ = function (ptr) {
  return function (a) {
    return function (b) {
      return function (state) {
        return function () {
          state.cache[ptr] = { main: state.context.createBiquadFilter() };
          state.cache[ptr].main.type = "highpass";
          genericStarter(state.cache[ptr].main, "frequency", a);
          genericStarter(state.cache[ptr].main, "Q", b);
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
          state.cache[ptr] = { main: state.context.createBiquadFilter() };
          state.cache[ptr].main.type = "highshelf";
          genericStarter(state.cache[ptr].main, "frequency", a);
          genericStarter(state.cache[ptr].main, "gain", b);
        };
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
                state.cache[ptr] = { main: state.context.createBufferSource() };
                state.cache[ptr].main.loop = true;
                state.cache[ptr].main.buffer = state.buffers[a];
                state.cache[ptr].main.loopStart = c;
                state.cache[ptr].main.loopEnd = d;
                genericStarter(state.cache[ptr].main, "playbackRate", b);
                if (onOff) {
                  state.cache[ptr].main.start(
                    state.timeToSet + b.timeOffset,
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
          state.cache[ptr] = { main: state.context.createBiquadFilter() };
          state.cache[ptr].main.type = "lowpass";
          genericStarter(state.cache[ptr].main, "frequency", a);
          genericStarter(state.cache[ptr].main, "Q", b);
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
          state.cache[ptr] = { main: state.context.createBiquadFilter() };
          state.cache[ptr].main.type = "lowshelf";
          genericStarter(state.cache[ptr].main, "frequency", a);
          genericStarter(state.cache[ptr].main, "gain", b);
        };
      };
    };
  };
};
exports.makeMicrophone_ = function (ptr) {
  return function (state) {
    return function () {
      state.cache[ptr] = {
        main: context.createMediaStreamSource(
          state.cache.microphones[Object.keys(state.cache.microphones)[0]]
        ),
      };
    };
  };
};

exports.makeNotch_ = function (ptr) {
  return function (a) {
    return function (b) {
      return function (state) {
        return function () {
          state.cache[ptr] = { main: state.context.createBiquadFilter() };
          state.cache[ptr].main.type = "notch";
          genericStarter(state.cache[ptr].main, "frequency", a);
          genericStarter(state.cache[ptr].main, "Q", b);
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
            state.cache[ptr] = { main: state.context.createBiquadFilter() };
            state.cache[ptr].main.type = "peaking";
            genericStarter(state.cache[ptr].main, "frequency", a);
            genericStarter(state.cache[ptr].main, "Q", b);
            genericStarter(state.cache[ptr].main, "gain", c);
          };
        };
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
            state.cache[ptr] = { main: state.context.createOscillator() };
            state.cache[ptr].main.setPeriodicWave(state.periodicWaves[a]);
            genericStarter(state.cache[ptr].main, "frequency", b);
            if (onOff) {
              state.cache[ptr].main.start(state.timeToSet + b.timeOffset);
            }
          };
        };
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
              state.cache[ptr] = { main: state.context.createBufferSource() };
              state.cache[ptr].main.buffer = state.buffers[a];
              genericStarter(state.cache[ptr].main, "playbackRate", c);
              if (onOff) {
                state.cache[ptr].main.start(state.timeToSet + c.timeOffset, b);
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
        state.recorders.concat(mediaRecorder);
        mediaRecorderSideEffectFn(mediaRecorder)();
        mediaRecorder.start();
        state.cache[ptr] = { main: state.context.createGain(), se: dest };
      };
    };
  };
};
exports.makeSawtoothOsc_ = function (ptr) {
  return function (onOff) {
    return function (a) {
      return function (state) {
        return function () {
          state.cache[ptr] = { main: state.context.createOscillator() };
          state.cache[ptr].main.type = "sawtooth";
          genericStarter(state.cache[ptr].main, "frequency", a);
          if (onOff) {
            state.cache[ptr].main.start(state.timeToSet + a.timeOffset);
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
          state.cache[ptr] = { main: state.context.createOscillator() };
          state.cache[ptr].main.type = "sine";
          genericStarter(state.cache[ptr].main, "frequency", a);
          if (onOff) {
            state.cache[ptr].main.start(state.timeToSet + a.timeOffset);
          }
        };
      };
    };
  };
};
exports.makeSpeaker_ = function (ptr) {
  return function (state) {
    return function () {
      state.cache[ptr] = {
        main: state.context.createGain(),
        se: context.destination,
      };
    };
  };
};

exports.makeSquareOsc_ = function (ptr) {
  return function (onOff) {
    return function (a) {
      return function (state) {
        return function () {
          state.cache[ptr] = { main: state.context.createOscillator() };
          state.cache[ptr].main.type = "square";
          genericStarter(state.cache[ptr].main, "frequency", a);
          if (onOff) {
            state.cache[ptr].main.start(state.timeToSet + a.timeOffset);
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
        state.cache[ptr] = { main: state.context.createDelay() };
        genericStarter(state.cache[ptr].main, "pan", a);
      };
    };
  };
};
exports.makeTriangleOsc_ = function (ptr) {
  return function (onOff) {
    return function (a) {
      return function (state) {
        return function () {
          state.cache[ptr] = { main: state.context.createOscillator() };
          state.cache[ptr].main.type = "triangle";
          genericStarter(state.cache[ptr].main, "frequency", a);
          if (onOff) {
            state.cache[ptr].main.start(state.timeToSet + a.timeOffset);
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
          state.cache[ptr] = { main: state.context.createWaveShaper() };
          state.cache[ptr].main.curve = state.floatArrays[a];
          state.cache[ptr].main.oversample = b;
        };
      };
    };
  };
};
exports.setOn_ = function (ptr) {
  return function (state) {
    return function () {
      state.cache[ptr].main.start();
    };
  };
};
exports.setOff_ = function (ptr) {
  return function (state) {
    return function () {
      state.cache[ptr].main.stop();
    };
  };
};
exports.setLoopStart_ = function (ptr) {
  return function (a) {
    return function (state) {
      return function () {
        state.cache[ptr].main.loopStart = a;
      };
    };
  };
};
exports.setLoopEnd_ = function (ptr) {
  return function (a) {
    return function (state) {
      return function () {
        state.cache[ptr].main.loopEnd = a;
      };
    };
  };
};
exports.setRatio_ = function (ptr) {
  return function (a) {
    return function (state) {
      return function () {
        genericSetter(state.cache[ptr].main, "ratio", state.timeOffset, a);
      };
    };
  };
};
exports.setOffset_ = function (ptr) {
  return function (a) {
    return function (state) {
      return function () {
        genericSetter(state.cache[ptr].main, "offset", state.timeOffset, a);
      };
    };
  };
};
exports.setAttack_ = function (ptr) {
  return function (a) {
    return function (state) {
      return function () {
        genericSetter(state.cache[ptr].main, "attack", state.timeOffset, a);
      };
    };
  };
};
exports.setGain_ = function (ptr) {
  return function (a) {
    return function (state) {
      return function () {
        genericSetter(state.cache[ptr].main, "gain", state.timeOffset, a);
      };
    };
  };
};
exports.setQ_ = function (ptr) {
  return function (a) {
    return function (state) {
      return function () {
        genericSetter(state.cache[ptr].main, "Q", state.timeOffset, a);
      };
    };
  };
};
exports.setPan_ = function (ptr) {
  return function (a) {
    return function (state) {
      return function () {
        genericSetter(state.cache[ptr].main, "pan", state.timeOffset, a);
      };
    };
  };
};
exports.setThreshold_ = function (ptr) {
  return function (a) {
    return function (state) {
      return function () {
        genericSetter(state.cache[ptr].main, "threshold", state.timeOffset, a);
      };
    };
  };
};
exports.setRelease_ = function (ptr) {
  return function (a) {
    return function (state) {
      return function () {
        genericSetter(state.cache[ptr].main, "release", state.timeOffset, a);
      };
    };
  };
};
exports.setKnee_ = function (ptr) {
  return function (a) {
    return function (state) {
      return function () {
        genericSetter(state.cache[ptr].main, "knee", state.timeOffset, a);
      };
    };
  };
};
exports.setDelay_ = function (ptr) {
  return function (a) {
    return function (state) {
      return function () {
        genericSetter(state.cache[ptr].main, "delay", state.timeOffset, a);
      };
    };
  };
};
exports.setPlaybackRate_ = function (ptr) {
  return function (a) {
    return function (state) {
      return function () {
        genericSetter(
          state.cache[ptr].main,
          "playbackRate",
          state.timeOffset,
          a
        );
      };
    };
  };
};
exports.setFrequency_ = function (ptr) {
  return function (a) {
    return function (state) {
      return function () {
        genericSetter(state.cache[ptr].main, "frequency", state.timeOffset, a);
      };
    };
  };
};

exports.makeAudioContext = function () {
  return new (window.AudioContext || window.webkitAudioContext)();
};

exports.makeAudioTrack = function (s) {
  return function () {
    var o = new Audio(s);
    o.crossOrigin = "anonymous";
    return o;
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
