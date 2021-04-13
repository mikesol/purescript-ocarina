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
        state.units[x].main.connect(state.units[y].main);
        if (state.units[y].se) {
          state.units[x].main.connect(state.units[y].se);
        }
      };
    };
  };
};
exports.disconnectXFromY_ = function (x) {
  return function (y) {
    return function (state) {
      return function () {
        state.units[x].main.disconnect(state.units[y].main);
        if (state.units[y].se) {
          state.units[x].main.disconnect(state.units[y].se);
        }
      };
    };
  };
};
exports.destroyUnit_ = function (ptr) {
  return function (state) {
    return function () {
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
      state.units = newCache;
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
          state.units[ptr] = { main: state.context.createBiquadFilter() };
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
          state.units[ptr] = { main: state.context.createBiquadFilter() };
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
          state.units[ptr] = { main: state.context.createConstantSource() };
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
        state.units[ptr] = { main: state.context.createConvolver() };
        state.units[ptr].main.buffer = state.buffers[a];
      };
    };
  };
};
exports.makeDelay_ = function (ptr) {
  return function (a) {
    return function (state) {
      return function () {
        state.units[ptr] = { main: state.context.createDelay() };
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
        state.units[ptr] = { main: state.context.createGain() };
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
          state.units[ptr] = { main: state.context.createBiquadFilter() };
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
          state.units[ptr] = { main: state.context.createBiquadFilter() };
          state.units[ptr].main.type = "highshelf";
          genericStarter(state.units[ptr].main, "frequency", a);
          genericStarter(state.units[ptr].main, "gain", b);
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
                state.units[ptr] = { main: state.context.createBufferSource() };
                state.units[ptr].main.loop = true;
                state.units[ptr].main.buffer = state.buffers[a];
                state.units[ptr].main.loopStart = c;
                state.units[ptr].main.loopEnd = d;
                genericStarter(state.units[ptr].main, "playbackRate", b);
                if (onOff) {
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
          state.units[ptr] = { main: state.context.createBiquadFilter() };
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
          state.units[ptr] = { main: state.context.createBiquadFilter() };
          state.units[ptr].main.type = "lowshelf";
          genericStarter(state.units[ptr].main, "frequency", a);
          genericStarter(state.units[ptr].main, "gain", b);
        };
      };
    };
  };
};
exports.makeMicrophone_ = function (ptr) {
  return function (state) {
    return function () {
      state.units[ptr] = {
        main: state.context.createMediaStreamSource(
          state.units.microphones[Object.keys(state.units.microphones)[0]]
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
          state.units[ptr] = { main: state.context.createBiquadFilter() };
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
            state.units[ptr] = { main: state.context.createBiquadFilter() };
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
exports.makePeriodicOsc_ = function (ptr) {
  return function (a) {
    return function (onOff) {
      return function (b) {
        return function (state) {
          return function () {
            state.units[ptr] = { main: state.context.createOscillator() };
            state.units[ptr].main.setPeriodicWave(state.periodicWaves[a]);
            genericStarter(state.units[ptr].main, "frequency", b);
            if (onOff) {
              state.units[ptr].main.start(state.writeHead + b.timeOffset);
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
              state.units[ptr] = { main: state.context.createBufferSource() };
              state.units[ptr].main.buffer = state.buffers[a];
              genericStarter(state.units[ptr].main, "playbackRate", c);
              if (onOff) {
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
        state.recorders.concat(mediaRecorder);
        mediaRecorderSideEffectFn(mediaRecorder)();
        mediaRecorder.start();
        state.units[ptr] = { main: state.context.createGain(), se: dest };
      };
    };
  };
};
exports.makeSawtoothOsc_ = function (ptr) {
  return function (onOff) {
    return function (a) {
      return function (state) {
        return function () {
          state.units[ptr] = { main: state.context.createOscillator() };
          state.units[ptr].main.type = "sawtooth";
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
          state.units[ptr] = { main: state.context.createOscillator() };
          state.units[ptr].main.type = "sine";
          genericStarter(state.units[ptr].main, "frequency", a);
          if (onOff) {
            state.units[ptr].main.start(state.writeHead + a.timeOffset);
          }
        };
      };
    };
  };
};
exports.makeSpeaker_ = function (ptr) {
  return function (state) {
    return function () {
      state.units[ptr] = {
        main: state.context.createGain(),
        se: state.context.destination,
      };
    };
  };
};

exports.makeSquareOsc_ = function (ptr) {
  return function (onOff) {
    return function (a) {
      return function (state) {
        return function () {
          state.units[ptr] = { main: state.context.createOscillator() };
          state.units[ptr].main.type = "square";
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
        state.units[ptr] = { main: state.context.createDelay() };
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
          state.units[ptr] = { main: state.context.createOscillator() };
          state.units[ptr].main.type = "triangle";
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
          state.units[ptr] = { main: state.context.createWaveShaper() };
          state.units[ptr].main.curve = state.floatArrays[a];
          state.units[ptr].main.oversample = b;
        };
      };
    };
  };
};
exports.setOn_ = function (ptr) {
  return function (state) {
    return function () {
      state.units[ptr].main.start();
    };
  };
};
exports.setOff_ = function (ptr) {
  return function (state) {
    return function () {
      state.units[ptr].main.stop();
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
        genericSetter(state.units[ptr].main, "delay", state.writeHead, a);
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
      };
    };
  };
};
exports.setFrequency_ = function (ptr) {
  return function (a) {
    return function (state) {
      return function () {
        genericSetter(state.units[ptr].main, "frequency", state.writeHead, a);
      };
    };
  };
};

exports.makeAudioContext = function () {
  return new (window.AudioContext || window.webkitAudioContext)();
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
