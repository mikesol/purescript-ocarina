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
// todo: merge two setters?
var workletSetter = function (unit, paramName, timeToSet, param) {
  if (param.transition === "Immediately") {
    if (param.cancel) {
      unit.parameters.get(paramName).cancelScheduledValues();
    } else {
      unit.parameters.get(paramName).value = param.param;
    }
  } else {
    if (param.cancel) {
      unit.parameters.get(paramName).cancelScheduledValues(timeToSet + param.timeOffset);
    } else {
      unit.parameters.get(paramName)[
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
var connectXToY = function (calledExternally) {
  return function (x) {
    return function (y) {
      return function (stateX) {
        return function (stateY) {
          return function () {
            if (calledExternally && (stateY.units[y].isSubgraph || stateY.units[y].isTumult)) { return; }
            stateX.units[x].main.connect(stateY.units[y].main);
            stateX.units[x].outgoing.push({ unit: y, state: stateY });
            stateY.units[y].incoming.push({ unit: x, state: stateX });
            if (stateY.units[y].se) {
              stateX.units[x].main.connect(stateY.units[y].se);
            }
          };
        };
      }
    }
  };
};
exports.connectXToY_ = function (x) {
  return function (y) {
    return function (state) {
      return connectXToY(true)(x)(y)(state)(state);
    }
  }
};
var disconnectXFromY = function (calledExternally) {
  return function (x) {
    return function (y) {
      return function (stateX) {
        return function (stateY) {
          return function () {
            if (calledExternally && (stateY.units[y].isSubgraph || stateY.units[y].isTumult)) { return; }
            stateX.units[x].main.disconnect(stateY.units[y].main);
            stateX.units[x].outgoing = stateX.units[x].outgoing.filter(function (i) {
              !(i.unit === y && i.state === stateY);
            });
            stateY.units[y].incoming = stateY.units[y].incoming.filter(function (i) {
              !(i.unit === x && i.state === stateX);
            });
            if (stateY.units[y].se) {
              stateX.units[x].main.disconnect(stateY.units[y].se);
            }
          };
        }
      };
    }
  };
};
exports.disconnectXFromY_ = function (x) {
  return function (y) {
    return function (state) {
      return disconnectXFromY(true)(x)(y)(state)(state);
    }
  }
};
exports.destroyUnit_ = function (ptr) {
  return function (state) {
    return function () {
      // hack for recorder
      if (state.units[ptr].recorder) {
        state.units[ptr].recorder.stop();
      }
      // hack for analyser
      if (state.units[ptr].analyser) {
        // effectful unsubscribe
        state.units[ptr].analyser();
      }
      if (state.units[ptr].input) {
        // input sources are never disconnected on disconnect, so we have to manually trigger this
        disconnectXFromY(false)(state.units[ptr].input)(ptr)(state.units[ptr].parent)(state)();
      }
      delete state.units[ptr];
    };
  };
};
exports.renderAudio = function (arrayToApply) {
  return function () {
    for (var i = 0; i < arrayToApply.length; i++) {
      arrayToApply[i]();
    }
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
exports.makeAnalyser_ = function (ptr) {
  return function (a) {
    return function (state) {
      return function () {
        var analyserSideEffectFunction = a;
        var dest = state.context.createAnalyser();
        // todo - unhardcode?
        dest.fftSize = 2048;
        // unsubscribe is effect unit
        var unsubscribe = analyserSideEffectFunction(dest)();
        state.units[ptr] = {
          outgoing: [],
          incoming: [],
          analyserOrig: analyserSideEffectFunction,
          analyser: unsubscribe,
          main: state.context.createGain(),
          se: dest,
        };
      };
    };
  };
};
exports.setAnalyserNodeCb_ = function (ptr) {
  return function (a) {
    return function (state) {
      return function () {
        if (state.units[ptr].analyserOrig === a) { return; }
        // first, unsubscribe
        state.units[ptr].analyser && state.units[ptr].analyser();
        state.units[ptr].analyser = a(state.units[ptr].se)();
        state.units[ptr].analyserOrig = a;
      };
    };
  };
};
exports.makeAudioWorkletNode_ = function (ptr) {
  return function (a) {
    return function (state) {
      return function () {
        var parameterData = {}
        var keys = Object.keys(a.parameterData);
        for (var i = 0; i < keys.length; i++) {
          if (a.parameterData[keys[i]].isJust) {
            parameterData[keys[i]] = a.parameterData[keys[i]].param
          }
        }
        state.units[ptr] = {
          outgoing: [],
          incoming: [],
          main: new AudioWorkletNode(state.context, a.name, {
            numberOfInputs: a.numberOfInputs
            , numberOfOutputs: a.numberOfOutputs
            , outputChannelCount: a.outputChannelCount
            , parameterData: parameterData
            , processorOptions: a.processorOptions
          }),
        };
      };
    };
  }
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
          applyResumeClosure(state.units[ptr]);
          if (onOff.param === "on") {
            state.units[ptr].main.start(state.writeHead + onOff.timeOffset);
          }
          state.units[ptr].onOff = onOff.param === "on";
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
        state.units[ptr].main.buffer = a;
      };
    };
  };
};
exports.makePassthroughConvolver_ = function (ptr) {
  return function (state) {
    return function () {
      state.units[ptr] = {
        outgoing: [],
        incoming: [],
        main: state.context.createConvolver(),
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
                  createFunction: createFunction,
                  resumeClosure: {
                    playbackRate: function (i) {
                      genericStarter(i, "playbackRate", b);
                    },
                    loopStart: function (i) {
                      i.loopStart = c;
                    },
                    loopEnd: function (i) {
                      i.loopEnd = d;
                    },
                    buffer: function (i) {
                      i.buffer = a
                    },
                  },
                  main: createFunction(),
                };
                if (onOff.param === "on") {
                  applyResumeClosure(state.units[ptr]);
                  state.units[ptr].main.start(state.writeHead + onOff.timeOffset, c);
                }
                state.units[ptr].onOff = onOff.param === "on";
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
exports.makeMicrophone_ = function (microphone) {
  return function (state) {
    return function () {
      state.units["microphone"] = {
        main: state.context.createMediaStreamSource(microphone),
        outgoing: [],
        incoming: [],
      };
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
                  i.setPeriodicWave(a);
                },
              },
              main: createFunction(),
            };
            applyResumeClosure(state.units[ptr]);
            if (onOff.param === "on") {
              state.units[ptr].main.start(state.writeHead + onOff.timeOffset);
            }
            state.units[ptr].onOff = onOff.param === "on";
          };
        };
      };
    };
  };
};
exports.makePeriodicOscV_ = function (ptr) {
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
                  i.setPeriodicWave(
                    makePeriodicWaveImpl(state.context)(a[0])(a[1])()
                  );
                },
              },
              main: createFunction(),
            };
            applyResumeClosure(state.units[ptr]);
            if (onOff.param === "on") {
              state.units[ptr].main.start(state.writeHead + onOff.timeOffset);
            }
            state.units[ptr].onOff = onOff.param === "on";
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
exports.makeInput_ = function (ptr) {
  return function (a) {
    return function (state) {
      return function () {
        state.units[ptr] = {
          outgoing: [],
          incoming: [],
          main: state.context.createGain(),
          parent: state.parent,
          input: a,
        };
        connectXToY(false)(a)(ptr)(state.parent)(state)();
        state.units[ptr].main.gain = 1.0;
      };
    };
  }
};
exports.makeTumultWithDeferredGraph_ = function (ptr) {
  return function (state) {
    return function () {
      state.units[ptr] = {
        outgoing: [],
        incoming: [],
        main: state.context.createGain(),
        isTumult: true
      };
    };
  };
};
exports.makeSubgraphWithDeferredScene_ = function (ptr) {
  return function (state) {
    return function () {
      state.units[ptr] = {
        outgoing: [],
        incoming: [],
        main: state.context.createGain(),
        isSubgraph: true
      };
    };
  };
};
exports.makeSubgraph_ = function (ptr) {
  return function (terminalPtr) {
    return function (vek) {
      return function (sceneM) {
        return function (envM) {
          return function (funk) {
            return function (state) {
              return function () {
                var children = [];
                var scenes = [];
                for (var i = 0; i < vek.length; i++) {
                  children[i] = {
                    context: state.context
                    , writeHead: state.writeHead
                    , units: {}
                    , parent: state
                  }
                  scenes[i] = sceneM(i)(vek[i]);
                }
                state.units[ptr] = {
                  outgoing: [],
                  incoming: [],
                  main: state.context.createGain(),
                  children: children,
                  isSubgraph: true,
                  scenes: scenes
                };
                state.units[ptr].main.gain = 1.0;
                for (var i = 0; i < scenes.length; i++) {
                  var applied = funk(envM(i)(vek[i]))(scenes[i]);
                  for (var j = 0; j < applied.instructions.length; j++) {
                    // thunk
                    applied.instructions[j](children[i])();
                  }
                  scenes[i] = applied.nextScene;
                }
                for (var i = 0; i < children.length; i++) {
                  connectXToY(false)(terminalPtr)(ptr)(children[i])(state)();
                }
              };
            };
          };
        };
      };
    }
  }
};
/**
 * 
 * String
  -> String
  -> Array (Set Instruction)
  -> Maybe (Set Instruction)
  -> (Set Instruction -> Maybe (Set Instruction))
  -> (Set Instruction -> Maybe (Set Instruction) -> Array (FFIAudioSnapshot' -> Effect Unit))
  -> FFIAudioSnapshot'
  -> Effect Unit
 */
exports.makeTumult_ = function (ptr) {
  return function (terminalPtr) {
    return function (scenes) {
      return function (nothing) {
        return function () {
          return function (arrMaker) {
            return function (state) {
              return function () {
                var children = [];
                for (var i = 0; i < scenes.length; i++) {
                  children[i] = {
                    context: state.context
                    , writeHead: state.writeHead
                    , units: {}
                    , parent: state
                  }
                }
                state.units[ptr] = {
                  outgoing: [],
                  incoming: [],
                  main: state.context.createGain(),
                  children: children,
                  isTumult: true,
                  scenes: scenes
                };
                state.units[ptr].main.gain = 1.0;
                for (var i = 0; i < scenes.length; i++) {
                  var curScene = arrMaker(scenes[i])(nothing);
                  for (var j = 0; j < curScene.length; j++) {
                    // thunk
                    curScene[j](children[i])();
                  }
                }
                var heads = [];
                for (var i = 0; i < children.length; i++) {
                  heads[i] = children[i].units[terminalPtr];
                }
                state.units[ptr].heads = heads;
                for (var i = 0; i < children.length; i++) {
                  connectXToY(false)(terminalPtr)(ptr)(children[i])(state)();
                }
              }
            };
          };
        };
      };
    }
  }
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
                bufferOffset: b,
                createFunction: createFunction,
                resumeClosure: {
                  playbackRate: function (i) {
                    genericStarter(i, "playbackRate", c);
                  },
                  buffer: function (i) {
                    i.buffer = a;
                  },
                },
                main: createFunction(),
              };
              if (onOff.param === "on") {
                applyResumeClosure(state.units[ptr]);
                state.units[ptr].main.start(state.writeHead + onOff.timeOffset, b);
              }
              state.units[ptr].onOff = onOff.param === "on";
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
        var mediaRecorderSideEffectFn = a;
        var dest = state.context.createMediaStreamDestination();
        var mediaRecorder = new MediaRecorder(dest.stream);
        mediaRecorderSideEffectFn(mediaRecorder)();
        mediaRecorder.start();
        state.units[ptr] = {
          outgoing: [],
          incoming: [],
          recorderOrig: mediaRecorderSideEffectFn,
          recorder: mediaRecorder,
          main: state.context.createGain(),
          se: dest,
        };
      };
    };
  };
};
// setting makes us stop the previous one if it exists
exports.setMediaRecorderCb_ = function (ptr) {
  return function (a) {
    return function (state) {
      return function () {
        if (state.units[ptr].recorderOrig === a) { return; }
        state.units[ptr].recorder && state.units[ptr].recorder.stop();
        var mediaRecorderSideEffectFn = a;
        state.units[ptr].recorderOrig = a;
        var mediaRecorder = new MediaRecorder(state.units[ptr].se);
        mediaRecorderSideEffectFn(mediaRecorder)();
        mediaRecorder.start();
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
          applyResumeClosure(state.units[ptr]);
          if (onOff.param === "on") {
            state.units[ptr].main.start(state.writeHead + onOff.timeOffset);
          }
          state.units[ptr].onOff = onOff.param === "on";
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
          applyResumeClosure(state.units[ptr]);
          if (onOff.param === "on") {
            state.units[ptr].main.start(state.writeHead + onOff.timeOffset);
          }
          state.units[ptr].onOff = onOff.param === "on";
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
          applyResumeClosure(state.units[ptr]);
          if (onOff.param === "on") {
            state.units[ptr].main.start(state.writeHead + onOff.timeOffset);
          }
          state.units[ptr].onOff = onOff.param === "on";
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
          applyResumeClosure(state.units[ptr]);
          if (onOff.param === "on") {
            state.units[ptr].main.start(state.writeHead + onOff.timeOffset);
          }
          state.units[ptr].onOff = onOff.param === "on";
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
          state.units[ptr].main.curve = a;
          state.units[ptr].main.oversample = b;
        };
      };
    };
  };
};
exports.setWaveShaperCurve_ = function (ptr) {
  return function (a) {
    return function (state) {
      return function () {
        state.units[ptr].main.curve = a;
      };
    };
  };
};
exports.setBuffer_ = function (ptr) {
  return function (buffer) {
    return function (state) {
      return function () {
        state.units[ptr].resumeClosure.buffer = function (i) {
          i.buffer = buffer
        };
      };
    };
  };
};
exports.setInput_ = function (ptr) {
  return function (a) {
    return function (state) {
      return function () {
        if (state.units[ptr].input && state.units[ptr].input === a) {
          return;
        }
        if (state.units[ptr].input) {
          disconnectXFromY(false)(state.units[ptr].input, ptr, state.parent, state);
        }
        state.units[ptr].input = a;
        connectXToY(false)(a)(ptr)(state.parent)(state)();
        state.units[ptr].main.gain = 1.0;
      };
    };
  }
};
exports.setSubgraph_ = function (ptr) {
  return function (terminalPtr) {
    return function (vek) {
      return function (sceneM) {
        return function (envM) {
          return function (funk) {
            return function (state) {
              return function () {
                var needsCreation = !(state.units[ptr] && state.units[ptr].children && state.units[ptr].scenes);
                if (needsCreation) {
                  var children = [];
                  var scenes = [];
                  for (var i = 0; i < vek.length; i++) {
                    children[i] = {
                      context: state.context
                      , writeHead: state.writeHead
                      , units: {}
                      , parent: state
                    }
                    scenes[i] = sceneM(i)(vek[i]);
                  }
                  state.units[ptr].incoming = [];
                  state.units[ptr].outgoing = [];
                  state.units[ptr].children = children;
                  state.units[ptr].scenes = scenes;
                  state.units[ptr].isSubgraph = true;
                } else {
                  for (var i = 0; i < vek.length; i++) {
                    state.units[ptr].children[i].writeHead = state.writeHead;
                  }
                }
                var scenes = state.units[ptr].scenes;
                var children = state.units[ptr].children;
                for (var i = 0; i < scenes.length; i++) {
                  var applied = funk(envM(i)(vek[i]))(scenes[i]);
                  for (var j = 0; j < applied.instructions.length; j++) {
                    // thunk
                    applied.instructions[j](children[i])();
                  }
                  scenes[i] = applied.nextScene;
                }
                if (needsCreation) {
                  for (var i = 0; i < children.length; i++) {
                    connectXToY(false)(terminalPtr)(ptr)(children[i])(state)();
                  }
                }
              };
            };
          };
        };
      };
    }
  }
};
exports.setTumult_ = function (ptr) {
  return function (terminalPtr) {
    return function (scenes) {
      return function (nothing) {
        return function (just) {
          return function (arrMaker) {
            return function (state) {
              return function () {
                var needsCreation = !(state.units[ptr] && state.units[ptr].children && state.units[ptr].scenes);
                if (needsCreation) {
                  var children = [];
                  for (var i = 0; i < scenes.length; i++) {
                    children[i] = {
                      context: state.context
                      , writeHead: state.writeHead
                      , units: {}
                      , parent: state
                    }
                  }
                  state.units[ptr].incoming = [];
                  state.units[ptr].outgoing = [];
                  state.units[ptr].children = children;
                  state.units[ptr].isTumult = true;
                } else {
                  for (var i = 0; i < scenes.length; i++) {
                    state.units[ptr].children[i].writeHead = state.writeHead;
                  }
                }
                var oldScenes = state.units[ptr].scenes;
                var children = state.units[ptr].children;
                for (var i = 0; i < scenes.length; i++) {
                  var oldScene = oldScenes && oldScenes[i] ? just(oldScenes[i]) : nothing;
                  var curScene = arrMaker(scenes[i])(oldScene);
                  for (var j = 0; j < curScene.length; j++) {
                    // thunk
                    curScene[j](children[i])();
                  }
                }
                state.units[ptr].scenes = scenes;
                // todo - add this logic to subgraph
                var heads = [];
                for (var i = 0; i < children.length; i++) {
                  heads[i] = children[i].units[terminalPtr];
                  if (state.units[ptr].heads[i] !== heads[i]) {
                    var tmp = {units:{}};
                    tmp.units[terminalPtr] = state.units[ptr].heads[i];
                    disconnectXFromY(false)(terminalPtr)(ptr)(tmp)(state)();
                    connectXToY(false)(terminalPtr)(ptr)(children[i])(state)();
                  }
                }
                state.units[ptr].heads = heads;
                if (needsCreation) {
                  for (var i = 0; i < children.length; i++) {
                    connectXToY(false)(terminalPtr)(ptr)(children[i])(state)();
                  }
                }
              };
            };
          }
        }
      }
    };
  };
};
exports.setConvolverBuffer_ = function (ptr) {
  return function (buffer) {
    return function (state) {
      return function () {
        state.units[ptr].main.buffer = buffer;
      };
    };
  };
};
exports.setPeriodicOsc_ = function (ptr) {
  return function (periodicOsc) {
    return function (state) {
      return function () {
        state.units[ptr].resumeClosure.periodicOsc = function (i) {
          i.setPeriodicWave(periodicOsc);
        };
      };
    };
  };
};
exports.setPeriodicOscV_ = function (ptr) {
  return function (periodicOsc) {
    return function (state) {
      return function () {
        state.units[ptr].resumeClosure.periodicOsc = function (i) {
          i.setPeriodicWave(
            makePeriodicWaveImpl(state.context)(periodicOsc[0])(
              periodicOsc[1]
            )()
          );
        };
      };
    };
  };
};
var applyResumeClosure = function (i) {
  for (var key in i.resumeClosure) {
    if (i.resumeClosure.hasOwnProperty(key)) {
      i.resumeClosure[key](i.main);
    }
  }
};

exports.setOnOff_ = function (ptr) {
  return function (onOff) {
    return function (state) {
      return function () {
        if (onOff.param === "on") {
          setOn_(ptr)(onOff)(state)();
        } else if (onOff.param === "off") {
          setOff_(ptr)(onOff)(state)();
        } else if (onOff.param === "offOn") {
          setOff_(ptr)({ param: "off", timeOffset: 0.0 })(state)();
          setOn_(ptr)({ param: "on", timeOffset: onOff.timeOffset })(state)();
        }
      };
    };
  };
};

var setOn_ = function (ptr) {
  return function (onOffInstr) {
    return function (state) {
      return function () {
        if (state.units[ptr].onOff) {
          return;
        }
        state.units[ptr].onOff = true;
        if (state.units[ptr].resumeClosure) {
          applyResumeClosure(state.units[ptr]);
        }
        if (state.units[ptr].bufferOffset) {
          state.units[ptr].main.start(
            state.writeHead + onOffInstr.timeOffset,
            state.units[ptr].bufferOffset
          );
        } else {
          state.units[ptr].main.start(state.writeHead + onOffInstr.timeOffset);
        }
      };
    };
  };
};

var setOff_ = function (ptr) {
  return function (onOffInstr) {
    return function (state) {
      return function () {
        if (!state.units[ptr].onOff) {
          return;
        }
        state.units[ptr].onOff = false;
        var oldMain = state.units[ptr].main;
        var oldOutgoing = state.units[ptr].outgoing.slice();
        oldMain.stop(state.writeHead + onOffInstr.timeOffset);
        // defer disconnection until stop has happened
        setTimeout(() => {
          for (var i = 0; i < oldOutgoing.length; i++) {
            var oogi = oldOutgoing[i];
            try {
              oldMain.disconnect(
                oogi.state.units[oogi.unit].main
              );
              if (oogi.state.units[oogi.unit].se) {
                oldMain.disconnect(
                  oogi.state.units[oogi.unit].se
                );
              }
            } catch (e) {
              console.log(e);
              // fail silently, as it means the unit is no longer available, but
              // as we are disconnecting it doesn't matter
              continue;
            }
          }
        }, 1000.0 * (state.writeHead + onOffInstr.timeOffset + 0.2 - state.context.currentTime));
        state.units[ptr].main = state.units[ptr].createFunction();
        for (var i = 0; i < state.units[ptr].outgoing.length; i++) {
          var ogi = state.units[ptr].outgoing[i];
          state.units[ptr].main.connect(
            ogi.state.units[ogi.unit].main
          );
          if (ogi.state.units[ogi.unit].se) {
            state.units[ptr].main.connect(
              ogi.state.units[ogi.unit].se
            );
          }
        }
      };
    };
  };
};
exports.setLoopStart_ = function (ptr) {
  return function (a) {
    return function (state) {
      return function () {
        state.units[ptr].main.loopStart = a;
        state.units[ptr].resumeClosure.loopStart = function (i) {
          i.loopStart = a;
        };
      };
    };
  };
};
exports.setBufferOffset_ = function (ptr) {
  return function (a) {
    return function (state) {
      return function () {
        state.units[ptr].bufferOffset = a;
      };
    };
  };
};
exports.setLoopEnd_ = function (ptr) {
  return function (a) {
    return function (state) {
      return function () {
        state.units[ptr].main.loopEnd = a;
        state.units[ptr].resumeClosure.loopEnd = function (i) {
          i.loopEnd = a;
        };
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
exports.setAudioWorkletParameter_ = function (ptr) {
  return function (a) {
    return function (b) {
      return function (state) {
        return function () {
          workletSetter(state.units[ptr].main, a, state.writeHead, b);
        };
      };
    };
  }
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
        // frequency defined for some non-generators
        // so check first for existence of resumeClosure
        if (state.units[ptr].resumeClosure) {
          state.units[ptr].resumeClosure.frequency = function (i) {
            genericStarter(i, "frequency", a);
          };
        }
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
exports.audioWorkletAddModule_ = function (ctx) {
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

var makePeriodicWaveImpl = function (ctx) {
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
exports.makePeriodicWaveImpl = makePeriodicWaveImpl;
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
// currently, there is no unsubscription logic to the media recorder
// in the case where a second subscriber is called, it will simply
// overwrite the first subscriber
// because of this, care needs to be taken in calling the "setMediaRecorderCb" function
// it will unset the previous one, which will result in the recording starting from the moment
// of being set
// if it is set in a loop, then there will effectively be no recording, as it will only capture the
// last couple milliseconds of the loop
exports.mediaRecorderToUrl = function (mimeType) {
  return function (handler) {
    return function (mediaRecorder) {
      return function () {
        var chunks = [];
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

exports.getFFTSize = function (analyserNode) {
  return function () {
    return analyserNode.fftSize;
  }
}

exports.setFFTSize = function (analyserNode) {
  return function (fftSize) {
    return function () {
      analyserNode.fftSize = fftSize;
    }
  }
}

exports.getSmoothingTimeConstant = function (analyserNode) {
  return function () {
    return analyserNode.smoothingTimeConstant;
  }
}

exports.setSmoothingTimeConstant = function (analyserNode) {
  return function (smoothingTimeConstant) {
    return function () {
      analyserNode.smoothingTimeConstant = smoothingTimeConstant;
    }
  }
}

exports.getMinDecibels = function (analyserNode) {
  return function () {
    return analyserNode.minDecibels;
  }
}

exports.setMinDecibels = function (analyserNode) {
  return function (minDecibels) {
    return function () {
      analyserNode.minDecibels = minDecibels;
    }
  }
}

exports.getMaxDecibels = function (analyserNode) {
  return function () {
    return analyserNode.maxDecibels;
  }
}

exports.setMaxDecibels = function (analyserNode) {
  return function (maxDecibels) {
    return function () {
      analyserNode.maxDecibels = maxDecibels;
    }
  }
}

exports.getFrequencyBinCount = function (analyserNode) {
  return function () {
    return analyserNode.frequencyBinCount;
  }
}

// https://developer.mozilla.org/en-US/docs/Web/API/AnalyserNode/getFloatTimeDomainData
exports.getFloatTimeDomainData = function (analyserNode) {
  return function () {
    var dataArray = new Float32Array(analyserNode.fftSize);
    analyserNode.getFloatTimeDomainData(dataArray);
    return dataArray;
  }
}

// https://developer.mozilla.org/en-US/docs/Web/API/AnalyserNode/getFloatFrequencyData
exports.getFloatFrequencyData = function (analyserNode) {
  return function () {
    var dataArray = new Float32Array(analyserNode.frequencyBinCount);
    analyserNode.getFloatFrequencyData(dataArray);
    return dataArray;
  }
}

// https://developer.mozilla.org/en-US/docs/Web/API/AnalyserNode/getByteTimeDomainData
exports.getByteTimeDomainData = function (analyserNode) {
  return function () {
    var dataArray = new Uint8Array(analyserNode.fftSize);
    analyserNode.getByteTimeDomainData(dataArray);
    return dataArray;
  }
}

// https://developer.mozilla.org/en-US/docs/Web/API/AnalyserNode/getByteFrequencyData
exports.getByteFrequencyData = function (analyserNode) {
  return function () {
    var dataArray = new Uint8Array(analyserNode.frequencyBinCount);
    analyserNode.getByteFrequencyData(dataArray);
    return dataArray;
  }
}
exports.bufferSampleRate = function (buffer) { return buffer.sampleRate }
exports.bufferLength = function (buffer) { return buffer.length }
exports.bufferDuration = function (buffer) { return buffer.duration }
exports.bufferNumberOfChannels = function (buffer) { return buffer.numberOfChannels }
