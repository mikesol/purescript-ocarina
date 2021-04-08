exports.touchAudio_ = function (context) {
  return function (audioInfo) {
    return {
      connect: {},
      disconnect:{},
      destroy:{},
      newUnit:{},
      setFrequency:{},
      setGain:{},
      setPan:{},
      setOffset:{},
    }
  };
};

exports.touchAudio_ = function (timeToSet) {
  return function (context) {
    return function (audioInfo) {
      return function (incoming) {
        return function (instructions) {
          return function () {
            // should never happen
            if (timeToSet < context.currentTime) {
              console.warn(
                "Programming error: we are setting in the past",
                timeToSet,
                context.currentTime
              );
              timeToSet = context.currentTime;
            }
            var nu = [];
            var lb = {};
            var generators = incoming.generators.slice();
            var recorders = incoming.recorders;
            var old = incoming.generators.slice();
            for (var i = 0; i < instructions.length; i++) {
              var c = instructions[i];
              if (predicates.isDisconnectFrom(c)) {
                getMainFromGenerator(generators[c.value0]).disconnect(
                  getMainFromGenerator(generators[c.value1])
                );
                var se = getSideEffectFromGenerator(generators[c.value1]);
                if (se) {
                  getMainFromGenerator(generators[c.value0]).disconnect(se);
                }
              } else if (predicates.isConnectTo(c)) {
                if (predicates.isNothing(c.value2)) {
                  getMainFromGenerator(generators[c.value0]).connect(
                    getMainFromGenerator(generators[c.value1])
                  );
                  var se = getSideEffectFromGenerator(generators[c.value1]);
                  if (se) {
                    getMainFromGenerator(generators[c.value0]).connect(se);
                  }
                } else {
                  getMainFromGenerator(generators[c.value0]).connect(
                    getMainFromGenerator(generators[c.value1]),
                    c.value2.value0.value0,
                    c.value2.value0.value1
                  );
                }
              } else if (predicates.isShuffle(c)) {
                generators[c.value1] = old[c.value0];
              } else if (predicates.isNewUnit(c)) {
                nu.push(c);
                generators[c.value0] = {
                  main: predicates.isSpeaker(c.value1)
                    ? context.createGain()
                    : predicates.isRecorder(c.value1)
                    ? context.createGain()
                    : predicates.isMicrophone(c.value1)
                    ? context.createMediaStreamSource(
                        audioInfo.microphones[
                          Object.keys(audioInfo.microphones)[0]
                        ]
                      )
                    : predicates.isPlay(c.value1)
                    ? context.createMediaElementSource(
                        audioInfo.tracks[c.value3.value0]
                      )
                    : predicates.isPlayBuf(c.value1)
                    ? context.createBufferSource()
                    : predicates.isLoopBuf(c.value1)
                    ? context.createBufferSource()
                    : predicates.isIIRFilter(c.value1)
                    ? context.createIIRFilter(
                        c.value6.value0.value0,
                        c.value6.value0.value1
                      )
                    : predicates.isLowpass(c.value1)
                    ? context.createBiquadFilter()
                    : predicates.isBandpass(c.value1)
                    ? context.createBiquadFilter()
                    : predicates.isLowshelf(c.value1)
                    ? context.createBiquadFilter()
                    : predicates.isHighshelf(c.value1)
                    ? context.createBiquadFilter()
                    : predicates.isNotch(c.value1)
                    ? context.createBiquadFilter()
                    : predicates.isAllpass(c.value1)
                    ? context.createBiquadFilter()
                    : predicates.isPeaking(c.value1)
                    ? context.createBiquadFilter()
                    : predicates.isHighpass(c.value1)
                    ? context.createBiquadFilter()
                    : predicates.isConvolver(c.value1)
                    ? context.createConvolver()
                    : predicates.isDynamicsCompressor(c.value1)
                    ? context.createDynamicsCompressor()
                    : predicates.isSawtoothOsc(c.value1)
                    ? context.createOscillator()
                    : predicates.isTriangleOsc(c.value1)
                    ? context.createOscillator()
                    : predicates.isPeriodicOsc(c.value1)
                    ? context.createOscillator()
                    : predicates.isWaveShaper(c.value1)
                    ? context.createWaveShaper()
                    : predicates.isDup(c.value1)
                    ? context.createGain()
                    : predicates.isStereoPanner(c.value1)
                    ? context.createStereoPanner()
                    : predicates.isPanner(c.value1)
                    ? context.createPanner()
                    : predicates.isSinOsc(c.value1)
                    ? context.createOscillator()
                    : predicates.isSquareOsc(c.value1)
                    ? context.createOscillator()
                    : predicates.isMul(c.value1)
                    ? (function () {
                        var nConnections = 0;
                        for (var j = 0; j < instructions.length; j++) {
                          // this hack is necessary because
                          // custom audio worklets need explicit
                          // channel assignments. maybe make explicit everywhere?
                          var d = instructions[j];
                          if (
                            predicates.isConnectTo(d) &&
                            d.value1 == c.value0
                          ) {
                            d.value2 = predicates.justly(
                              predicates.tupply(0)(nConnections)
                            );
                            nConnections += 1;
                          }
                        }
                        return new AudioWorkletNode(context, "ps-aud-mul", {
                          numberOfInputs: nConnections,
                          numberOfOutputs: 1,
                        });
                      })()
                    : predicates.isAudioWorkletGenerator(c.value1) ||
                      predicates.isAudioWorkletProcessor(c.value1) ||
                      predicates.isAudioWorkletAggregator(c.value1)
                    ? (function () {
                        var initialParams = {};
                        for (var j = 0; j < instructions.length; j++) {
                          var d = instructions[j];
                          if (
                            predicates.isSetCustomParam(d) &&
                            d.value0 == c.value0
                          ) {
                            initialParams[d.value1] = d.value2;
                          }
                        }
                        if (predicates.isAudioWorkletAggregator(c.value1)) {
                          var nConnections = 0;
                          for (var j = 0; j < instructions.length; j++) {
                            // this hack is necessary because
                            // custom audio worklets need explicit
                            // channel assignments. maybe make explicit everywhere?
                            var d = instructions[j];
                            if (
                              predicates.isConnectTo(d) &&
                              d.value1 == c.value0
                            ) {
                              d.value2 = predicates.justly(
                                predicates.tupply(0)(nConnections)
                              );
                              nConnections += 1;
                            }
                          }
                        }
                        return new AudioWorkletNode(context, c.value3.value0, {
                          numberOfInputs: predicates.isAudioWorkletGenerator(
                            c.value1
                          )
                            ? 0
                            : predicates.isAudioWorkletProcessor(c.value1)
                            ? 1
                            : 2,
                          numberOfOutputs: 1,
                          parameterData: initialParams,
                        });
                      })()
                    : predicates.isAdd(c.value1)
                    ? context.createGain()
                    : predicates.isDelay(c.value1)
                    ? context.createDelay(10.0) // magic number for 10 seconds...make tweakable?
                    : predicates.isConstant(c.value1)
                    ? context.createConstantSource()
                    : predicates.isGain(c.value1)
                    ? context.createGain()
                    : predicates.isSplitRes(c.value1)
                    ? context.createGain()
                    : predicates.isDupRes(c.value1)
                    ? context.createGain()
                    : predicates.isSplitter(c.value1)
                    ? context.createChannelSplitter(c.value2.value0)
                    : predicates.isMerger(c.value1)
                    ? context.createChannelMerger(c.value2.value0)
                    : null,
                };
                if (predicates.isSpeaker(c.value1)) {
                  generators[c.value0].se = context.destination;
                } else if (predicates.isRecorder(c.value1)) {
                  var mediaRecorderSideEffectFn =
                    audioInfo.recorders[c.value3.value0];
                  var dest = context.createMediaStreamDestination();
                  var mediaRecorder = new MediaRecorder(dest.stream);
                  recorders = recorders.concat(mediaRecorder);
                  mediaRecorderSideEffectFn(mediaRecorder)();
                  mediaRecorder.start();
                  generators[c.value0].se = dest;
                }
              } else if (predicates.isSetFrequency(c)) {
                genericSetter(
                  predicates,
                  generators,
                  c,
                  "frequency",
                  timeToSet
                );
              } else if (predicates.isSetPan(c)) {
                genericSetter(predicates, generators, c, "pan", timeToSet);
              } else if (predicates.isSetGain(c)) {
                genericSetter(predicates, generators, c, "gain", timeToSet);
              } else if (predicates.isSetQ(c)) {
                genericSetter(predicates, generators, c, "Q", timeToSet);
              } else if (predicates.isSetBuffer(c)) {
                var myArrayBuffer = context.createBuffer(
                  c.value2.length,
                  c.value2[0].length,
                  c.value1
                );
                for (
                  var channel = 0;
                  channel < myArrayBuffer.numberOfChannels;
                  channel++
                ) {
                  var nowBuffering = myArrayBuffer.getChannelData(channel);
                  for (var i = 0; i < myArrayBuffer.length; i++) {
                    nowBuffering[i] = c.value2[channel][i];
                  }
                }
                getMainFromGenerator(
                  generators[c.value0]
                ).buffer = myArrayBuffer;
              } else if (predicates.isSetDelay(c)) {
                genericSetter(
                  predicates,
                  generators,
                  c,
                  "delayTime",
                  timeToSet
                );
              } else if (predicates.isSetOffset(c)) {
                genericSetter(predicates, generators, c, "offset", timeToSet);
              } else if (predicates.isSetLoopStart(c)) {
                lb[c.value0] = c.value1;
                getMainFromGenerator(generators[c.value0]).loopStart = c.value1;
              } else if (predicates.isSetLoopEnd(c)) {
                getMainFromGenerator(generators[c.value0]).loopEnd = c.value1;
              } else if (predicates.isSetOversample(c)) {
                getMainFromGenerator(generators[c.value0]).oversample =
                  c.value1;
              } else if (predicates.isSetCurve(c)) {
                var curve = new Float32Array(c.value1.length);
                for (var i = 0; i < c.value1.length; i++) {
                  curve[i] = c.value1[i];
                }

                getMainFromGenerator(generators[c.value0]).curve = curve;
              } else if (predicates.isSetPlaybackRate(c)) {
                genericSetter(
                  predicates,
                  generators,
                  c,
                  "playbackRate",
                  timeToSet
                );
              } else if (predicates.isSetThreshold(c)) {
                genericSetter(
                  predicates,
                  generators,
                  c,
                  "threshold",
                  timeToSet
                );
              } else if (predicates.isSetKnee(c)) {
                genericSetter(predicates, generators, c, "knee", timeToSet);
              } else if (predicates.isSetRatio(c)) {
                genericSetter(predicates, generators, c, "ratio", timeToSet);
              } else if (predicates.isSetAttack(c)) {
                genericSetter(predicates, generators, c, "attack", timeToSet);
              } else if (predicates.isSetRelease(c)) {
                genericSetter(predicates, generators, c, "release", timeToSet);
              } else if (predicates.isSetCustomParam(c)) {
                getMainFromGenerator(generators[c.value0])
                  .parameters.get(c.value1)
                  .linearRampToValueAtTime(c.value2, timeToSet + c.value3);
              } else if (predicates.isStop(c)) {
                getMainFromGenerator(generators[c.value0]).stop();
              } else if (predicates.isFree(c)) {
                delete generators[c.value0];
              } else if (predicates.isSetConeInnerAngle(c)) {
                getMainFromGenerator(generators[c.value0]).coneInnerAngle =
                  c.value1;
              } else if (predicates.isSetConeOuterAngle(c)) {
                getMainFromGenerator(generators[c.value0]).coneOuterAngle =
                  c.value1;
              } else if (predicates.isSetConeOuterGain(c)) {
                getMainFromGenerator(generators[c.value0]).coneOuterGain =
                  c.value1;
              } else if (predicates.isSetDistanceModel(c)) {
                getMainFromGenerator(generators[c.value0]).distanceModel =
                  c.value1;
              } else if (predicates.isSetMaxDistance(c)) {
                getMainFromGenerator(generators[c.value0]).maxDistance =
                  c.value1;
              } else if (predicates.isSetOrientationX(c)) {
                genericSetter(
                  predicates,
                  generators,
                  c,
                  "orientationX",
                  timeToSet
                );
              } else if (predicates.isSetOrientationY(c)) {
                genericSetter(
                  predicates,
                  generators,
                  c,
                  "orientationY",
                  timeToSet
                );
              } else if (predicates.isSetOrientationZ(c)) {
                genericSetter(
                  predicates,
                  generators,
                  c,
                  "orientationZ",
                  timeToSet
                );
              } else if (predicates.isSetPanningModel(c)) {
                getMainFromGenerator(generators[c.value0]).panningModel =
                  c.value1;
              } else if (predicates.isSetPositionX(c)) {
                genericSetter(
                  predicates,
                  generators,
                  c,
                  "positionX",
                  timeToSet
                );
              } else if (predicates.isSetPositionY(c)) {
                genericSetter(
                  predicates,
                  generators,
                  c,
                  "positionY",
                  timeToSet
                );
              } else if (predicates.isSetPositionZ(c)) {
                genericSetter(
                  predicates,
                  generators,
                  c,
                  "positionZ",
                  timeToSet
                );
              } else if (predicates.isSetRefDistance(c)) {
                getMainFromGenerator(generators[c.value0]).refDistance =
                  c.value1;
              } else if (predicates.isSetRolloffFactor(c)) {
                getMainFromGenerator(generators[c.value0]).rolloffFactor =
                  c.value1;
              }
            }
            for (var i = 0; i < nu.length; i++) {
              var c = nu[i];
              if (predicates.isLoopBuf(c.value1)) {
                getMainFromGenerator(generators[c.value0]).loop = true;
                getMainFromGenerator(generators[c.value0]).buffer =
                  audioInfo.buffers[c.value3.value0];
                getMainFromGenerator(generators[c.value0]).start(
                  predicates.isNothing(c.value4)
                    ? 0.0
                    : timeToSet + c.value4.value0,
                  lb[c.value0]
                );
              } else if (predicates.isWaveShaper(c.value1)) {
                getMainFromGenerator(generators[c.value0]).curve =
                  audioInfo.floatArrays[c.value3.value0];
              } else if (predicates.isConvolver(c.value1)) {
                getMainFromGenerator(generators[c.value0]).buffer =
                  audioInfo.buffers[c.value3.value0];
              } else if (predicates.isPlayBuf(c.value1)) {
                getMainFromGenerator(generators[c.value0]).loop = false;
                getMainFromGenerator(generators[c.value0]).buffer =
                  audioInfo.buffers[c.value3.value0];
                getMainFromGenerator(generators[c.value0]).start(
                  predicates.isNothing(c.value4)
                    ? 0.0
                    : timeToSet + c.value4.value0,
                  c.value5.value0
                );
              } else if (predicates.isPlay(c.value1)) {
                // todo - if the same element is resumed via play it won't
                // work in the current setup
                // this is because there is a 1-to-1 relationship between source
                // and media element
                // the current workaround is to create multiple media elements.
                // todo - add delay somehow...
                audioInfo.tracks[c.value3.value0].play();
              } else if (predicates.isConstant(c.value1)) {
                getMainFromGenerator(generators[c.value0]).start(
                  predicates.isNothing(c.value4)
                    ? 0.0
                    : timeToSet + c.value4.value0
                );
              } else if (predicates.isLowpass(c.value1)) {
                getMainFromGenerator(generators[c.value0]).type = "lowpass";
              } else if (predicates.isBandpass(c.value1)) {
                getMainFromGenerator(generators[c.value0]).type = "bandpass";
              } else if (predicates.isLowshelf(c.value1)) {
                getMainFromGenerator(generators[c.value0]).type = "lowshelf";
              } else if (predicates.isHighshelf(c.value1)) {
                getMainFromGenerator(generators[c.value0]).type = "highshelf";
              } else if (predicates.isNotch(c.value1)) {
                getMainFromGenerator(generators[c.value0]).type = "notch";
              } else if (predicates.isAllpass(c.value1)) {
                getMainFromGenerator(generators[c.value0]).type = "allpass";
              } else if (predicates.isPeaking(c.value1)) {
                getMainFromGenerator(generators[c.value0]).type = "peaking";
              } else if (predicates.isHighpass(c.value1)) {
                getMainFromGenerator(generators[c.value0]).type = "highpass";
              } else if (predicates.isSinOsc(c.value1)) {
                getMainFromGenerator(generators[c.value0]).type = "sine";
                getMainFromGenerator(generators[c.value0]).start(
                  predicates.isNothing(c.value4)
                    ? 0.0
                    : timeToSet + c.value4.value0
                );
              } else if (predicates.isSquareOsc(c.value1)) {
                getMainFromGenerator(generators[c.value0]).type = "square";
                getMainFromGenerator(generators[c.value0]).start(
                  predicates.isNothing(c.value4)
                    ? 0.0
                    : timeToSet + c.value4.value0
                );
              } else if (predicates.isTriangleOsc(c.value1)) {
                getMainFromGenerator(generators[c.value0]).type = "triangle";
                getMainFromGenerator(generators[c.value0]).start(
                  predicates.isNothing(c.value4)
                    ? 0.0
                    : timeToSet + c.value4.value0
                );
              } else if (predicates.isSawtoothOsc(c.value1)) {
                getMainFromGenerator(generators[c.value0]).type = "sawtooth";
                getMainFromGenerator(generators[c.value0]).start(
                  predicates.isNothing(c.value4)
                    ? 0.0
                    : timeToSet + c.value4.value0
                );
              } else if (predicates.isPeriodicOsc(c.value1)) {
                getMainFromGenerator(generators[c.value0]).setPeriodicWave(
                  audioInfo.periodicWaves[c.value3.value0]
                );
                getMainFromGenerator(generators[c.value0]).start(
                  predicates.isNothing(c.value4)
                    ? 0.0
                    : timeToSet + c.value4.value0
                );
              } else if (predicates.isSplitRes(c.value1)) {
                getMainFromGenerator(
                  generators[c.value0]
                ).gain.linearRampToValueAtTime(1.0, timeToSet);
              } else if (predicates.isDupRes(c.value1)) {
                getMainFromGenerator(
                  generators[c.value0]
                ).gain.linearRampToValueAtTime(1.0, timeToSet);
              }
            }
            return { generators: generators, recorders: recorders };
          };
        };
      };
    };
  };
};
