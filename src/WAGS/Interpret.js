var SINGLE_NUMBER = "singleNumber";
var CANCELLATION = "cancellation";
var IMMEDIATELY = "immediately";
var NO_RAMP = "noRamp";
var LINEAR_RAMP = "linearRamp";
var EXPONENTIAL_RAMP = "exponentialRamp";
var ENVELOPE = "envelope";
exports.context = function () {
	return new (window.AudioContext || window.webkitAudioContext)();
};
exports.contextState = function (audioCtx) {
	return function () {
		return audioCtx.state;
	};
};
exports.contextResume = function (audioCtx) {
	return function () {
		return audioCtx.resume();
	};
};
var isOn = function (param) {
	return param.type === "on" || param.type === "offOn" || param.type === "ons";
};
var makeid = function (length) {
	var result = "";
	var characters =
		"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
	var charactersLength = characters.length;
	for (var i = 0; i < length; i++) {
		result += characters.charAt(Math.floor(Math.random() * charactersLength));
	}
	return result;
};

exports.makeFFIAudioSnapshot = function (audioCtx) {
	return function () {
		return {
			context: audioCtx,
			writeHead: 0.0,
			units: {},
			unqidfr: makeid(10),
		};
	};
};

exports.contextFromSnapshot = function (snapshot) {
	return snapshot.context;
};

exports.advanceWriteHead = function (snapshot) {
	return function (writeHead) {
		return function () {
			snapshot.writeHead = writeHead;
		};
	};
};

exports.close = function (audioCtx) {
	return function () {
		audioCtx.close();
	};
};
var genericStarter = function (unit, name, param) {
	if (param.type === ENVELOPE) {
		unit[name].setValueCurveAtTime(
			param.value.values,
			0.0,
			param.value.duration
		);
	} else if (param.type === SINGLE_NUMBER) {
		unit[name].value = param.value.param;
	}
};
var protoSetter = function (thingee, timeToSet, param) {
	if (
		param.type === SINGLE_NUMBER &&
		param.value.transition.type === IMMEDIATELY
	) {
		thingee.value = param.value.param;
	} else {
		if (param.type === SINGLE_NUMBER) {
			thingee[
				param.value.transition.type === NO_RAMP
					? "setValueAtTime"
					: param.value.transition.type === LINEAR_RAMP
					? "linearRampToValueAtTime"
					: param.value.transition.type === EXPONENTIAL_RAMP
					? "exponentialRampToValueAtTime"
					: "linearRampToValueAtTime"
			](param.value.param, timeToSet + param.value.timeOffset);
		} else if (isCancellation(param)) {
			param.value.hold
				? thingee.cancelAndHoldAtTime(timeToSet + param.value.timeOffset)
				: thingee.cancelScheduledValues(timeToSet + param.value.timeOffset);
		} else if (param.type === ENVELOPE) {
			// envelope is last option
			thingee.cancelScheduledValues(timeToSet + param.value.timeOffset - 0.001);
			thingee.setValueCurveAtTime(
				param.value.values,
				timeToSet + param.value.timeOffset,
				param.value.duration
			);
		} else {
			throw new Error("No idea what to do with " + JSON.stringify(param));
		}
	}
};
var workletSetter = function (unit, paramName, timeToSet, param) {
	return protoSetter(unit.parameters.get(paramName), timeToSet, param);
};
var genericSetter = function (unit, name, timeToSet, param) {
	return protoSetter(unit[name], timeToSet, param);
};
var connectXToY = function (calledExternally) {
	return function (x) {
		return function (y) {
			return function (stateX) {
				return function (stateY) {
					return function () {
						if (
							calledExternally &&
							(stateY.units[y].isSubgraph || stateY.units[y].isTumult)
						) {
							return;
						}
						stateX.units[x].main.connect(stateY.units[y].main);
						stateX.units[x].outgoing.push({ unit: y, state: stateY });
						stateY.units[y].incoming.push({ unit: x, state: stateX });
						if (stateY.units[y].se) {
							stateX.units[x].main.connect(stateY.units[y].se);
						}
					};
				};
			};
		};
	};
};
exports.connectXToY_ = function (x) {
	return function (state) {
		return connectXToY(true)(x.fromId)(x.toId)(state)(state);
	};
};
var disconnectXFromY = function (calledExternally) {
	return function (x) {
		return function (y) {
			return function (stateX) {
				return function (stateY) {
					return function () {
						if (
							calledExternally &&
							(stateY.units[y].isSubgraph || stateY.units[y].isTumult)
						) {
							return;
						}
						stateX.units[x].main.disconnect(stateY.units[y].main);
						stateX.units[x].outgoing = stateX.units[x].outgoing.filter(
							function (i) {
								return !(i.unit === y && i.state.unqidfr === stateY.unqidfr);
							}
						);
						stateY.units[y].incoming = stateY.units[y].incoming.filter(
							function (i) {
								return !(i.unit === x && i.state.unqidfr === stateX.unqidfr);
							}
						);
						if (stateY.units[y].se) {
							stateX.units[x].main.disconnect(stateY.units[y].se);
						}
					};
				};
			};
		};
	};
};
exports.disconnectXFromY_ = function (x) {
	return function (state) {
		return disconnectXFromY(true)(x.fromId)(x.toId)(state)(state);
	};
};
exports.destroyUnit_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
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
				disconnectXFromY(false)(state.units[ptr].input)(ptr)(
					state.units[ptr].parent
				)(state)();
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
exports.makeAllpass_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			state.units[ptr] = {
				outgoing: [],
				incoming: [],
				main: state.context.createBiquadFilter(),
			};
			state.units[ptr].main.type = "allpass";
			genericStarter(state.units[ptr].main, "frequency", a.freq);
			genericStarter(state.units[ptr].main, "Q", a.q);
		};
	};
};
exports.makeAnalyser_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			var analyserSideEffectFunction = a.cb;
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
exports.setAnalyserNodeCb_ = function (aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var a = aa.cb;
			if (state.units[ptr].analyserOrig === a) {
				return;
			}
			// first, unsubscribe
			state.units[ptr].analyser && state.units[ptr].analyser();
			state.units[ptr].analyser = a(state.units[ptr].se)();
			state.units[ptr].analyserOrig = a;
		};
	};
};
var isCancellation = function (a) {
	return a.type === CANCELLATION;
};
exports.makeAudioWorkletNode_ = function (aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var a = aa.options;
			var parameterData = {};
			var keys = Object.keys(a.parameterData);
			for (var i = 0; i < keys.length; i++) {
				if (a.parameterData[keys[i]].type === SINGLE_NUMBER) {
					parameterData[keys[i]] = a.parameterData[keys[i]].value.param;
				}
			}
			state.units[ptr] = {
				outgoing: [],
				incoming: [],
				main: new AudioWorkletNode(state.context, a.name, {
					numberOfInputs: a.numberOfInputs,
					numberOfOutputs: a.numberOfOutputs,
					outputChannelCount: a.outputChannelCount,
					parameterData: parameterData,
					processorOptions: a.processorOptions,
				}),
			};
		};
	};
};
exports.makeBandpass_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			state.units[ptr] = {
				outgoing: [],
				incoming: [],
				main: state.context.createBiquadFilter(),
			};
			state.units[ptr].main.type = "bandpass";
			genericStarter(state.units[ptr].main, "frequency", a.freq);
			genericStarter(state.units[ptr].main, "Q", a.q);
		};
	};
};
exports.makeConstant_ = function (aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var onOff = aa.onOff;
			var a = aa.offset;
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
			if (isOn(onOff.onOff)) {
				state.units[ptr].main.start(state.writeHead + onOff.timeOffset);
			}
			state.units[ptr].onOff = isOn(onOff.onOff);
		};
	};
};

exports.makeConvolver_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			state.units[ptr] = {
				outgoing: [],
				incoming: [],
				main: state.context.createConvolver(),
			};
			state.units[ptr].main.buffer = a.buffer;
		};
	};
};

exports.makePassthroughConvolver_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			state.units[ptr] = {
				outgoing: [],
				incoming: [],
				main: state.context.createConvolver(),
			};
		};
	};
};
exports.makeDelay_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			state.units[ptr] = {
				outgoing: [],
				incoming: [],
				main: state.context.createDelay(),
			};
			genericStarter(state.units[ptr].main, "delayTime", a.delayTime);
		};
	};
};
exports.makeDynamicsCompressor_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			state.units[ptr] = {
				main: state.context.createDynamicsCompressor(),
				outgoing: [],
				incoming: [],
			};
			genericStarter(state.units[ptr].main, "threshold", a.threshold);
			genericStarter(state.units[ptr].main, "knee", a.knee);
			genericStarter(state.units[ptr].main, "ratio", a.ratio);
			genericStarter(state.units[ptr].main, "attack", a.attack);
			genericStarter(state.units[ptr].main, "release", a.release);
		};
	};
};
exports.makeGain_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			state.units[ptr] = {
				outgoing: [],
				incoming: [],
				main: state.context.createGain(),
			};
			genericStarter(state.units[ptr].main, "gain", a.gain);
		};
	};
};
exports.makeHighpass_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			state.units[ptr] = {
				outgoing: [],
				incoming: [],
				main: state.context.createBiquadFilter(),
			};
			state.units[ptr].main.type = "highpass";
			genericStarter(state.units[ptr].main, "frequency", a.freq);
			genericStarter(state.units[ptr].main, "Q", a.q);
		};
	};
};
exports.makeHighshelf_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			state.units[ptr] = {
				outgoing: [],
				incoming: [],
				main: state.context.createBiquadFilter(),
			};
			state.units[ptr].main.type = "highshelf";
			genericStarter(state.units[ptr].main, "frequency", a.freq);
			genericStarter(state.units[ptr].main, "gain", a.gain);
		};
	};
};
exports.makeLoopBufWithDeferredBuffer_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
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
exports.makeLoopBuf_ = function (aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var onOff = aa.onOff;
			var a = aa.buffer;
			var b = aa.playbackRate;
			var c = aa.loopStart;
			var d = aa.loopEnd;
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
						i.buffer = a;
					},
				},
				main: createFunction(),
			};
			if (isOn(onOff.onOff)) {
				applyResumeClosure(state.units[ptr]);
				state.units[ptr].main.start(state.writeHead + onOff.timeOffset, c);
			}
			state.units[ptr].onOff = isOn(onOff.onOff);
		};
	};
};
exports.makeLowpass_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			state.units[ptr] = {
				outgoing: [],
				incoming: [],
				main: state.context.createBiquadFilter(),
			};
			state.units[ptr].main.type = "lowpass";
			genericStarter(state.units[ptr].main, "frequency", a.freq);
			genericStarter(state.units[ptr].main, "Q", a.q);
		};
	};
};
exports.makeLowshelf_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			state.units[ptr] = {
				outgoing: [],
				incoming: [],
				main: state.context.createBiquadFilter(),
			};
			state.units[ptr].main.type = "lowshelf";
			genericStarter(state.units[ptr].main, "frequency", a.freq);
			genericStarter(state.units[ptr].main, "gain", a.gain);
		};
	};
};
exports.makeMediaElement_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			var elt = a.element;
			var createFunction = function () {
				var unit = state.context.createMediaElementSource(elt);
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

exports.makeMicrophone_ = function (a) {
	return function (state) {
		return function () {
			state.units["microphone"] = {
				main: state.context.createMediaStreamSource(a.microphone),
				outgoing: [],
				incoming: [],
			};
		};
	};
};
exports.makeNotch_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			state.units[ptr] = {
				outgoing: [],
				incoming: [],
				main: state.context.createBiquadFilter(),
			};
			state.units[ptr].main.type = "notch";
			genericStarter(state.units[ptr].main, "frequency", a.freq);
			genericStarter(state.units[ptr].main, "Q", a.q);
		};
	};
};

exports.makePeaking_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			state.units[ptr] = {
				outgoing: [],
				incoming: [],
				main: state.context.createBiquadFilter(),
			};
			state.units[ptr].main.type = "peaking";
			genericStarter(state.units[ptr].main, "frequency", a.freq);
			genericStarter(state.units[ptr].main, "Q", a.q);
			genericStarter(state.units[ptr].main, "gain", a.gain);
		};
	};
};

exports.makePeriodicOscWithDeferredOsc_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
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

exports.makePeriodicOsc_ = function (aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var a = aa.wave;
			var onOff = aa.onOff;
			var b = aa.freq;
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
			if (isOn(onOff.onOff)) {
				state.units[ptr].main.start(state.writeHead + onOff.timeOffset);
			}
			state.units[ptr].onOff = isOn(onOff.onOff);
		};
	};
};
exports.makePeriodicOscV_ = function (aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var a = aa.realImg;
			var onOff = aa.onOff;
			var b = aa.freq;

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
							makePeriodicWaveImpl(state.context)(a.real)(a.img)()
						);
					},
				},
				main: createFunction(),
			};
			applyResumeClosure(state.units[ptr]);
			if (isOn(onOff.onOff)) {
				state.units[ptr].main.start(state.writeHead + onOff.timeOffset);
			}
			state.units[ptr].onOff = isOn(onOff.onOff);
		};
	};
};
exports.makePlayBufWithDeferredBuffer_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
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
exports.makeInput_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			state.units[ptr] = {
				outgoing: [],
				incoming: [],
				main: state.context.createGain(),
				parent: state.parent,
				input: a.input,
			};
			connectXToY(false)(a.input)(ptr)(state.parent)(state)();
			state.units[ptr].main.gain.value = 1.0;
		};
	};
};

exports.makeSubgraph_ = function (ptr) {
	return function (terminalPtr) {
		return function (envs) {
			return function (sceneM) {
				return function (funk) {
					return function (state) {
						return function () {
							var children = [];
							var scenes = [];
							for (var i = 0; i < envs.length; i++) {
								children[i] = {
									context: state.context,
									writeHead: state.writeHead,
									units: {},
									unqidfr: makeid(10),
									parent: state,
								};
								scenes[i] = sceneM(i);
							}
							state.units[ptr] = {
								outgoing: [],
								incoming: [],
								main: state.context.createGain(),
								children: children,
								funk: funk,
								isSubgraph: true,
								scenes: scenes,
							};
							state.units[ptr].main.gain.value = 1.0;
							for (var i = 0; i < scenes.length; i++) {
								var applied = funk(envs[i])(scenes[i]);
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
	};
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
										context: state.context,
										writeHead: state.writeHead,
										units: {},
										parent: state,
										unqidfr: makeid(10),
									};
								}
								state.units[ptr] = {
									outgoing: [],
									incoming: [],
									main: state.context.createGain(),
									children: children,
									isTumult: true,
									scenes: scenes,
								};
								state.units[ptr].main.gain.value = 1.0;
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
							};
						};
					};
				};
			};
		};
	};
};
exports.makePlayBuf_ = function (aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var a = aa.buffer;
			var b = aa.bufferOffset;
			var onOff = aa.onOff;
			var c = aa.playbackRate;
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
			if (isOn(onOff.onOff)) {
				applyResumeClosure(state.units[ptr]);
				state.units[ptr].main.start(state.writeHead + onOff.timeOffset, b);
			}
			state.units[ptr].onOff = isOn(onOff.onOff);
		};
	};
};
exports.makeMultiPlayBuf_ = function (aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var onOff = aa.onOff;
			var playbackRate = aa.playbackRate;
			var createFunction = function (onOff) {
				var tbos = [onOff.value.starts].concat(onOff.value.next);
				for (var i = 0; i < tbos.length; i++) {
					var j = i + state.units[ptr].subsIdx;
					state.units[ptr].subs[j] = { units: { } };
					state.units[ptr].actives.push(j);
					state.units[ptr].subs[j].units[ptr] = {
						outgoing: [],
						incoming: [],
						main: state.context.createBufferSource(),
						tbo: tbos[i],
						resumeClosure: {
							playbackRate: function(i) {
								genericStarter(i, "playbackRate", playbackRate);
							},
							buffer: (function(j) {
								return function(n) {
									n.buffer = state.units[ptr].subs[j].units[ptr].tbo.b;
								};
							})(j),
						},
					};
				}
				state.units[ptr].subsIdx = tbos.length;
			};
			state.units[ptr] = {
				outgoing: [],
				incoming: [],
				main: state.context.createGain(),
				subs: [],
				subsIdx: 0,
				actives: [],
				createFunction: createFunction,
				resumeClosure: {},
			};
			state.units[ptr].main.gain.value = 1.0;
			if (isOn(onOff)) {
				createFunction(onOff);
				var startOffset = 0.0;
				var stopOffset = state.units[ptr].subs[0].units[ptr].tbo.t;
				for (var i = 0; i < state.units[ptr].subs.length; i++) {
					applyResumeClosure(state.units[ptr].subs[i].units[ptr]);
					connectXToY(false)(ptr)(ptr)(state.units[ptr].subs[i])(state)();
					startOffset += state.units[ptr].subs[i].units[ptr].tbo.t;
					state.units[ptr].subs[i].units[ptr].main.start(
						state.writeHead + startOffset, state.units[ptr].subs[i].units[ptr].tbo.o
					);
					if (i !== state.units[ptr].subs.length - 1) {
						stopOffset += state.units[ptr].subs[i + 1].units[ptr].tbo.t;
						state.units[ptr].subs[i].units[ptr].main.stop(
							state.writeHead + stopOffset
						);
					}
					state.units[ptr].subs[i].units[ptr].main.addEventListener("ended", (function (i) {
						return function () {
							state.units[ptr].actives = state.units[ptr].actives.filter(function (j) {
								return i !== j;
							});
							delete state.units[ptr].subs[i];
						};
					}(i)));
				}
			}
			state.units[ptr].isOn = isOn(onOff);
		};
	};
};
exports.makeRecorder_ = function (aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var mediaRecorderSideEffectFn = aa.cb;
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
// setting makes us stop the previous one if it exists
exports.setMediaRecorderCb_ = function (aa) {
	return function (state) {
		return function () {
			var a = aa.cb;
			var ptr = aa.id;
			if (state.units[ptr].recorderOrig === a) {
				return;
			}
			state.units[ptr].recorder && state.units[ptr].recorder.stop();
			var mediaRecorderSideEffectFn = a;
			state.units[ptr].recorderOrig = a;
			var mediaRecorder = new MediaRecorder(state.units[ptr].se);
			mediaRecorderSideEffectFn(mediaRecorder)();
			mediaRecorder.start();
		};
	};
};

exports.makeSawtoothOsc_ = function (aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var onOff = aa.onOff;
			var a = aa.freq;
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
			if (isOn(onOff.onOff)) {
				state.units[ptr].main.start(state.writeHead + onOff.timeOffset);
			}
			state.units[ptr].onOff = isOn(onOff.onOff);
		};
	};
};
exports.makeSinOsc_ = function (aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var onOff = aa.onOff;
			var a = aa.freq;

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
			if (isOn(onOff.onOff)) {
				state.units[ptr].main.start(state.writeHead + onOff.timeOffset);
			}
			state.units[ptr].onOff = isOn(onOff.onOff);
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

exports.makeSquareOsc_ = function (aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var onOff = aa.onOff;
			var a = aa.freq;

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
			if (isOn(onOff.onOff)) {
				state.units[ptr].main.start(state.writeHead + onOff.timeOffset);
			}
			state.units[ptr].onOff = isOn(onOff.onOff);
		};
	};
};

exports.makeStereoPanner_ = function (aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var a = aa.pan;
			state.units[ptr] = {
				outgoing: [],
				incoming: [],
				main: state.context.createStereoPanner(),
			};
			genericStarter(state.units[ptr].main, "pan", a);
		};
	};
};
exports.makeTriangleOsc_ = function (aa) {
	return function (state) {
		var ptr = aa.id;
		var onOff = aa.onOff;
		var a = aa.freq;

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
			if (isOn(onOff.onOff)) {
				state.units[ptr].main.start(state.writeHead + onOff.timeOffset);
			}
			state.units[ptr].onOff = isOn(onOff.onOff);
		};
	};
};

exports.makeWaveShaper_ = function (aa) {
	return function (state) {
		var ptr = aa.id;
		var a = aa.curve;
		var b = aa.oversample;
		return function () {
			state.units[ptr] = {
				outgoing: [],
				incoming: [],
				main: state.context.createWaveShaper(),
			};
			state.units[ptr].main.curve = a;
			// .type because it's a key-only variant
			state.units[ptr].main.oversample = b.type;
		};
	};
};

exports.setWaveShaperCurve_ = function (aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var a = aa.curve;
			state.units[ptr].main.curve = a;
		};
	};
};

exports.setBuffer_ = function (aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var buffer = aa.buffer;
			state.units[ptr].resumeClosure.buffer = function (i) {
				i.buffer = buffer;
			};
		};
	};
};

exports.setInput_ = function (aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var a = aa.input;
			if (state.units[ptr].input && state.units[ptr].input === a) {
				return;
			}
			if (state.units[ptr].input) {
				disconnectXFromY(false)(
					state.units[ptr].input,
					ptr,
					state.parent,
					state
				);
			}
			state.units[ptr].input = a;
			connectXToY(false)(a)(ptr)(state.parent)(state)();
			state.units[ptr].main.gain.value = 1.0;
		};
	};
};

exports.setSubgraph_ = function (ptr) {
	return function (envs) {
		return function (state) {
			return function () {
				for (var i = 0; i < envs.length; i++) {
					state.units[ptr].children[i].writeHead = state.writeHead;
				}
				var scenes = state.units[ptr].scenes;
				var children = state.units[ptr].children;
				for (var i = 0; i < scenes.length; i++) {
					var applied = state.units[ptr].funk(envs[i])(scenes[i]);
					for (var j = 0; j < applied.instructions.length; j++) {
						// thunk
						applied.instructions[j](children[i])();
					}
					scenes[i] = applied.nextScene;
				}
			};
		};
	};
};

exports.setSingleSubgraph_ = function (ptr) {
	return function (i) {
		return function (env) {
			return function (state) {
				return function () {
					state.units[ptr].children[i].writeHead = state.writeHead;
					var scenes = state.units[ptr].scenes;
					var children = state.units[ptr].children;
					var applied = state.units[ptr].funk(env)(scenes[i]);
					for (var j = 0; j < applied.instructions.length; j++) {
						// thunk
						applied.instructions[j](children[i])();
					}
					scenes[i] = applied.nextScene;
				};
			};
		};
	};
};

exports.setTumult_ = function (ptr) {
	return function (terminalPtr) {
		return function (scenes) {
			return function (nothing) {
				return function (just) {
					return function (arrMaker) {
						return function (state) {
							return function () {
								var needsCreation = !(
									state.units[ptr] &&
									state.units[ptr].children &&
									state.units[ptr].scenes
								);
								if (needsCreation) {
									var children = [];
									for (var i = 0; i < scenes.length; i++) {
										children[i] = {
											context: state.context,
											writeHead: state.writeHead,
											units: {},
											parent: state,
											unqidfr: makeid(10),
										};
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
									var oldScene =
										oldScenes && oldScenes[i] ? just(oldScenes[i]) : nothing;
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
										var tmp = { units: {} };
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
					};
				};
			};
		};
	};
};
exports.setConvolverBuffer_ = function (aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var buffer = aa.buffer;
			state.units[ptr].main.buffer = buffer;
		};
	};
};
exports.setPeriodicOsc_ = function (aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var periodicOsc = aa.wave;
			state.units[ptr].resumeClosure.periodicOsc = function (i) {
				i.setPeriodicWave(periodicOsc);
			};
		};
	};
};

exports.setPeriodicOscV_ = function (aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var periodicOsc = aa.realImg;
			state.units[ptr].resumeClosure.periodicOsc = function (i) {
				i.setPeriodicWave(
					makePeriodicWaveImpl(state.context)(periodicOsc.real)(
						periodicOsc.img
					)()
				);
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

exports.setOnOff_ = function (aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var onOff = aa.onOff;
			if (onOff.onOff.type === "on") {
				setOn_(ptr)(onOff)(state)();
			} else if (onOff.onOff.type === "off") {
				setOff_(ptr)(onOff)(state)();
			} else if (onOff.onOff.type === "offOn") {
				setOff_(ptr)({ onOff: { type: "off" }, timeOffset: 0.0 })(state)();
				setOn_(ptr)({ onOff: { type: "on" }, timeOffset: onOff.timeOffset })(
					state
				)();
			}
		};
	};
};

exports.setMultiPlayBufOnOff_ = function (aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var onOff = aa.onOff;
			if (onOff.type === "ons") {
				state.units[ptr].createFunction(onOff);
				var startOffset = 0.0;
				var stopOffset = state.units[ptr].subs[state.units[ptr].subsIdx].units[ptr].tbo.t;
				for (var i = state.units[ptr].subsIdx; i < state.units[ptr].subs.length; i++) {
					applyResumeClosure(state.units[ptr].subs[i].units[ptr]);
					connectXToY(false)(ptr)(ptr)(state.units[ptr].subs[i])(state)();
					startOffset += state.units[ptr].subs[i].units[ptr].tbo.t;
					state.units[ptr].subs[i].units[ptr].main.start(
						state.writeHead + startOffset, state.units[ptr].subs[i].units[ptr].tbo.o
					);
					if (i !== state.units[ptr].subs.length - 1) {
						stopOffset += state.units[ptr].subs[i + 1].units[ptr].tbo.t;
						state.units[ptr].subs[i].units[ptr].main.stop(
							state.writeHead + stopOffset
						);
					}
					state.units[ptr].subs[i].units[ptr].main.addEventListener("ended", (function (i) {
						return function () {
							state.units[ptr].actives = state.units[ptr].actives.filter(function (j) {
								return i !== j;
							});
							delete state.units[ptr].subs[i];
						};
					}(i)));
				}
			} else if (onOff.type === "off") {

			}
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
							oldMain.disconnect(oogi.state.units[oogi.unit].main);
							if (oogi.state.units[oogi.unit].se) {
								oldMain.disconnect(oogi.state.units[oogi.unit].se);
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
					state.units[ptr].main.connect(ogi.state.units[ogi.unit].main);
					if (ogi.state.units[ogi.unit].se) {
						state.units[ptr].main.connect(ogi.state.units[ogi.unit].se);
					}
				}
			};
		};
	};
};
exports.setLoopStart_ = function (aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var a = aa.loopStart;
			state.units[ptr].main.loopStart = a;
			state.units[ptr].resumeClosure.loopStart = function (i) {
				i.loopStart = a;
			};
		};
	};
};

exports.setBufferOffset_ = function (aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var a = aa.bufferOffset;
			state.units[ptr].bufferOffset = a;
		};
	};
};
exports.setLoopEnd_ = function (aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var a = aa.loopEnd;

			state.units[ptr].main.loopEnd = a;
			state.units[ptr].resumeClosure.loopEnd = function (i) {
				i.loopEnd = a;
			};
		};
	};
};

exports.setRatio_ = function (aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var a = aa.ratio;
			genericSetter(state.units[ptr].main, "ratio", state.writeHead, a);
		};
	};
};

exports.setOffset_ = function (aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var a = aa.offset;

			genericSetter(state.units[ptr].main, "offset", state.writeHead, a);
			state.units[ptr].resumeClosure.offset = function (i) {
				genericStarter(i, "offset", a);
			};
		};
	};
};

exports.setAttack_ = function (aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var a = aa.attack;

			genericSetter(state.units[ptr].main, "attack", state.writeHead, a);
		};
	};
};

exports.setAudioWorkletParameter_ = function (aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var a = aa.paramName;
			var b = aa.paramValue;
			workletSetter(state.units[ptr].main, a, state.writeHead, b);
		};
	};
};
exports.setGain_ = function (aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var a = aa.gain;
			// TODO: test removing this hack
			// this was added at a time when there was a bug in the
			// transition names. the FFI was getting incorrect name strings
			// and, as a result, wound up always using linear transitions
			// this sounded fine except in cases when the transition was
			// immediate, in which case no transition happened.
			// now that that bug is fixed, this may no longer be needed
			// a way to test would be any of the synth-intensive works
			// like a fast back prelude or fugue
			// if you remove this and they are click free
			// then it is safe to remove
			if (!state.units[ptr].main.gain.value && a.param) {
				state.units[ptr].main.gain.value = 0.0;
			}
			genericSetter(state.units[ptr].main, "gain", state.writeHead, a);
		};
	};
};

exports.setQ_ = function (aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var a = aa.q;
			genericSetter(state.units[ptr].main, "Q", state.writeHead, a);
		};
	};
};
exports.setPan_ = function (aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var a = aa.pan;
			genericSetter(state.units[ptr].main, "pan", state.writeHead, a);
		};
	};
};
exports.setThreshold_ = function (aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var a = aa.threshold;
			genericSetter(state.units[ptr].main, "threshold", state.writeHead, a);
		};
	};
};
exports.setRelease_ = function (aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var a = aa.release;
			genericSetter(state.units[ptr].main, "release", state.writeHead, a);
		};
	};
};
exports.setKnee_ = function (aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var a = aa.knee;
			genericSetter(state.units[ptr].main, "knee", state.writeHead, a);
		};
	};
};
exports.setDelay_ = function (aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var a = aa.delay;
			genericSetter(state.units[ptr].main, "delayTime", state.writeHead, a);
		};
	};
};
exports.setPlaybackRate_ = function (aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var a = aa.playbackRate;
			if (state.units[ptr].actives) {
				for (var i = 0; i < state.units[ptr].actives; i++) {
					var ix = state.units[ptr].actives[i];
					genericSetter(state.units[ptr].main.subs[ix], "playbackRate", state.writeHead, a);
					state.units[ptr].resumeClosure.playbackRate = function (i) {
						genericStarter(i, "playbackRate", a);
					};
				}
			} else {
				genericSetter(state.units[ptr].main, "playbackRate", state.writeHead, a);
				state.units[ptr].resumeClosure.playbackRate = function (i) {
					genericStarter(i, "playbackRate", a);
				};
			}
		};
	};
};
exports.setFrequency_ = function (aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var a = aa.frequency;
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
exports.fetchArrayBuffer = function (s) {
	return function () {
		{
			return fetch(s).then(
				function (b) {
					return b.arrayBuffer();
				},
				function (e) {
					console.error("Error fetching buffer", e);
					return Promise.reject(e);
				}
			);
		}
	};
};
exports.decodeAudioDataFromArrayBuffer = function (ctx) {
	return function (b) {
		return function () {
			return ctx.decodeAudioData(b);
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
exports.mediaRecorderToBlob = function (mimeType) {
	return function (handler) {
		return function (mediaRecorder) {
			return function () {
				var chunks = [];
				mediaRecorder.ondataavailable = function (evt) {
					chunks.push(evt.data);
				};

				mediaRecorder.onstop = function () {
					var blob = new Blob(chunks, { type: mimeType });
					handler(blob)();
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
	};
};

exports.setFFTSize = function (analyserNode) {
	return function (fftSize) {
		return function () {
			analyserNode.fftSize = fftSize;
		};
	};
};

exports.getSmoothingTimeConstant = function (analyserNode) {
	return function () {
		return analyserNode.smoothingTimeConstant;
	};
};

exports.setSmoothingTimeConstant = function (analyserNode) {
	return function (smoothingTimeConstant) {
		return function () {
			analyserNode.smoothingTimeConstant = smoothingTimeConstant;
		};
	};
};

exports.getMinDecibels = function (analyserNode) {
	return function () {
		return analyserNode.minDecibels;
	};
};

exports.setMinDecibels = function (analyserNode) {
	return function (minDecibels) {
		return function () {
			analyserNode.minDecibels = minDecibels;
		};
	};
};

exports.getMaxDecibels = function (analyserNode) {
	return function () {
		return analyserNode.maxDecibels;
	};
};

exports.setMaxDecibels = function (analyserNode) {
	return function (maxDecibels) {
		return function () {
			analyserNode.maxDecibels = maxDecibels;
		};
	};
};

exports.getFrequencyBinCount = function (analyserNode) {
	return function () {
		return analyserNode.frequencyBinCount;
	};
};

// https://developer.mozilla.org/en-US/docs/Web/API/AnalyserNode/getFloatTimeDomainData
exports.getFloatTimeDomainData = function (analyserNode) {
	return function () {
		var dataArray = new Float32Array(analyserNode.fftSize);
		analyserNode.getFloatTimeDomainData(dataArray);
		return dataArray;
	};
};

// https://developer.mozilla.org/en-US/docs/Web/API/AnalyserNode/getFloatFrequencyData
exports.getFloatFrequencyData = function (analyserNode) {
	return function () {
		var dataArray = new Float32Array(analyserNode.frequencyBinCount);
		analyserNode.getFloatFrequencyData(dataArray);
		return dataArray;
	};
};

// https://developer.mozilla.org/en-US/docs/Web/API/AnalyserNode/getByteTimeDomainData
exports.getByteTimeDomainData = function (analyserNode) {
	return function () {
		var dataArray = new Uint8Array(analyserNode.fftSize);
		analyserNode.getByteTimeDomainData(dataArray);
		return dataArray;
	};
};

// https://developer.mozilla.org/en-US/docs/Web/API/AnalyserNode/getByteFrequencyData
exports.getByteFrequencyData = function (analyserNode) {
	return function () {
		var dataArray = new Uint8Array(analyserNode.frequencyBinCount);
		analyserNode.getByteFrequencyData(dataArray);
		return dataArray;
	};
};
exports.bufferSampleRate = function (buffer) {
	return buffer.sampleRate;
};
exports.bufferLength = function (buffer) {
	return buffer.length;
};
exports.bufferDuration = function (buffer) {
	return buffer.duration;
};
exports.bufferNumberOfChannels = function (buffer) {
	return buffer.numberOfChannels;
};
exports.constant0Hack = function (context) {
	return function () {
		var constant = context.createConstantSource();
		constant.offset.value = 0.0;
		constant.connect(context.destination);
		constant.start();
		return function () {
			constant.stop();
			constant.disconnect(context.destination);
		};
	};
};
