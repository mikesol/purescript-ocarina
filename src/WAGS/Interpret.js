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
var NUMERIC = "numeric";
var SUDDEN = "sudden";
var CANCELLATION = "cancellation";
var NO_RAMP = "noRamp";
var LINEAR_RAMP = "linearRamp";
var EXPONENTIAL_RAMP = "exponentialRamp";
var ENVELOPE = "envelope";
var isOn = function (param) {
	return param.type === "on" || param.type === "offOn";
};
var isCancellation = function (a) {
	return a.type === CANCELLATION;
};
var protoSetter = function (thingee, deprecatedTimeToSet, param) {
	if (param.type === SUDDEN) {
		thingee.value = param.value.n;
	} else {
		if (param.type === NUMERIC) {
			thingee[
				param.value.t.type === NO_RAMP
					? "setValueAtTime"
					: param.value.t.type === LINEAR_RAMP
					? "linearRampToValueAtTime"
					: param.value.t.type === EXPONENTIAL_RAMP
					? "exponentialRampToValueAtTime"
					: "linearRampToValueAtTime"
			](param.value.n, deprecatedTimeToSet + param.value.o);
		} else if (isCancellation(param)) {
			param.value.hold
				? thingee.cancelAndHoldAtTime(deprecatedTimeToSet + param.value.o)
				: thingee.cancelScheduledValues(deprecatedTimeToSet + param.value.o);
		} else if (param.type === ENVELOPE) {
			// envelope is last option
			thingee.cancelScheduledValues(
				deprecatedTimeToSet + param.value.o - 0.001
			);
			thingee.setValueCurveAtTime(
				param.value.p,
				deprecatedTimeToSet + param.value.o,
				param.value.d
			);
		} else {
			throw new Error("No idea what to do with " + JSON.stringify(param));
		}
	}
};
var workletSetter = function (unit, paramName, deprecatedTimeToSet, param) {
	return protoSetter(
		unit.parameters.get(paramName),
		deprecatedTimeToSet,
		param
	);
};
var genericSetter = function (unit, name, deprecatedTimeToSet, param) {
	return protoSetter(unit[name], deprecatedTimeToSet, param);
};
var mConnectXToY_ = function (x) {
	return function (y) {
		return function (state) {
			return function () {
				if (y.type === "just") {
					connectXToY_(x)(y.value)(state)();
				}
			};
		};
	};
};
var connectXToY_ = function (x) {
	return function (y) {
		return function (state) {
			return function () {
				state.units[x].outgoing.push(y);
				state.units[y].incoming.push(x);
				state.units[x].main.connect(state.units[y].main);
				if (state.units[y].se) {
					state.units[x].main.connect(state.units[y].se);
				}
			};
		};
	};
};
exports.connectXToY_ = function (x) {
	return function (y) {
		return function (state) {
			return function () {
				setImmediate(function () {
					connectXToY_(x)(y)(state)();
				});
			};
		};
	};
};
var disconnectXFromY_ = function (x) {
	return function (y) {
		return function (state) {
			return function () {
				state.units[x].outgoing = state.units[x].outgoing.filter(function (i) {
					return !(i === y);
				});
				state.units[y].incoming = state.units[y].incoming.filter(function (i) {
					return !(i === x);
				});
				state.units[x].main.connect(state.units[y].main);
				if (state.units[y].se) {
					state.units[x].main.connect(state.units[y].se);
				}
			};
		};
	};
};
exports.disconnectXFromY_ = disconnectXFromY_;
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
			delete state.units[ptr];
		};
	};
};
// allpass
exports.makeAllpass_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			state.units[ptr] = {
				outgoing: [a.parent],
				incoming: [],
				main: new BiquadFilterNode(state.context, {
					type: "allpass",
					Q: a.q,
					frequency: a.frequency,
				}),
			};
			mConnectXToY_(ptr)(a.parent)(state)();
		};
	};
};

// analyser
exports.makeAnalyser_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			var analyserSideEffectFunction = a.cb;
			var dest = new AnalyserNode(state.context, a);
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
			mConnectXToY_(ptr)(a.parent)(state)();
		};
	};
};

// audio worklet node
exports.makeAudioWorkletNode_ = function (aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var a = aa.options;
			state.units[ptr] = {
				outgoing: [],
				incoming: [],
				main: new AudioWorkletNode(state.context, a.name, {
					numberOfInputs: a.numberOfInputs,
					numberOfOutputs: a.numberOfOutputs,
					outputChannelCount: a.outputChannelCount,
					parameterData: a.parameterData,
					processorOptions: a.processorOptions,
				}),
			};
			mConnectXToY_(ptr)(aa.parent)(state)();
		};
	};
};

// bandpass
exports.makeBandpass_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			state.units[ptr] = {
				outgoing: [a.parent],
				incoming: [],
				main: new BiquadFilterNode(state.context, {
					type: "bandpass",
					Q: a.q,
					frequency: a.frequency,
				}),
			};
			mConnectXToY_(ptr)(a.parent)(state)();
		};
	};
};

// constant
exports.makeConstant_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			var onOff = a.onOff;
			var createClosure = function (context, i) {
				return new ConstantSourceNode(context, i);
			};
			var resume = { offset: a.offset };
			state.units[ptr] = {
				//outgoing: [a.parent],
				outgoing: [],
				incoming: [],
				resume: resume,
				createClosure: createClosure,
				main: createClosure(state.context, resume),
			};
			mConnectXToY_(ptr)(a.parent)(state)();
			// var oo = isOn(onOff.onOff);
			// if (oo) {
			// 	state.units[ptr].main.start(
			// 		state.deprecatedWriteHead + onOff.timeOffset
			// 	);
			// }
			// state.units[ptr].onOff = oo;
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
				main: new ConvolverNode(state.context, { buffer: a.buffer }),
			};
			mConnectXToY_(ptr)(a.parent)(state)();
		};
	};
};

// delay
exports.makeDelay_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			state.units[ptr] = {
				outgoing: [a.parent],
				incoming: [],
				main: new DelayNode(state.context, {
					delayTime: a.delayTime,
				}),
			};
			mConnectXToY_(ptr)(a.parent)(state)();
		};
	};
};

// dynamicsCompressor
exports.makeDynamicsCompressor_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			state.units[ptr] = {
				outgoing: [a.parent],
				incoming: [],
				main: new DynamicsCompressorNode(state.context, {
					knee: a.knee,
					ratio: a.ratio,
					threshold: a.threshold,
					attack: a.attack,
					release: a.release,
				}),
			};
			mConnectXToY_(ptr)(a.parent)(state)();
		};
	};
};

// dynamicsCompressor
exports.makeGain_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			state.units[ptr] = {
				outgoing: [a.parent],
				incoming: [],
				main: new GainNode(state.context, {
					gain: a.gain,
				}),
			};
			mConnectXToY_(ptr)(a.parent)(state)();
		};
	};
};
// highpass
exports.makeHighpass_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			state.units[ptr] = {
				outgoing: [a.parent],
				incoming: [],
				main: new BiquadFilterNode(state.context, {
					type: "highpass",
					Q: a.q,
					frequency: a.frequency,
				}),
			};
			mConnectXToY_(ptr)(a.parent)(state)();
		};
	};
};

// highshelf
exports.makeHighshelf_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			state.units[ptr] = {
				outgoing: [a.parent],
				incoming: [],
				main: new BiquadFilterNode(state.context, {
					type: "highshelf",
					frequency: a.frequency,
					gain: a.gain,
				}),
			};
			mConnectXToY_(ptr)(a.parent)(state)();
		};
	};
};

// input
exports.makeInput_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			var parent = a.parent;
			setImmediate(function () {
				connectXToY_(ptr)(parent)(state)();
			});
		};
	};
};

// loopBuf
exports.makeLoopBuf_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			var onOff = a.onOff;
			var createClosure = function (context, i) {
				return new AudioBufferSourceNode(context, i);
			};
			var resume = {
				loop: true,
				buffer: a.buffer,
				loopStart: a.loopStart,
				loopEnd: a.loopEnd,
				playbackRate: a.playbackRate,
			};
			state.units[ptr] = {
				//outgoing: [a.parent],
				outgoing: [],
				incoming: [],
				resume: resume,
				createClosure: createClosure,
				main: createClosure(state.context, resume),
			};
			mConnectXToY_(ptr)(a.parent)(state)();
			// var oo = isOn(onOff.onOff);
			// if (oo) {
			// 	state.units[ptr].main.start(
			// 		state.deprecatedWriteHead + onOff.timeOffset
			// 	);
			// }
			// state.units[ptr].onOff = oo;
		};
	};
};

// lowpass
exports.makeLowpass_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			state.units[ptr] = {
				outgoing: [a.parent],
				incoming: [],
				main: new BiquadFilterNode(state.context, {
					type: "lowpass",
					Q: a.q,
					frequency: a.frequency,
				}),
			};
			mConnectXToY_(ptr)(a.parent)(state)();
		};
	};
};

// lowshelf
exports.makeLowshelf_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			state.units[ptr] = {
				outgoing: [a.parent],
				incoming: [],
				main: new BiquadFilterNode(state.context, {
					type: "lowshelf",
					frequency: a.frequency,
					gain: a.gain,
				}),
			};
			mConnectXToY_(ptr)(a.parent)(state)();
		};
	};
};

// media element

exports.makeMediaElement_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			var elt = a.element;
			var createClosure = function () {
				var unit = state.context.createMediaElementSource(elt);
				return unit;
			};
			state.units[ptr] = {
				outgoing: [a.parent],
				incoming: [],
				createClosure: createClosure,
				resumeClosure: {},
				main: createClosure(),
			};
			mConnectXToY_(ptr)(a.parent)(state)();
		};
	};
};

// microphone
exports.makeMicrophone_ = function (a) {
	return function (state) {
		return function () {
			state.units[a.id] = {
				main: state.context.createMediaStreamSource(a.microphone),
				outgoing: [a.parent],
				incoming: [],
			};
		};
	};
};

// notch
exports.makeNotch_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			state.units[ptr] = {
				outgoing: [a.parent],
				incoming: [],
				main: new BiquadFilterNode(state.context, {
					type: "notch",
					frequency: a.frequency,
					Q: a.q,
				}),
			};
			mConnectXToY_(ptr)(a.parent)(state)();
		};
	};
};

// peaking
exports.makePeaking_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			state.units[ptr] = {
				outgoing: [a.parent],
				incoming: [],
				main: new BiquadFilterNode(state.context, {
					type: "peaking",
					frequency: a.frequency,
					Q: a.q,
					gain: a.gain,
				}),
			};
			mConnectXToY_(ptr)(a.parent)(state)();
		};
	};
};

// periodic osc
exports.makePeriodicOsc_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			var onOff = a.onOff;
			var createClosure = function (context, i) {
				var o = new OscillatorNode(context, i);
				o.setPeriodicWave(
					i.spec.type === "wave"
						? i.spec.value
						: makePeriodicWaveImpl(state.context)(i.spec.value.real)(
								i.spec.value.img
						  )()
				);
				return o;
			};
			var resume = { frequency: a.frequency, type: "custom", spec: a.spec };
			state.units[ptr] = {
				//outgoing: [a.parent],
				outgoing: [],
				incoming: [],
				resume: resume,
				createClosure: createClosure,
				main: createClosure(state.context, resume),
			};
			mConnectXToY_(ptr)(a.parent)(state)();
			// var oo = isOn(onOff.onOff);
			// if (oo) {
			// 	state.units[ptr].main.start(
			// 		state.deprecatedWriteHead + onOff.timeOffset
			// 	);
			// }
			// state.units[ptr].onOff = oo;
		};
	};
};

// playBuf
exports.makePlayBuf_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			var onOff = a.onOff;
			var createClosure = function (context, i) {
				return new AudioBufferSourceNode(context, i);
			};
			var resume = {
				loop: false,
				buffer: a.buffer,
				playbackRate: a.playbackRate,
			};
			state.units[ptr] = {
				//outgoing: [a.parent],
				outgoing: [],
				incoming: [],
				bufferOffset: a.bufferOffset,
				duration: a.duration,
				resume: resume,
				createClosure: createClosure,
				main: createClosure(state.context, resume),
			};
			mConnectXToY_(ptr)(a.parent)(state)();
			// var oo = isOn(onOff.onOff);
			// if (oo) {
			// 	state.units[ptr].main.start(
			// 		state.deprecatedWriteHead + onOff.timeOffset,
			// 		a.bufferOffset,
			// 		a.duration.type === "just" ? a.duration.value : undefined
			// 	);
			// }
			// state.units[ptr].onOff = oo;
		};
	};
};

// recorder
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
			mConnectXToY_(ptr)(aa.parent)(state)();
		};
	};
};

// sawtooth osc
exports.makeSawtoothOsc_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			var onOff = a.onOff;
			var createClosure = function (context, i) {
				return new OscillatorNode(context, i);
			};
			var resume = { frequency: a.frequency, type: "sawtooth" };
			state.units[ptr] = {
				//outgoing: [a.parent],
				outgoing: [],
				incoming: [],
				resume: resume,
				createClosure: createClosure,
				main: createClosure(state.context, resume),
			};
			mConnectXToY_(ptr)(a.parent)(state)();
			// var oo = isOn(onOff.onOff);
			// if (oo) {
			// 	state.units[ptr].main.start(
			// 		state.deprecatedWriteHead + onOff.timeOffset
			// 	);
			// }
			// state.units[ptr].onOff = oo;
		};
	};
};

// sine osc
exports.makeSinOsc_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			var onOff = a.onOff;
			var createClosure = function (context, i) {
				return new OscillatorNode(context, i);
			};
			var resume = { frequency: a.frequency, type: "sine" };
			state.units[ptr] = {
				//outgoing: [a.parent],
				outgoing: [],
				incoming: [],
				resume: resume,
				createClosure: createClosure,
				main: createClosure(state.context, resume),
			};
			mConnectXToY_(ptr)(a.parent)(state)();
			// var oo = isOn(onOff.onOff);
			// if (oo) {
			// 	state.units[ptr].main.start(
			// 		state.deprecatedWriteHead + onOff.timeOffset
			// 	);
			// }
			// state.units[ptr].onOff = oo;
		};
	};
};
exports.makeSubgraph_ = function (ptr) {
	return function (parent) {
		return function (sceneM) {
			return function (state) {
				return function () {
					var children = {};
					var pushers = {};
					var unsu = {};
					state.units[ptr] = {
						parent: parent,
						sceneM: sceneM,
						pushers: pushers,
						children: children,
						unsu: unsu,
					};
				};
			};
		};
	};
};

exports.removeSubgraph_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			var j = a.pos;
			var children = state.units[ptr].children;
			var unsu = state.units[ptr].unsu;
			if (children[j] === undefined) {
				return;
			}
			for (var k = 0; k < children[j].terminalPtrs.length; k++) {
				disconnectXFromY_(children[j].terminalPtrs[k])(state.units[ptr].parent)(
					children[j]
				)();
			}
			// unsubscribe
			unsu[j]();
			// delete unused
			delete children[j];
			delete unsu[j];
		};
	};
};

exports.insertOrUpdateSubgraph_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			var env = a.env;
			var j = a.pos;
			var index = a.index;
			var children = state.units[ptr].children;
			var unsu = state.units[ptr].unsu;
			var pushers = state.units[ptr].pushers;
			var needsConnecting = false;
			if (env !== null && unsu[j] === undefined) {
				children[j] = {
					units: {},
					terminus: state.units[ptr].parent,
					unqidfr: makeid(10),
					parent: ptr,
					terminalPtrs: [],
					context: state.context,
					deprecatedWriteHead: state.deprecatedWriteHead,
				};
				children[j].units[state.units[ptr].parent] =
					state.units[state.units[ptr].parent];
				var sg = state.units[ptr].sceneM(index)();
				unsu[j] = sg.actualized(
					(
						(jIs) => (instr) => () =>
							instr(children[jIs])()
					)(j)
				)();
				pushers[j] = sg.pusher;
				needsConnecting = true;
			}
			pushers[j](env)();
			if (needsConnecting) {
				for (var k = 0; k < children[j].terminalPtrs.length; k++) {
					connectXToY_(children[j].terminalPtrs[k])(state.units[ptr].parent)(
						children[j]
					)();
				}
			}
		};
	};
};
// make speaker
exports.makeSpeaker_ = function (a) {
	return function (state) {
		return function () {
			state.units[a.id] = {
				outgoing: [],
				incoming: [],
				main: state.context.createGain(),
				se: state.context.destination,
			};
		};
	};
};
// pan
exports.makeStereoPanner_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			state.units[ptr] = {
				outgoing: [a.parent],
				incoming: [],
				main: new StereoPannerNode(state.context, {
					pan: a.pan,
				}),
			};
			mConnectXToY_(ptr)(a.parent)(state)();
		};
	};
};

// square osc
exports.makeSquareOsc_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			var onOff = a.onOff;
			var createClosure = function (context, i) {
				return new OscillatorNode(context, i);
			};
			var resume = { frequency: a.frequency, type: "square" };
			state.units[ptr] = {
				//outgoing: [a.parent],
				outgoing: [],
				incoming: [],
				resume: resume,
				createClosure: createClosure,
				main: createClosure(state.context, resume),
			};
			mConnectXToY_(ptr)(a.parent)(state)();
			// var oo = isOn(onOff.onOff);
			// if (oo) {
			// 	state.units[ptr].main.start(
			// 		state.deprecatedWriteHead + onOff.timeOffset
			// 	);
			// }
			// state.units[ptr].onOff = oo;
		};
	};
};

// triangle osc
exports.makeTriangleOsc_ = function (a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			var onOff = a.onOff;
			var createClosure = function (context, i) {
				return new OscillatorNode(context, i);
			};
			var resume = { frequency: a.frequency, type: "triangle" };
			state.units[ptr] = {
				//outgoing: [a.parent],
				outgoing: [],
				incoming: [],
				resume: resume,
				createClosure: createClosure,
				main: createClosure(state.context, resume),
			};
			mConnectXToY_(ptr)(a.parent)(state)();
			// var oo = isOn(onOff.onOff);
			// if (oo) {
			// 	state.units[ptr].main.start(
			// 		state.deprecatedWriteHead + onOff.timeOffset
			// 	);
			// }
			// state.units[ptr].onOff = oo;
		};
	};
};
// wave shaper
exports.makeWaveShaper_ = function (aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var a = aa.curve;
			var b = aa.oversample;
			state.units[ptr] = {
				outgoing: [a.parent],
				incoming: [],
				main: new WaveShaperNode(state.context, {
					curve: a,
					oversample: b.type,
				}),
			};
			mConnectXToY_(ptr)(a.parent)(state)();
		};
	};
};

// set analyser

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

// recorder

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

// waveshaper curve
exports.setWaveShaperCurve_ = function (aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var a = aa.curve;
			state.units[ptr].main.curve = a;
		};
	};
};
exports.setAudioWorkletParameter_ = function (aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var a = aa.paramName;
			var b = aa.paramValue;
			workletSetter(state.units[ptr].main, a, state.deprecatedWriteHead, b);
		};
	};
};
exports.setGain_ = function (aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var a = aa.gain;
			genericSetter(
				state.units[ptr].main,
				"gain",
				state.deprecatedWriteHead,
				a
			);
			if (state.units[ptr].resume) {
				state.units[ptr].resume.gain = a;
			}
		};
	};
};

exports.setQ_ = function (aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var a = aa.q;
			genericSetter(state.units[ptr].main, "Q", state.deprecatedWriteHead, a);
			if (state.units[ptr].resume) {
				state.units[ptr].resume.Q = a;
			}
		};
	};
};
exports.setBuffer_ = function (aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var a = aa.buffer;
			if (state.units[ptr].resume) {
				state.units[ptr].resume.buffer = a;
			}
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
			var ptr = a.id;
			var a = aa.spec;
			if (state.units[ptr].resume) {
				state.units[ptr].resume.spec = a;
			}
		};
	};
};
exports.setPan_ = function (aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var a = aa.pan;
			genericSetter(state.units[ptr].main, "pan", state.deprecatedWriteHead, a);
			if (state.units[ptr].resume) {
				state.units[ptr].resume.pan = a;
			}
		};
	};
};
exports.setThreshold_ = function (aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var a = aa.threshold;
			genericSetter(
				state.units[ptr].main,
				"threshold",
				state.deprecatedWriteHead,
				a
			);
			if (state.units[ptr].resume) {
				state.units[ptr].resume.threshold = a;
			}
		};
	};
};
exports.setLoopStart_ = function (aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var a = aa.loopStart;
			state.units[ptr].main.loopStart = a;
			state.units[ptr].resume.loopStart = a;
		};
	};
};
exports.setLoopEnd_ = function (aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var a = aa.loopEnd;
			state.units[ptr].main.loopEnd = a;
			state.units[ptr].resume.loopEnd = a;
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
exports.setRelease_ = function (aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var a = aa.release;
			genericSetter(
				state.units[ptr].main,
				"release",
				state.deprecatedWriteHead,
				a
			);
			if (state.units[ptr].resume) {
				state.units[ptr].resume.release = a;
			}
		};
	};
};
exports.setOffset_ = function (aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var a = aa.release;
			genericSetter(
				state.units[ptr].main,
				"offset",
				state.deprecatedWriteHead,
				a
			);
			if (state.units[ptr].resume) {
				state.units[ptr].resume.offset = a;
			}
		};
	};
};

exports.setRatio_ = function (aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var a = aa.release;
			genericSetter(
				state.units[ptr].main,
				"ratio",
				state.deprecatedWriteHead,
				a
			);
			if (state.units[ptr].resume) {
				state.units[ptr].resume.ratio = a;
			}
		};
	};
};
exports.setAttack_ = function (aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var a = aa.release;
			genericSetter(
				state.units[ptr].main,
				"attack",
				state.deprecatedWriteHead,
				a
			);
			if (state.units[ptr].resume) {
				state.units[ptr].resume.attack = a;
			}
		};
	};
};
exports.setKnee_ = function (aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var a = aa.knee;
			genericSetter(
				state.units[ptr].main,
				"knee",
				state.deprecatedWriteHead,
				a
			);
			if (state.units[ptr].resume) {
				state.units[ptr].resume.knee = a;
			}
		};
	};
};
exports.setDelay_ = function (aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var a = aa.delay;
			genericSetter(
				state.units[ptr].main,
				"delayTime",
				state.deprecatedWriteHead,
				a
			);
			if (state.units[ptr].resume) {
				state.units[ptr].resume.delayTime = a;
			}
		};
	};
};
exports.setPlaybackRate_ = function (aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var a = aa.playbackRate;
			genericSetter(
				state.units[ptr].main,
				"playbackRate",
				state.deprecatedWriteHead,
				a
			);
			if (state.units[ptr].resume) {
				state.units[ptr].resume.playbackRate = a;
			}
		};
	};
};
exports.setFrequency_ = function (aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var a = aa.frequency;
			genericSetter(
				state.units[ptr].main,
				"frequency",
				state.deprecatedWriteHead,
				a
			);
			// frequency defined for some non-generators
			// so check first for existence of resumeClosure
			if (state.units[ptr].resume) {
				state.units[ptr].resume.frequency = a;
			}
		};
	};
};
///////////
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

var setOn_ = function (ptr) {
	return function (onOffInstr) {
		return function (state) {
			return function () {
				if (state.units[ptr].onOff) {
					return;
				}
				state.units[ptr].onOff = true;
				state.units[ptr].main = state.units[ptr].createClosure(
					state.context,
					state.units[ptr].resume
				);
				for (var i = 0; i < state.units[ptr].outgoing.length; i++) {
					var ogi = state.units[ptr].outgoing[i];
					state.units[ptr].main.connect(state.units[ogi].main);
					if (state.units[ogi].se) {
						state.units[ptr].main.connect(state.units[ogi].se);
					}
				}
				if (state.units[ptr].bufferOffset) {
					state.units[ptr].main.start(
						state.deprecatedWriteHead + onOffInstr.timeOffset,
						state.units[ptr].bufferOffset
					);
				} else {
					state.units[ptr].main.start(
						state.deprecatedWriteHead + onOffInstr.timeOffset
					);
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
				oldMain.stop(state.deprecatedWriteHead + onOffInstr.timeOffset);
				// defer disconnection until stop has happened
				setTimeout(() => {
					for (var i = 0; i < oldOutgoing.length; i++) {
						var oogi = oldOutgoing[i];
						try {
							oldMain.disconnect(state.units[oogi].main);
							if (state.units[oogi].se) {
								oldMain.disconnect(state.units[oogi].se);
							}
						} catch (e) {
							console.log(e);
							// fail silently, as it means the unit is no longer available, but
							// as we are disconnecting it doesn't matter
							continue;
						}
					}
				}, 1000.0 * (state.deprecatedWriteHead + onOffInstr.timeOffset + 0.2 - state.context.currentTime));
			};
		};
	};
};
///////////
// various and sundry... mostly sundry... //
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
exports.makeFFIAudioSnapshot = function (audioCtx) {
	return function () {
		return {
			context: audioCtx,
			deprecatedWriteHead: 0.0,
			units: {},
			unqidfr: makeid(10),
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

exports.contextFromSnapshot = function (snapshot) {
	return snapshot.context;
};

exports.advanceWriteHead = function (snapshot) {
	return function (deprecatedWriteHead) {
		return function () {
			snapshot.deprecatedWriteHead = deprecatedWriteHead;
		};
	};
};

exports.close = function (audioCtx) {
	return function () {
		audioCtx.close();
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
exports.getAudioClockTime = function (ctx) {
	return function () {
		return ctx.currentTime;
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
