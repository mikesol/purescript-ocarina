var NUMERIC = "numeric";
var SUDDEN = "sudden";
var UNIT = "unit";
var CANCEL = "cancel";
var NO_RAMP = "step";
var LINEAR_RAMP = "linear";
var EXPONENTIAL_RAMP = "exponential";
var ENVELOPE = "envelope";
var protoSetter = function (thingee, ctrl, param, state) {
	if (param.type === SUDDEN) {
		thingee.value = param.value.n;
	} else if (param.type === UNIT) {
		if (ctrl.id) {
			disconnectCtrl(ctrl.id, state);
		}
		state.units[param.value.i].main.connect(thingee);
		ctrl.id = param.value.i;
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
			](param.value.n, param.value.o);
		} else if (param.type === CANCEL) {
			param.value.hold
				? thingee.cancelAndHoldAtTime(param.value.o)
				: thingee.cancelScheduledValues(param.value.o);
		} else if (param.type === ENVELOPE) {
			// envelope is last option
			const tm = param.value.o;
			thingee.cancelScheduledValues(Math.max(0.0, tm));
			thingee.setValueCurveAtTime(param.value.p, tm, param.value.d);
		} else {
			throw new Error("No idea what to do with " + JSON.stringify(param));
		}
	}
};
var workletSetter = function (state, unit, paramName, controllers, param) {
	if (!controllers[paramName]) {
		controllers[paramName] = {};
	}
	return protoSetter(
		unit.parameters.get(paramName),
		controllers[paramName],
		param,
		state
	);
};
var genericSetter = function (state, unit, name, controllers, param) {
	if (!controllers[name]) {
		controllers[name] = {};
	}
	return protoSetter(unit[name], controllers[name], param, state);
};
var addToScope = function (mbe, ptr, scope$, state) {
	// todo: unhinge from internal representation of scope...
	const scope = mbe('@fan@')((x) => x)(scope$);
	if (!state.scopes[scope]) {
		state.scopes[scope] = [];
	}
	state.scopes[scope].push(ptr);
	state.units[ptr].scope = scope;
};
var doDeferredConnections = function (ptr, state) {
	if (state.toConnect[ptr]) {
		state.toConnect[ptr].forEach(function (conn) {
			if (conn.w) {
				if (state.units[conn.w]) {
					conn.f();
				} else {
					if (!state.toConnect[conn.w]) {
						state.toConnect[conn.w] = [];
					}
					state.toConnect[conn.w].push({ f: conn.f });
				}
			} else {
				conn.f();
			}
		});
		delete state.toConnect[ptr];
	}
};
var mConnectXToY_ = function (mbe, x, y$, state) {
	mbe()((y) => connectXToYInternal_(x, y, state))(y$);
};
var connectXToYInternal_ = function (x, y, state) {
	var connectF = function () {
		state.units[x].audioOutgoing.push(y);
		if (!state.units[x].pendingOn) {
			state.units[x].main.connect(state.units[y].main);
			if (state.units[y].se) {
				state.units[x].main.connect(state.units[y].se);
			}
		}
	};
	if (!state.units[x]) {
		if (!state.toConnect[x]) {
			state.toConnect[x] = [];
		}
		var conn = { f: connectF };
		if (y !== x && !state.units[y]) {
			conn.w = y;
		}
		state.toConnect[x].push(conn);
		return;
	}
	if (!state.units[y]) {
		if (!state.toConnect[y]) {
			state.toConnect[y] = [];
		}
		var conn = { f: connectF };
		if (y !== x && !state.units[x]) {
			conn.w = x;
		}
		state.toConnect[y].push(conn);
		return;
	}
	connectF();
};

export function deleteFromCache_(a) {
	return function (state) {
		return function () {
			delete state.units[a.id];
		};
	};
}

export function connectXToY_(parameters) {
	return function (state) {
		return function () {
			connectXToYInternal_(parameters["from"], parameters["to"], state);
		};
	};
}

var disconnectCtrl = function (ptr, state) {
	if (state.units[ptr].scope === "@fan@") {
		return;
	}
	const scope = state.units[ptr].scope;
	state.scopes[scope].forEach((scp) => {
		delete state.units[scp];
	});
	delete state.scopes[scope];
};

export function disconnectXFromY_(a) {
	return function (state) {
		return function () {
			var x = a.from;
			var y = a.to;
			if (!state.units[x]) {
				// Has already been cleared via other scope
				return;
			}
			state.units[x].audioOutgoing = state.units[x].audioOutgoing.filter(
				function (i) {
					return !(i === y);
				}
			);
			state.units[x].main.disconnect(state.units[y].main);
			if (state.units[y].se) {
				state.units[x].main.disconnect(state.units[y].se);
			}
			if (state.units[x].scope === "@fan@") {
				return;
			}
			const scope = state.units[x].scope;
			state.scopes[scope].forEach((scp) => {
				delete state.units[scp];
			});
			delete state.scopes[scope];
		};
	};
}

// allpass
export const makeAllpass_ = (mbe) => (a) => (state) => () => {
	var ptr = a.id;
	state.units[ptr] = {
		controllers: {},
		audioOutgoing: [],
		controlOutgoing: [],
		main: new BiquadFilterNode(state.context, {
			type: "allpass",
			Q: a.q,
			frequency: a.frequency,
		}),
	};
	addToScope(mbe, ptr, a.scope, state);
	doDeferredConnections(ptr, state);
	mConnectXToY_(mbe, ptr, a.parent, state);
};

// analyser
export const makeAnalyser_ = (mbe) => (a) => (state) => () => {
	var ptr = a.id;
	var analyserSideEffectFunction = a.cb;
	var dest = new AnalyserNode(state.context, a);
	// unsubscribe is effect unit
	var unsubscribe = analyserSideEffectFunction(dest)();
	state.units[ptr] = {
		controllers: {},
		audioOutgoing: [],
		controlOutgoing: [],
		analyserOrig: analyserSideEffectFunction,
		analyser: unsubscribe,
		main: state.context.createGain(),
		se: dest,
	};
	addToScope(mbe, ptr, a.scope, state);
	doDeferredConnections(ptr, state);
	mConnectXToY_(mbe, ptr, a.parent, state);
};

// audio worklet node
export const makeAudioWorkletNode_ = (mbe) => (a) => (state) => () => {
	var ptr = a.id;
	var opts = a.options;
	state.units[ptr] = {
		controllers: {},
		audioOutgoing: [],
		controlOutgoing: [],
		main: new AudioWorkletNode(state.context, opts.name, {
			numberOfInputs: opts.numberOfInputs,
			numberOfOutputs: opts.numberOfOutputs,
			outputChannelCount: opts.outputChannelCount,
			parameterData: opts.parameterData,
			processorOptions: opts.processorOptions,
		}),
	};
	addToScope(mbe, ptr, a.scope, state);
	doDeferredConnections(ptr, state);
	mConnectXToY_(mbe, ptr, a.parent, state);
};

// bandpass
export const makeBandpass_ = (mbe) => (a) => (state) => () => {
	var ptr = a.id;
	state.units[ptr] = {
		controllers: {},
		audioOutgoing: [],
		controlOutgoing: [],
		main: new BiquadFilterNode(state.context, {
			type: "bandpass",
			Q: a.q,
			frequency: a.frequency,
		}),
	};
	addToScope(mbe, ptr, a.scope, state);
	doDeferredConnections(ptr, state);
	mConnectXToY_(mbe, ptr, a.parent, state);
};

// constant
export const makeConstant_ = (mbe) => (a) => (state) => () => {
	var ptr = a.id;
	// var onOff = a.onOff;
	var createClosure = function (context, i) {
		return new ConstantSourceNode(context, i);
	};
	var resume = { offset: a.offset };
	state.units[ptr] = {
		controllers: {},
		audioOutgoing: [],
		controlOutgoing: [],
		resume: resume,
		createClosure: createClosure,
		onOff: false,
		pendingOn: true,
		main: createClosure(state.context, resume), // needed so that setters don't error out, even though not started yet
	};
	addToScope(mbe, ptr, a.scope, state);
	doDeferredConnections(ptr, state);
	mConnectXToY_(mbe, ptr, a.parent, state);
};

export const makeConvolver_ = (mbe) => (a) => (state) => () => {
	var ptr = a.id;
	state.units[ptr] = {
		controllers: {},
		audioOutgoing: [],
		controlOutgoing: [],
		main: new ConvolverNode(state.context, { buffer: a.buffer }),
	};
	addToScope(mbe, ptr, a.scope, state);
	doDeferredConnections(ptr, state);
	mConnectXToY_(mbe, ptr, a.parent, state);
};

// delay
export const makeDelay_ = (mbe) => (a) => (state) => () => {
	var ptr = a.id;
	state.units[ptr] = {
		controllers: {},
		audioOutgoing: [],
		controlOutgoing: [],
		main: new DelayNode(state.context, {
			delayTime: a.delayTime,
			maxDelayTime: a.maxDelayTime,
		}),
	};
	addToScope(mbe, ptr, a.scope, state);
	doDeferredConnections(ptr, state);
	mConnectXToY_(mbe, ptr, a.parent, state);
};

// dynamicsCompressor
export const makeDynamicsCompressor_ = (mbe) => (a) => (state) => () => {
	var ptr = a.id;
	state.units[ptr] = {
		controllers: {},
		audioOutgoing: [],
		controlOutgoing: [],
		main: new DynamicsCompressorNode(state.context, {
			knee: a.knee,
			ratio: a.ratio,
			threshold: a.threshold,
			attack: a.attack,
			release: a.release,
		}),
	};
	addToScope(mbe, ptr, a.scope, state);
	doDeferredConnections(ptr, state);
	mConnectXToY_(mbe, ptr, a.parent, state);
};
// gain
export const makeGain_ = (mbe) => (a) => (state) => () => {
	var ptr = a.id;
	state.units[ptr] = {
		controllers: {},
		audioOutgoing: [],
		controlOutgoing: [],
		main: new GainNode(state.context, {
			gain: a.gain,
		}),
	};
	addToScope(mbe, ptr, a.scope, state);
	doDeferredConnections(ptr, state);
	mConnectXToY_(mbe, ptr, a.parent, state);
};

// highpass
export const makeHighpass_ = (mbe) => (a) => (state) => () => {
	var ptr = a.id;
	state.units[ptr] = {
		controllers: {},
		audioOutgoing: [],
		controlOutgoing: [],
		main: new BiquadFilterNode(state.context, {
			type: "highpass",
			Q: a.q,
			frequency: a.frequency,
		}),
	};
	addToScope(mbe, ptr, a.scope, state);
	doDeferredConnections(ptr, state);
	mConnectXToY_(mbe, ptr, a.parent, state);
};

// highshelf
export const makeHighshelf_ = (mbe) => (a) => (state) => () => {
	var ptr = a.id;
	state.units[ptr] = {
		controllers: {},
		audioOutgoing: [],
		controlOutgoing: [],
		main: new BiquadFilterNode(state.context, {
			type: "highshelf",
			frequency: a.frequency,
			gain: a.gain,
		}),
	};
	addToScope(mbe, ptr, a.scope, state);
	doDeferredConnections(ptr, state);
	mConnectXToY_(mbe, ptr, a.parent, state);
};

export const makeIIRFilter_ = (mbe) => (a) => (state) => () => {
	var ptr = a.id;
	state.units[ptr] = {
		controllers: {},
		audioOutgoing: [],
		controlOutgoing: [],
		main: new IIRFilterNode(state.context, {
			feedforward: a.feedforward,
			feedback: a.feedback,
		}),
	};
	addToScope(mbe, ptr, a.scope, state);
	doDeferredConnections(ptr, state);
	mConnectXToY_(mbe, ptr, a.parent, state);
};

// loopBuf
export const makeLoopBuf_ = (mbe) => (a) => (state) => () => {
	var ptr = a.id;
	// var onOff = a.onOff;
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
		controllers: {},
		audioOutgoing: [],
		controlOutgoing: [],
		resume: resume,
		createClosure: createClosure,
		onOff: false,
		pendingOn: true,
		main: createClosure(state.context, resume), // needed so that setters don't error out, even though not started yet
	};
	addToScope(mbe, ptr, a.scope, state);
	doDeferredConnections(ptr, state);
	mConnectXToY_(mbe, ptr, a.parent, state);
};

// lowpass
export const makeLowpass_ = (mbe) => (a) => (state) => () => {
	var ptr = a.id;
	state.units[ptr] = {
		controllers: {},
		audioOutgoing: [],
		controlOutgoing: [],
		main: new BiquadFilterNode(state.context, {
			type: "lowpass",
			Q: a.q,
			frequency: a.frequency,
		}),
	};
	addToScope(mbe, ptr, a.scope, state);
	doDeferredConnections(ptr, state);
	mConnectXToY_(mbe, ptr, a.parent, state);
};

// lowshelf
export const makeLowshelf_ = (mbe) => (a) => (state) => () => {
	var ptr = a.id;
	state.units[ptr] = {
		controllers: {},
		audioOutgoing: [],
		controlOutgoing: [],
		main: new BiquadFilterNode(state.context, {
			type: "lowshelf",
			frequency: a.frequency,
			gain: a.gain,
		}),
	};
	addToScope(mbe, ptr, a.scope, state);
	doDeferredConnections(ptr, state);
	mConnectXToY_(mbe, ptr, a.parent, state);
};

// media element

export const makeMediaElement_ = (mbe) => (a) => (state) => () => {
	var ptr = a.id;
	var elt = a.element;
	var createClosure = function () {
		var unit = state.context.createMediaElementSource(elt);
		return unit;
	};
	state.units[ptr] = {
		controllers: {},
		audioOutgoing: [],
		controlOutgoing: [],
		createClosure: createClosure,
		resumeClosure: {},
		main: createClosure(),
	};
	addToScope(mbe, ptr, a.scope, state);
	doDeferredConnections(ptr, state);
	mConnectXToY_(mbe, ptr, a.parent, state);
};
// microphone
export const makeMicrophone_ = (mbe) => (a) => (state) => () => {
	var ptr = a.id;
	state.units[a.id] = {
		main: state.context.createMediaStreamSource(a.microphone),
		controllers: {},
		audioOutgoing: [],
		controlOutgoing: [],
	};
	addToScope(mbe, ptr, a.scope, state);
	doDeferredConnections(ptr, state);
	mConnectXToY_(mbe, ptr, a.parent, state);
};

// notch
export const makeNotch_ = (mbe) => (a) => (state) => () => {
	var ptr = a.id;
	state.units[ptr] = {
		controllers: {},
		audioOutgoing: [],
		controlOutgoing: [],
		main: new BiquadFilterNode(state.context, {
			type: "notch",
			frequency: a.frequency,
			Q: a.q,
		}),
	};
	addToScope(mbe, ptr, a.scope, state);
	doDeferredConnections(ptr, state);
	mConnectXToY_(mbe, ptr, a.parent, state);
};

// peaking
export const makePeaking_ = (mbe) => (a) => (state) => () => {
	var ptr = a.id;
	state.units[ptr] = {
		controllers: {},
		audioOutgoing: [],
		controlOutgoing: [],
		main: new BiquadFilterNode(state.context, {
			type: "peaking",
			frequency: a.frequency,
			Q: a.q,
			gain: a.gain,
		}),
	};
	addToScope(mbe, ptr, a.scope, state);
	doDeferredConnections(ptr, state);
	mConnectXToY_(mbe, ptr, a.parent, state);
};

// periodic osc
export const makePeriodicOsc_ = (mbe) => (a) => (state) => () => {
	var ptr = a.id;
	// var onOff = a.onOff;
	var createClosure = function (context, i) {
		var opts = {
			frequency: i.frequency,
			periodicWave:
				i.spec.type === "wave"
					? i.spec.value
					: makePeriodicWaveImpl(state.context)(i.spec.value.real)(
							i.spec.value.img
					  )(),
		};
		var o = new OscillatorNode(context, opts);
		return o;
	};
	var resume = { frequency: a.frequency, type: "custom", spec: a.spec };
	state.units[ptr] = {
		controllers: {},
		audioOutgoing: [],
		controlOutgoing: [],
		resume: resume,
		createClosure: createClosure,
		onOff: false,
		pendingOn: true,
		main: createClosure(state.context, resume), // needed so that setters don't error out, even though not started yet
	};
	addToScope(mbe, ptr, a.scope, state);
	doDeferredConnections(ptr, state);
	mConnectXToY_(mbe, ptr, a.parent, state);
};

// playBuf
export const makePlayBuf_ = (mbe) => (a) => (state) => () => {
	var ptr = a.id;
	// var onOff = a.onOff;
	var createClosure = function (context, i) {
		var opts = {
			loop: i.loop,
			buffer: i.buffer,
			playbackRate: i.playbackRate,
		};
		return new AudioBufferSourceNode(context, opts);
	};
	var resume = {
		loop: false,
		buffer: a.buffer,
		playbackRate: a.playbackRate,
		bufferOffset: a.bufferOffset,
		duration: mbe(undefined)((x) => x)(a.duration),
	};
	state.units[ptr] = {
		controllers: {},
		audioOutgoing: [],
		controlOutgoing: [],
		resume: resume,
		createClosure: createClosure,
		onOff: false,
		pendingOn: true,
		main: createClosure(state.context, resume), // needed so that setters don't error out, even though not started yet
	};
	addToScope(mbe, ptr, a.scope, state);
	doDeferredConnections(ptr, state);
	mConnectXToY_(mbe, ptr, a.parent, state);
};

// recorder
export const makeRecorder_ = (mbe) => (a) => (state) => () => {
	var ptr = a.id;
	var mediaRecorderSideEffectFn = a.cb;
	var dest = state.context.createMediaStreamDestination();
	var mediaRecorder = new MediaRecorder(dest.stream);
	mediaRecorderSideEffectFn(mediaRecorder)();
	mediaRecorder.start();
	state.units[ptr] = {
		controllers: {},
		audioOutgoing: [],
		controlOutgoing: [],
		recorderOrig: mediaRecorderSideEffectFn,
		recorder: mediaRecorder,
		main: state.context.createGain(),
		se: dest,
	};
	addToScope(mbe, ptr, a.scope, state);
	doDeferredConnections(ptr, state);
	mConnectXToY_(mbe, ptr, a.parent, state);
};

// sawtooth osc
export const makeSawtoothOsc_ = (mbe) => (a) => (state) => () => {
	var ptr = a.id;
	// var onOff = a.onOff;
	var createClosure = function (context, i) {
		return new OscillatorNode(context, i);
	};
	var resume = { frequency: a.frequency, type: "sawtooth" };
	state.units[ptr] = {
		controllers: {},
		audioOutgoing: [],
		controlOutgoing: [],
		resume: resume,
		createClosure: createClosure,
		onOff: false,
		pendingOn: true,
		main: createClosure(state.context, resume), // needed so that setters don't error out, even though not started yet
	};
	addToScope(mbe, ptr, a.scope, state);
	doDeferredConnections(ptr, state);
	mConnectXToY_(mbe, ptr, a.parent, state);
};

// sine osc
export const makeSinOsc_ = (mbe) => (a) => (state) => () => {
	var ptr = a.id;
	// var onOff = a.onOff;
	var createClosure = function (context, i) {
		return new OscillatorNode(context, i);
	};
	var resume = { frequency: a.frequency, type: "sine" };
	state.units[ptr] = {
		controllers: {},
		audioOutgoing: [],
		controlOutgoing: [],
		resume: resume,
		createClosure: createClosure,
		onOff: false,
		pendingOn: true,
		main: createClosure(state.context, resume), // needed so that setters don't error out, even though not started yet
	};
	addToScope(mbe, ptr, a.scope, state);
	doDeferredConnections(ptr, state);
	mConnectXToY_(mbe, ptr, a.parent, state);
};

// make speaker
export const makeSpeaker_ = (a) => (state) => () => {
	state.units[a.id] = {
		controllers: {},
		audioOutgoing: [],
		controlOutgoing: [],
		main: state.context.createGain(),
		se: state.context.destination,
	};
};

// pan
export const makeStereoPanner_ = (mbe) => (a) => (state) => () => {
	var ptr = a.id;
	state.units[ptr] = {
		controllers: {},
		audioOutgoing: [],
		controlOutgoing: [],
		main: new StereoPannerNode(state.context, {
			pan: a.pan,
		}),
	};
	addToScope(mbe, ptr, a.scope, state);
	doDeferredConnections(ptr, state);
	mConnectXToY_(mbe, ptr, a.parent, state);
};

// square osc
export const makeSquareOsc_ = (mbe) => (a) => (state) => () => {
	var ptr = a.id;
	// var onOff = a.onOff;
	var createClosure = function (context, i) {
		return new OscillatorNode(context, i);
	};
	var resume = { frequency: a.frequency, type: "square" };
	state.units[ptr] = {
		controllers: {},
		audioOutgoing: [],
		controlOutgoing: [],
		resume: resume,
		createClosure: createClosure,
		onOff: false,
		pendingOn: true,
		main: createClosure(state.context, resume), // needed so that setters don't error out, even though not started yet
	};
	addToScope(mbe, ptr, a.scope, state);
	doDeferredConnections(ptr, state);
	mConnectXToY_(mbe, ptr, a.parent, state);
};

// triangle osc
export const makeTriangleOsc_ = (mbe) => (a) => (state) => () => {
	var ptr = a.id;
	// var onOff = a.onOff;
	var createClosure = function (context, i) {
		return new OscillatorNode(context, i);
	};
	var resume = { frequency: a.frequency, type: "triangle" };
	state.units[ptr] = {
		controllers: {},
		audioOutgoing: [],
		controlOutgoing: [],
		resume: resume,
		createClosure: createClosure,
		onOff: false,
		pendingOn: true,
		main: createClosure(state.context, resume), // needed so that setters don't error out, even though not started yet
	};
	addToScope(mbe, ptr, a.scope, state);
	doDeferredConnections(ptr, state);
	mConnectXToY_(mbe, ptr, a.parent, state);
};

// wave shaper
export const makeWaveShaper_ = (mbe) => (a) => (state) => () => {
	var ptr = a.id;
	var curve = a.curve;
	var b = a.oversample;
	state.units[ptr] = {
		controllers: {},
		audioOutgoing: [],
		controlOutgoing: [],
		main: new WaveShaperNode(state.context, {
			curve: curve,
			oversample: b.type,
		}),
	};
	addToScope(mbe, ptr, a.scope, state);
	doDeferredConnections(ptr, state);
	mConnectXToY_(mbe, ptr, a.parent, state);
};

// set analyser

export function setAnalyserNodeCb_(aa) {
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
}

// recorder

// setting makes us stop the previous one if it exists
export function setMediaRecorderCb_(aa) {
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
}

// waveshaper curve
export function setWaveShaperCurve_(aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var a = aa.curve;
			state.units[ptr].main.curve = a;
		};
	};
}

export function setAudioWorkletParameter_(aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var a = aa.paramName;
			var b = aa.paramValue;
			workletSetter(
				state,
				state.units[ptr].main,
				a,
				state.units[ptr].controllers,
				b
			);
		};
	};
}

const recalcResume = function (a, u, v) {
	if (u.resume) {
		if (a.value.n !== undefined) {
			u.resume[v] = a.value.n;
		}
	}
};

export function setGain_(aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var a = aa.gain;
			genericSetter(
				state,
				state.units[ptr].main,
				"gain",
				state.units[ptr].controllers,
				a
			);
			recalcResume(a, state.units[ptr], "gain");
		};
	};
}

export function setQ_(aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var a = aa.q;
			genericSetter(
				state,
				state.units[ptr].main,
				"Q",
				state.units[ptr].controllers,
				a
			);
			recalcResume(a, state.units[ptr], "Q");
		};
	};
}

export function setBuffer_(aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var a = aa.buffer;
			if (state.units[ptr].resume) {
				state.units[ptr].resume.buffer = a;
			}
		};
	};
}

export function setConvolverBuffer_(aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var buffer = aa.buffer;
			state.units[ptr].main.buffer = buffer;
		};
	};
}

export function setPeriodicOsc_(aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var a = aa.spec;
			if (state.units[ptr].resume) {
				state.units[ptr].resume.spec = a;
			}
		};
	};
}

export function setPan_(aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var a = aa.pan;
			genericSetter(
				state,
				state.units[ptr].main,
				"pan",
				state.units[ptr].controllers,
				a
			);
			recalcResume(a, state.units[ptr], "pan");
		};
	};
}

export function setThreshold_(aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var a = aa.threshold;
			genericSetter(
				state,
				state.units[ptr].main,
				"threshold",
				state.units[ptr].controllers,
				a
			);
			recalcResume(a, state.units[ptr], "threshold");
		};
	};
}

export function setLoopStart_(aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var a = aa.loopStart;
			state.units[ptr].main.loopStart = a;
			state.units[ptr].resume.loopStart = a;
		};
	};
}

export function setLoopEnd_(aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var a = aa.loopEnd;
			state.units[ptr].main.loopEnd = a;
			state.units[ptr].resume.loopEnd = a;
		};
	};
}

export function setBufferOffset_(aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var a = aa.bufferOffset;
			state.units[ptr].resume.bufferOffset = a;
		};
	};
}

export function setDuration_(mbe) {
	return function (aa) {
		return function (state) {
			return function () {
				var ptr = aa.id;
				var a = aa.duration;
				state.units[ptr].duration = mbe(undefined)((x) => x)(a);
			};
		};
	};
}

export function setRelease_(aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var a = aa.release;
			genericSetter(
				state,
				state.units[ptr].main,
				"release",
				state.units[ptr].controllers,
				a
			);
			recalcResume(a, state.units[ptr], "release");
		};
	};
}

export function setOffset_(aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var a = aa.offset;
			genericSetter(
				state,
				state.units[ptr].main,
				"offset",
				state.units[ptr].controllers,
				a
			);
			recalcResume(a, state.units[ptr], "offset");
		};
	};
}

export function setRatio_(aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var a = aa.ratio;
			genericSetter(
				state,
				state.units[ptr].main,
				"ratio",
				state.units[ptr].controllers,
				a
			);
			recalcResume(a, state.units[ptr], "ratio");
		};
	};
}

export function setAttack_(aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var a = aa.attack;
			genericSetter(
				state,
				state.units[ptr].main,
				"attack",
				state.units[ptr].controllers,
				a
			);
			recalcResume(a, state.units[ptr], "attack");
		};
	};
}

export function setKnee_(aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var a = aa.knee;
			genericSetter(
				state,
				state.units[ptr].main,
				"knee",
				state.units[ptr].controllers,
				a
			);
			recalcResume(a, state.units[ptr], "knee");
		};
	};
}

export function setDelay_(aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var a = aa.delayTime;
			genericSetter(
				state,
				state.units[ptr].main,
				"delayTime",
				state.units[ptr].controllers,
				a
			);
			recalcResume(a, state.units[ptr], "delayTime");
		};
	};
}

export function setPlaybackRate_(aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var a = aa.playbackRate;
			genericSetter(
				state,
				state.units[ptr].main,
				"playbackRate",
				state.units[ptr].controllers,
				a
			);
			recalcResume(a, state.units[ptr], "playbackRate");
		};
	};
}

export function setFrequency_(aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var a = aa.frequency;
			genericSetter(
				state,
				state.units[ptr].main,
				"frequency",
				state.units[ptr].controllers,
				a
			);
			recalcResume(a, state.units[ptr], "frequency");
		};
	};
}

///////////
export function setOnOff_(aa) {
	return function (state) {
		return function () {
			var ptr = aa.id;
			var onOff = aa.onOff;
			if (onOff.x.type === "on") {
				setOn_(ptr)(onOff)(state)();
			} else if (onOff.x.type === "off") {
				setOff_(ptr)(onOff)(state)();
			}
		};
	};
}

var setOn_ = function (ptr) {
	return function (onOffInstr) {
		return function (state) {
			return function () {
				if (state.units[ptr].onOff) {
					return;
				}
				state.units[ptr].pendingOn = false;
				state.units[ptr].onOff = true;
				state.units[ptr].main = state.units[ptr].createClosure(
					state.context,
					state.units[ptr].resume
				);
				for (var i = 0; i < state.units[ptr].audioOutgoing.length; i++) {
					var ogi = state.units[ptr].audioOutgoing[i];
					state.units[ptr].main.connect(state.units[ogi].main);
					if (state.units[ogi].se) {
						state.units[ptr].main.connect(state.units[ogi].se);
					}
				}
				if (state.units[ptr].resume && state.units[ptr].resume.bufferOffset) {
					if (typeof state.units[ptr].resume.duration === "number") {
						state.units[ptr].main.start(
							state.deprecatedWriteHead + onOffInstr.o,
							state.units[ptr].resume.bufferOffset,
							state.units[ptr].resume.duration
						);
					} else {
						state.units[ptr].main.start(
							state.deprecatedWriteHead + onOffInstr.o,
							state.units[ptr].resume.bufferOffset
						);
					}
				} else if (
					state.units[ptr].resume &&
					state.units[ptr].resume.loopStart
				) {
					state.units[ptr].main.start(
						state.deprecatedWriteHead + onOffInstr.o,
						state.units[ptr].resume.loopStart
					);
				} else {
					state.units[ptr].main.start(state.deprecatedWriteHead + onOffInstr.o);
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
				// defer disconnection until stop has happened
				oldMain.addEventListener("ended", () => {
					oldMain.disconnect();
				});
				oldMain.stop(state.deprecatedWriteHead + onOffInstr.o);
			};
		};
	};
};

///////////
// various and sundry... mostly sundry... //
export function makeFloatArray(fa) {
	var r = new Float32Array(fa.length);
	for (var i = 0; i < fa.length; i++) {
		r[i] = fa[i];
	}
	return r;
}

export function stopMediaRecorder(mediaRecorder) {
	return function () {
		mediaRecorder.stop();
	};
}

export function isTypeSupported(mimeType) {
	return function () {
		return MediaRecorder.isTypeSupported(mimeType);
	};
}

// currently, there is no unsubscription logic to the media recorder
// in the case where a second subscriber is called, it will simply
// overwrite the first subscriber
// because of this, care needs to be taken in calling the "setMediaRecorderCb" function
// it will unset the previous one, which will result in the recording starting from the moment
// of being set
// if it is set in a loop, then there will effectively be no recording, as it will only capture the
// last couple milliseconds of the loop
export function mediaRecorderToBlob(mimeType) {
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
}

export function getBrowserMediaStreamImpl(audio) {
	return function (video) {
		return function () {
			return navigator.mediaDevices.getUserMedia({
				audio: audio,
				video: video,
			});
		};
	};
}

export function getFFTSize(analyserNode) {
	return function () {
		return analyserNode.fftSize;
	};
}

export function setFFTSize(analyserNode) {
	return function (fftSize) {
		return function () {
			analyserNode.fftSize = fftSize;
		};
	};
}

export function getSmoothingTimeConstant(analyserNode) {
	return function () {
		return analyserNode.smoothingTimeConstant;
	};
}

export function setSmoothingTimeConstant(analyserNode) {
	return function (smoothingTimeConstant) {
		return function () {
			analyserNode.smoothingTimeConstant = smoothingTimeConstant;
		};
	};
}

export function getMinDecibels(analyserNode) {
	return function () {
		return analyserNode.minDecibels;
	};
}

export function setMinDecibels(analyserNode) {
	return function (minDecibels) {
		return function () {
			analyserNode.minDecibels = minDecibels;
		};
	};
}

export function getMaxDecibels(analyserNode) {
	return function () {
		return analyserNode.maxDecibels;
	};
}

export function setMaxDecibels(analyserNode) {
	return function (maxDecibels) {
		return function () {
			analyserNode.maxDecibels = maxDecibels;
		};
	};
}

export function getFrequencyBinCount(analyserNode) {
	return function () {
		return analyserNode.frequencyBinCount;
	};
}

// https://developer.mozilla.org/en-US/docs/Web/API/AnalyserNode/getFloatTimeDomainData
export function getFloatTimeDomainData(analyserNode) {
	return function () {
		var dataArray = new Float32Array(analyserNode.fftSize);
		analyserNode.getFloatTimeDomainData(dataArray);
		return dataArray;
	};
}

// https://developer.mozilla.org/en-US/docs/Web/API/AnalyserNode/getFloatFrequencyData
export function getFloatFrequencyData(analyserNode) {
	return function () {
		var dataArray = new Float32Array(analyserNode.frequencyBinCount);
		analyserNode.getFloatFrequencyData(dataArray);
		return dataArray;
	};
}

// https://developer.mozilla.org/en-US/docs/Web/API/AnalyserNode/getByteTimeDomainData
export function getByteTimeDomainData(analyserNode) {
	return function () {
		var dataArray = new Uint8Array(analyserNode.fftSize);
		analyserNode.getByteTimeDomainData(dataArray);
		return dataArray;
	};
}

// https://developer.mozilla.org/en-US/docs/Web/API/AnalyserNode/getByteFrequencyData
export function getByteFrequencyData(analyserNode) {
	return function () {
		var dataArray = new Uint8Array(analyserNode.frequencyBinCount);
		analyserNode.getByteFrequencyData(dataArray);
		return dataArray;
	};
}

export function bufferSampleRate(buffer) {
	return buffer.sampleRate;
}

export function bufferLength(buffer) {
	return buffer.length;
}

export function bufferDuration(buffer) {
	return buffer.duration;
}

export function bufferNumberOfChannels(buffer) {
	return buffer.numberOfChannels;
}

export function constant0Hack_(context) {
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
}

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
export { makePeriodicWaveImpl };

export function makeFFIAudioSnapshot(audioCtx) {
	return function () {
		return {
			context: audioCtx,
			deprecatedWriteHead: 0.0,
			units: {},
			scopes: {},
			unsu: {},
			toConnect: {},
		};
	};
}

export function audioWorkletAddModule_(ctx) {
	return function (s) {
		return function () {
			{
				return ctx.audioWorklet.addModule(s);
			}
		};
	};
}

export function contextFromSnapshot(snapshot) {
	return snapshot.context;
}

export function advanceWriteHead(snapshot) {
	return function (deprecatedWriteHead) {
		return function () {
			snapshot.deprecatedWriteHead = deprecatedWriteHead;
		};
	};
}

export function close_(audioCtx) {
	return function () {
		audioCtx.close();
	};
}

export function decodeAudioDataFromBase64EncodedString(ctx) {
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
}

export function fetchArrayBuffer(s) {
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
}

export function decodeAudioDataFromArrayBuffer(ctx) {
	return function (b) {
		return function () {
			return ctx.decodeAudioData(b);
		};
	};
}

export function context_() {
	return new (window.AudioContext || window.webkitAudioContext)();
}

export function contextState_(audioCtx) {
	return function () {
		return audioCtx.state;
	};
}

export function contextResume_(audioCtx) {
	return function () {
		return audioCtx.resume();
	};
}

export function getAudioClockTime(ctx) {
	return function () {
		return ctx.currentTime;
	};
}

export function makeAudioBuffer_(ctx) {
	return function (b) {
		return function () {
			var myArrayBuffer = ctx.createBuffer(
				b.arr.length,
				b.arr[0].length,
				b.size
			);
			for (
				var channel = 0;
				channel < myArrayBuffer.numberOfChannels;
				channel++
			) {
				var nowBuffering = myArrayBuffer.getChannelData(channel);
				for (var i = 0; i < myArrayBuffer.length; i++) {
					nowBuffering[i] = b.arr[channel][i];
				}
			}
			return myArrayBuffer;
		};
	};
}
