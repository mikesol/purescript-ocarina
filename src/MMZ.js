exports.actualizeMMZ_ = function (mmz) {
	return function () {
		if (!mmz.ctx.start.actualized) {
			mmz.ctx.start.actualized = true;
			mmz.ctx.start.event()();
		}
	};
};
exports.mmzStart_ = function (e) {
	const next = {
		type: "start",
		value: undefined,
		cbs: [],
		next: [],
		memo: undefined,
		actualized: false,
		event: e,
		ctx: [],
	};
	next.ctx.push(next);
	next.ctx.start = next;
	return next;
};
exports.mmzMap = function (ab) {
	return function (fa) {
		const next = {
			type: "map",
			value: { ab: ab, fa: fa },
			cbs: [],
			next: [],
			memo: undefined,
			ctx: fa.ctx,
		};
		fa.ctx.push(next);
		fa.next.push(next);
		return next;
	};
};
exports.mmzAlt = function (fa) {
	return function (fb) {
		const next = {
			type: "alt",
			value: { fa: fa, fb: fb },
			cbs: [],
			next: [],
			memo: undefined,
			ctx: fa.ctx,
		};
		fa.ctx.push(next);
		fa.next.push(next);
		fb.next.push(next);
		return next;
	};
};
exports.mmzEmpty = {
	type: "empty",
	cbs: [],
	next: [],
	memo: [],
	resolving: true,
};
exports.mmzApply = function (fab) {
	return function (fa) {
		const next = {
			type: "apply",
			value: { fab: fab, fa: fa },
			cbs: [],
			next: [],
			memo: undefined,
			ctx: fa.ctx,
		};
		fa.ctx.push(next);
		fa.next.push(next);
		fab.next.push(next);
		return next;
	};
};
exports.mmzPartitionMap = function (aelr) {
	return function (fa) {
		const left = {
			type: "partitionMapLeft",
			value: { aelr: aelr, fa: fa },
			cbs: [],
			next: [],
			memo: undefined,
			ctx: fa.ctx,
		};
		const right = {
			type: "partitionMapRight",
			value: { aelr: aelr, fa: fa },
			cbs: [],
			next: [],
			memo: undefined,
			ctx: fa.ctx,
		};
		fa.ctx.push(left);
		fa.ctx.push(right);
		fa.next.push(left);
		fa.next.push(right);
		return { left: left, right: right };
	};
};
exports.mmzFold = function (abb) {
	return function (fa) {
		return function (b) {
			const next = {
				type: "fold",
				value: { abb: abb, fa: fa, b: b },
				cbs: [],
				next: [],
				memo: undefined,
				ctx: fa.ctx,
			};
			fa.ctx.push(next);
			fa.next.push(next);
			return next;
		};
	};
};

const runMMZInternal = function (opts, mmz) {
	if (mmz.resolving === false) {
		mmz.resolving = true;
		if (mmz.type === "start") {
			mmz.memo = [mmz.value];
		} else if (mmz.type === "map") {
			runMMZInternal(opts, mmz.value.fa);
			mmz.memo = [];
			for (var i = 0; i < mmz.value.fa.memo.length; i++) {
				mmz.memo.push(mmz.value.ab(mmz.value.fa.memo[i]));
			}
		} else if (mmz.type === "apply") {
			runMMZInternal(opts, mmz.value.fab);
			runMMZInternal(opts, mmz.value.fa);
			mmz.memo = [];
			for (var i = 0; i < mmz.value.fab.memo.length; i++) {
				for (var j = 0; j < mmz.value.fa.memo.length; j++) {
					mmz.memo.push(mmz.value.fab.memo[i](mmz.value.fa.memo[j]));
				}
			}
		} else if (mmz.type === "alt") {
			runMMZInternal(opts, mmz.value.fa);
			runMMZInternal(opts, mmz.value.fb);
			mmz.memo = [];
			for (var i = 0; i < mmz.value.fa.memo.length; i++) {
				mmz.memo.push(mmz.value.fa.memo[i]);
			}
			for (var i = 0; i < mmz.value.fb.memo.length; i++) {
				mmz.memo.push(mmz.value.fb.memo[i]);
			}
		} else if (mmz.type === "empty") {
			mmz.memo = [];
		} else if (mmz.type === "partitionMapLeft") {
			runMMZInternal(opts, mmz.value.fa);
			var l = [];
			var e = opts.either((x) => l.push(x))(() => {});
			for (var i = 0; i < mmz.value.fa.memo.length; i++) {
				e(mmz.value.aelr(mmz.value.fa.memo[i]));
			}
			mmz.memo = l;
		} else if (mmz.type === "partitionMapRight") {
			runMMZInternal(opts, mmz.value.fa);
			var r = [];
			var e = opts.either(() => {})((x) => r.push(x));
			for (var i = 0; i < mmz.value.fa.memo.length; i++) {
				e(mmz.value.aelr(mmz.value.fa.memo[i]));
			}
			mmz.memo = r;
		} else if (mmz.type === "fold") {
			runMMZInternal(opts, mmz.value.fa);
			mmz.memo = [];
			var b = mmz.value.b;
			for (var i = 0; i < mmz.value.fa.memo.length; i++) {
				b = mmz.value.abb(mmz.value.fa.memo[i])(b);
				mmz.memo.push(b);
			}
			mmz.value.b = b;
		} else {
			throw new Error("Failed pattern match on mmz", mmz);
		}
		if (mmz.type !== "empty") {
			for (var i = 0; i < mmz.cbs.length; i++) {
				for (var j = 0; j < mmz.memo.length; j++) {
					mmz.cbs[i](mmz.memo[j])();
				}
			}
			for (var i = 0; i < mmz.next.length; i++) {
				runMMZInternal(opts, mmz.next[i]);
			}
		}
	}
};
exports.addSubscription_ = function (sub) {
	return function (mmz) {
		return function () {
			if (mmz.type !== "empty") {
				mmz.cbs.push(sub);
				if (mmz.memo) {
					for (var j = 0; j < mmz.memo.length; j++) {
						sub(mmz.memo[j])();
					}
				}
			}
		};
	};
};

exports.removeSubscription_ = function (sub) {
	return function (mmz) {
		return function () {
			if (mmz.type !== "empty") {
				mmz.cbs.filter(function (x) {
					return x !== sub;
				});
			}
		};
	};
};
exports.runMMZ_ = function (opts) {
	return function (r) {
		return function (mmz) {
			return function () {
				for (var i = 0; i < mmz.ctx.length; i++) {
					mmz.ctx[i].memo = undefined;
					mmz.ctx[i].resolving = false;
				}
				mmz.ctx.start.value = r;
				runMMZInternal(opts, mmz.ctx.start);
			};
		};
	};
};
