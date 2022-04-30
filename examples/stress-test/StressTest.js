export function stressTest_(context) {
	return function (wh) {
		return function () {
			return function () {
				const N = 100;
				const L = 12.0;
				const LM2 = L - 2.0;
				const sines = [];
				const gains = [];
				for (var i = 0; i < N; i++) {
					sines.push(
						new OscillatorNode(context, { frequency: 440.0 + i * 10.0 })
					);
					gains.push(new GainNode(context, { gain: 0.0 }));
					sines[i].connect(gains[i]);
					gains[i].connect(context.destination);
					sines[i].start();
				}
				return wh(function () {
					return function () {
						const time = context.currentTime;
						for (var i = 0; i < N; i++) {
							const barrier = (i * LM2) / N;
							const modT = time % L;
							if (modT > barrier && modT < barrier + 0.25) {
								gains[i].gain.value =
									0.02 + 0.01 * Math.sin(Math.PI * time * 2.0);
								sines[i].frequency.value =
									440.0 + i * 10.0 + i * Math.sin(Math.PI * time);
							} else {
								gains[i].gain.value = 0.0;
							}
						}
					};
				})();
			};
		};
	};
}
