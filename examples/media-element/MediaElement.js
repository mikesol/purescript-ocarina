exports.makeAudio_ = function () {
	var audio = new Audio(
		"https://freesound.org/data/previews/268/268511_4157918-lq.mp3"
	);
	audio.crossOrigin = "anonymous";
	audio.loop = true;
	return audio;
};
exports.playAudio_ = function (audio) {
	return function () {
		audio.play();
	};
};
exports.pauseAudio_ = function (audio) {
	return function () {
		audio.pause();
	};
};