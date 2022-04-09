// Memoize an event
//
// Keeps one long-running subscription that broadcasts to
// downstream subscribers.
//
// For data sent on subscribe, this will not help, since it needs
// to create a subscription to see what data is currently
// being sent on subscribe. That is, each subscription creates
// a second short-lived subscription just to ping it for an initial value.
//
// But for later events, the data will be shared across several
// subscriptions. For example, a naive implementation of `map f` will
// recompute f *for each subscription*, instead of sharing that across
// the actual event.
//
// Thus this places a certain contract on `event`, that it behaves
// like a "purely functional" event in some sense. Malicious events
// could send random data to random subscribers.
//
// It also requires the events to keep track of their subscriptions
// and not release essential resources until all subscriptions are
// cancelled.
exports.memoize = function memoize(event) {
	var downstream = {};
	var id = 0;
	var upstream;
	var receive = function (a) {
		return function () {
			for (let k in downstream) {
				//console.log('sending',k,a);
				downstream[k](a)();
			}
		};
	};
	return function (subscriber) {
		return function () {
			//console.log('mmz');
			var capture = id;
			id += 1;
			var wasEmpty = !Object.keys(downstream).length;
			downstream[capture] = subscriber;
			if (wasEmpty) {
				// Create a global subscription for this memoized Event.
				// Note that this will properly call subscriber() with initial data
				// since it was already added to downstream.
				upstream = event(receive)();
			} else {
				// Subscribe the new subscription, and then immediately cancel it,
				// so it only receives initial data - and from then on it receives
				// the memoized stream.
				//
				// Note: this means that `map f (pure v)` is not memoized /shrug
				//
				// It will be the responsibility of `event` to deal with these
				// temporary subscriptions: it must keep track of how many
				// subscriptions it has and not deallocate essential resources
				// until they are all cancelled.
				event(subscriber)()();
			}
			return function () {
				// Remove the subscription from the list
				delete downstream[capture];
				// And cancel the upstream subscription if it was the last one
				if (!Object.keys(downstream).length) upstream();
			};
		};
	};
};
