var __classPrivateFieldSet = (this && this.__classPrivateFieldSet) || function (receiver, state, value, kind, f) {
    if (kind === "m") throw new TypeError("Private method is not writable");
    if (kind === "a" && !f) throw new TypeError("Private accessor was defined without a setter");
    if (typeof state === "function" ? receiver !== state || !f : !state.has(receiver)) throw new TypeError("Cannot write private member to an object whose class did not declare it");
    return (kind === "a" ? f.call(receiver, value) : f ? f.value = value : state.set(receiver, value)), value;
};
var __classPrivateFieldGet = (this && this.__classPrivateFieldGet) || function (receiver, state, kind, f) {
    if (kind === "a" && !f) throw new TypeError("Private accessor was defined without a getter");
    if (typeof state === "function" ? receiver !== state || !f : !state.has(receiver)) throw new TypeError("Cannot read private member from an object whose class did not declare it");
    return kind === "m" ? f : kind === "a" ? f.call(receiver) : f ? f.value : state.get(receiver);
};
var _ArrayIterator_values, _ArrayIterator_index;
/**
 * Conforms to JS iterator spec:
 * https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Iterators_and_Generators#iterators
 *
 * Single-use iterator.
 *
 * Adds `peek` and `skip` methods.
 */
export default class ArrayIterator {
    constructor(values, initialIndex = 0) {
        _ArrayIterator_values.set(this, void 0);
        _ArrayIterator_index.set(this, void 0);
        __classPrivateFieldSet(this, _ArrayIterator_values, values, "f");
        __classPrivateFieldSet(this, _ArrayIterator_index, initialIndex, "f");
    }
    /**
     * Conforms to JS iterator spec:
     * https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Iterators_and_Generators#iterators
     */
    next() {
        const done = this.isDone();
        if (done) {
            return { done, value: null };
        }
        const value = __classPrivateFieldGet(this, _ArrayIterator_values, "f")[__classPrivateFieldGet(this, _ArrayIterator_index, "f")];
        __classPrivateFieldSet(this, _ArrayIterator_index, __classPrivateFieldGet(this, _ArrayIterator_index, "f") + 1, "f");
        return { done: false, value };
    }
    /**
     * Return value n (default: 0) items ahead without advancing
     * iterator.
     */
    peek(n = 0) {
        return __classPrivateFieldGet(this, _ArrayIterator_values, "f")[__classPrivateFieldGet(this, _ArrayIterator_index, "f") + n];
    }
    /**
     * Advance the iterator `n` items without returning the values.
     */
    skip(n = 1) {
        __classPrivateFieldSet(this, _ArrayIterator_index, __classPrivateFieldGet(this, _ArrayIterator_index, "f") + n, "f");
    }
    /**
     * True if we've already returned the last value via `next()`.
     */
    isDone() {
        return __classPrivateFieldGet(this, _ArrayIterator_index, "f") >= __classPrivateFieldGet(this, _ArrayIterator_values, "f").length;
    }
}
_ArrayIterator_values = new WeakMap(), _ArrayIterator_index = new WeakMap();
//# sourceMappingURL=array_iterator.js.map