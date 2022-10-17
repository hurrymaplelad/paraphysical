/**
 * Conforms to JS iterator spec:
 * https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Iterators_and_Generators#iterators
 *
 * Single-use iterator.
 *
 * Adds `peek` and `skip` methods.
 */
export default class ArrayIterator<T> {
    #private;
    constructor(values: ReadonlyArray<T>, initialIndex?: number);
    /**
     * Conforms to JS iterator spec:
     * https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Iterators_and_Generators#iterators
     */
    next(): Readonly<{
        done: false;
        value: T;
    } | {
        done: true;
        value: null;
    }>;
    /**
     * Return value n (default: 0) items ahead without advancing
     * iterator.
     */
    peek(n?: number): T | null;
    /**
     * Advance the iterator `n` items without returning the values.
     */
    skip(n?: number): void;
    /**
     * True if we've already returned the last value via `next()`.
     */
    isDone(): boolean;
}
