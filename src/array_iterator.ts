/**
 * Conforms to JS iterator spec:
 * https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Iterators_and_Generators#iterators
 *
 * Single-use iterator.
 *
 * Adds `peek` and `skip` methods.
 */
export default class ArrayIterator<T> {
  #values: ReadonlyArray<T>;
  #index: number;

  constructor(values: ReadonlyArray<T>, initialIndex: number = 0) {
    this.#values = values;
    this.#index = initialIndex;
  }

  /**
   * Conforms to JS iterator spec:
   * https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Iterators_and_Generators#iterators
   */
  next(): Readonly<{ done: false; value: T } | { done: true; value: null }> {
    const done = this.isDone();
    if (done) {
      return { done, value: null };
    }
    const value = this.#values[this.#index];
    this.#index += 1;
    return { done: false, value };
  }

  /**
   * Return value n (default: 0) items ahead without advancing
   * iterator.
   */
  peek(n = 0): T | null {
    return this.#values[this.#index + n];
  }

  /**
   * Advance the iterator `n` items without returning the values.
   */
  skip(n = 1): void {
    this.#index += n;
  }

  /**
   * True if we've already returned the last value via `next()`.
   */
  isDone(): boolean {
    return this.#index >= this.#values.length;
  }
}
