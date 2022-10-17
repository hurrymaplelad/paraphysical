export const PositiveInt16 = {
    isValid(n) {
        return Number.isInteger(n) && n <= PositiveInt16.max &&
            n >= PositiveInt16.min;
    },
    min: 1,
    max: 32767,
    get description() {
        return `an integer between ${PositiveInt16.min} and ${PositiveInt16.max}`;
    },
};
export const LineLabelNumber = PositiveInt16;
/**
 * Iterates the described range of numbers.
 * @param start Inclusive. Defaults to 0.
 * @param end   Exclusive.
 * @param step  Defaults to 1
 */
export function* range({ start, end, step }) {
    start = start ?? 0;
    step = step ?? 1;
    for (let i = start; i < end; i++) {
        yield i;
    }
}
//# sourceMappingURL=numbers.js.map