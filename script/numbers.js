"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.range = exports.LineLabelNumber = exports.PositiveInt16 = void 0;
exports.PositiveInt16 = {
    isValid(n) {
        return Number.isInteger(n) && n <= exports.PositiveInt16.max &&
            n >= exports.PositiveInt16.min;
    },
    min: 1,
    max: 32767,
    get description() {
        return `an integer between ${exports.PositiveInt16.min} and ${exports.PositiveInt16.max}`;
    },
};
exports.LineLabelNumber = exports.PositiveInt16;
/**
 * Iterates the described range of numbers.
 * @param start Inclusive. Defaults to 0.
 * @param end   Exclusive.
 * @param step  Defaults to 1
 */
function* range({ start, end, step }) {
    start = start ?? 0;
    step = step ?? 1;
    for (let i = start; i < end; i++) {
        yield i;
    }
}
exports.range = range;
//# sourceMappingURL=numbers.js.map