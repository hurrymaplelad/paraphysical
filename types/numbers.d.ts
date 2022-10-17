export interface NumberKind {
    isValid(n: number): boolean;
    readonly description: string;
    readonly max: number;
    readonly min: number;
}
export declare const PositiveInt16: NumberKind;
export declare const LineLabelNumber: NumberKind;
/**
 * Iterates the described range of numbers.
 * @param start Inclusive. Defaults to 0.
 * @param end   Exclusive.
 * @param step  Defaults to 1
 */
export declare function range({ start, end, step }: {
    start?: number;
    end: number;
    step?: number;
}): IterableIterator<number>;
