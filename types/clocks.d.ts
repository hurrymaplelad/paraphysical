export interface Clock {
    /**
     * Returns current seconds since Unix epoch.
     */
    getTimestamp(): number;
    initialTimestamp(): number;
    /**
     * Seconds since the timer's initial time.
     */
    elapsedSeconds(): number;
    lap(): number;
}
export declare function getSystemTimestamp(): number;
export declare class SystemClock implements Clock {
    #private;
    scale: number;
    constructor(options?: {
        scale?: number;
        initialTimestamp?: number;
    } | null);
    initialTimestamp(): number;
    getTimestamp(): number;
    elapsedSeconds(): number;
    lap(): number;
}
export declare class ManualClock implements Clock {
    #private;
    constructor(options?: {
        initialTimestamp?: number;
    } | null);
    tick(seconds: number): void;
    getTimestamp(): number;
    initialTimestamp(): number;
    elapsedSeconds(): number;
    lap(): number;
}
