export declare type DateTimeParts = {
    fractionalHour: number;
};
export declare class DateTime {
    #private;
    readonly timestamp: number;
    readonly timezone: string;
    constructor(timestamp: number, timezone: string);
    monthNumber(): number;
    weekdayNumber(): number;
    dayOfMonth(): number;
    hour(): number;
    fractionalHour(): number;
    minute(): number;
    second(digits?: 0 | 1 | 2 | 3): number;
}
