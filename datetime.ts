const WEEKDAY_NUMBERS: { [_: string]: number } = {
  Monday: 1,
  Tuesday: 2,
  Wednesday: 3,
  Thursday: 4,
  Friday: 5,
  Saturday: 6,
  Sunday: 7,
};

export type DateTimeParts = {
  fractionalHour: number;
};

export class DateTime {
  readonly timestamp: number;
  readonly timezone: string;

  constructor(timestamp: number, timezone: string) {
    this.timestamp = timestamp;
    this.timezone = timezone;
  }

  #part(options: Intl.DateTimeFormatOptions): Intl.DateTimeFormatPart {
    const locale = "en-US";
    const format = new Intl.DateTimeFormat(locale, {
      ...options,
      timeZone: this.timezone,
      hour12: false,
    });
    const partsList = format.formatToParts(this.timestamp * 1000);
    return partsList[0];
  }

  /* [1-12] */
  monthNumber(): number {
    const { value } = this.#part({ month: "numeric" });
    return parseInt(value, 10);
  }

  /* [1-7], 1=Monday */
  weekdayNumber(): number {
    const { value } = this.#part({ weekday: "long" });
    return WEEKDAY_NUMBERS[value];
  }

  /* [1-31] */
  dayOfMonth(): number {
    const { value } = this.#part({ day: "numeric" });
    return parseInt(value, 10);
  }

  /* [0-23] */
  hour(): number {
    const { value } = this.#part({ hour: "numeric" });
    return parseInt(value, 10);
  }

  /* [0.0-23.999] */
  fractionalHour(): number {
    return this.hour() +
      this.minute() / 60 +
      this.second() / (60 * 60);
  }

  /* [0-59] */
  minute(): number {
    const { value } = this.#part({ minute: "numeric" });
    return parseInt(value, 10);
  }

  second(digits: 0 | 1 | 2 | 3 = 0): number {
    const { value: seconds } = this.#part({ second: "numeric" });
    const fractionalSeconds = digits === 0 ? "0" : this.#part({
      fractionalSecondDigits: digits,
    }).value;
    return Number(`${seconds}.${fractionalSeconds}`);
  }
}
