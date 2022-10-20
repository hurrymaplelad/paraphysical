"use strict";
var __classPrivateFieldGet = (this && this.__classPrivateFieldGet) || function (receiver, state, kind, f) {
    if (kind === "a" && !f) throw new TypeError("Private accessor was defined without a getter");
    if (typeof state === "function" ? receiver !== state || !f : !state.has(receiver)) throw new TypeError("Cannot read private member from an object whose class did not declare it");
    return kind === "m" ? f : kind === "a" ? f.call(receiver) : f ? f.value : state.get(receiver);
};
var _DateTime_instances, _DateTime_part;
Object.defineProperty(exports, "__esModule", { value: true });
exports.DateTime = void 0;
const WEEKDAY_NUMBERS = {
    Monday: 1,
    Tuesday: 2,
    Wednesday: 3,
    Thursday: 4,
    Friday: 5,
    Saturday: 6,
    Sunday: 7,
};
class DateTime {
    constructor(timestamp, timezone) {
        _DateTime_instances.add(this);
        Object.defineProperty(this, "timestamp", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: void 0
        });
        Object.defineProperty(this, "timezone", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: void 0
        });
        this.timestamp = timestamp;
        this.timezone = timezone;
    }
    /* [1-12] */
    monthNumber() {
        const { value } = __classPrivateFieldGet(this, _DateTime_instances, "m", _DateTime_part).call(this, { month: "numeric" });
        return parseInt(value, 10);
    }
    /* [1-7], 1=Monday */
    weekdayNumber() {
        const { value } = __classPrivateFieldGet(this, _DateTime_instances, "m", _DateTime_part).call(this, { weekday: "long" });
        return WEEKDAY_NUMBERS[value];
    }
    /* [1-31] */
    dayOfMonth() {
        const { value } = __classPrivateFieldGet(this, _DateTime_instances, "m", _DateTime_part).call(this, { day: "numeric" });
        return parseInt(value, 10);
    }
    /* [0-23] */
    hour() {
        const { value } = __classPrivateFieldGet(this, _DateTime_instances, "m", _DateTime_part).call(this, { hour: "numeric" });
        return parseInt(value, 10);
    }
    /* [0.0-23.999] */
    fractionalHour() {
        return this.hour() +
            this.minute() / 60 +
            this.second() / (60 * 60);
    }
    /* [0-59] */
    minute() {
        const { value } = __classPrivateFieldGet(this, _DateTime_instances, "m", _DateTime_part).call(this, { minute: "numeric" });
        return parseInt(value, 10);
    }
    second(digits = 0) {
        const { value: seconds } = __classPrivateFieldGet(this, _DateTime_instances, "m", _DateTime_part).call(this, { second: "numeric" });
        const fractionalSeconds = digits === 0 ? "0" : __classPrivateFieldGet(this, _DateTime_instances, "m", _DateTime_part).call(this, {
            fractionalSecondDigits: digits,
        }).value;
        return Number(`${seconds}.${fractionalSeconds}`);
    }
}
exports.DateTime = DateTime;
_DateTime_instances = new WeakSet(), _DateTime_part = function _DateTime_part(options) {
    const locale = "en-US";
    const format = new Intl.DateTimeFormat(locale, {
        ...options,
        timeZone: this.timezone,
        hour12: false,
    });
    const partsList = format.formatToParts(this.timestamp * 1000);
    return partsList[0];
};
