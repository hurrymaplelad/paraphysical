"use strict";
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
var _SystemClock_lapStartTimestamp, _SystemClock_initialTimestamp, _ManualClock_lapStartTimestamp, _ManualClock_initialTimestamp, _ManualClock_timestamp;
Object.defineProperty(exports, "__esModule", { value: true });
exports.ManualClock = exports.SystemClock = exports.getSystemTimestamp = void 0;
function getSystemTimestamp() {
    return Math.floor(Date.now() / 1000);
}
exports.getSystemTimestamp = getSystemTimestamp;
class SystemClock {
    constructor(options = null) {
        Object.defineProperty(this, "scale", {
            enumerable: true,
            configurable: true,
            writable: true,
            value: void 0
        });
        _SystemClock_lapStartTimestamp.set(this, void 0);
        _SystemClock_initialTimestamp.set(this, void 0);
        this.scale = options?.scale ?? 1;
        const initialTimestamp = options?.initialTimestamp ?? getSystemTimestamp();
        __classPrivateFieldSet(this, _SystemClock_initialTimestamp, initialTimestamp, "f");
        __classPrivateFieldSet(this, _SystemClock_lapStartTimestamp, initialTimestamp, "f");
    }
    initialTimestamp() {
        return __classPrivateFieldGet(this, _SystemClock_initialTimestamp, "f");
    }
    getTimestamp() {
        return __classPrivateFieldGet(this, _SystemClock_initialTimestamp, "f") +
            this.scale * (getSystemTimestamp() - __classPrivateFieldGet(this, _SystemClock_initialTimestamp, "f"));
    }
    elapsedSeconds() {
        return this.getTimestamp() - __classPrivateFieldGet(this, _SystemClock_initialTimestamp, "f");
    }
    lap() {
        const timestamp = this.getTimestamp();
        const elapsedTime = timestamp - __classPrivateFieldGet(this, _SystemClock_lapStartTimestamp, "f");
        __classPrivateFieldSet(this, _SystemClock_lapStartTimestamp, timestamp, "f");
        return elapsedTime;
    }
}
exports.SystemClock = SystemClock;
_SystemClock_lapStartTimestamp = new WeakMap(), _SystemClock_initialTimestamp = new WeakMap();
class ManualClock {
    constructor(options = null) {
        _ManualClock_lapStartTimestamp.set(this, void 0);
        _ManualClock_initialTimestamp.set(this, void 0);
        _ManualClock_timestamp.set(this, void 0);
        const initialTimestamp = options?.initialTimestamp ?? 0;
        __classPrivateFieldSet(this, _ManualClock_initialTimestamp, initialTimestamp, "f");
        __classPrivateFieldSet(this, _ManualClock_timestamp, initialTimestamp, "f");
        __classPrivateFieldSet(this, _ManualClock_lapStartTimestamp, initialTimestamp, "f");
    }
    tick(seconds) {
        __classPrivateFieldSet(this, _ManualClock_timestamp, __classPrivateFieldGet(this, _ManualClock_timestamp, "f") + seconds, "f");
    }
    getTimestamp() {
        return __classPrivateFieldGet(this, _ManualClock_timestamp, "f");
    }
    initialTimestamp() {
        return __classPrivateFieldGet(this, _ManualClock_initialTimestamp, "f");
    }
    elapsedSeconds() {
        return __classPrivateFieldGet(this, _ManualClock_timestamp, "f") - __classPrivateFieldGet(this, _ManualClock_initialTimestamp, "f");
    }
    lap() {
        const timestamp = this.getTimestamp();
        const elapsedTime = timestamp - __classPrivateFieldGet(this, _ManualClock_lapStartTimestamp, "f");
        __classPrivateFieldSet(this, _ManualClock_lapStartTimestamp, timestamp, "f");
        return elapsedTime;
    }
}
exports.ManualClock = ManualClock;
_ManualClock_lapStartTimestamp = new WeakMap(), _ManualClock_initialTimestamp = new WeakMap(), _ManualClock_timestamp = new WeakMap();
