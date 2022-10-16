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

export function getSystemTimestamp(): number {
  return Math.floor(Date.now() / 1000);
}

export class SystemClock implements Clock {
  scale: number;
  #lapStartTimestamp: number;
  #initialTimestamp: number;

  constructor(
    options: { scale?: number; initialTimestamp?: number } | null = null,
  ) {
    this.scale = options?.scale ?? 1;
    const initialTimestamp = options?.initialTimestamp ?? getSystemTimestamp();
    this.#initialTimestamp = initialTimestamp;
    this.#lapStartTimestamp = initialTimestamp;
  }

  initialTimestamp(): number {
    return this.#initialTimestamp;
  }

  getTimestamp(): number {
    return this.#initialTimestamp +
      this.scale * (getSystemTimestamp() - this.#initialTimestamp);
  }

  elapsedSeconds(): number {
    return this.getTimestamp() - this.#initialTimestamp;
  }

  lap(): number {
    const timestamp = this.getTimestamp();
    const elapsedTime = timestamp - this.#lapStartTimestamp;
    this.#lapStartTimestamp = timestamp;
    return elapsedTime;
  }
}

export class ManualClock implements Clock {
  #lapStartTimestamp: number;
  #initialTimestamp: number;
  #timestamp: number;

  constructor(
    options: { initialTimestamp?: number } | null = null,
  ) {
    const initialTimestamp = options?.initialTimestamp ?? 0;
    this.#initialTimestamp = initialTimestamp;
    this.#timestamp = initialTimestamp;
    this.#lapStartTimestamp = initialTimestamp;
  }

  tick(seconds: number): void {
    this.#timestamp += seconds;
  }

  getTimestamp(): number {
    return this.#timestamp;
  }

  initialTimestamp(): number {
    return this.#initialTimestamp;
  }

  elapsedSeconds(): number {
    return this.#timestamp - this.#initialTimestamp;
  }

  lap(): number {
    const timestamp = this.getTimestamp();
    const elapsedTime = timestamp - this.#lapStartTimestamp;
    this.#lapStartTimestamp = timestamp;
    return elapsedTime;
  }
}
