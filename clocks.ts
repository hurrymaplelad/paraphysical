export interface Clock {
  /**
   * Returns current seconds since Unix epoch.
   */
  getTimestamp(): number;

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

  getTimestamp(): number {
    return this.#initialTimestamp +
      this.scale * (getSystemTimestamp() - this.#initialTimestamp);
  }

  lap(): number {
    const timestamp = this.getTimestamp();
    const elapsedTime = timestamp - this.#lapStartTimestamp;
    this.#lapStartTimestamp = timestamp;
    return elapsedTime;
  }
}
