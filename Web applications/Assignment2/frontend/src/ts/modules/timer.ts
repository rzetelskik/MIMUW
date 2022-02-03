import { stringifySeconds } from "../common/utils.js";

export class Timer {
  private timerHTML: HTMLElement;
  private startDate: Date;
  private endDate: Date;
  private lapStartDate: Date;
  private interval: number;

  constructor(timerHTML: HTMLElement) {
    this.timerHTML = timerHTML;
    this.endDate = null;
  }

  private getSeconds(end: Date, start: Date) {
    return Math.round((end.getTime() - start.getTime()) / 1000);
  }

  private countAndSet = () => {
    const seconds = this.getSeconds(new Date(), this.startDate);
    this.timerHTML.innerText = stringifySeconds(seconds);
  };

  public start() {
    this.lapStartDate = this.startDate = new Date();
    this.endDate = null;
    this.interval = window.setInterval(this.countAndSet, 1000);
  }

  // Return [lap time, total time].
  public stop(): [number, number] {
    if (this.endDate) {
      return [0, 0];
    }

    this.endDate = new Date();
    clearInterval(this.interval);

    return [this.getSeconds(this.endDate, this.lapStartDate), this.getSeconds(this.endDate, this.startDate)];
  }

  public lap(): number {
    if (this.endDate) {
      return 0;
    }

    const prevLapStartDate = this.lapStartDate;
    this.lapStartDate = new Date();

    return this.getSeconds(this.lapStartDate, prevLapStartDate);
  }
}
