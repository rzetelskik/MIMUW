import { IStats, IQuestionStats } from "../../../lib/lib";
import { stringifySeconds, stringifyPenalty } from "./common/utils.js";

class StatsWorker {
  #csrfToken: string | undefined;
  #quizId: number | undefined;
  #user: string | undefined;

  #checkAuthorised = async (): Promise<void> => {
    const res: any = await fetch(`/user-details/`, { method: "GET" });
    const data: any = await res.json();

    if (res.status !== 200) throw new Error(data.message);
    if (data.user === undefined) throw new Error("Unauthorised.");

    this.#csrfToken = await res.headers.get("CSRF-Header");
    this.#user = data.user;
  };

  #getStats = async (): Promise<IStats> => {
    const res: any = await fetch(`/stats/${this.#quizId}`, {
      method: "GET",
      credentials: "same-origin",
    });

    const data = await res.json();
    if (res.status !== 200) throw new Error(data.message);

    return data.stats;
  };

  #getAnswerStatsHTML = (questionStats: IQuestionStats): string => {
    const score: number = questionStats.penalty + questionStats.time;
    const highlight: string = questionStats.correct
      ? "has-text-success has-background-success-light"
      : "has-text-danger has-background-danger-light";

    return `<tr class="${highlight}">
              <td>${questionStats.questionNumber}</td>
              <td>${questionStats.answer}</td>
              <td>${score}</td>
              <td>${stringifySeconds(questionStats.time)}</td>
              <td>${stringifyPenalty(questionStats.penalty)}</td>
            </tr>`;
  };

  #populateAnswersStats = (stats: IStats): void => {
    const answersTable: HTMLElement = document.getElementById("answers-list") as HTMLElement;

    stats.questionStats
      .sort((a, b) => a.questionNumber - b.questionNumber)
      .forEach((el: IQuestionStats) => {
        answersTable.insertAdjacentHTML("beforeend", this.#getAnswerStatsHTML(el));
      });
  };

  #setTitleQuizId = (): void => {
    document.getElementById("title-quiz-id").innerText = String(this.#quizId);
  };

  #setTitleUsername = (): void => {
    document.getElementById("title-username").innerText = this.#user;
  };

  #getLeaderboard = async (): Promise<IStats[]> => {
    const res: any = await fetch(`/leaderboard/${this.#quizId}`, {
      method: "GET",
      credentials: "same-origin",
    });

    const data = await res.json();
    if (res.status !== 200) throw new Error(data.message);

    return data.leaderboard;
  };

  #getLeaderHTML = (stats: IStats) => {
    const score: number = stats.time + stats.penalty;
    const highlight: string = stats.username === this.#user ? "is-selected" : "";

    return `<tr class="${highlight}">
              <td>${stats.username}</td>
              <td>${score}</td>
              <td>${stringifySeconds(stats.time)}</td>
              <td>${stringifyPenalty(stats.penalty)}</td>
            </tr>`;
  };

  #populateLeaderboard = (leaderboard: IStats[]): void => {
    const leaderboardTable: HTMLElement = document.getElementById("leaderboard-list") as HTMLElement;

    leaderboard.forEach((el: IStats) => {
      leaderboardTable.insertAdjacentHTML("beforeend", this.#getLeaderHTML(el));
    });
  };

  #getAverageTimeHTML = (averageTime: IQuestionStats) => {
    return `<tr>
              <td>${averageTime.questionNumber}</td>
              <td>${stringifySeconds(averageTime.time)}</td>
            </tr>`;
  };

  #getAverageTimes = async (): Promise<IQuestionStats[]> => {
    const res: any = await fetch(`/times/${this.#quizId}`, {
      method: "GET",
      credentials: "same-origin",
    });

    const data: any = await res.json();
    if (res.status !== 200) throw new Error(data.message);

    return data.times;
  };

  #populateAverageTimes = (averageTimes: IQuestionStats[]): void => {
    const averageTimesTable: HTMLElement = document.getElementById("average-times-list") as HTMLElement;

    averageTimes.forEach((el: IQuestionStats) => {
      averageTimesTable.insertAdjacentHTML("beforeend", this.#getAverageTimeHTML(el));
    });
  };

  setUp = async () => {
    const quizRegEx = /^\?id=(\d+)$/.exec(window.location.search);
    if (!quizRegEx) throw new Error("No quiz to fetch.");

    this.#quizId = parseInt(quizRegEx[1], 10);

    try {
      await this.#checkAuthorised();
      this.#setTitleQuizId();
      this.#setTitleUsername();

      this.#populateAnswersStats(await this.#getStats());
      this.#populateLeaderboard(await this.#getLeaderboard());
      this.#populateAverageTimes(await this.#getAverageTimes());
    } catch (err) {
      alert(err.message);
    }
  };
}

document.addEventListener(
  "DOMContentLoaded",
  async (event: Event): Promise<void> => {
    event.preventDefault();

    const main: StatsWorker = new StatsWorker();
    await main.setUp();
  }
);
