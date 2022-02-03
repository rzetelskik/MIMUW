import { Timer } from "./modules/timer.js";
import { stringifySeconds, stringifyPenalty } from "./common/utils.js";
import { IQuestion, IQuiz, IAnswer, IStats, IQuestionStats } from "../../../lib/lib";

class Quiz {
  #csrfToken: string | undefined;
  #quiz: IQuiz;
  #timer: Timer;
  #currentQuestionNum: number;
  #answers: IAnswer[];
  #answerTime: number[];
  #questionsAnswered: number;
  #finished: boolean;

  constructor(timer: Timer) {
    this.#timer = timer;
    this.#currentQuestionNum = 0;
    this.#questionsAnswered = 0;
    this.#finished = false;
  }

  private allQuestionsAnswered = (): boolean => {
    return this.#questionsAnswered === this.#quiz.questions.length;
  };

  private setIntroduction = (introduction: HTMLElement): void => {
    (introduction.querySelector(".message-body") as HTMLElement).innerText = this.#quiz.introduction;
    (introduction.querySelector(".delete") as HTMLButtonElement).onclick = () => introduction.remove();
  };

  private setProgressBar = (progress: HTMLProgressElement): void => {
    progress.setAttribute("value", this.#currentQuestionNum.toString());
    progress.setAttribute("max", this.#quiz.questions.length.toString());
  };

  private getReprQuestionNumber = (questionNumber: number): string => {
    return (questionNumber + 1).toString();
  };

  private setQuestion = (questionElement: HTMLElement): void => {
    const currentQuestion: IQuestion = this.#quiz.questions[this.#currentQuestionNum];
    (questionElement.querySelector("#question-number") as HTMLElement).innerText = this.getReprQuestionNumber(
      this.#currentQuestionNum
    );
    (questionElement.querySelector("#penalty-value") as HTMLElement).innerText = stringifyPenalty(
      currentQuestion.penalty
    );
    (questionElement.querySelector("#question-body") as HTMLElement).innerText = currentQuestion.question;
  };

  private switchButton(button: HTMLButtonElement, enabled: boolean): void {
    enabled ? button.removeAttribute("disabled") : button.setAttribute("disabled", "disabled");
  }

  private setButtons = (prev: HTMLButtonElement, next: HTMLButtonElement, finish: HTMLButtonElement): void => {
    this.switchButton(prev, this.#currentQuestionNum > 0 && !this.#finished);
    this.switchButton(next, this.#currentQuestionNum < this.#quiz.questions.length - 1 && !this.#finished);
    this.switchButton(finish, this.allQuestionsAnswered() && !this.#finished);
  };

  private fillAnswer = (answerField: HTMLInputElement): void => {
    if (this.#finished) {
      answerField.setAttribute("disabled", "disabled");
    } else {
      answerField.value = this.#answers[this.#currentQuestionNum].answer
        ? String(this.#answers[this.#currentQuestionNum].answer)
        : "";
    }
  };

  private refresh = (updateInputField: boolean): void => {
    this.setProgressBar(document.getElementById("progress-bar") as HTMLProgressElement);
    this.setQuestion(document.getElementById("question-panel"));
    this.setButtons(
      document.getElementById("button-prev") as HTMLButtonElement,
      document.getElementById("button-next") as HTMLButtonElement,
      document.getElementById("button-finish") as HTMLButtonElement
    );
    if (updateInputField) this.fillAnswer(document.getElementById("user-answer") as HTMLInputElement);
  };

  private addQuestionTime = (questionNum: number, time: number): void => {
    this.#answerTime[questionNum] += time;
  };

  private prevQuestion = (event: Event): void => {
    if (this.#currentQuestionNum > 0) {
      this.addQuestionTime(this.#currentQuestionNum--, this.#timer.lap());
      this.refresh(true);
    }
  };

  private nextQuestion = (event: Event): void => {
    if (this.#currentQuestionNum < this.#quiz.questions.length) {
      this.addQuestionTime(this.#currentQuestionNum++, this.#timer.lap());
      this.refresh(true);
    }
  };

  private highlightInvalidAnswer = () => {
    const inputField: HTMLInputElement = document.getElementById("user-answer") as HTMLInputElement;

    inputField.classList.add("is-danger");
  };

  private removeInvalidAnswerHighlight = () => {
    const inputField: HTMLInputElement = document.getElementById("user-answer") as HTMLInputElement;

    inputField.classList.remove("is-danger");
  };

  private isInputValid = (input: string): boolean => {
    const regex: RegExp = new RegExp(`^(-)?[0-9]+$`);

    return regex.test(input);
  };

  private updateAnswer = (event: Event): void => {
    const input = event.target as HTMLInputElement;
    const prev = this.#answers[this.#currentQuestionNum].answer;

    if (input.value && this.isInputValid(input.value)) {
      if (prev === null) this.#questionsAnswered++;
      this.#answers[this.#currentQuestionNum].answer = Number(input.value);

      this.removeInvalidAnswerHighlight();
    } else {
      if (prev !== null) this.#questionsAnswered--;
      this.#answers[this.#currentQuestionNum].answer = null;

      this.highlightInvalidAnswer();
    }

    this.refresh(false);
  };

  private getQuestionResultHTML = (res: IQuestionStats): string => {
    return `<tr>
      <td class=${res.correct ? "has-text-success" : "has-text-danger"}>
      ${res.questionNumber}</td><td>${stringifySeconds(res.time)}</td>
      <td>${res.correct ? "-" : stringifyPenalty(this.#quiz.questions[res.questionNumber - 1].penalty)}</td>
      </tr>`;
  };

  private displayPopup = (): void => {
    document.getElementById("popup").classList.add("is-active");
  };

  #fillTimePercentage = (totalTime: number) => {
    this.#answerTime.forEach((el: number, i: number) => {
      this.#answers[i].timePercentage = Math.round((el / totalTime) * 100);
    });
  };

  #sendAnswers = async (): Promise<IStats> => {
    const res: any = await fetch("/answers/", {
      method: "POST",
      credentials: "same-origin",
      headers: {
        "Content-type": "application/json",
        "CSRF-Token": this.#csrfToken,
      },
      body: JSON.stringify({ id: this.#quiz.id, answers: this.#answers }),
    });

    if (res.status !== 200) {
      // TODO
      alert("Server connection error.");
    }

    return (await res.json()).stats;
  };

  #populateQuestionResults = (results: IQuestionStats[]): void => {
    const questionTableParent = document.getElementById("results-body");

    results.sort((a, b) => a.questionNumber - b.questionNumber);
    results.forEach((res) => {
      questionTableParent.insertAdjacentHTML("beforeend", this.getQuestionResultHTML(res));
    })
  }

  #populateCummulativeResults = (time: number, penalty: number): void => {
    document.getElementById("score").innerText = String(time + penalty);
    document.getElementById("total-time").innerText = stringifySeconds(time);
    document.getElementById("total-penalty").innerText = stringifyPenalty(penalty);
  }

  #populatePopup = (stats: IStats): void => {
    this.#populateQuestionResults(stats.questionStats);
    this.#populateCummulativeResults(stats.time, stats.penalty);
  }

  #finishQuiz = async (event: Event): Promise<void> => {
    if (this.allQuestionsAnswered) {
      let lapTime: number;
      let totalTime: number;
      [lapTime, totalTime] = this.#timer.stop();
      this.addQuestionTime(this.#currentQuestionNum, lapTime);
      this.#finished = true;
      this.refresh(true);

      this.#fillTimePercentage(totalTime);
      this.#populatePopup(await this.#sendAnswers());
      this.displayPopup();
    }
  };

  #getQuiz = async (id: number): Promise<IQuiz> => {
    const res: any = await fetch(`/quiz/${id}`, {
      method: "GET",
      credentials: "same-origin",
    });

    const data = await res.json();
    if (res.status !== 200) {
      alert(data.message);
      return;
    }
    this.#csrfToken = await res.headers.get("CSRF-Header");

    return data.quiz;
  };

  #prepareQuiz = (quiz: IQuiz): void => {
    this.#quiz = quiz;
    this.#quiz.questions.sort((a, b) => a.number - b.number);

    this.#answerTime = new Array<number>(this.#quiz.questions.length).fill(0);
    this.#answers = new Array<IAnswer>(this.#quiz.questions.length)
      .fill(undefined)
      .map((_, i) => ({ questionNumber: i + 1, timePercentage: 0, answer: null }));
  };

  setUp = async (): Promise<void> => {
    const quizId = /^\?id=(\d+)$/.exec(window.location.search);
    if (!quizId) {
      alert("No quiz to fetch.");
      return;
    }
    this.#prepareQuiz(await this.#getQuiz(parseInt(quizId[1], 10)));

    this.setIntroduction(document.getElementById("introduction"));
    (document.getElementById("button-prev") as HTMLButtonElement).onclick = this.prevQuestion;
    (document.getElementById("button-next") as HTMLButtonElement).onclick = this.nextQuestion;
    (document.getElementById("button-finish") as HTMLButtonElement).onclick = this.#finishQuiz;
    (document.getElementById("user-answer") as HTMLInputElement).oninput = this.updateAnswer;
    this.refresh(false);
    this.#timer.start();
  };
}

document.addEventListener(
  "DOMContentLoaded",
  async (event): Promise<void> => {
    const quiz = new Quiz(new Timer(document.getElementById("timer")));
    await quiz.setUp();
  }
);
