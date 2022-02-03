import { IQuiz, IStats } from "../../../lib/lib";
import { stringifySeconds, stringifyPenalty } from "./common/utils.js";

class QuizWorker {
  #csrfToken: string | undefined;

  #checkAuthorised = async (): Promise<string | undefined> => {
    const res: any = await fetch(`/user-details/`, { method: "GET" });
    const data: any = await res.json();

    if (res.status !== 200) throw new Error(data.message);

    this.#csrfToken = await res.headers.get("CSRF-Header");
    return data.user;
  };

  #setLoginModal = (active: boolean): void => {
    (document.getElementById("input-username") as HTMLInputElement).value = "";
    (document.getElementById("input-password") as HTMLInputElement).value = "";

    const modal: HTMLElement = document.getElementById("login-modal");
    active ? modal.classList.add("is-active") : modal.classList.remove("is-active");
  };

  #setPasswordModal = (active: boolean): void => {
    (document.getElementById("change-password") as HTMLInputElement).value = "";
    (document.getElementById("change-password-new") as HTMLInputElement).value = "";
    (document.getElementById("change-password-new-repeat") as HTMLInputElement).value = "";

    const modal: HTMLElement = document.getElementById("password-modal");
    active ? modal.classList.add("is-active") : modal.classList.remove("is-active");
  };

  #setAddQuizModal = (active: boolean): void => {
    (document.getElementById("input-quiz-json") as HTMLInputElement).value = "";

    const modal: HTMLElement = document.getElementById("add-quiz-modal");
    active ? modal.classList.add("is-active") : modal.classList.remove("is-active");
  };

  #login = async (): Promise<void> => {
    const username: string = (document.getElementById("input-username") as HTMLInputElement).value;
    const password: string = (document.getElementById("input-password") as HTMLInputElement).value;

    const res: any = await fetch(`/login/`, {
      method: "POST",
      credentials: "same-origin",
      headers: {
        "Content-type": "application/json",
        "CSRF-Token": this.#csrfToken,
      },
      body: JSON.stringify({ username, password }),
    });

    if (res.status !== 200) {
      const data = await res.json();
      alert(data.message);
    }

    await this.#refresh();
  };

  #logout = async (): Promise<void> => {
    const res: any = await fetch(`/logout/`, {
      method: "POST",
      credentials: "same-origin",
      headers: { "CSRF-Token": this.#csrfToken },
    });

    if (res.status !== 200) {
      const data = await res.json();
      alert(data.message);
    }
    await this.#refresh();
  };

  #changePassword = async (): Promise<void> => {
    const password: string = (document.getElementById("change-password") as HTMLInputElement).value;
    const newPassword: string = (document.getElementById("change-password-new") as HTMLInputElement).value;
    const newPasswordRepeat: string = (document.getElementById("change-password-new-repeat") as HTMLInputElement).value;

    if (newPassword !== newPasswordRepeat) throw new Error("Passwords do not match.");

    const res: any = await fetch(`/change-password/`, {
      method: "POST",
      credentials: "same-origin",
      headers: {
        "Content-type": "application/json",
        "CSRF-Token": this.#csrfToken,
      },
      body: JSON.stringify({ password, new_password: newPassword }),
    });

    if (res.status !== 200) {
      const data = await res.json();
      alert(data.message);
    }

    await this.#refresh();
  };

  #addQuiz = async (): Promise<void> => {
    const quiz: string = (document.getElementById("input-quiz-json") as HTMLInputElement).value;

    const res: any = await fetch(`/quiz-upload/`, {
      method: "POST",
      credentials: "same-origin",
      headers: {
        "Content-type": "application/json",
        "CSRF-Token": this.#csrfToken,
      },
      body: JSON.stringify({ quiz }),
    });

    if (res.status === 200) alert("Quiz uploaded successfully.");
    else {
      const data = await res.json();
      alert(data.message);
    }

    await this.#refresh();
  };

  #setNavbarButtons = (authorised: boolean): void => {
    const loginButton: HTMLButtonElement = document.getElementById("login-button") as HTMLButtonElement;
    const logoutButton: HTMLButtonElement = document.getElementById("logout-button") as HTMLButtonElement;
    const passwordButton: HTMLButtonElement = document.getElementById("password-button") as HTMLButtonElement;
    const addQuizButton: HTMLButtonElement = document.getElementById("add-quiz-button") as HTMLButtonElement;

    if (authorised) {
      loginButton.classList.add("is-hidden");
      logoutButton.classList.remove("is-hidden");
      passwordButton.classList.remove("is-hidden");
      addQuizButton.classList.remove("is-hidden");
    } else {
      loginButton.classList.remove("is-hidden");
      logoutButton.classList.add("is-hidden");
      passwordButton.classList.add("is-hidden");
      addQuizButton.classList.add("is-hidden");
    }
  };

  #setButtonEvents = () => {
    document.getElementById("login-button").onclick = () => this.#setLoginModal(true);
    document.getElementById("login-confirm").onclick = () => this.#login();
    document.getElementById("login-cancel").onclick = () => this.#setLoginModal(false);
    document.getElementById("logout-button").onclick = () => this.#logout();
    document.getElementById("password-button").onclick = () => this.#setPasswordModal(true);
    document.getElementById("change-password-confirm").onclick = () => this.#changePassword();
    document.getElementById("change-password-cancel").onclick = () => this.#setPasswordModal(false);
    document.getElementById("add-quiz-button").onclick = () => this.#setAddQuizModal(true);
    document.getElementById("add-quiz-confirm").onclick = () => this.#addQuiz();
    document.getElementById("add-quiz-cancel").onclick = () => this.#setAddQuizModal(false);
  };

  #getQuizList = async (): Promise<IQuiz[]> => {
    const res = await fetch(`/quiz-list/`, {
      method: "GET",
      credentials: "same-origin",
    });

    const data = await res.json();
    if (res.status !== 200) {
      throw new Error(data.message);
    }

    return data.quizes;
  };

  #fillQuizList = (quizes: IQuiz[]) => {
    const quizTable: HTMLElement = document.getElementById("quiz-list") as HTMLElement;
    quizTable.innerHTML = "";

    quizes.forEach((quiz: IQuiz) => {
      quizTable.insertAdjacentHTML(
        "beforeend",
        `<tr>
          <td>${quiz.id}</td>
          <td>${quiz.introduction}</td>
          <td><a href="quiz_template.html?id=${quiz.id}" class="button is-success is-pulled-right">Solve!</a></td>
        </tr>`
      );
    });
  };

  #getStatsList = async (): Promise<IStats[]> => {
    const res = await fetch(`/stats-list/`, {
      method: "GET",
      credentials: "same-origin",
    });

    const data = await res.json();
    if (res.status !== 200) {
      throw new Error(data.message);
    }

    return data.stats;
  };

  #fillStats = (stats: IStats[]) => {
    const statsTable: HTMLElement = document.getElementById("stats-list") as HTMLElement;
    statsTable.innerHTML = "";

    stats.forEach((res: IStats) => {
      const score: number = res.time + res.penalty;
      statsTable.insertAdjacentHTML(
        "beforeend",
        `<tr>
          <td>${res.quiz_id}</td>
          <td>${score}</td>
          <td>${stringifySeconds(res.time)}</td>
          <td>${stringifyPenalty(res.penalty)}</td>
          <td><a href="stats_template.html?id=${res.quiz_id}" class="button is-link is-pulled-right">Details</a></td>
        </tr>`
      );
    });
  };

  #setTableDisplay = (authorised: boolean, id: string) => {
    const list: HTMLElement = document.getElementById(id);
    authorised ? list.classList.remove("is-hidden") : list.classList.add("is-hidden");
  };

  #refresh = async () => {
    console.log("refresh");
    try {
      const authorised: boolean = (await this.#checkAuthorised()) !== undefined;
      this.#setNavbarButtons(authorised);
      this.#setLoginModal(false);
      this.#setPasswordModal(false);
      this.#setAddQuizModal(false);
      this.#setTableDisplay(!authorised, "guest-box");
      this.#setTableDisplay(authorised, "quiz-box");
      this.#setTableDisplay(authorised, "stats-box");
      if (authorised) {
        this.#fillQuizList(await this.#getQuizList());
        this.#fillStats(await this.#getStatsList());
      }
    } catch (err) {
      alert(err.message);
    }
  };

  setUp = async (): Promise<void> => {
    this.#setButtonEvents();
    await this.#refresh();
  };
}

document.addEventListener(
  "DOMContentLoaded",
  async (event: Event): Promise<void> => {
    event.preventDefault();

    const main: QuizWorker = new QuizWorker();
    await main.setUp();
  }
);
