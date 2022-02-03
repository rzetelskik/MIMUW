import express from "express";
import cookieParser from "cookie-parser";
import bodyParser from "body-parser";
import csurf from "csurf";
import * as sqlite from "sqlite3";
import session from "express-session";
const connectSqlite = require("connect-sqlite3");
import {
  databaseConnect,
  databaseDisconnect,
  asyncCheckPassword,
  asyncChangePassword,
  dbName,
  asyncDbRun,
} from "./database_utils";
import path from "path";
import { IQuiz, IAnswer, IStats, IQuestionStats, IQuizRaw } from "../../../lib/lib";
import {
  saveQuiz,
  getQuizList,
  getQuiz,
  checkSolved,
  checkAnswers,
  saveStats,
  getStatsList,
  getStats,
  getLeaderboard,
  getAverageTimes,
  sanitizeQuiz,
  areAnswersValid,
  isQuizValid,
} from "./quiz";
import { destroySession, destroyAllSessions } from "./session";
import { Error, createStatusError } from "./error";

sqlite.verbose();

const sqliteStore = connectSqlite(session);

const app = express();

const csrfProtection = csurf({ cookie: true });

app.set("port", 3000);
app.set("static", path.join(__dirname, "../../../../../frontend/src/public"));
app.set("secret", "PqHUG5IjgVb8laHI3onvTIm8iJ8=");

app.use(bodyParser.json());
app.use(express.static(app.get("static")));
app.use(bodyParser.urlencoded({ extended: true }));
app.use(cookieParser(app.get("secret")));
app.use(
  session({
    secret: app.get("secret"),
    cookie: { maxAge: 12 * 60 * 60 * 1000 },
    resave: false,
    saveUninitialized: true,
    store: new sqliteStore({ db: dbName, table: "sessions" }),
  })
);
app.use(csrfProtection);

app.get("/", (req, res) => {
  res.sendFile("quiz.html", { root: app.get("static") });
});

app.get("/user-details", (req, res, next) => {
  res.setHeader("CSRF-Header", req.csrfToken());
  res.status(200).json({ user: req.session.authorised });
});

const forbidUnathorised = (req: any, res: any, next: any) => {
  if (!req.session.authorised) next(createStatusError(403, "Access forbidden."));
  else next();
};

app.post("/login", (req, res, next) => {
  databaseConnect(sqlite.OPEN_READWRITE)
    .then(async (db: sqlite.Database) => {
      const username: string = req.body.username;
      const password: string = req.body.password;

      if (username && password && (await asyncCheckPassword(db, username, password))) {
        req.session.authorised = username;
        res.sendStatus(200);
      } else {
        throw createStatusError(401, "Invalid credentials.");
      }
      await databaseDisconnect(db);
    })
    .catch((err: Error) =>
      err.message !== "User does not exist." ? next(createStatusError(401, err.message)) : next(err)
    );
});

app.post("/logout", forbidUnathorised, (req, res, next) => {
  try {
    destroySession(req.session);
    res.sendStatus(200);
  } catch (err) {
    next(err);
    return;
  }
});

app.post("/change-password", forbidUnathorised, (req, res, next) => {
  databaseConnect(sqlite.OPEN_READWRITE)
    .then(async (db: sqlite.Database) => {
      const username: string = req.session.authorised;
      const password: string = req.body.password;
      const newPassword: string = req.body.new_password;

      if (!password || !newPassword || !(await asyncCheckPassword(db, username, password)))
        throw createStatusError(401, "Password change failed.");

      await asyncChangePassword(db, username, newPassword);
      await destroyAllSessions(db, username);

      res.sendStatus(200);

      await databaseDisconnect(db);
    })
    .catch((err: Error) => next(err));
});

app.get("/quiz-list", forbidUnathorised, (req, res, next) => {
  databaseConnect(sqlite.OPEN_READONLY)
    .then(async (db: sqlite.Database) => {
      const quizList: IQuiz[] = await getQuizList(db, req.session.authorised);

      res.status(200).json({ quizes: quizList });
    })
    .catch((err: Error) => next(err));
});

app.get("/quiz/:quizId(\\d+)", forbidUnathorised, (req, res, next) => {
  const quizId: number = parseInt(req.params.quizId, 10);

  databaseConnect(sqlite.OPEN_READONLY)
    .then(async (db: sqlite.Database) => {
      if (await checkSolved(db, quizId, req.session.authorised))
        throw createStatusError(403, "Quiz has already been solved.");

      const quiz: IQuiz = await getQuiz(db, quizId, true);

      // Add quiz's start time to the current session.
      if (!req.session.startTime) req.session.startTime = [];
      req.session.startTime[quizId] = Date.now();

      await databaseDisconnect(db);
      res.setHeader("CSRF-Header", req.csrfToken());
      res.status(200).json({ quiz });
    })
    .catch((err: Error) => next(err));
});

app.post("/answers/", forbidUnathorised, (req, res, next) => {
  const username: string = req.session.authorised;
  const quizId: number = req.body.id;
  const startTime: number = req.session.startTime[quizId];
  const answers: IAnswer[] = req.body.answers;

  if (!startTime) next(createStatusError(403, "Forbidden."));
  if (!areAnswersValid(answers)) next(createStatusError(401, "Invalid answers."));

  databaseConnect(sqlite.OPEN_READWRITE)
    .then(async (db: sqlite.Database) => {
      const stats: IStats = await checkAnswers(db, quizId, startTime, answers);

      await saveStats(db, quizId, username, stats);

      await databaseDisconnect(db);
      res.status(200).json({ stats });
    })
    .catch((err: Error) => next(err));
});

app.get("/stats-list/", forbidUnathorised, (req, res, next) => {
  const username: string = req.session.authorised;

  databaseConnect(sqlite.OPEN_READONLY)
    .then(async (db: sqlite.Database) => {
      const stats: IStats[] = await getStatsList(db, username);

      await databaseDisconnect(db);
      res.status(200).json({ stats });
    })
    .catch((err: Error) => next(err));
});

app.get("/stats/:quizId(\\d+)", forbidUnathorised, (req, res, next) => {
  const username: string = req.session.authorised;
  const quizId: number = parseInt(req.params.quizId, 10);

  databaseConnect(sqlite.OPEN_READONLY)
    .then(async (db: sqlite.Database) => {
      const stats: IStats = await getStats(db, quizId, username);

      res.status(200).json({ stats });
    })
    .catch((err: Error) => next(err));
});

app.get("/leaderboard/:quizId(\\d+)", forbidUnathorised, (req, res, next) => {
  const quizId: number = parseInt(req.params.quizId, 10);

  databaseConnect(sqlite.OPEN_READONLY)
    .then(async (db: sqlite.Database) => {
      const leaderboard: IStats[] = await getLeaderboard(db, quizId);

      await databaseDisconnect(db);
      res.status(200).json({ leaderboard });
    })
    .catch((err: Error) => next(err));
});

app.get("/times/:quizId(\\d+)", forbidUnathorised, (req, res, next) => {
  const quizId: number = parseInt(req.params.quizId, 10);

  databaseConnect(sqlite.OPEN_READONLY)
    .then(async (db: sqlite.Database) => {
      const times: IQuestionStats[] = await getAverageTimes(db, quizId);

      await databaseDisconnect(db);
      res.status(200).json({ times });
    })
    .catch((err: Error) => next(err));
});

app.post("/quiz-upload/", forbidUnathorised, (req, res, next) => {
  try {
    if (!isQuizValid(JSON.parse(req.body.quiz))) throw new Error();
  } catch {
    next(createStatusError(401, "Invalid quiz."));
    return;
  }

  databaseConnect(sqlite.OPEN_READWRITE)
    .then(async (db: sqlite.Database) => {
      let quiz: IQuiz;

      try {
        quiz = sanitizeQuiz(JSON.parse(req.body.quiz));
      } catch (err) {
        throw createStatusError(403, "Invalid quiz.");
      }

      try {
        await saveQuiz(db, quiz);
      } catch (err) {
        await asyncDbRun(db, `ROLLBACK`);
        throw createStatusError(500, "Quiz upload failed.");
      }

      await databaseDisconnect(db);
      res.sendStatus(200);
    })
    .catch((err: Error) => next(err));
});

app.use((req, res, next) => {
  next(createStatusError(404, "NotFound"));
});

app.use((err: Error, req: any, res: any, next: any) => {
  res.locals.message = err.message;
  res.locals.error = req.app.get("env") === "development" ? err : {};

  res.status(err.status || 500).json({ message: err.message });
});

const server = app.listen(app.get("port"));
server.on("error", onError);
server.on("listening", onListening);

function onError(error: Error) {
  if (error.syscall !== "listen") {
    throw error;
  }

  const bind: string = "Port: " + String(app.get("port"));

  switch (error.code) {
    case "EACCES":
      console.error(bind + " requires elevated privileges");
      process.exit(1);
    case "EADDRINUSE":
      console.error(bind + " is already in use");
      process.exit(1);
    default:
      throw error;
  }
}

function onListening() {
  console.debug(`Server listening at http://localhost:3000/ in ${app.get("env")} mode`);
}
