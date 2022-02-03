import * as sqlite from "sqlite3";
import { databaseConnect, databaseDisconnect, asyncDbRun, asyncCreateUser } from "./database_utils";
import { saveQuiz } from "./quiz";

const demoQuizList = [
  `{
    "id": 1,
    "introduction": "Hi! This is a simple demo quiz. In order to finish it, you have to answer each question. \
    Only integers are allowed. Every wrong answer results in a penalty added on top of your total time. \
    The lower your end result is, the better.",
    "questions": [
      {"question": "2 + 2", "answer": 4, "penalty": 20},
      {"question": "3 + 3 * 3", "answer": 12, "penalty": 5},
      {"question": "-12 * 4", "answer": -48, "penalty": 2},
      {"question": "1 + 2", "answer": 3, "penalty": 30}
    ]
  }`,
  `{"id": 2,
    "introduction": "Another demo test.",
    "questions": [
      {"question": "1 + 1", "answer": 2, "penalty": 2},
      {"question": "2 + 2", "answer": 4, "penalty": 4}
    ]}`,
];

// tslint:disable-next-line: no-bitwise
databaseConnect(sqlite.OPEN_CREATE | sqlite.OPEN_READWRITE)
  .then(async (db: sqlite.Database) => {
    await asyncDbRun(
      db,
      `CREATE TABLE IF NOT EXISTS users (
        username TEXT PRIMARY KEY,
        password TEXT NOT NULL
      )`
    );

    await asyncDbRun(
      db,
      `CREATE TABLE IF NOT EXISTS quizes (
        id NUMBER PRIMARY KEY NOT NULL,
        introduction TEXT NOT NULL
      )`
    );

    await asyncDbRun(
      db,
      `CREATE TABLE IF NOT EXISTS questions (
        quiz_id INTEGER NOT NULL,
        number INTEGER NOT NULL,
        question TEXT NOT NULL,
        answer INTEGER NOT NULL,
        penalty INTEGER NOT NULL,
        PRIMARY KEY(quiz_id, number),
        FOREIGN KEY(quiz_id) REFERENCES quizes(id)
      )`
    );

    await asyncDbRun(
      db,
      `CREATE TABLE IF NOT EXISTS quiz_stats (
        quiz_id INTEGER NOT NULL,
        username TEXT NOT NULL,
        time INTEGER NOT NULL,
        penalty INTEGER NOT NULL,
        PRIMARY KEY(quiz_id, username),
        FOREIGN KEY(quiz_id) REFERENCES quizes(id),
        FOREIGN KEY(username) REFERENCES users(username)
      )`
    );

    await asyncDbRun(
      db,
      `CREATE TABLE IF NOT EXISTS user_answers(
        quiz_id INTEGER NOT NULL,
        username TEXT NOT NULL,
        question_number INTEGER NOT NULL,
        time INTEGER NOT NULL,
        correct INTEGER NOT NULL,
        PRIMARY KEY(username, quiz_id, question_number),
        FOREIGN KEY(username) REFERENCES users(username),
        FOREIGN KEY(quiz_id) REFERENCES quizes(id),
        FOREIGN KEY(question_number) REFERENCES questions(number)
      )`
    );

    await asyncCreateUser(db, "user1", "user1");
    await asyncCreateUser(db, "user2", "user2");
    for (const quiz of demoQuizList) await saveQuiz(db, JSON.parse(quiz));

    await databaseDisconnect(db);

    console.log("Database created successfully.");
  })
  .catch((err: Error) => {
    console.error(err);
  });
