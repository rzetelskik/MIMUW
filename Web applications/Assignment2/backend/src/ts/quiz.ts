import * as sqlite from "sqlite3";
import { asyncDbRun, asyncDbAll, asyncDbGet } from "./database_utils";
import { IQuestion, IQuiz, IAnswer, IQuestionStats, IStats, IQuizRaw, IQuestionRaw } from "../../../lib/lib";
const sanitizeHtml = require("sanitize-html");
import { createStatusError } from "./error";

const saveQuestion = async (
  db: sqlite.Database,
  quizId: number,
  question: IQuestion,
  questionNumber: number
): Promise<void> => {
  await asyncDbRun(db, `INSERT INTO questions (quiz_id, number, question, answer, penalty) VALUES (?, ?, ?, ?, ?)`, [
    quizId,
    questionNumber,
    question.question,
    question.answer,
    question.penalty,
  ]);
};

const isQuestionValid = (question: any): question is IQuestionRaw => {
  return !(
    !("question" in question) ||
    !("answer" in question) ||
    !("penalty" in question) ||
    !(typeof question.question === "string") ||
    !(typeof question.answer === "number") ||
    !(typeof question.penalty === "number") ||
    !question.question.trim()
  );
};

export const isQuizValid = (quiz: any): quiz is IQuizRaw => {
  if (
    !("id" in quiz) ||
    !("introduction" in quiz) ||
    !("questions" in quiz) ||
    !(typeof quiz.id === "number") ||
    !(typeof quiz.introduction === "string" || !(quiz.questions instanceof Array) || !quiz.questions.length)
  )
    return false;

  for (const question of quiz.questions) {
    if (!isQuestionValid(question)) return false;
  }

  return true;
};

export const saveQuiz = async (db: sqlite.Database, quiz: IQuiz) => {
  await asyncDbRun(db, `BEGIN TRANSACTION`);
  const id: number = await asyncDbRun(db, `INSERT INTO quizes (id, introduction) VALUES (?, ?)`, [
    quiz.id,
    quiz.introduction,
  ]);

  for (const [i, q] of quiz.questions.entries()) {
    await saveQuestion(db, id, q, i + 1);
  }
  await asyncDbRun(db, `COMMIT`);
};

const sanitizeQuestions = (questions: IQuestionRaw[]): IQuestion[] => {
  if (!questions.length) throw new Error("Quiz needs at least one question.");

  return new Array<IQuestion>(questions.length).fill(undefined).map((_, i) => ({
    number: i + 1,
    question: sanitizeHtml(questions[i].question.trim(), { allowedTags: [], allowedAttributes: {} }),
    answer: questions[i].answer,
    penalty: questions[i].penalty,
  }));
};

export const sanitizeQuiz = (quiz: IQuizRaw): IQuiz => {
  return {
    id: quiz.id,
    introduction: sanitizeHtml(quiz.introduction.trim(), { allowedTags: [], allowedAttributes: {} }),
    questions: sanitizeQuestions(quiz.questions),
  };
};

export const getQuizList = async (db: sqlite.Database, user: string): Promise<IQuiz[]> => {
  return asyncDbAll(
    db,
    `SELECT id, introduction FROM quizes LEFT JOIN (SELECT quiz_id, username FROM quiz_stats WHERE username = ?) quiz_stats ON id = quiz_id WHERE quiz_id IS NULL`,
    [user]
  );
};

export const getQuiz = async (db: sqlite.Database, id: number, forClient: boolean): Promise<IQuiz> => {
  const quiz: IQuiz = await asyncDbGet(db, `SELECT * FROM quizes WHERE id = ?`, [id]);
  if (!quiz) throw new Error("Quiz does not exist.");

  quiz.questions = await asyncDbAll(
    db,
    `SELECT number, question` +
      (forClient ? `` : `, answer`) +
      `, penalty FROM questions WHERE quiz_id = ? ORDER BY number ASC`,
    [id]
  );

  return quiz;
};

export const checkSolved = async (db: sqlite.Database, quizId: number, user: string): Promise<boolean> => {
  return (
    (await asyncDbGet(db, `SELECT * FROM quiz_stats WHERE quiz_id = ? AND username = ?`, [quizId, user])) !== undefined
  );
};

export const areAnswersValid = (answers: any[]): answers is IAnswer[] => {
  for (const answer of answers) {
    if (
      !("answer" in answer) ||
      !("questionNumber" in answer) ||
      !("timePercentage" in answer) ||
      !(typeof answer.answer === "number") ||
      !(typeof answer.questionNumber === "number") ||
      !(typeof answer.timePercentage === "number")
    )
      return false;
  }

  return true;
};

export const checkAnswers = async (
  db: sqlite.Database,
  quizId: number,
  startTime: number,
  answers: IAnswer[]
): Promise<IStats> => {
  const quiz: IQuiz = await getQuiz(db, quizId, false);
  const time: number = Math.round((Date.now() - startTime) / 1000);
  let penalty: number = 0;

  answers = answers
    .sort((a, b) => a.questionNumber - b.questionNumber)
    .filter((answer, i) => answer.questionNumber === i + 1);
  if (answers.length !== quiz.questions.length) throw new Error("Invalid answers.");

  const results: IQuestionStats[] = new Array<IQuestionStats>(quiz.questions.length).fill(undefined).map((_, i) => ({
    questionNumber: i + 1,
    correct: quiz.questions[i].answer === answers[i].answer,
    time: Math.round((answers[i].timePercentage / 100) * time),
  }));

  results.forEach((el, i) => {
    if (!el.correct) penalty += quiz.questions[i].penalty;
  });

  return { time, penalty, questionStats: results };
};

const saveQuestionStat = async (
  db: sqlite.Database,
  quizId: number,
  user: string,
  questionStat: IQuestionStats
): Promise<void> => {
  await asyncDbRun(
    db,
    `INSERT INTO user_answers (quiz_id, username, question_number, time, correct) VALUES(?, ?, ?, ?, ?)`,
    [quizId, user, questionStat.questionNumber, questionStat.time, questionStat.correct]
  );
};

export const saveStats = async (db: sqlite.Database, quizId: number, user: string, stats: IStats): Promise<void> => {
  await asyncDbRun(db, `BEGIN TRANSACTION`);
  try {
    await asyncDbRun(db, `INSERT INTO quiz_stats (quiz_id, username, time, penalty) VALUES(?, ?, ?, ?)`, [
      quizId,
      user,
      stats.time,
      stats.penalty,
    ]);

    stats.questionStats.forEach(async (el: IQuestionStats) => await saveQuestionStat(db, quizId, user, el));
    await asyncDbRun(db, `COMMIT`);
  } catch (err) {
    await asyncDbRun(db, `ROLLBACK`);
    throw err.errno === 19 ? createStatusError(403, "Quiz has already been solved.") : err;
  }
};

export const getStatsList = async (db: sqlite.Database, user: string): Promise<IStats[]> => {
  return await asyncDbAll(db, `SELECT quiz_id, time, penalty FROM quiz_stats WHERE username = ?`, [user]);
};

export const getStats = async (db: sqlite.Database, quizId: number, user: string): Promise<IStats> => {
  const stats: IStats = await asyncDbGet(
    db,
    `SELECT time, penalty FROM quiz_stats WHERE quiz_id = ? AND username = ?`,
    [quizId, user]
  );

  if (!stats) throw new Error("Quiz has not been solved yet.");

  stats.questionStats = await asyncDbAll(
    db,
    `SELECT question_number as questionNumber, answer, correct, time, CASE correct WHEN 1 THEN 0 ELSE penalty END penalty
     FROM user_answers JOIN questions
     ON
     user_answers.quiz_id=questions.quiz_id
     AND
     user_answers.question_number = questions.number
     WHERE
     username = ?
     AND
     questions.quiz_id = ?`,
    [user, quizId]
  );

  return stats;
};

export const getLeaderboard = async (db: sqlite.Database, quizId: number): Promise<IStats[]> => {
  return await asyncDbAll(
    db,
    `SELECT username, time, penalty FROM quiz_stats WHERE quiz_id = ? ORDER BY time + penalty ASC LIMIT 5`,
    [quizId]
  );
};

export const getAverageTimes = async (db: sqlite.Database, quizId: number): Promise<IQuestionStats[]> => {
  return await asyncDbAll(
    db,
    `SELECT question_number as questionNumber, ROUND(AVG(time)) time FROM user_answers WHERE quiz_id = ? GROUP BY question_number`,
    [quizId]
  );
};
