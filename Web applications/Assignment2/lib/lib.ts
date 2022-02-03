export interface IQuestion {
  number: number;
  question: string;
  answer?: number;
  penalty?: number;
}

export interface IQuiz {
  id: number;
  introduction: string;
  questions: IQuestion[];
  solved?: boolean;
}

export interface IQuestionRaw {
  question: string;
  answer: number;
  penalty: number;
}

export interface IQuizRaw {
  id: number;
  introduction: string;
  questions: IQuestionRaw[];
}

export interface IAnswer {
  questionNumber: number;
  timePercentage: number;
  answer: number | null;
}

export interface IQuestionStats {
  questionNumber: number;
  answer?: string;
  correct?: boolean;
  time: number;
  penalty?: number;
}

export interface IStats {
  quiz_id?: number;
  username?: string;
  time: number;
  penalty: number;
  questionStats?: IQuestionStats[];
}